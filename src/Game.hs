{-# LANGUAGE LambdaCase #-}
-- | Core game state and manipulation for the text adventure engine
module Game where

import Types
import Data.List (intercalate, find, elemIndex, foldl', isPrefixOf, isInfixOf, nub, stripPrefix)
import Data.Bits (shiftR)
import Data.Char (toLower, isDigit, isSpace)
import Data.Maybe (listToMaybe, fromMaybe)
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Word (Word64)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable
import Ansi (stripAnsi)

-- | Default empty game world
emptyGameWorld :: GameWorld
emptyGameWorld = GameWorld
    { rooms              = Map.empty
    , itemDefs           = Map.empty
    , npcDefs            = Map.empty
    , entityInteractions = Map.empty
    , itemInteractions   = Map.empty
    , questDefs          = Map.empty
    , vehicleDefs        = Map.empty
    , verbDefs           = Map.empty
    , varDefs            = Map.empty
    , triggerDefs        = []
    , combatProfile      = CombatClassic Nothing
    , worldName          = ""
    , worldGamePolicy    = defaultGamePolicy
    , abilities          = Map.empty
    , worldEndArt        = Map.empty
    , worldTitleArt      = emptyAscii
    , worldClips         = Map.empty
    , cardDefs           = Map.empty
    , sandboxZones       = Map.empty
    }

-- | Default empty game state
emptyGameState :: GameState
emptyGameState = GameState
    { world = emptyGameWorld
    , save = SaveState
        { player             = Player 100 100 10 5 Map.empty
        , currentRoom        = "start"
        , inventory          = []
        , itemStates         = Map.empty
        , npcStates          = Map.empty
        , entityStates       = Map.empty
        , flags              = Map.empty
        , turnCount          = 0
        , gameOver           = False
        , gameOverReason     = Nothing
        , visitedRooms       = Set.empty
        , equipment          = Map.empty
        , conditions         = Map.empty
        , activeQuests       = Map.empty
        , completedQuests    = Set.empty
        , vehicleStates      = Map.empty
        , currentVehicle     = Nothing
        , activeDialogue     = Nothing
        , rngState           = initialRngState
        , variables          = Map.empty
        , triggerStates      = Map.empty
        , exitOverrides      = Map.empty
        , deckState          = Nothing
        , dynamicRooms       = Map.empty
        }
    , pendingNarrative = Nothing
    , pendingAnimation = Nothing
    , pendingCutscene = Nothing
    , diagnostics = []
    }

-- ---------------------------------------------------------------------------
-- Lookups
-- ---------------------------------------------------------------------------

-- | Helper to look up a room by RoomID: checks dynamicRooms in SaveState first,
-- then falls back to static rooms in GameWorld.
lookupRoom :: RoomID -> GameState -> Maybe Room
lookupRoom rId st =
    Map.lookup rId (dynamicRooms (save st))
    <|> Map.lookup rId (rooms (world st))

-- | Helper to get current room from game state (ensures dynamic/sandbox room exists)
getCurrentRoom :: GameState -> Maybe Room
getCurrentRoom state =
    let cur = currentRoom (save state)
        stEnsured = ensureRoomExists cur "" North state
    in lookupRoom cur stEnsured

-- | Seed for procedural generation (defaults to initialRngState or world.seed var).
getWorldSeed :: GameState -> Word64
getWorldSeed st = case Map.lookup "world.seed" (variables (save st)) of
    Just (VVInt s) -> fromIntegral s
    _              -> initialRngState

-- | Pick a biome template using weighted random roll from seed.
pickBiome :: [BiomeTemplate] -> Word64 -> Maybe BiomeTemplate
pickBiome [] _ = Nothing
pickBiome [b] _ = Just b
pickBiome biomes seed =
    let validBiomes = filter (\b -> btWeight b > 0) biomes
    in case validBiomes of
        [] -> listToMaybe biomes
        [b] -> Just b
        bs  ->
            let totalWeight = sum [ btWeight b | b <- bs ]
                roll = fromIntegral (seed `mod` fromIntegral totalWeight)
                go [] _ = head bs
                go (b:rest) acc
                    | acc + btWeight b > roll = b
                    | otherwise = go rest (acc + btWeight b)
            in Just (go bs 0)

-- | Pure substring replacement.
replaceSubstr :: String -> String -> String -> String
replaceSubstr _ _ [] = []
replaceSubstr needle repl haystack@(c:cs)
    | null needle = haystack
    | needle `isPrefixOf` haystack = repl ++ replaceSubstr needle repl (drop (length needle) haystack)
    | otherwise = c : replaceSubstr needle repl cs

-- | Replace coordinate placeholders {x}, {y}, {z} in string.
replaceCoords :: String -> Int -> Int -> Int -> String
replaceCoords pat x y z =
    let s1 = replaceSubstr "{x}" (show x) pat
        s2 = replaceSubstr "{y}" (show y) s1
    in replaceSubstr "{z}" (show z) s2

-- | Replace coordinate placeholders in CondText.
replaceCondTextCoords :: CondText -> Int -> Int -> Int -> CondText
replaceCondTextCoords ct x y z =
    CondText
        { ctDefault = replaceCoords (ctDefault ct) x y z
        , ctVariants = map (\v -> v { tvText = replaceCoords (tvText v) x y z }) (ctVariants ct)
        }

-- | Canonicalize a room ID against known sandbox zones.
canonicalRoomId :: GameWorld -> RoomID -> RoomID
canonicalRoomId gw rId =
    case Map.lookup rId (sandboxZones gw) of
        Just sz -> let (ox, oy, oz) = szOrigin sz in sandboxRoomId rId ox oy oz
        Nothing -> case stripPrefix "sandbox_" rId of
            Just zone | Map.member zone (sandboxZones gw) && not (isCoordFormat zone rId) ->
                let (ox, oy, oz) = szOrigin (sandboxZones gw Map.! zone)
                in sandboxRoomId zone ox oy oz
            _ -> rId
  where
    isCoordFormat z str = case parseSandboxRoomId str of
        Just (z', _, _, _) -> z' == z
        Nothing            -> False

-- | Default biome template fallback.
defaultBiomeTemplate :: String -> BiomeTemplate
defaultBiomeTemplate zone = BiomeTemplate
    { btId = "wilderness"
    , btWeight = 1
    , btNamePattern = "Wildnis [{x}, {y}]"
    , btDescription = plainText "Unberuehrte Wildnis erstreckt sich in alle Richtungen."
    , btTags = ["outdoor", "wilderness", zone]
    , btAsciiArt = emptyAscii
    , btPassableDirs = [North, South, East, West]
    }

-- | Generate a dynamic sandbox cell and insert it into dynamicRooms.
generateSandboxRoom :: SandboxZone -> Int -> Int -> Int -> RoomID -> Direction -> GameState -> (Room, GameState)
generateSandboxRoom sz x y z fromRoom fromDir st =
    let zone = szId sz
        rId = sandboxRoomId zone x y z
        baseSeed = getWorldSeed st
        cellSeed = deriveCellSeed baseSeed zone x y z
        mBiome = pickBiome (szBiomes sz) cellSeed
        biome = fromMaybe (defaultBiomeTemplate zone) mBiome
        rName = replaceCoords (btNamePattern biome) x y z
        rDesc = replaceCondTextCoords (btDescription biome) x y z
        baseExits = Map.fromList
            [ (dir, Open (sandboxRoomId zone (x + dx) (y + dy) (z + dz)))
            | dir <- btPassableDirs biome
            , let (dx, dy, dz) = directionDelta dir
            ]
        finalExits = if null fromRoom
                     then baseExits
                     else Map.insert (oppositeDirection fromDir) (Open fromRoom) baseExits
        room = Room
            { roomId            = rId
            , roomName          = rName
            , roomDescription   = rDesc
            , roomConnections   = finalExits
            , roomTags          = Set.fromList ("sandbox" : zone : btTags biome)
            , roomLightFlag     = Nothing
            , roomOnEnter       = Nothing
            , roomOnLook        = Nothing
            , roomOnExit        = Nothing
            , roomSearchOutcome = Nothing
            , roomAscii         = btAsciiArt biome
            , roomIntro         = Nothing
            , roomFloor         = szDefaultFloor sz <|> Just 1
            }
        newDyn = Map.insert rId room (dynamicRooms (save st))
        st' = st { save = (save st) { dynamicRooms = newDyn } }
    in (room, st')

-- | Ensure that a room exists; if it is an ungenerated sandbox cell, instantiates it.
ensureRoomExists :: RoomID -> RoomID -> Direction -> GameState -> GameState
ensureRoomExists dest fromRoom fromDir st =
    case lookupRoom dest st of
        Just _  -> st
        Nothing -> case parseSandboxRoomId dest of
            Just (zone, x, y, z) ->
                case Map.lookup zone (sandboxZones (world st)) of
                    Just sz -> snd (generateSandboxRoom sz x y z fromRoom fromDir st)
                    Nothing -> st
            Nothing -> st

-- | Record an engine-level diagnostic (P2-23). Runtime-only: `GameState` has no
--   JSON instance, so nothing serializes it. Diagnostics describe a *content*
--   error the author has to fix (e.g. an effect nested past `maxOutcomeDepth`)
--   and must never be mixed into the player's text.
addDiagnostic :: String -> GameState -> GameState
addDiagnostic msg st = st { diagnostics = diagnostics st ++ [msg] }

-- | Get all visible items in a location.
--   Hidden items only appear once they have been discovered via `search`.
--
--   This scans the whole `itemStates` map, and a command calls it 2-3 times
--   (room + inventory) plus `getNPCsInRoom`. Review P2-12 suggested a
--   `Map RoomID [ItemID]` index in the state; that was **rejected** (decision,
--   Cluster E): `GameState` is serialized, so an index is a second source of
--   truth that has to stay consistent across every item move and every
--   save/load — a silently wrong index loses items. Measured cost of the
--   complete per-command scan work at TheFog scale (50 items, 50 triggers):
--   12 ns; at 100x that size (5000/5000): 130 ns. Not worth that risk.
getItemsInLocation :: Location -> GameState -> [ItemDef]
getItemsInLocation loc state =
    [ def
    | (iId, st) <- Map.toList (itemStates (save state))
    , itemLocation st == loc
    , Just def <- [Map.lookup iId (itemDefs (world state))]
    , not (itemHidden def) || itemDiscovered st
    ]

-- | All items in a location, including hidden ones (used internally)
getAllItemsInLocation :: Location -> GameState -> [ItemDef]
getAllItemsInLocation loc state =
    let itemIds = Map.keys $ Map.filter (\s -> itemLocation s == loc) (itemStates (save state))
    in [def | iId <- itemIds, Just def <- [Map.lookup iId (itemDefs (world state))]]

-- | Check if player has an item in inventory
hasItem :: ItemID -> GameState -> Bool
hasItem iId state = case Map.lookup iId (itemStates (save state)) of
    Just itemState -> itemLocation itemState == CarriedBy "player"
    Nothing        -> False

-- | Check if an item is currently equipped
isEquipped :: ItemID -> GameState -> Bool
isEquipped iId state = iId `elem` Map.elems (equipment (save state))

-- | Item currently equipped in a slot
equippedInSlot :: EquipSlot -> GameState -> Maybe ItemID
equippedInSlot slot state = Map.lookup slot (equipment (save state))

syncInventory :: SaveState -> SaveState
syncInventory saveState =
    saveState
        { inventory =
            Map.keys
                (Map.filter (\itemState -> itemLocation itemState == CarriedBy "player") (itemStates saveState))
        }

-- ---------------------------------------------------------------------------
-- Visited rooms
-- ---------------------------------------------------------------------------

-- | Has the player been in this room before?
isRoomVisited :: RoomID -> GameState -> Bool
isRoomVisited rId state = rId `Set.member` visitedRooms (save state)

-- | Mark a room visited / unvisited (lives in SaveState, not in Room)
setRoomVisited :: RoomID -> Bool -> GameState -> GameState
setRoomVisited rId visited state = state
    { save = (save state)
        { visitedRooms = (if visited then Set.insert else Set.delete) rId (visitedRooms (save state)) }
    }

-- | Mark the current room as visited
markCurrentRoomVisited :: GameState -> GameState
markCurrentRoomVisited state = setRoomVisited (currentRoom (save state)) True state

-- ---------------------------------------------------------------------------
-- Movement
-- ---------------------------------------------------------------------------

-- | Move player to a different room
moveToRoom :: RoomID -> GameState -> GameState
moveToRoom destinationRoom state = state { save = (save state) { currentRoom = destinationRoom } }

-- | Rogue Phase 3 (M4): the single runtime lookup for a room's exits — the
--   static 'roomConnections' overlaid by 'exitOverrides'. Every consumer of
--   dynamic exits (movement, door aliases in the parser, tab completion)
--   goes through this function, so `SetExit`/`RemoveExit` are visible
--   everywhere at once. `Just exit` replaces, `Nothing` removes — even a
--   statically present connection.
effectiveConnections :: GameState -> RoomID -> Map.Map Direction Exit
effectiveConnections state rId =
    case lookupRoom rId state of
        Nothing   -> Map.empty
        Just room ->
            let overrides = Map.filterWithKey
                                (\(r, _) _ -> r == rId)
                                (exitOverrides (save state))
                connsWithCanon = Map.map canonExit (roomConnections room)
            in Map.foldlWithKey step connsWithCanon overrides
  where
    canonExit (Open to)     = Open (canonicalRoomId (world state) to)
    canonExit (Locked to k) = Locked (canonicalRoomId (world state) to) k
    step acc (_, dir) (Just exit) = Map.insert dir (canonExit exit) acc
    step acc (_, dir) Nothing     = Map.delete dir acc

-- | Check if a direction is valid from current room (static + dynamic exits)
canMove :: Direction -> GameState -> Bool
canMove dir state = case getCurrentRoom state of
    Just room -> dir `Map.member` effectiveConnections state (roomId room)
    Nothing   -> False

-- | Get exit in a given direction (static + dynamic exits)
getExitInDirection :: Direction -> GameState -> Maybe Exit
getExitInDirection dir state = case getCurrentRoom state of
    Just room -> Map.lookup dir (effectiveConnections state (roomId room))
    Nothing   -> Nothing

-- ---------------------------------------------------------------------------
-- Items
-- ---------------------------------------------------------------------------

-- | Add item to player's inventory
pickupItem :: ItemID -> GameState -> GameState
pickupItem iId state = relocateItem iId (CarriedBy "player") state

-- | Remove item from player's inventory to current room
dropItem :: ItemID -> GameState -> GameState
dropItem iId state = relocateItem iId (InRoom (currentRoom (save state))) state

-- | Give item directly to player inventory (e.g., NPC reward, loot)
giveItem :: ItemID -> GameState -> GameState
giveItem iId state = relocateItem iId (CarriedBy "player") state

-- | Move an item to a specific room (e.g., loot drop)
moveItemToRoom :: ItemID -> RoomID -> GameState -> GameState
moveItemToRoom iId targetRoom state = relocateItem iId (InRoom targetRoom) state

-- | Consume an item, removing it from play entirely
consumeItem :: ItemID -> GameState -> GameState
consumeItem iId state = relocateItem iId Removed state

-- | Central relocation: move an item to a new location and keep inventory
--   and equipment consistent.  If the item was equipped it is unequipped
--   automatically (dropping/consuming a worn item must not keep its bonuses),
--   and health is clamped to the new effective max afterwards.
relocateItem :: ItemID -> Location -> GameState -> GameState
relocateItem iId newLoc state =
    let saveState = save state
        updatedSave = saveState
            { itemStates = Map.adjust (\s -> s { itemLocation = newLoc }) iId (itemStates saveState)
            , equipment  = Map.filter (/= iId) (equipment saveState)
            }
        st' = state { save = syncInventory updatedSave }
    in clampHealthToMax st'

-- | Clamp current health to effective max health (e.g. after losing a maxhp bonus)
clampHealthToMax :: GameState -> GameState
clampHealthToMax state =
    let p = player (save state)
        newHealth = min (playerHealth p) (effectiveMaxHealth state)
    in state { save = (save state) { player = p { playerHealth = newHealth } } }

-- | Modify an item's property
modifyItemProp :: String -> String -> Int -> GameState -> GameState
modifyItemProp iId prop delta state = state
    { save = (save state) { itemStates = Map.adjust (\s ->
        let currentVal = Map.findWithDefault 0 prop (itemProps s)
        in s { itemProps = Map.insert prop (currentVal + delta) (itemProps s) }
        ) iId (itemStates (save state)) } }

-- | Reveal a hidden item
discoverItem :: ItemID -> GameState -> GameState
discoverItem iId state = state
    { save = (save state) { itemStates = Map.adjust (\s -> s { itemDiscovered = True }) iId (itemStates (save state)) } }

-- ---------------------------------------------------------------------------
-- Equipment
-- ---------------------------------------------------------------------------

-- | Look up the ItemDef for an item
lookupItem :: ItemID -> GameState -> Maybe ItemDef
lookupItem iId state = Map.lookup iId (itemDefs (world state))

-- | Equip an item the player is carrying.
--   Fails if the item is not carried, not equippable, or the slot is occupied.
equipItem :: ItemID -> GameState -> Either String GameState
equipItem iId state =
    case lookupItem iId state of
        Nothing -> Left ("There is no item '" ++ iId ++ "'.")
        Just def -> case itemEquipSlot def of
            Nothing -> Left ("You cannot equip the " ++ itemName def ++ ".")
            Just slot ->
                if not (hasItem iId state)
                then Left ("You need to be carrying the " ++ itemName def ++ ".")
                else case equippedInSlot slot state of
                    Just other | other /= iId ->
                        Left ("You already have the " ++ other ++ " equipped there. Unequip it first.")
                    _ -> Right $ state { save = (save state) { equipment = Map.insert slot iId (equipment (save state)) } }

-- | Unequip an item by id
unequipItem :: ItemID -> GameState -> GameState
unequipItem iId state =
    let newEquip = Map.filter (/= iId) (equipment (save state))
    in state { save = (save state) { equipment = newEquip } }

-- | Unequip whatever occupies a slot
unequipSlot :: EquipSlot -> GameState -> GameState
unequipSlot slot state = state { save = (save state) { equipment = Map.delete slot (equipment (save state)) } }

-- | Sum a numeric projection over all equipped items
sumEquipBonus :: (EquipEffect -> Maybe Int) -> GameState -> Int
sumEquipBonus f state =
    sum [ v
        | slotItem <- Map.elems (equipment (save state))
        , Just def <- [lookupItem slotItem state]
        , eff <- itemEquipEffects def
        , Just v <- [f eff]
        ]

-- | Effective attack = base + equipment bonuses
effectiveAttack :: GameState -> Int
effectiveAttack state = playerAttack (player (save state)) + sumEquipBonus attackOf state
  where
    attackOf (AttackBonus n) = Just n
    attackOf _               = Nothing

-- | Effective defense = base + equipment bonuses
effectiveDefense :: GameState -> Int
effectiveDefense state = playerDefense (player (save state)) + sumEquipBonus defenseOf state
  where
    defenseOf (DefenseBonus n) = Just n
    defenseOf _                = Nothing

-- | Effective max health = base + equipment bonuses
effectiveMaxHealth :: GameState -> Int
effectiveMaxHealth state = playerMaxHealth (player (save state)) + sumEquipBonus hpOf state
  where
    hpOf (MaxHealthBonus n) = Just n
    hpOf _                  = Nothing

-- | Human-readable equipment summary
equipmentSummary :: GameState -> String
equipmentSummary state
    | Map.null (equipment (save state)) = "You have nothing equipped."
    | otherwise =
        "Equipment:\n" ++ unlines
            [ "  " ++ show slot ++ ": " ++ nameFor iId
            | (slot, iId) <- Map.toList (equipment (save state))
            ]
  where
    nameFor iId = maybe iId itemName (lookupItem iId state)

-- ---------------------------------------------------------------------------
-- Entities
-- ---------------------------------------------------------------------------

-- | Get entity state
getEntityState :: String -> GameState -> Maybe String
getEntityState entity state = Map.lookup entity (entityStates (save state))

-- | Set entity state
setEntityState :: String -> String -> GameState -> GameState
setEntityState entity val state = state { save = (save state) { entityStates = Map.insert entity val (entityStates (save state)) } }

-- ---------------------------------------------------------------------------
-- Player
-- ---------------------------------------------------------------------------

-- | Update player health
updatePlayerHealth :: (Int -> Int) -> GameState -> GameState
updatePlayerHealth f state =
    let p = player (save state)
        newHealth = max 0 (min (effectiveMaxHealth state) (f (playerHealth p)))
    in state { save = (save state) { player = p { playerHealth = newHealth } } }

-- | Check if player is dead
isPlayerDead :: GameState -> Bool
isPlayerDead state = playerHealth (player (save state)) <= 0

-- ---------------------------------------------------------------------------
-- NPCs
-- ---------------------------------------------------------------------------

-- | Get all NPCs in a specific room
getNPCsInRoom :: RoomID -> GameState -> [NPCDef]
getNPCsInRoom rId state =
    let npcIds = Map.keys $ Map.filter (\s -> npcLocation s == InRoom rId) (npcStates (save state))
    in [def | nId <- npcIds, Just def <- [Map.lookup nId (npcDefs (world state))]]

-- | An NPC's status, when it has state at all (`Nothing` = no state recorded).
npcStatusOf :: NPCID -> GameState -> Maybe String
npcStatusOf nId state = npcStatus <$> Map.lookup nId (npcStates (save state))

-- | Is this NPC a body? The single rule for "a corpse is not a partner":
--   combat targeting, party membership and the room listing all ask this one
--   predicate, so the definition lives in exactly one place.
isDeadNPC :: NPCID -> GameState -> Bool
isDeadNPC nId state = npcStatusOf nId state == Just "dead"

-- | Update NPC state (health, status, etc)
updateNPCState :: String -> NPCState -> GameState -> GameState
updateNPCState targetNpcId newNpcState state = state
    { save = (save state) { npcStates = Map.insert targetNpcId newNpcState (npcStates (save state)) } }

-- | Mark an NPC dead and fire state-change triggers. The **corpse stays where it
--   fell**: `npcLocation` is untouched, so `look at <name>` and `watch` still
--   find it and the body's art can show its dead variant. Everything that must
--   not involve a body (combat targeting, party membership, the room's "Also
--   here" list) filters on the status instead — see `isDeadNPC`. `Removed` is
--   for consumed items, not for bodies.
--   Also unlocks any exit locked by this entity (entityStates -> "unlocked").
--   Killing an already-dead NPC is a no-op, which bounds event recursion:
--   a trigger that re-kills the same NPC cannot loop.
--   Use `killNPCWithMsg` when the death-event messages should reach the
--   player (the outcome interpreter does; it returns them with the state).
killNPC :: String -> GameState -> GameState
killNPC targetNpcId state = fst (killNPCWithMsg targetNpcId state)

-- | `killNPC`, keeping the messages of the fired `OnStateChange` trigger rules.
killNPCWithMsg :: String -> GameState -> (GameState, String)
killNPCWithMsg targetNpcId state =
    case npcStatus <$> Map.lookup targetNpcId (npcStates (save state)) of
        Just "dead" -> (state, "")
        _ ->
            let state' = state
                    { save = (save state)
                        { npcStates = Map.adjust (\s -> s { npcStatus = "dead" }) targetNpcId (npcStates (save state))
                        , entityStates = Map.insert targetNpcId "unlocked" (entityStates (save state))
                        } }
            in fireTriggers (OnStateChange targetNpcId) state'

-- | Set an entity's state and fire its `OnStateChange` event, so authored
--   `on: state <entity>` rules react to an explicit `set_state:` exactly as
--   they do to an NPC death (`killNPCWithMsg`).
--   Writing the state the entity already has is a no-op: that idempotent
--   guard IS the recursion bound for a rule whose effect re-writes the same
--   state from its own `on: state` handler.
setEntityStateWithEvents :: String -> String -> GameState -> (GameState, String)
setEntityStateWithEvents eId val state
    | getEntityState eId state == Just val = (state, "")
    | otherwise = fireTriggers (OnStateChange eId) (setEntityState eId val state)

-- | Resolve dynamic or placeholder actor identifiers (e.g. "target", "chosen", "current_target")
--   to the specific NPC ID stored in the variable "cmd.target".
resolveActorNpcId :: NPCID -> GameState -> NPCID
resolveActorNpcId nId state
    | nId `elem` ["target", "chosen", "current_target"] =
        case getVariable "cmd.target" state of
            Just (VVText s) -> s
            _               -> nId
    | otherwise = nId

-- | Modify an NPC's property
modifyNPCProp :: String -> String -> Int -> GameState -> GameState
modifyNPCProp nIdRaw prop delta state =
    let nId = resolveActorNpcId nIdRaw state
    in state
        { save = (save state) { npcStates = Map.adjust (\s ->
            let currentVal = Map.findWithDefault 0 prop (npcProps s)
            in s { npcProps = Map.insert prop (currentVal + delta) (npcProps s) }
            ) nId (npcStates (save state)) } }

-- | Move an NPC to a different room
moveNPCToRoom :: String -> RoomID -> GameState -> GameState
moveNPCToRoom nId targetRoom state = state
    { save = (save state) { npcStates = Map.adjust (\s -> s { npcLocation = InRoom targetRoom }) nId (npcStates (save state)) } }

-- ---------------------------------------------------------------------------
-- Party (Phase 7g)
-- ---------------------------------------------------------------------------

-- | The follow variable of a party member: `party.<npcId>`, 1 = following.
--   Kept in the existing VarMap, so party membership survives save/load with
--   no new state field (module rule: no state silo).
partyVariable :: NPCID -> String
partyVariable nId = "party." ++ nId

-- | Is this NPC currently following the player?
isInParty :: NPCID -> GameState -> Bool
isInParty nId state = getVariable (partyVariable nId) state == Just (VVInt 1)

-- | Living party members standing in the player's room — the combat roster
--   the 7f resolver receives. Deterministic (sorted by NPC id).
partyMembersInRoom :: GameState -> [NPCID]
partyMembersInRoom state =
    [ nId
    | (nId, ns) <- Map.toList (npcStates (save state))
    , npcStatus ns /= "dead"
    , npcLocation ns == InRoom (currentRoom (save state))
    , isInParty nId state ]

-- | Party members follow the player into `room`. Called from every place the
--   player's room changes (walking, teleport, boarding / leaving a vehicle),
--   so following needs no per-(room x npc) trigger rules.
--   Dead members stay where they fell.
followParty :: RoomID -> GameState -> GameState
followParty room state =
    foldl' (\st nId -> moveNPCToRoom nId room st) state
        [ nId
        | (nId, ns) <- Map.toList (npcStates (save state))
        , npcStatus ns /= "dead"
        , npcLocation ns /= InRoom room
        , isInParty nId state ]

-- | Set the current dialogue node for an NPC
setDialogueNode :: String -> Maybe String -> GameState -> GameState
setDialogueNode nId node state =
    let curRoom = currentRoom (save state)
        mDef = Map.lookup nId (npcDefs (world state))
        defaultSt = NPCState
            { npcLocation = InRoom curRoom
            , npcStatus = "alive"
            , npcHealth = mDef >>= npcMaxHealth
            , npcProps = Map.empty
            , npcDialogueNode = node
            }
        adjustSt s = s { npcDialogueNode = node }
    in state
        { save = (save state)
            { npcStates = Map.insertWith (\_ old -> adjustSt old) nId defaultSt (npcStates (save state)) } }

-- | Set the active dialogue partner
setActiveDialogue :: Maybe NPCID -> GameState -> GameState
setActiveDialogue mNpc state = state { save = (save state) { activeDialogue = mNpc } }

-- | Clear the active dialogue partner
clearActiveDialogue :: GameState -> GameState
clearActiveDialogue = setActiveDialogue Nothing

-- | Clamp NPC health to its max health from the definition
clampNPCHealth :: String -> GameState -> GameState
clampNPCHealth nId state = case Map.lookup nId (npcDefs (world state)) of
    Just nDef -> case npcMaxHealth nDef of
        Just maxHp -> state
            { save = (save state)
                { npcStates = Map.adjust (\s -> s { npcHealth = fmap (min maxHp) (npcHealth s) }) nId (npcStates (save state)) } }
        Nothing -> state
    Nothing -> state

-- ---------------------------------------------------------------------------
-- Flags & game over
-- ---------------------------------------------------------------------------

-- | Set a general-purpose flag
setFlag :: String -> String -> GameState -> GameState
setFlag flagName flagValue state = state
    { save = (save state) { flags = Map.insert flagName flagValue (flags (save state)) } }

-- | Get a general-purpose flag
getFlag :: String -> GameState -> Maybe String
getFlag flagName state = Map.lookup flagName (flags (save state))

-- | End the game with a reason
endGame :: GameOverReason -> GameState -> GameState
endGame reason state = state
    { save = (save state) { gameOver = True, gameOverReason = Just reason } }

-- | Increment the turn counter (called once per command)
incrementTurnCount :: GameState -> GameState
incrementTurnCount state = state
    { save = (save state) { turnCount = turnCount (save state) + 1 } }

-- ---------------------------------------------------------------------------
-- Combat helpers
-- ---------------------------------------------------------------------------

-- | Does the player carry or wear an item with the given tag?
playerHasTaggedItem :: String -> GameState -> Bool
playerHasTaggedItem tag state =
    any (\iId -> maybe False (Set.member tag . itemTags) (lookupItem iId state))
        (inventory (save state) ++ Map.elems (equipment (save state)))

-- | Check if a target string matches any living NPC in the room (for weapon routing)
isLivingNPCInRoom :: String -> GameState -> Bool
isLivingNPCInRoom target state =
    let currentRoomId = currentRoom (save state)
        roomNPCs = getNPCsInRoom currentRoomId state
        matchTarget npc = any (\kw -> lower target == lower kw) (npcId npc : npcName npc : npcKeywords npc)
        -- An NPC without recorded state is not living either (unchanged rule).
        npcIsAlive npc = maybe False (/= "dead") (npcStatusOf (npcId npc) state)
    in any (\npc -> matchTarget npc && npcIsAlive npc) roomNPCs
  where
    lower = map (\c -> if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c)

-- ---------------------------------------------------------------------------
-- Skills (Phase 2)
-- ---------------------------------------------------------------------------

-- | Look up a skill value (0 if the player does not have it)
getSkill :: SkillID -> GameState -> Int
getSkill skillId state = Map.findWithDefault 0 skillId (playerSkills (player (save state)))

-- | Modify a skill by a delta (creating it at 0 + delta if unknown)
modifySkill :: SkillID -> Int -> GameState -> GameState
modifySkill skillId delta state = state
    { save = (save state)
        { player = (player (save state))
            { playerSkills =
                Map.alter (\case
                    Just v -> Just (v + delta)
                    Nothing -> Just delta)
                skillId (playerSkills (player (save state))) } } }

-- ---------------------------------------------------------------------------
-- Conditions (Phase 2)
-- ---------------------------------------------------------------------------

-- | Apply (or refresh) a timed status effect
applyCondition :: String -> Int -> Maybe Effect -> Maybe Effect -> GameState -> GameState
applyCondition name turns tick end state = state
    { save = (save state)
        { conditions = Map.insert name (Condition name turns tick end) (conditions (save state)) } }

-- | Remove a status effect
clearCondition :: String -> GameState -> GameState
clearCondition name state = state
    { save = (save state) { conditions = Map.delete name (conditions (save state)) } }

-- | Is a status effect active?
hasCondition :: String -> GameState -> Bool
hasCondition name state = Map.member name (conditions (save state))

-- | Advance one turn: tick all conditions, fire tick outcomes, remove expired
--   ones and fire their end outcomes.
--   Returns the new state plus all messages produced.
tickConditions :: GameState -> (GameState, [String])
tickConditions state = foldl' step (state, []) (Map.toList (conditions (save state)))
  where
    step (st, msgs) (name, cond) =
        let remaining = condRemaining cond - 1
        in if remaining <= 0
           then let (stEnd, mEnd) = maybe (st, "") (\o -> applyOutcome o "" st) (condEndOutcome cond)
                    st' = clearCondition name stEnd
                in (st', if null mEnd then msgs else msgs ++ [mEnd])
           else let st1 = st { save = (save st) { conditions = Map.adjust (\c -> c { condRemaining = remaining }) name (conditions (save st)) } }
                    (st2, mTick) = maybe (st1, "") (\o -> applyOutcome o "" st1) (condTickOutcome cond)
                in (st2, if null mTick then msgs else msgs ++ [mTick])

-- ---------------------------------------------------------------------------
-- Variables (Phase 3b)
-- ---------------------------------------------------------------------------

-- | Read an adventure-declared variable value.
getVariable :: String -> GameState -> Maybe VariableValue
getVariable name state = Map.lookup name (variables (save state))

-- | Set an adventure-declared variable.
setVariable :: String -> VariableValue -> GameState -> GameState
setVariable name val state = state
    { save = (save state) { variables = Map.insert name val (variables (save state)) } }

-- | Clamp an integer to the min/max declared for a VTInt variable (P1-8).
clampToVarDef :: String -> Int -> GameState -> Int
clampToVarDef name v state =
    case vdVarType <$> Map.lookup name (varDefs (world state)) of
        Just (VTInt lo hi) ->
            let vLo = maybe v (\l -> max v l) lo
            in maybe vLo (\h -> min vLo h) hi
        _ -> v

-- | Set a variable, clamping int values to the declared VTInt bounds (P1-8).
setVariableChecked :: String -> VariableValue -> GameState -> GameState
setVariableChecked name val state =
    case val of
        VVInt n -> setVariable name (VVInt (clampToVarDef name n state)) state
        _       -> setVariable name val state

-- ---------------------------------------------------------------------------
-- Combat round state (Phase 7f-3, step A1)
-- ---------------------------------------------------------------------------
--
-- The round state lives in the adventure VarMap under the reserved `combat.`
-- prefix — like `faction.`, `party.` and `ship.` — instead of a new `SaveState`
-- field: no migration, and save/load works like for any other variable. The
-- worldbuilder rejects an author-declared variable in this namespace
-- (`CombatVariableClash`), because the engine owns these entries.
--
-- The state is written by the tactical resolver (A2) and read by the enemy
-- reaction, which is an ordinary `on: turn` rule gated on `combat.engaged` — an
-- engine special case would be a second code path, and 7f must not have one.
-- See `plan-7f3-tactical-7h2-shipduell.md`.

-- | VarMap prefix of the combat round state.
combatVarPrefix :: String
combatVarPrefix = "combat."

-- | Rounds fought since the current fight started (0 = not in a fight).
combatRoundKey :: String
combatRoundKey = combatVarPrefix ++ "round"

-- | 1 while a fight is being resolved, 0 otherwise. A reaction rule reads it via
--   `compare_var: { name: combat.engaged, op: gte, value: 1 }`.
combatEngagedKey :: String
combatEngagedKey = combatVarPrefix ++ "engaged"

-- | Current round of the fight (0 when absent — no declaration needed, the
--   VarMap entry is created on demand).
combatRound :: GameState -> Int
combatRound st = case getVariable combatRoundKey st of
    Just (VVInt n) -> n
    _              -> 0

setCombatRound :: Int -> GameState -> GameState
setCombatRound n = setVariable combatRoundKey (VVInt n)

-- | Whether a fight is being resolved.
isCombatEngaged :: GameState -> Bool
isCombatEngaged st = case getVariable combatEngagedKey st of
    Just (VVInt n) -> n >= 1
    _              -> False

setCombatEngaged :: Bool -> GameState -> GameState
setCombatEngaged True  = setVariable combatEngagedKey (VVInt 1)
setCombatEngaged False = setVariable combatEngagedKey (VVInt 0)

-- | The last player action inside a tactical fight ("attack", "defend",
--   "flee", "ability"). The enemy's `on: turn` rule can gate on this with the
--   text predicate `{ var: combat.action, is: defend }` — `compare_var` only
--   compares integers, so it cannot read this key.
combatActionKey :: String
combatActionKey = combatVarPrefix ++ "action"

-- | VarMap key for the player's initiative value (Phase 7f-3, step A3).
combatInitiativePlayerKey :: String
combatInitiativePlayerKey = combatVarPrefix ++ "initiative.player"

-- | VarMap key for the engaged target's initiative value (Phase 7f-3, step A3).
--   One key per target id — NPCs and ships share this space, which is safe
--   because only one target is engaged at a time and the value is rewritten
--   every round. (The former name said "Npc" while the emitted key never did.)
combatInitiativeKey :: NPCID -> String
combatInitiativeKey tid = combatVarPrefix ++ "initiative." ++ tid

-- | VarMap key for the last used ability (Phase 7f-3, step A3).
combatAbilityKey :: String
combatAbilityKey = combatVarPrefix ++ "ability"

-- | Evaluate a Predicate against the current game state.
evalPredicate :: Predicate -> GameState -> Bool
evalPredicate PTrue _ = True
evalPredicate (PNot p) st = not (evalPredicate p st)
evalPredicate (PAll ps) st = all (\p -> evalPredicate p st) ps
evalPredicate (PAny ps) st = any (\p -> evalPredicate p st) ps
evalPredicate (PlayerHas iId) st = hasItem iId st
evalPredicate (HasFlag f) st = getFlag f st == Just "true"
-- A state predicate checks the entity's state in whichever layer stores it:
-- `entityStates` (exit locks, doors, `set_state:` targets), an NPC's status
-- ("alive"/"dead", as `killNPC` and the combat path set it) or an item's status
-- ("intact"/…). Reading only `entityStates` made `{state: <npc>, is: alive}` —
-- which `starship.yaml` and `combo.yaml` both use — always false.
evalPredicate (EntityHasState entity expected) st =
    getEntityState entity st == Just expected
        || npcStateMatches
        || itemStateMatches
  where
    npcStateMatches = case Map.lookup entity (npcStates (save st)) of
        Just ns -> npcStatus ns == expected
        Nothing -> False
    itemStateMatches = case Map.lookup entity (itemStates (save st)) of
        Just is -> itemStatus is == expected
        Nothing -> False
evalPredicate (RoomHasTag rId tag) st =
    case lookupRoom rId st of
        Just room -> tag `Set.member` roomTags room
        Nothing   -> False
evalPredicate (Location eId rId) st
    | eId == "player" = currentRoom (save st) == rId
    | otherwise =
        case Map.lookup eId (npcStates (save st)) of
            Just ns  -> npcLocation ns == InRoom rId
            Nothing  -> case Map.lookup eId (itemStates (save st)) of
                Just is -> itemLocation is == InRoom rId
                Nothing -> False
evalPredicate (CompareVar name op n) st =
    case Map.lookup name (variables (save st)) of
        Just (VVInt v) -> fromMaybe False (compareValues op v n)
        _              -> False
-- A text variable equals a literal. This is the read side of `variables:` with
-- `type: text`: the value is a `VVText`, and no other predicate can inspect it
-- (`compare_var` only compares numbers, `compare` resolves text to 0). Such a
-- variable is written by the engine itself (`combat.action` / `combat.ability`,
-- which would otherwise be write-only) and seeded by `initial:` /
-- `initial_variables:`. The authored `set_var` outcome takes integers only
-- (`AOSetVar String Int`), so it cannot produce a text value.
evalPredicate (VarIs name expected) st =
    case Map.lookup name (variables (save st)) of
        Just (VVText v) -> v == expected
        _               -> False
evalPredicate (Compare lhs op rhs) st =
    let lval = resolveValueRef lhs st
        rval = resolveValueRef rhs st
    in case compareValues op lval rval of
        Just b  -> b
        Nothing -> False

-- | Resolve a ValueRef to an Int for comparisons.
resolveValueRef :: ValueRef -> GameState -> Int
resolveValueRef (VRVariable name) st =
    case Map.lookup name (variables (save st)) of
        Just (VVInt n)  -> n
        Just (VVText s) -> case reads s of [(n,"")] -> n; _ -> 0
        _               -> case name of
            "player.hp"         -> playerHealth (player (save st))
            "player.health"     -> playerHealth (player (save st))
            "player.max_hp"     -> playerMaxHealth (player (save st))
            "player.max_health" -> playerMaxHealth (player (save st))
            "turn.count"        -> turnCount (save st)
            "turns"             -> turnCount (save st)
            _                   -> 0
resolveValueRef (VRFlag f) st =
    case getFlag f st of
        Just "true"  -> 1
        Just _       -> 0
        Nothing      -> 0
resolveValueRef (VRItemProp iId prop) st =
    case Map.lookup iId (itemStates (save st)) of
        Just is -> Map.findWithDefault 0 prop (itemProps is)
        Nothing -> 0
resolveValueRef (VRActorProp ActorPlayer PHealth) st =
    playerHealth (player (save st))
resolveValueRef (VRActorProp (ActorNPC eId) PHealth) st =
    case Map.lookup (resolveActorNpcId eId st) (npcStates (save st)) of
        Just ns -> fromMaybe 0 (npcHealth ns)
        Nothing -> 0
resolveValueRef (VRActorProp (ActorNPC nId) (PCustom prop)) st =
    case Map.lookup (resolveActorNpcId nId st) (npcStates (save st)) of
        Just ns -> Map.findWithDefault 0 prop (npcProps ns)
        Nothing -> 0
resolveValueRef (VRActorProp (ActorRoom rId) PVisited) st =
    if Set.member rId (visitedRooms (save st)) then 1 else 0
resolveValueRef (VRActorProp (ActorShip vId) PHealth) st =
    case getVariable ("ship." ++ vId ++ ".hull") st of
        Just (VVInt n) -> n
        _              -> 0
resolveValueRef (VRActorProp _ _) _ = 0
resolveValueRef VRPlayerHealth st =
    playerHealth (player (save st))

-- | Compare two Int values using the comparator. Returns Nothing on invalid op
--   (same behaviour as False for unknown variables).
compareValues :: Comparator -> Int -> Int -> Maybe Bool
compareValues CEq  a b = Just (a == b)
compareValues CNeq a b = Just (a /= b)
compareValues CLt  a b = Just (a <  b)
compareValues CLte a b = Just (a <= b)
compareValues CGt  a b = Just (a >  b)
compareValues CGte a b = Just (a >= b)

-- ---------------------------------------------------------------------------
-- Arithmetic expression evaluator (Phase 1A)
-- ---------------------------------------------------------------------------

-- | Evaluate an arithmetic expression against the current game state.
--   Division and modulo by 0 return 0 safely.
evalExpr :: Expr -> GameState -> Int
evalExpr expr st = case expr of
    ELit n -> n
    EVar v -> resolveValueRef (VRVariable v) st
    EAdd a b -> evalExpr a st + evalExpr b st
    ESub a b -> evalExpr a st - evalExpr b st
    EMul a b -> evalExpr a st * evalExpr b st
    EDiv a b ->
        let d = evalExpr b st
        in if d == 0 then 0 else evalExpr a st `div` d
    EMod a b ->
        let m = evalExpr b st
        in if m == 0 then 0 else evalExpr a st `mod` m
    EMin a b -> min (evalExpr a st) (evalExpr b st)
    EMax a b -> max (evalExpr a st) (evalExpr b st)
    EClamp mn mx v ->
        let l = evalExpr mn st
            h = evalExpr mx st
            val = evalExpr v st
            low = min l h
            high = max l h
        in max low (min high val)

-- ---------------------------------------------------------------------------
-- String interpolation with variables (Phase 1B)
-- ---------------------------------------------------------------------------

-- | Format a template string by interpolating variables from GameState.
--   Supports:
--     - Plain variables: {var:gold} or {gold}
--     - Sign modifier: {bilanz:+} (forces + on non-negative numbers)
--     - Width padding: {gold:6} (right-aligned) or {gold:-6} (left-aligned)
--     - System variables: {player.hp}, {player.max_hp}, {turn.count}, {room.name}
--     - Escaped braces: \{...\} or {{...}}
formatWithVars :: String -> GameState -> String
formatWithVars str st = formatStringWith str (lookupVarForFormat st)

lookupVarForFormat :: GameState -> String -> Maybe String
lookupVarForFormat st name
    | Just val <- Map.lookup name (variables (save st)) =
        Just (varToString val)
    | name `elem` ["player.hp", "player.health"] =
        Just (show (playerHealth (player (save st))))
    | name `elem` ["player.max_hp", "player.max_health"] =
        Just (show (playerMaxHealth (player (save st))))
    | name `elem` ["turn.count", "turns"] =
        Just (show (turnCount (save st)))
    | name `elem` ["room.name", "current_room.name"] =
        Just (fromMaybe "" (roomName <$> getCurrentRoom st))
    | name `elem` ["room.id", "current_room.id", "room"] =
        Just (currentRoom (save st))
    | name `elem` Map.keys (varDefs (world st)) =
        Just "0"
    | otherwise = Nothing
  where
    varToString (VVInt n)  = show n
    varToString (VVBool b) = if b then "true" else "false"
    varToString (VVText s) = s

applyVarModifier :: String -> String -> String
applyVarModifier str modif =
    let forceSign = '+' `elem` modif
        widthPart = filter (/= '+') modif
        signedStr = if forceSign
                    then case str of
                        ('-':_) -> str
                        _       -> '+' : str
                    else str
    in case widthPart of
        ('-':digits) | not (null digits) && all isDigit digits ->
            let w = read digits :: Int
            in signedStr ++ replicate (max 0 (w - length signedStr)) ' '
        digits | not (null digits) && all isDigit digits ->
            let w = read digits :: Int
            in replicate (max 0 (w - length signedStr)) ' ' ++ signedStr
        _ -> signedStr

formatStringWith :: String -> (String -> Maybe String) -> String
formatStringWith [] _ = []
formatStringWith ('\\':'{':cs) env = '{' : formatStringWith cs env
formatStringWith ('\\':'}':cs) env = '}' : formatStringWith cs env
formatStringWith ('{':'{':cs) env = '{' : formatStringWith cs env
formatStringWith ('}':'}':cs) env = '}' : formatStringWith cs env
formatStringWith ('{':cs) env =
    case span (/= '}') cs of
        (inside, '}':rest) ->
            let (isExplicitVar, clean) = if "var:" `isPrefixOf` inside
                                        then (True, drop 4 inside)
                                        else (False, inside)
                (varName, modif) = case break (== ':') clean of
                    (name, ':':m) -> (name, m)
                    (name, _)     -> (name, "")
            in case env varName of
                Just val -> applyVarModifier val modif ++ formatStringWith rest env
                Nothing
                    | isExplicitVar -> applyVarModifier "0" modif ++ formatStringWith rest env
                    | otherwise     -> '{' : inside ++ "}" ++ formatStringWith rest env
        _ -> '{' : formatStringWith cs env
formatStringWith (c:cs) env = c : formatStringWith cs env

-- ---------------------------------------------------------------------------
-- Conditional text (Phase 3g)
-- ---------------------------------------------------------------------------

-- | Resolve a CondText: first variant whose predicate holds wins; otherwise
--   the default is returned.
resolveCondText :: CondText -> GameState -> String
resolveCondText ct state =
    let raw = case [tvText tv | tv <- ctVariants ct, evalPredicate (tvWhen tv) state] of
            (s:_) -> s
            []    -> ctDefault ct
    in formatWithVars raw state

-- ---------------------------------------------------------------------------
-- ASCII art (Phase B/C/D)
-- ---------------------------------------------------------------------------

-- | Resolve ASCII art for rendering: the passive animation frame when the art
--   is animated and `every > 0`, otherwise the state-dependent base art. Pure
--   function of `GameState` (frame index comes from `turnCount`).
resolveAsciiArt :: AsciiArt -> GameState -> String
resolveAsciiArt art state =
    maybe (resolveCondText (aaStatic art) state) (\f -> resolveCondText f state)
          (passiveFrame art state)

-- | The passive animation frame at the current turn, if any.
passiveFrame :: AsciiArt -> GameState -> Maybe CondText
passiveFrame art state
    | null (aaFrames art) = Nothing
    | aaEvery art <= 0    = Nothing
    | otherwise           = Just (aaFrames art !! idx)
  where
    n = length (aaFrames art)
    idx = (turnCount (save state) `div` aaEvery art) `mod` n

-- | Every resolved frame of an art, in order — what `watch` plays back. A
--   non-animated art yields its single (possibly empty) static text.
asciiFrames :: AsciiArt -> GameState -> [String]
asciiFrames art state
    | null (aaFrames art) = [t | let t = resolveCondText (aaStatic art) state, not (null t)]
    | otherwise           = map (\f -> resolveCondText f state) (aaFrames art)

-- | Default playback rate for arts that carry no rate of their own (Phase H,
--   H1): roughly three frames per second, matching the former fixed constant.
--   The 350-ms figure is now a documented default for `frames`/`every` arts
--   without `fps`, not an engine-wide constant.
defaultFrameMicros :: Int
defaultFrameMicros = 350000

-- | Pure playback of a piece of art (Phase H, H1): the frames to show and the
--   delay the frontend should wait between them, in microseconds. Ambient
--   loops carry their own rate (`fps` in `ambient`); everything else plays at
--   the default rate. The frontend does all waiting — the core never does.
asciiPlayback :: AsciiArt -> GameState -> ([String], Int)
asciiPlayback art state = case aaAmbient art of
    Just amb
        | not (null (ambFrames amb))
        , ambFps amb > 0  -> (ambFrames amb, 1000000 `div` ambFps amb)
    _ -> (asciiFrames art state, defaultFrameMicros)

-- | The end banner a world defines for a game-over reason, if any. Keys are
--   @"death"@, @"victory"@ or the custom reason string (Phase G).
endArtFor :: GameOverReason -> GameState -> Maybe AsciiArt
endArtFor reason state =
    case Map.lookup key (worldEndArt (world state)) of
        Just art | not (isEmptyAscii art) -> Just art
        _                                 -> Nothing
  where
    key = case reason of
        Death    -> "death"
        Victory  -> "victory"
        Custom s -> s

-- ---------------------------------------------------------------------------
-- Hotspots (Phase E)
-- ---------------------------------------------------------------------------

-- | The n-th hotspot (1-based) of an art, if any.
hotspotAt :: AsciiArt -> Int -> Maybe Hotspot
hotspotAt art n
    | n >= 1 && n <= length (aaHotspots art) = Just (aaHotspots art !! (n - 1))
    | otherwise = Nothing

-- | If the target is a bare number, resolve it to the target of the matching
--   hotspot in the current room's art. Otherwise the target is unchanged. This
--   is what makes `look at 3` select the third marked object.
resolveHotspotTarget :: String -> GameState -> String
resolveHotspotTarget targetStr state
    | not (null targetStr) && all isDigit targetStr =
        case getCurrentRoom state >>= \r -> hotspotAt (roomAscii r) (read targetStr) of
            Just hs -> hsTarget hs
            Nothing -> targetStr
    | otherwise = targetStr

-- | Highlight the hotspot markers in resolved art. The SGR codes are stripped
--   by the output filter when colour is off, so the core stays colour-blind.
--   Whitespace glyphs are ignored (they cannot be a marker).
highlightHotspots :: AsciiArt -> String -> String
highlightHotspots art s0 = foldl' mark s0 (aaHotspots art)
  where
    mark s h
      | isSpace (hsGlyph h) = s
      | otherwise = replaceChar (hsGlyph h) (marker (hsGlyph h)) s
    marker c = "\ESC[1;33m" ++ [c] ++ "\ESC[0m"

-- | Resolved art with marker highlighting, as printed by `look`.
renderArtForLook :: AsciiArt -> GameState -> String
renderArtForLook art state = highlightHotspots art (resolveAsciiArt art state)

-- | Replace every occurrence of a character.
replaceChar :: Char -> String -> String -> String
replaceChar from to = concatMap (\c -> if c == from then to else [c])

-- ---------------------------------------------------------------------------
-- Outcome interpreter (single, shared implementation)
--   Used by command execution (Parser), condition ticks, quest rewards and
--   vehicle condition ticks — no divergent fallback clones.
-- ---------------------------------------------------------------------------

-- | Maximum nesting depth for outcomes. Prevents runaway recursion from
--   malformed content (e.g. a CheckFlag whose branch loops back).
maxOutcomeDepth :: Int
maxOutcomeDepth = 20

-- | Outcome application with a threaded RNG salt and recursion depth.
--   Returns (state, message, nextSalt, nextDepth).
applyOutcomeWith :: Int -> Int -> Effect -> ItemID -> GameState -> (GameState, String, Int)
applyOutcomeWith depth salt outcome targetId state
    | depth > maxOutcomeDepth =
        -- P2-23: this is a content error, not game text. Report it on the
        -- diagnostic channel so it reaches the author (`GameLoop` prints new
        -- diagnostics to stderr) instead of appearing in the player's output.
        ( addDiagnostic
            ("[engine] maximum outcome depth exceeded (depth " ++ show depth
             ++ " > " ++ show maxOutcomeDepth
             ++ "): an effect (or a self-triggering event) is nested too deeply")
            state
        , "", salt )
    | otherwise = case outcome of
    SendMessage msg -> (state, formatWithVars msg state, salt)

    Sequence outcomes ->
        let (st', msg', salt') = foldl' (\(st, acc, s) o ->
                let (st2, m2, s2) = applyOutcomeWith (depth + 1) s o targetId st
                in (st2, joinMessages acc m2, s2))
                (state, "", salt) outcomes
        in (st', msg', salt')

    SetValue vr ev ->
        let (state', msg') = applySetValue vr ev state
        in (state', msg', salt)

    ComputeValue vr expr ->
        let val = evalExpr expr state
            (state', msg') = applySetValue vr (EVInt val) state
        in (state', msg', salt)

    ModifyValue VRPlayerHealth delta ->
        let state' = if delta >= 0
                     then updatePlayerHealth (+ delta) state
                     else let st' = updatePlayerHealth (+ delta) state
                          in if isPlayerDead st' then endGame Death st' else st'
        in (state', "", salt)
    ModifyValue vr delta ->
        let (state', m) = modifyValueProp vr delta state
        in (state', m, salt)

    MoveEntity eid (InRoom room) ->
        let state' = moveEntityToRoom eid room state
        in (state', "", salt)
    MoveEntity eid (CarriedBy _) ->
        let state' = giveItem eid state
        in (state', "", salt)
    MoveEntity eid Removed ->
        let state' = consumeItem eid state
        in (state', "", salt)
    MoveEntity eid (EquippedBy _) ->
        case equipItem eid state of
            Left err    -> (state, err, salt)
            Right st'   -> (st', "", salt)
    MoveEntity _ (InContainer _) ->
        -- P1-11: `InContainer` as a runtime effect is a silent no-op — it
        -- moved nothing and said nothing, so an author writing
        -- `move: ... in_container: chest` got no effect and no error. The
        -- container mechanism is a compile-time START placement only
        -- (`in_container:` on items, validated by `checkContainerRefs`).
        -- Items are moved in/out via `give`/`drop`/`consume` like any other
        -- location. This clause is kept as a documented no-op so the
        -- outcome tree stays exhaustive; it is NOT a supported container API.
        (state, "You can't move an item into a container that way.", salt)

    QuestOp StartQuest qId ->
        if canStartQuest qId state
        then (startQuest qId state, "", salt)
        else (state, "You cannot start that quest right now.", salt)
    QuestOp AdvanceQuest qId ->
        if Map.member qId (activeQuests (save state))
        then (advanceQuest qId state, "", salt)
        else (state, "That quest is not active.", salt)
    QuestOp CompleteQuest qId ->
        if Map.member qId (activeQuests (save state))
        then let (st', rewardMsg) = completeQuestWithMsg qId state
             in (st', rewardMsg, salt)
        else (state, "That quest is not active.", salt)

    Conditional predicate thenOutcome elseOutcome ->
        if evalPredicate predicate state
        then applyOutcomeWith (depth + 1) salt thenOutcome targetId state
        else applyOutcomeWith (depth + 1) salt elseOutcome targetId state

    RandomChoice [] -> (state, "", salt)
    RandomChoice weighted ->
        let totalWeight = max 1 (sum (map fst weighted))
            -- Advance first, then draw from the HIGH bits of the LCG state.
            -- `nextRng` is an LCG mod 2^64 whose low bits have tiny periods
            -- (bit 0 toggles every step), so a raw `mod` would degenerate a
            -- two-way choice into A,B,A,B. Bits 33+ have full period.
            rng = nextRng (rngState (save state) + fromIntegral salt)
            pick = fromIntegral ((rng `shiftR` 33) `mod` fromIntegral totalWeight)
            st' = state { save = (save state) { rngState = rng } }
            go :: Int -> [(Int, Effect)] -> Effect
            go _ [(_, e)] = e
            go acc ((w, e):rest)
                | pick < acc + w = e
                | otherwise = go (acc + w) rest
            go _ [] = Noop
        in applyOutcomeWith (depth + 1) (salt + 1) (go 0 weighted) targetId st'

    GameEnd reason msg -> (endGame reason state, formatWithVars msg state, salt)

    -- Phase H/H4: queue a cutscene for one playback at the frontend. Unknown
    -- or unusable clips are compile errors (UnknownClip/ClipFpsInvalid/
    -- ClipFramesEmpty), so a miss here is a silent no-op, not a player error.
    PlayClip clipId ->
        case Map.lookup clipId (worldClips (world state)) of
            Just clip | not (null (clipFrames clip))
                     , clipFps clip > 0 ->
                (state { pendingCutscene =
                            Just (clipFrames clip, 1000000 `div` clipFps clip) }
                , "", salt)
            _ -> (state, "", salt)

    ApplyCondition name turns tick end -> (applyCondition name turns tick end state, "", salt)
    ClearCondition name -> (clearCondition name state, "", salt)

    -- P1-20: fire `OnCustomEvent name`. The event depth is threaded into the
    -- nested trigger pass (depth + 1), so a rule whose effect raises the same
    -- event again terminates at `maxOutcomeDepth` instead of looping.
    RaiseEvent name ->
        let (st', m) = fireTriggersWithDepth (depth + 1) (OnCustomEvent name) state
        in (st', m, salt)

    ModifySkill skillId delta -> (modifySkill skillId delta state, "", salt)

    -- Rogue Phase 3: dynamic exits. `SetExit` writes an override (replacing
    -- the static connection for that (room, direction)); a runtime `Locked`
    -- exit seeds its entity state as "locked" lazily (M4: 'initialEntityStates'
    -- only knows static exits, so a fresh `locked_by` entity would otherwise
    -- have no state at all). `RemoveExit` marks the connection as gone — even
    -- a statically present exit disappears. Silent by design: the author adds
    -- a message effect alongside.
    SetExit from dir exit ->
        let ss = save state
            withEntity = case exit of
                Locked _ e | Map.notMember e (entityStates ss) ->
                    ss { entityStates = Map.insert e "locked" (entityStates ss) }
                _ -> ss
            in (state { save = withEntity
                { exitOverrides = Map.insert (from, dir) (Just exit) (exitOverrides withEntity) } }
            , "", salt)
    RemoveExit from dir ->
        (state { save = (save state)
            { exitOverrides = Map.insert (from, dir) Nothing (exitOverrides (save state)) } }
        , "", salt)

    -- Narrative: store lines + follow-up for interactive display
    Narrative nls followUp ->
        let formatted = map (`formatWithVars` state) nls
        in (state { pendingNarrative = Just (formatted, followUp) }, intercalate "\n" formatted, salt)

    DrawCards n -> (drawCards n state, "", salt)
    DiscardHand -> (discardHand state, "", salt)
    DiscardCard cid -> (discardCard cid state, "", salt)
    ExhaustCard cid -> (exhaustCard cid state, "", salt)
    AddCardToDeck cid dest -> (addCardToDeck cid dest state, "", salt)
    ShuffleDeck -> (shuffleDeck state, "", salt)

    GenerateRoom rawId rawName rawDesc from toDir returnDir ->
        let ss = save state
            newId = formatWithVars rawId state
            rName = formatWithVars rawName state
            rDesc = formatWithVars rawDesc state
            fromId = if from `elem` ["current", "current_room"] then currentRoom ss else from
            mFromFloor = lookupRoom fromId state >>= roomFloor
            newFloor = case (mFromFloor, toDir) of
                (Just fl, Down) -> Just (fl + 1)
                (Just fl, Up)   -> Just (max 1 (fl - 1))
                (Just fl, _)    -> Just fl
                (Nothing, _)    -> Nothing
            newRoom = Room
                { roomId = newId
                , roomName = rName
                , roomDescription = plainText rDesc
                , roomConnections = Map.singleton returnDir (Open fromId)
                , roomTags = Set.empty
                , roomLightFlag = Nothing
                , roomOnEnter = Nothing
                , roomOnLook = Nothing
                , roomOnExit = Nothing
                , roomSearchOutcome = Nothing
                , roomAscii = emptyAscii
                , roomIntro = Nothing
                , roomFloor = newFloor
                }
            dyn' = Map.insert newId newRoom (dynamicRooms ss)
            overrides' = Map.insert (fromId, toDir) (Just (Open newId)) (exitOverrides ss)
            ss' = ss { dynamicRooms = dyn', exitOverrides = overrides' }
        in (state { save = ss' }, "", salt)

    Noop -> (state, "", salt)

-- | Apply SetValue: set a value reference to a new value (handles flags,
--   variables, entity states). Returns the state plus any message produced by
--   the fired side effects (an entity-state change fires its `OnStateChange`).
applySetValue :: ValueRef -> EffectValue -> GameState -> (GameState, String)
applySetValue (VRFlag name) val state =
    (setFlag name (effectValueToString val) state, "")
applySetValue (VRVariable name) val state =
    (setVariableChecked name (effectValToVarVal val) state, "")
applySetValue (VRActorProp (ActorEntity eId) PState) val state =
    setEntityStateWithEvents eId (effectValueToString val) state
applySetValue (VRActorProp ActorPlayer PRoom) val state =
    (fst (transitionToRoom (effectValueToString val) (clearActiveDialogue state)), "")
applySetValue (VRActorProp (ActorRoom rId) PVisited) val state =
    let b = case val of { EVInt n -> n /= 0; EVBool v -> v; _ -> False }
    in (setRoomVisited rId b state, "")
applySetValue (VRActorProp ActorPlayer PHealth) val state =
    let n = case val of { EVInt m -> m; _ -> 0 }
    in if n <= 0 then (endGame Death (setPlayerHP n state), "") else (setPlayerHP n state, "")
applySetValue (VRActorProp (ActorShip vId) PHealth) val state =
    let n = case val of { EVInt m -> m; _ -> 0 }
    in (setVariableChecked ("ship." ++ vId ++ ".hull") (VVInt n) state, "")
applySetValue VRPlayerHealth val state =
    let n = case val of { EVInt m -> m; _ -> 0 }
    in if n <= 0 then (endGame Death (setPlayerHP n state), "") else (setPlayerHP n state, "")
applySetValue _ _ state = (state, "")

-- | Apply ModifyValue to a non-player-health reference.
--   Returns the updated state plus any message produced by a side effect
--   (an NPC death firing its `OnStateChange` rules) — messages are threaded,
--   never discarded.
modifyValueProp :: ValueRef -> Int -> GameState -> (GameState, String)
modifyValueProp (VRFlag name) delta state =
    let cur = case getFlag name state of
            Just "true" -> 1
            _           -> 0
        newVal = if cur + delta > 0 then "true" else "false"
    in (setFlag name newVal state, "")
modifyValueProp (VRVariable name) delta state =
    let cur = case getVariable name state of
            Just (VVInt n)  -> n
            Just (VVText s) -> case reads s of [(n,_)] -> n; _ -> 0
            _               -> 0
    in (setVariableChecked name (VVInt (cur + delta)) state, "")
modifyValueProp (VRItemProp iId prop) delta state =
    (modifyItemProp iId prop delta state, "")
modifyValueProp (VRActorProp (ActorNPC eId) PHealth) delta state
    | resolveActorNpcId eId state `elem` ["all", "all_enemies"] =
        let curRoom = currentRoom (save state)
            enemies = [ npcId def
                      | def <- getNPCsInRoom curRoom state
                      , not (isDeadNPC (npcId def) state)
                      , not (isInParty (npcId def) state)
                      ]
            step (st, msgs) eid =
                let (st', m) = modifyNPCHealth eid delta st
                in (st', if null m then msgs else msgs ++ [m])
            (stFin, msgsFin) = foldl' step (state, []) enemies
        in (stFin, intercalate "\n" msgsFin)
    | otherwise =
        modifyNPCHealth (resolveActorNpcId eId state) delta state
modifyValueProp (VRActorProp (ActorNPC nId) (PCustom prop)) delta state =
    (modifyNPCProp (resolveActorNpcId nId state) prop delta state, "")
modifyValueProp (VRActorProp ActorPlayer PHealth) delta state =
    let cur = playerHealth (player (save state))
        newHP = cur + delta
    in if newHP <= 0
       then (endGame Death (setPlayerHP newHP state), "")
       else (setPlayerHP newHP state, "")
modifyValueProp (VRActorProp (ActorShip vId) PHealth) delta state =
    let cur = case getVariable ("ship." ++ vId ++ ".hull") state of
            Just (VVInt n) -> n
            _              -> 0
    in (setVariableChecked ("ship." ++ vId ++ ".hull") (VVInt (cur + delta)) state, "")
modifyValueProp _ _ state = (state, "")

-- | Convert EffectValue to VariableValue
effectValToVarVal :: EffectValue -> VariableValue
effectValToVarVal (EVInt n)    = VVInt n
effectValToVarVal (EVString s) = VVText s
effectValToVarVal (EVBool b)   = VVInt (if b then 1 else 0)

-- | Convert EffectValue to String (for flag/state values)
effectValueToString :: EffectValue -> String
effectValueToString (EVInt n)    = show n
effectValueToString (EVString s) = s
effectValueToString (EVBool b)   = if b then "true" else "false"

-- | Modify an NPC's health, killing them if <= 0. The kill's state-change
--   event messages are threaded back to the caller.
modifyNPCHealth :: NPCID -> Int -> GameState -> (GameState, String)
modifyNPCHealth nIdRaw delta state =
    let nId = resolveActorNpcId nIdRaw state
    in case Map.lookup nId (npcStates (save state)) of
        Nothing -> (state, "")
        Just n ->
            let oldHealth = fromMaybe 0 (npcHealth n)
                newHealth = oldHealth + delta
                state' = updateNPCState nId (n { npcHealth = Just newHealth }) state
                state'' = clampNPCHealth nId state'
            in if newHealth <= 0 then killNPCWithMsg nId state'' else (state'', "")

-- | Move an entity (player or NPC) to a room
moveEntityToRoom :: EntityID -> RoomID -> GameState -> GameState
moveEntityToRoom "player" room state =
    fst (transitionToRoom room (clearActiveDialogue state))
moveEntityToRoom eId room state =
    moveNPCToRoom eId room state

-- | Set player HP directly (clamped to max)
setPlayerHP :: Int -> GameState -> GameState
setPlayerHP n state =
    let saveSt = save state
        p = player saveSt
        maxHp = playerMaxHealth p
        p' = p { playerHealth = max 0 (min maxHp n) }
    in state { save = saveSt { player = p' } }

-- | Public wrapper: apply a single outcome starting at depth 0 / salt 0
applyOutcome :: Effect -> ItemID -> GameState -> CommandResult
applyOutcome outcome targetId state =
    let (st, msg, _) = applyOutcomeWith 0 0 outcome targetId state
    in (st, msg)

-- | Join two message fragments, dropping the empty ones. The trigger path
--   (`combineMessages` in GameLoop) always did this; `Sequence`/`applyOutcomes`
--   appended unconditionally, so an effect without a message inserted a blank
--   line into the game text.
joinMessages :: String -> String -> String
joinMessages acc m
    | null m    = acc
    | null acc  = m
    | otherwise = acc ++ "\n" ++ m

-- | Apply zero or more outcomes in sequence
applyOutcomes :: [Effect] -> ItemID -> GameState -> CommandResult
applyOutcomes outcomes targetId state =
    let (st, msg, _) = foldl' (\(s, acc, slt) o ->
            let (s2, m2, slt2) = applyOutcomeWith 0 slt o targetId s
            in (s2, joinMessages acc m2, slt2))
            (state, "", 0) outcomes
    in (st, msg)

-- | Run a room hook (onEnter / onLook / onExit) if one is defined
runRoomHook :: (Room -> Maybe Effect) -> RoomID -> GameState -> (GameState, String)
runRoomHook hook rId state =
    case lookupRoom rId state >>= hook of
        Nothing -> (state, "")
        Just outcome -> applyOutcome outcome "" state

-- | Find direction of exit from one room to another
findExitDirection :: RoomID -> RoomID -> GameState -> Maybe Direction
findExitDirection fromId toId st =
    let conns = effectiveConnections st fromId
        matches = [ dir | (dir, exit) <- Map.toList conns, exitRoomID exit == toId ]
    in listToMaybe matches

-- | Move the player to another room, running exit/enter hooks and marking the
--   destination visited. Used by both walking (Go) and TransitionRoom outcomes
--   so data-driven teleports behave exactly like walks.
transitionToRoom :: RoomID -> GameState -> (GameState, String)
transitionToRoom rawDest state =
    let gw = world state
        dest = canonicalRoomId gw rawDest
        cur = currentRoom (save state)
        mDir = findExitDirection cur dest state
        stateWithRoom = ensureRoomExists dest cur (fromMaybe North mDir) state
        stAfterDialogue = clearActiveDialogue stateWithRoom
        (stAfterExit, exitMsg) = runRoomHook roomOnExit cur stAfterDialogue
        moved = moveToRoom dest stAfterExit
        visited = markCurrentRoomVisited moved
        (finalState, enterMsg) = runRoomHook roomOnEnter dest visited
        followed = followParty dest finalState
        -- Phase H/H4: the destination's `intro` clip plays once. An effect
        -- fired by on_enter (play_clip) wins — never overwrite it.
        withIntro = case pendingCutscene followed of
            Just _  -> followed
            Nothing -> case introCutsceneOf dest followed of
                Just cs -> followed { pendingCutscene = Just cs }
                Nothing -> followed
        fullMsg = intercalate "\n" (filter (not . null) [exitMsg, enterMsg])
    in (withIntro, fullMsg)

-- | The cutscene a room's `intro` resolves to (Phase H/H4): frames and rate
--   from the world's clip map, or Nothing when the room has no intro or the
--   clip is unusable (compile-time validated, so silent here).
introCutsceneOf :: RoomID -> GameState -> Maybe ([String], Int)
introCutsceneOf rId state = do
    r <- lookupRoom rId state
    clip <- Map.lookup (fromMaybe "" (roomIntro r)) (worldClips (world state))
    guard (not (null (clipFrames clip)))
    guard (clipFps clip > 0)
    pure (clipFrames clip, 1000000 `div` clipFps clip)

-- ---------------------------------------------------------------------------
-- Quests (Phase 2)
-- ---------------------------------------------------------------------------

-- | Look up a quest definition
lookupQuest :: QuestID -> GameState -> Maybe Quest
lookupQuest qId state = Map.lookup qId (questDefs (world state))

-- | Is the quest active, and if so at which stage index (0-based)?
questStage :: QuestID -> GameState -> Maybe Int
questStage qId state = Map.lookup qId (activeQuests (save state))

-- | Is the quest completed?
questCompleted :: QuestID -> GameState -> Bool
questCompleted qId state = Set.member qId (completedQuests (save state))

-- | Can the quest be started? (exists, not active, not completed, prereqs met)
canStartQuest :: QuestID -> GameState -> Bool
canStartQuest qId state = case lookupQuest qId state of
    Nothing -> False
    Just q ->
        not (questCompleted qId state)
            && not (Map.member qId (activeQuests (save state)))
            && all (\(f, v) -> getFlag f state == Just v) (Map.toList (questPrereqs q))

-- | Start a quest at stage 0
startQuest :: QuestID -> GameState -> GameState
startQuest qId state = state
    { save = (save state) { activeQuests = Map.insert qId 0 (activeQuests (save state)) } }

-- | Advance a quest to the next stage, or complete it if already at the last
advanceQuest :: QuestID -> GameState -> GameState
advanceQuest qId state =
    case Map.lookup qId (activeQuests (save state)) of
        Nothing -> state
        Just idx -> case lookupQuest qId state of
            Nothing -> state
            Just q
                | idx + 1 >= length (questStages q) -> completeQuest qId state
                | otherwise -> state
                    { save = (save state) { activeQuests = Map.insert qId (idx + 1) (activeQuests (save state)) } }

-- | Mark a quest completed and fire its reward.
--   Returns the new state plus the reward message ("" if none).
completeQuestWithMsg :: QuestID -> GameState -> (GameState, String)
completeQuestWithMsg qId state =
    let withoutActive = state
            { save = (save state)
                { activeQuests = Map.delete qId (activeQuests (save state))
                , completedQuests = Set.insert qId (completedQuests (save state)) } }
    in case lookupQuest qId state >>= questReward of
        Nothing -> (withoutActive, "")
        Just outcome -> applyOutcome outcome "" withoutActive

-- | Mark a quest completed (ignoring the reward message)
completeQuest :: QuestID -> GameState -> GameState
completeQuest qId state = fst (completeQuestWithMsg qId state)

-- | Journal text: active quests with their current stage, then completed ones
journalText :: GameState -> String
journalText state =
    let active = Map.toList (activeQuests (save state))
        completed = Set.toList (completedQuests (save state))
        activeLines =
            [ "- " ++ questName q ++ ": " ++ stageText q idx
            | (qId, idx) <- active
            , Just q <- [lookupQuest qId state] ]
        completedLines =
            [ "- " ++ questName q ++ " (completed)"
            | qId <- completed
            , Just q <- [lookupQuest qId state] ]
    in case (activeLines, completedLines) of
        ([], []) -> "Your journal is empty."
        _ -> "=== Journal ===\n" ++
             (if null activeLines then "" else unlines ("Active:" : activeLines)) ++
             (if null completedLines then "" else unlines ("Completed:" : completedLines))
  where
    stageText q idx =
        case drop idx (questStages q) of
            (s:_) -> qsText s ++ maybe "" (\h -> " (Hint: " ++ h ++ ")") (qsHint s)
            []    -> "?"

-- ---------------------------------------------------------------------------
-- Vehicles (Phase 3)
-- ---------------------------------------------------------------------------

-- | Look up a vehicle definition
lookupVehicle :: VehicleID -> GameState -> Maybe VehicleDef
lookupVehicle vId state = Map.lookup vId (vehicleDefs (world state))

-- | Look up a vehicle's dynamic state (defaults if unknown)
getVehicleState :: VehicleID -> GameState -> VehicleState
getVehicleState vId state =
    Map.findWithDefault (VehicleState "" Nothing Set.empty Map.empty) vId (vehicleStates (save state))

-- | Set a vehicle's dynamic state
setVehicleState :: VehicleID -> VehicleState -> GameState -> GameState
setVehicleState vId vs state = state
    { save = (save state) { vehicleStates = Map.insert vId vs (vehicleStates (save state)) } }

-- | Which vehicle (if any) owns this room id as an interior room?
vehicleForRoom :: RoomID -> GameState -> Maybe VehicleID
vehicleForRoom rId state =
    fmap fst (find (\(_, v) -> rId `elem` vehicleRooms v) (Map.toList (vehicleDefs (world state))))

-- | Is the player currently inside a vehicle?
isInsideVehicle :: GameState -> Bool
isInsideVehicle state = currentVehicle (save state) /= Nothing

-- | Is the current room one of the vehicle's interior rooms?
inVehicleRoom :: GameState -> Bool
inVehicleRoom state = case currentVehicle (save state) of
    Just vId -> case lookupVehicle vId state of
        Just v -> currentRoom (save state) `elem` vehicleRooms v
        Nothing -> False
    Nothing -> False

-- | All stops of a vehicle in route order (Map order = key order)
vehicleStopList :: VehicleDef -> [(RoomID, VehicleStop)]
vehicleStopList v
    | null (vehicleRoute v) = Map.toList (vehicleStops v)
    | otherwise =
        [ (rId, stop)
        | rId <- vehicleRoute v
        , Just stop <- [Map.lookup rId (vehicleStops v)] ]

-- | The next stop after the current one (wrapping around the route)
nextVehicleStop :: VehicleDef -> RoomID -> Maybe (RoomID, VehicleStop)
nextVehicleStop v cur =
    let stops = vehicleStopList v
        curIdx = elemIndex cur (map fst stops)
    in case curIdx of
        Just i  -> let n = (i + 1) `mod` length stops in Just (stops !! n)
        Nothing -> listToMaybe stops

-- | Enter a vehicle. Fails with a message if that isn't possible here.
enterVehicle :: VehicleID -> GameState -> Either String (GameState, String)
enterVehicle vId state = case lookupVehicle vId state of
    Nothing -> Left ("There is no '" ++ vId ++ "' here to enter.")
    Just v ->
        let vState = getVehicleState vId state
            stopRoom = vsCurrentStop vState
        in if currentRoom (save state) /= stopRoom
           then Left ("The " ++ vehicleName v ++ " is not here.")
           else Right
                ( followParty (vehicleEntryRoom v)
                    ( state { save = (save state)
                        { currentVehicle = Just vId
                        , currentRoom = vehicleEntryRoom v
                        , visitedRooms = Set.insert (vehicleEntryRoom v) (visitedRooms (save state)) } } )
                , "You board the " ++ vehicleName v ++ "." )

-- | Exit the current vehicle back to its current stop's outside room
exitVehicle :: GameState -> Either String (GameState, String)
exitVehicle state = case currentVehicle (save state) of
    Nothing -> Left "You are not in a vehicle."
    Just vId -> case lookupVehicle vId state of
        Nothing -> Left "You are not in a vehicle."
        Just v ->
            let vState = getVehicleState vId state
                outside = vsCurrentStop vState
            in Right
                ( followParty outside
                    ( state { save = (save state)
                        { currentVehicle = Nothing
                        , currentRoom = outside
                        , visitedRooms = Set.insert outside (visitedRooms (save state)) } } )
                , "You disembark from the " ++ vehicleName v ++ "." )

-- | Move a vehicle to a stop's outside room (updates its position).
--   Returns updated state; the caller decides whether the player travels with it.
moveVehicleToStop :: VehicleID -> RoomID -> GameState -> GameState
moveVehicleToStop vId stopRoom state
    | currentVehicle (save state) == Just vId =
        -- Player is aboard: travel with the vehicle
        followParty stopRoom $
            setVehicleState vId ((getVehicleState vId state) { vsCurrentStop = stopRoom })
                state { save = (save state) { currentRoom = stopRoom } }
    | otherwise =
        setVehicleState vId ((getVehicleState vId state) { vsCurrentStop = stopRoom }) state

-- | Consume the fare for a PaidVehicle stop, if one is required.
--   Returns Left with the error message when the player cannot pay.
payStopCost :: VehicleStop -> GameState -> Either String GameState
payStopCost stop state = case stopCost stop of
    Nothing -> Right state
    Just (costItem, errMsg) ->
        if hasItem costItem state
        then Right (consumeItem costItem state)
        else Left errMsg

-- | PlayerControlled: drive to a station by name (matched against stop labels
--   and outside-room ids). Must be done from the cockpit.
driveVehicle :: VehicleID -> String -> GameState -> Either String (GameState, String)
driveVehicle vId targetStr state = case lookupVehicle vId state of
    Nothing -> Left ("There is no '" ++ vId ++ "'.")
    Just v
        | vehicleType v /= PlayerControlled ->
            Left ("You can't steer the " ++ vehicleName v ++ "; it follows its own route.")
        | otherwise ->
            let target = map toLower targetStr
                matches = [ (rId, stop)
                          | (rId, stop) <- vehicleStopList v
                          , target `elem` [map toLower (stopLabel stop), map toLower rId]
                          , rId /= vsCurrentStop (getVehicleState vId state) ]
            in case matches of
                [] -> Left ("You can't drive there from here. Stations: "
                            ++ intercalate ", " (map (stopLabel . snd) (vehicleStopList v)))
                ((destId, stop):_) ->
                    if currentVehicle (save state) == Just vId
                       && Just (currentRoom (save state)) /= vehicleCockpitRoom v
                    then Left "You need to be at the controls to drive."
                    else case payStopCost stop state of
                        Left err -> Left err
                        Right st ->
                            let st' = moveVehicleToStop vId destId st
                            in Right (st', "You drive to " ++ stopLabel stop ++ ".")

-- | AutomaticRoute/PaidVehicle: advance to the next stop (`wait`).
--   Only while aboard; pays the fare for PaidVehicles.
advanceVehicleRoute :: GameState -> Either String (GameState, String)
advanceVehicleRoute state = case currentVehicle (save state) of
    Nothing -> Left "You are not on a vehicle."
    Just vId -> case lookupVehicle vId state of
        Nothing -> Left "You are not on a vehicle."
        Just v
            | vehicleType v == PlayerControlled ->
                Left "This vehicle only moves when you drive it."
            | otherwise ->
                let vState = getVehicleState vId state
                in case nextVehicleStop v (vsCurrentStop vState) of
                    Nothing -> Left "The route has no further stops."
                    Just (destId, stop) -> case payStopCost stop state of
                        Left err -> Left err
                        Right st ->
                            let st' = moveVehicleToStop vId destId st
                            in Right (st', "You travel on to " ++ stopLabel stop ++ ".")

-- | Refuel: add fuel units (capped at max) via an item interaction.
--   Returns Nothing if the vehicle takes no fuel.
refuelVehicle :: VehicleID -> Int -> GameState -> Maybe (GameState, String)
refuelVehicle vId amount state = case lookupVehicle vId state >>= vehicleFuelProp of
    Nothing -> Nothing
    Just fs ->
        let vs = getVehicleState vId state
            maxFuel = fsMax fs
            cur = fromMaybe 0 (vsFuel vs)
            newFuel = min maxFuel (cur + amount)
        in Just (setVehicleState vId (vs { vsFuel = Just newFuel }) state,
                 "The " ++ maybe vId vehicleName (lookupVehicle vId state) ++ " is fuelled ("
                    ++ show newFuel ++ "/" ++ show maxFuel ++ ").")

-- | Clear a vehicle condition (e.g. after `repair`)
clearVehicleCondition :: VehicleID -> String -> GameState -> GameState
clearVehicleCondition vId condId state =
    let vs = getVehicleState vId state
    in setVehicleState vId (vs { vsActiveConditions = Set.delete condId (vsActiveConditions vs) }) state

-- | Fire a vehicle-wide condition effect, if defined.
--   Returns the (possibly updated) state plus a message ("" if nothing fired).
vehicleConditionTick :: GameState -> (GameState, String)
vehicleConditionTick state = case currentVehicle (save state) of
    Nothing -> (state, "")
    Just vId -> case lookupVehicle vId state of
        Nothing -> (state, "")
        Just v ->
            let vState = getVehicleState vId state
                activeConds = Set.toList (vsActiveConditions vState)
                outcomes = [ o
                           | c <- activeConds
                           , Just o <- [Map.lookup c (vehicleConditionEffects v)] ]
            in if null outcomes
               then (state, "")
               else
                   let (st', msgs) = foldl' (\(s, ms) o ->
                            let (s2, m2) = applyOutcome o "" s
                            in (s2, if null m2 then ms else ms ++ [m2]))
                            (state, []) outcomes
                   in (st', intercalate "\n" msgs)

-- | Vehicle flavour for `look`: room override + active conditions + fuel
vehicleLookAddon :: GameState -> Maybe String
vehicleLookAddon state = case currentVehicle (save state) of
    Nothing -> Nothing
    Just vId -> case lookupVehicle vId state of
        Nothing -> Nothing
        Just v ->
            let vState = getVehicleState vId state
                condLine = if Set.null (vsActiveConditions vState)
                           then ""
                           else "Warning: " ++ intercalate ", " (Set.toList (vsActiveConditions vState))
                                ++ "!"
                fuelLine = case (vehicleFuelProp v, vsFuel vState) of
                    (Just fs, Just f) ->
                        if f <= 0
                            then "\nOut of " ++ fsItem fs ++ " (0/" ++ show (fsMax fs) ++ ")"
                            else "\nFuel (" ++ fsItem fs ++ ": " ++ show f ++ "/" ++ show (fsMax fs) ++ ")"
                    _ -> ""
                statusLine = unlines (filter (not . null) [condLine]) ++
                             (if null condLine then "" else "\n") ++ fuelLine
            in if null (trim statusLine) then Nothing else Just (trim statusLine)
  where
    trim = f . f where f = reverse . dropWhile (== '\n')

-- ---------------------------------------------------------------------------
-- Trigger system (Phase 3f)
-- ---------------------------------------------------------------------------

-- | Fire triggers matching the given event type (entry point, nesting depth 0).
--   Returns updated state and accumulated messages from all triggered effects.
fireTriggers :: EventType -> GameState -> (GameState, String)
fireTriggers = fireTriggersWithDepth 0

-- | Like `fireTriggers`, but carrying the current event nesting depth so that
--   recursively raising events (`RaiseEvent`) cannot loop forever.
fireTriggersWithDepth :: Int -> EventType -> GameState -> (GameState, String)
fireTriggersWithDepth depth event state
    | depth > maxOutcomeDepth =
        -- P2-23: a silently dropped recursion is the *authoring* error this
        -- guard exists for (a rule raising its own event); report it on the
        -- diagnostic channel instead of discarding it. Defensive: with the
        -- current call graph the `applyOutcomeWith` guard above fires first,
        -- because `RaiseEvent` bumps the depth before re-entering here.
        ( addDiagnostic
            ("[engine] trigger nesting exceeded " ++ show maxOutcomeDepth
             ++ " while handling " ++ show event
             ++ ": a rule probably raises its own event")
            state
        , "" )
    | otherwise =
        let triggers = triggerDefs (world state)
            matching = filter (\t -> trEvent t == event) triggers
        in fireTriggerList depth matching state

-- | Fire a specific list of triggers (internal, also used by nested call from effects).
--
--   The trigger list is filtered linearly per event, and a command raises up to
--   five events, so this is O(5 x |triggers|) per command. Review P2-13
--   suggested a `Map EventType [TriggerDef]` in the `GameWorld`; that was
--   **rejected** (decision, Cluster E): the map is serialized, and a correctly
--   compiled but stale/empty index would silently stop every trigger from
--   firing — a much worse failure than 12 ns of scanning (see the measurement
--   note on `getItemsInLocation`).
fireTriggerList :: Int -> [TriggerDef] -> GameState -> (GameState, String)
fireTriggerList depth triggers state =
    foldl' fireOne (state, "") triggers
  where
    -- Read the trigger state from the CURRENT fold state, not a snapshot
    -- taken at entry: a nested event round fired by an earlier trigger's
    -- effect may already have marked a later trigger as fired, and a stale
    -- snapshot would let an `once: true` rule fire a second time.
    fireOne (st, acc) tr =
        let tId = trId tr
            tState = Map.lookup tId (triggerStates (save st))
            alreadyFired = maybe False tsFired tState
            cooldownRemaining = maybe 0 tsCooldownRemaining tState
        in if trOnce tr && alreadyFired
           then (st, acc)
           else if cooldownRemaining > 0
           then (decrementCooldown tId st, acc)
           else case trCondition tr of
                Just p  -> if evalPredicate p st
                           then applyTrigEffects tId tr st acc
                           else (st, acc)
                Nothing -> applyTrigEffects tId tr st acc

    decrementCooldown tId st =
        let newTs = Map.adjust (\s -> s { tsCooldownRemaining = max 0 (tsCooldownRemaining s - 1) }) tId (triggerStates (save st))
        in st { save = (save st) { triggerStates = newTs } }

    applyTrigEffects tId tr st acc =
        let (st', msgs) = foldl' (\(s, a) e ->
                let (s', m, _) = applyOutcomeWith (depth + 1) 0 e "" s
                in (s', a ++ m ++ "\n")) (st { save = (save st) { triggerStates = updatedTs } }, acc) (trEffects tr)
            updatedTs = Map.insert tId (TriggerState True (trCooldown tr)) (triggerStates (save st))
        in (st', msgs)

-- ---------------------------------------------------------------------------
-- Deck & Card Operations (Schritt 2 / Phase 2A & 2B)
-- ---------------------------------------------------------------------------

-- | Fisher-Yates shuffle (from the back) using 64-bit RNG state.
shuffleList :: [a] -> Word64 -> ([a], Word64)
shuffleList [] rng = ([], rng)
shuffleList xs rng0 = go (Seq.fromList xs) (length xs) rng0
  where
    go s n r
        | n <= 1    = (Foldable.toList s, r)
        | otherwise =
            let r' = nextRng r
                pick = fromIntegral ((r' `shiftR` 33) `mod` fromIntegral n)
                xi   = Seq.index s pick
                xj   = Seq.index s (n - 1)
                s'   = Seq.update (n - 1) xi (Seq.update pick xj s)
            in go s' (n - 1) r'

-- | Helper to modify DeckState if present in SaveState.
modifyDeckState :: (DeckState -> GameState -> GameState) -> GameState -> GameState
modifyDeckState f st = case deckState (save st) of
    Nothing -> st
    Just ds -> f ds st

-- | Draw up to n cards from draw pile into hand (capped by maxHandSize).
--   If draw pile runs out, discard pile is shuffled into draw pile.
drawCards :: Int -> GameState -> GameState
drawCards n st
    | n <= 0    = st
    | otherwise = modifyDeckState go st
  where
    go ds currentSt =
        let currentHand = hand ds
            curDraw = drawPile ds
            curDiscard = discardPile ds
            handCap = maxHandSize ds
            freeSpace = max 0 (handCap - length currentHand)
            toDraw = min n freeSpace
        in if toDraw <= 0
           then currentSt
           else if length curDraw >= toDraw
                then let (drawn, remainingDraw) = splitAt toDraw curDraw
                         ds' = ds { hand = currentHand ++ drawn, drawPile = remainingDraw }
                     in currentSt { save = (save currentSt) { deckState = Just ds' } }
                else -- Need to draw what we have, then shuffle discard pile
                     let drawnFromDraw = curDraw
                         neededMore = toDraw - length drawnFromDraw
                     in if null curDiscard
                        then let ds' = ds { hand = currentHand ++ drawnFromDraw, drawPile = [] }
                             in currentSt { save = (save currentSt) { deckState = Just ds' } }
                        else let (shuffledDiscard, newRng) = shuffleList curDiscard (rngState (save currentSt))
                                 (drawnFromDiscard, remainingNewDraw) = splitAt neededMore shuffledDiscard
                                 ds' = ds
                                     { hand = currentHand ++ drawnFromDraw ++ drawnFromDiscard
                                     , drawPile = remainingNewDraw
                                     , discardPile = []
                                     }
                                 ss' = (save currentSt) { deckState = Just ds', rngState = newRng }
                             in currentSt { save = ss' }

-- | Discard all cards from hand to discard pile.
discardHand :: GameState -> GameState
discardHand st = modifyDeckState go st
  where
    go ds currentSt =
        let ds' = ds { discardPile = discardPile ds ++ hand ds, hand = [] }
        in currentSt { save = (save currentSt) { deckState = Just ds' } }

-- | Discard a specific card from hand by ID (first matching occurrence).
discardCard :: CardID -> GameState -> GameState
discardCard cid st = modifyDeckState go st
  where
    go ds currentSt =
        case removeFirst cid (hand ds) of
            Nothing -> currentSt
            Just remainingHand ->
                let ds' = ds { hand = remainingHand, discardPile = discardPile ds ++ [cid] }
                in currentSt { save = (save currentSt) { deckState = Just ds' } }

-- | Exhaust a specific card (from hand, or discard/draw if not in hand) to exhaust pile.
exhaustCard :: CardID -> GameState -> GameState
exhaustCard cid st = modifyDeckState go st
  where
    go ds currentSt =
        case removeFirst cid (hand ds) of
            Just remainingHand ->
                let ds' = ds { hand = remainingHand, exhaustPile = exhaustPile ds ++ [cid] }
                in currentSt { save = (save currentSt) { deckState = Just ds' } }
            Nothing -> case removeFirst cid (discardPile ds) of
                Just remainingDiscard ->
                    let ds' = ds { discardPile = remainingDiscard, exhaustPile = exhaustPile ds ++ [cid] }
                    in currentSt { save = (save currentSt) { deckState = Just ds' } }
                Nothing -> case removeFirst cid (drawPile ds) of
                    Just remainingDraw ->
                        let ds' = ds { drawPile = remainingDraw, exhaustPile = exhaustPile ds ++ [cid] }
                        in currentSt { save = (save currentSt) { deckState = Just ds' } }
                    Nothing -> currentSt

-- | Add a card to the deck at the specified destination.
addCardToDeck :: CardID -> DeckDestination -> GameState -> GameState
addCardToDeck cid dest st = modifyDeckState go st
  where
    go ds currentSt =
        let ds' = case dest of
                DestDraw    -> ds { drawPile = cid : drawPile ds }
                DestDiscard -> ds { discardPile = discardPile ds ++ [cid] }
                DestHand    -> if length (hand ds) < maxHandSize ds
                               then ds { hand = hand ds ++ [cid] }
                               else ds { discardPile = discardPile ds ++ [cid] }
        in currentSt { save = (save currentSt) { deckState = Just ds' } }

-- | Shuffle the current draw pile.
shuffleDeck :: GameState -> GameState
shuffleDeck st = modifyDeckState go st
  where
    go ds currentSt =
        let (shuffled, newRng) = shuffleList (drawPile ds) (rngState (save currentSt))
            ds' = ds { drawPile = shuffled }
            ss' = (save currentSt) { deckState = Just ds', rngState = newRng }
        in currentSt { save = ss' }

-- | Helper to remove the first occurrence of an element from a list.
removeFirst :: Eq a => a -> [a] -> Maybe [a]
removeFirst _ [] = Nothing
removeFirst x (y:ys)
    | x == y    = Just ys
    | otherwise = (y :) <$> removeFirst x ys

-- | Remove an element at 0-based index from a list.
removeAt :: Int -> [a] -> [a]
removeAt idx xs
    | idx < 0   = xs
    | otherwise = take idx xs ++ drop (idx + 1) xs

-- | Normalize text to lower case.
normalizeText :: String -> String
normalizeText = map toLower

-- | All lowercase aliases for an item.
itemAliases :: ItemDef -> [String]
itemAliases item = nub $ map normalizeText (itemId item : itemName item : itemKeywords item)

-- | All lowercase aliases for an NPC.
npcAliases :: NPCDef -> [String]
npcAliases npc = nub $ map normalizeText (npcId npc : npcName npc : npcKeywords npc)

-- | Check if a target string matches an item definition by ID, name, or keywords.
matchesItemTarget :: String -> ItemDef -> Bool
matchesItemTarget target item
    | null (words target) = False
    | otherwise           = normalizeText target `elem` itemAliases item

-- | Stop words ignored during target matching for cards.
cardStopWords :: [String]
cardStopWords = ["the", "a", "an", "some", "that", "this", "der", "die", "das", "ein", "eine", "den", "dem"]

-- | Check if a target string matches an NPC definition by ID, name, or keywords.
matchesNPCTarget :: String -> NPCDef -> Bool
matchesNPCTarget tgt npc
    | null (words tgt) = False
    | otherwise =
        let lower = map toLower
            tgtWords = words tgt
            cleanedTgt = lower (unwords (filter (`notElem` cardStopWords) tgtWords))
            rawTgt = lower (unwords tgtWords)
            candidates = npcAliases npc
        in rawTgt `elem` candidates
           || (not (null cleanedTgt) && cleanedTgt `elem` candidates)
           || any (\c -> (not (null rawTgt) && rawTgt `isInfixOf` c)
                      || (not (null cleanedTgt) && cleanedTgt `isInfixOf` c)) candidates

-- | Validate and deduct resource costs for playing a card.
--   Supports "player.<resource>" or "<resource>".
checkResourceCosts :: Map.Map String Int -> GameState -> Either String GameState
checkResourceCosts costs st =
    let costList = Map.toList costs
        resolveVar name =
            let pKey = "player." ++ name
            in case getVariable pKey st of
                Just (VVInt v) -> (pKey, v)
                _ -> case getVariable name st of
                    Just (VVInt v) -> (name, v)
                    _              -> (pKey, 0)
        checkOne (res, req) =
            let (_, cur) = resolveVar res
            in if cur >= req
               then Right ()
               else Left ("Not enough " ++ res ++ " (need " ++ show req ++ ", have " ++ show cur ++ ").")
    in case mapM_ checkOne costList of
        Left err -> Left err
        Right () ->
            let deduct stAcc (res, req) =
                    let (varKey, cur) = resolveVar res
                    in setVariableChecked varKey (VVInt (cur - req)) stAcc
            in Right (foldl' deduct st costList)

-- | Validate card target against living enemies in current room.
validateTarget :: CardTarget -> Maybe String -> GameState -> Either String (String, GameState)
validateTarget targetReq mTarget st =
    let curRoom = currentRoom (save st)
        roomNPCs = getNPCsInRoom curRoom st
        livingEnemies = [npc | npc <- roomNPCs, not (isDeadNPC (npcId npc) st), not (isInParty (npcId npc) st)]
    in case targetReq of
        TargetNone ->
            Right ("", st)
        TargetSelf ->
            Right ("player", setVariableChecked "cmd.target" (VVText "player") st)
        TargetAllEnemies ->
            if null livingEnemies
            then Left "There are no living enemies here to target."
            else Right ("all enemies", setVariableChecked "cmd.target" (VVText "all") st)
        TargetSingleEnemy ->
            if null livingEnemies
            then Left "There are no living enemies here to target."
            else case mTarget of
                Nothing ->
                    if length livingEnemies == 1
                    then let sole = head livingEnemies
                         in Right (npcName sole, setVariableChecked "cmd.target" (VVText (npcId sole)) st)
                    else Left ("Please specify a target (e.g. 'play <n> <target>'). Available: "
                               ++ intercalate ", " (map npcName livingEnemies))
                Just tStr ->
                    let matches = filter (matchesNPCTarget tStr) livingEnemies
                    in case matches of
                        [] -> Left ("No living enemy matches '" ++ tStr ++ "'.")
                        (targetNpc:_) ->
                            Right (npcName targetNpc, setVariableChecked "cmd.target" (VVText (npcId targetNpc)) st)

-- | Play a card from hand by 1-based index, with optional target.
playCard :: Int -> Maybe String -> GameState -> CommandResult
playCard idx mTarget st = case deckState (save st) of
    Nothing -> (st, "You don't have a deck to play cards from.")
    Just ds ->
        let curHand = hand ds
        in if idx < 1 || idx > length curHand
           then (st, "Invalid card number " ++ show idx ++ ". You have "
                     ++ show (length curHand) ++ " card(s) in hand.")
           else
               let cId = curHand !! (idx - 1)
               in case Map.lookup cId (cardDefs (world st)) of
                   Nothing -> (st, "Unknown card: '" ++ cId ++ "'.")
                   Just card ->
                       case validateTarget (cardTarget card) mTarget st of
                           Left err -> (st, err)
                           Right (targetLabel, stTargeted) ->
                               case checkResourceCosts (cardCost card) stTargeted of
                                   Left costErr -> (st, costErr)
                                   Right stCostPaid ->
                                       let dsCurrent = fromMaybe ds (deckState (save stCostPaid))
                                           handAfter = removeAt (idx - 1) (hand dsCurrent)
                                           dsAfter = if cardExhaust card
                                                     then dsCurrent { hand = handAfter
                                                                    , exhaustPile = exhaustPile dsCurrent ++ [cId] }
                                                     else dsCurrent { hand = handAfter
                                                                    , discardPile = discardPile dsCurrent ++ [cId] }
                                           stAfterCard = stCostPaid
                                               { save = (save stCostPaid) { deckState = Just dsAfter } }
                                           (stFinal, effectMsgs) = foldl' (\(sAcc, msgsAcc) eff ->
                                               let (s', m) = applyOutcome eff "" sAcc
                                               in (s', if null m then msgsAcc else msgsAcc ++ [m])
                                               ) (stAfterCard, []) (cardEffects card)
                                           header = "You play " ++ cardName card
                                                    ++ (if null targetLabel then "" else " on " ++ targetLabel)
                                                    ++ (if cardExhaust card then " (Exhausted)." else ".")
                                           allMsg = intercalate "\n" (filter (not . null) (header : effectMsgs))
                                       in (stFinal, allMsg)

-- | End the player's turn:
--   - Discards remaining hand cards
--   - Resets player.block to 0
--   - Restores energy to player.max_energy (default: 3)
--   - Draws cards (default: 5 or player.draw_per_turn)
endTurn :: GameState -> CommandResult
endTurn st = case deckState (save st) of
    Nothing -> (st, "You don't have a deck to end your turn.")
    Just _ds ->
        let st1 = discardHand st
            st2 = setVariableChecked "player.block" (VVInt 0)
                    (if Map.member "block" (variables (save st1))
                     then setVariableChecked "block" (VVInt 0) st1
                     else st1)
            maxE = case getVariable "player.max_energy" st2 of
                Just (VVInt m) -> m
                _ -> case getVariable "max_energy" st2 of
                    Just (VVInt m) -> m
                    _              -> 3
            st3 = setVariableChecked "player.energy" (VVInt maxE)
                    (if Map.member "energy" (variables (save st2))
                     then setVariableChecked "energy" (VVInt maxE) st2
                     else st2)
            drawCount = case getVariable "player.draw_per_turn" st3 of
                Just (VVInt d) -> d
                _ -> case getVariable "draw_per_turn" st3 of
                    Just (VVInt d) -> d
                    _              -> 5
            st4 = drawCards drawCount st3
            msg = "Turn ended. Energy restored to " ++ show maxE
                  ++ ". Drew " ++ show drawCount ++ " cards."
        in (st4, msg)

-- | Visible width of a string, ignoring ANSI CSI escape sequences.
visibleWidth :: String -> Int
visibleWidth = length . stripAnsi

-- | Pad a string on the right to reach the desired visible width.
padRightVisible :: Int -> String -> String
padRightVisible w s =
    let cur = visibleWidth s
    in if cur < w then s ++ replicate (w - cur) ' ' else s

-- | Break a string into words and wrap to lines of at most maxW characters.
wrapWords :: Int -> String -> [String]
wrapWords _ "" = []
wrapWords maxW text = go (words text)
  where
    go [] = []
    go (w : ws) =
        let (lineWords, rest) = takeLine (length w) [w] ws
        in unwords lineWords : go rest
    takeLine _ acc [] = (reverse acc, [])
    takeLine curLen acc (w : ws)
        | curLen + 1 + length w <= maxW = takeLine (curLen + 1 + length w) (w : acc) ws
        | otherwise                      = (reverse acc, w : ws)

-- | 24-bit ANSI styling codes for card types (Ruby Red, Sapphire Blue, Golden Yellow, Shadow Purple, Grey).
cardTypeAnsiColor :: CardType -> String
cardTypeAnsiColor CardAttack = "\ESC[38;2;220;50;50m"
cardTypeAnsiColor CardSkill  = "\ESC[38;2;60;130;240m"
cardTypeAnsiColor CardPower  = "\ESC[38;2;240;190;40m"
cardTypeAnsiColor CardCurse  = "\ESC[38;2;160;60;200m"
cardTypeAnsiColor CardStatus = "\ESC[38;2;150;150;150m"

-- | German localized label for card types.
cardTypeLabel :: CardType -> String
cardTypeLabel CardAttack = "[Angriff]"
cardTypeLabel CardSkill  = "[Fertigkeit]"
cardTypeLabel CardPower  = "[Macht]"
cardTypeLabel CardCurse  = "[Fluch]"
cardTypeLabel CardStatus = "[Status]"

-- | Render a single card as a multi-line box (width 16).
renderCardBox :: Int -> Card -> [String]
renderCardBox idx card =
    let topBorder = "┌──────────────┐"
        botBorder = "└──────────────┘"
        emptyInner = "│              │"

        costStr = case Map.lookup "energy" (cardCost card) of
            Just c  -> "(" ++ show c ++ ")"
            Nothing -> if Map.null (cardCost card) then "(0)" else "(" ++ show (sum (Map.elems (cardCost card))) ++ ")"
        prefix = show idx ++ ". "
        availNameW = max 1 (14 - length prefix - length costStr - 1)
        namePart = take availNameW (cardName card)
        gapLen = max 1 (14 - length prefix - length namePart - length costStr)
        headerLine = "│" ++ take 14 (prefix ++ namePart ++ replicate gapLen ' ' ++ costStr ++ replicate 14 ' ') ++ "│"

        cColor = cardTypeAnsiColor (cardType card)
        cLabel = cardTypeLabel (cardType card)
        rawTag = cColor ++ cLabel ++ "\ESC[0m"
        tagVisible = length cLabel
        leftPad = max 0 ((14 - tagVisible) `div` 2)
        rightPad = max 0 (14 - tagVisible - leftPad)
        typeLine = "│" ++ replicate leftPad ' ' ++ rawTag ++ replicate rightPad ' ' ++ "│"

        wrapped = wrapWords 12 (cardDescription card)
        descLines = case wrapped of
            []        -> [emptyInner, emptyInner]
            [l]       -> ["│ " ++ padRightVisible 12 l ++ " │", emptyInner]
            (l1:l2:_) -> ["│ " ++ padRightVisible 12 l1 ++ " │", "│ " ++ padRightVisible 12 l2 ++ " │"]
    in [topBorder, headerLine, typeLine, emptyInner] ++ descLines ++ [botBorder]

-- | Tile multi-line text boxes horizontally with 2-space padding between boxes.
--   Wraps into a new row of boxes when adding another box would exceed maxWidth.
--   Pads boxes in each row vertically to match the height of the tallest box in that row.
hcatBoxes :: Int -> [[String]] -> [String]
hcatBoxes _ [] = []
hcatBoxes maxW allBoxes =
    let spacing = 2
        normBoxes = [ (maximum (0 : map visibleWidth b), b) | b <- allBoxes ]

        groupRows [] = []
        groupRows ((w, b) : rest) =
            let (row, remainder) = takeRow (w + spacing) [ (w, b) ] rest
            in map snd row : groupRows remainder

        takeRow _ current [] = (reverse current, [])
        takeRow usedWidth current ((w, b) : next)
            | usedWidth + w <= maxW =
                takeRow (usedWidth + w + spacing) ((w, b) : current) next
            | otherwise =
                (reverse current, (w, b) : next)

        rows = groupRows normBoxes

        renderRow [] = []
        renderRow rowBoxes =
            let maxH = maximum (0 : map length rowBoxes)
                boxWidths = map (\b -> maximum (0 : map visibleWidth b)) rowBoxes
                padBoxVert h w b =
                    let extraLines = h - length b
                        padded = map (padRightVisible w) b
                    in padded ++ replicate extraLines (replicate w ' ')
                paddedBoxes = zipWith (padBoxVert maxH) boxWidths rowBoxes
                stitchLines lineIdx =
                    intercalate (replicate spacing ' ') [b !! lineIdx | b <- paddedBoxes]
            in [stitchLines i | i <- [0 .. maxH - 1]]

    in intercalate [""] (map renderRow rows)

-- | Compact combat HUD above hand cards in deckbuilder mode.
renderDeckCombatHud :: GameState -> DeckState -> [String]
renderDeckCombatHud st ds =
    let w = 78
        doubleLine = replicate w '═'
        singleLine = replicate w '─'

        p = player (save st)
        hp = playerHealth p
        maxHp = effectiveMaxHealth st
        blockVal = case getVariable "player.block" st of
            Just (VVInt b) -> b
            _ -> case getVariable "block" st of
                Just (VVInt b) -> b
                _              -> 0
        energyVal = case getVariable "player.energy" st of
            Just (VVInt e) -> e
            _ -> case getVariable "energy" st of
                Just (VVInt e) -> e
                _              -> 3
        maxEnergyVal = case getVariable "player.max_energy" st of
            Just (VVInt m) -> m
            _ -> case getVariable "max_energy" st of
                Just (VVInt m) -> m
                _              -> 3

        deckCnt = length (drawPile ds)
        discCnt = length (discardPile ds)
        exhCnt  = length (exhaustPile ds)
        exhStr  = if exhCnt > 0 then " | Erschöpft: " ++ show exhCnt else ""

        statusBar = " [Deck: " ++ show deckCnt ++ "] ─── HP " ++ show hp ++ "/" ++ show maxHp
                    ++ " | Block: " ++ show blockVal
                    ++ " | Energie: " ++ show energyVal ++ "/" ++ show maxEnergyVal
                    ++ exhStr
                    ++ " ─── [Ablage: " ++ show discCnt ++ "]"

        curRoom = currentRoom (save st)
        roomEnemies = [ def
                      | def <- getNPCsInRoom curRoom st
                      , not (isDeadNPC (npcId def) st)
                      , not (isInParty (npcId def) st)
                      ]

        enemyLines = concatMap formatEnemy roomEnemies
        formatEnemy def =
            let eHp = case Map.lookup (npcId def) (npcStates (save st)) of
                    Just ns -> fromMaybe 0 (npcHealth ns)
                    Nothing -> 0
                eMax = fromMaybe eHp (npcMaxHealth def)
                eLine = " GEGNER: " ++ npcName def ++ " (HP: " ++ show eHp ++ "/" ++ show eMax ++ ")"
                mIntent = case getVariable ("intent." ++ npcId def) st of
                    Just (VVText it) -> Just it
                    _ -> case getVariable ("combat.intent." ++ npcId def) st of
                        Just (VVText it) -> Just it
                        _                -> Nothing
                intentLine = case mIntent of
                    Just it -> [" ABSICHT: " ++ it]
                    Nothing -> []
            in eLine : intentLine
    in [doubleLine, statusBar, singleLine]
       ++ (if null enemyLines then [" (Keine Gegner im Raum)"] else enemyLines)
       ++ [doubleLine]

-- | Display the current hand in horizontal tile layout with combat HUD.
showHand :: GameState -> CommandResult
showHand st = case deckState (save st) of
    Nothing -> (st, "You don't have a deck.")
    Just ds ->
        let curHand = hand ds
            hudLines = renderDeckCombatHud st ds
        in if null curHand
           then (st, intercalate "\n" (hudLines ++ ["Your hand is empty."]))
           else
               let lookupCardBox idx cId = case Map.lookup cId (cardDefs (world st)) of
                       Just c  -> renderCardBox idx c
                       Nothing ->
                           [ "┌──────────────┐"
                           , "│ " ++ padRightVisible 12 (show idx ++ ". " ++ take 8 cId) ++ " │"
                           , "│  [Unbekannt] │"
                           , "│              │"
                           , "│ Nicht        │"
                           , "│ gefunden     │"
                           , "└──────────────┘"
                           ]
                   cardBoxes = zipWith lookupCardBox [1 :: Int ..] curHand
                   tiledHand = hcatBoxes 80 cardBoxes
               in (st, intercalate "\n" (hudLines ++ [""] ++ tiledHand))

-- | Display draw pile summary.
showDeck :: GameState -> CommandResult
showDeck st = case deckState (save st) of
    Nothing -> (st, "You don't have a deck.")
    Just ds ->
        let curDraw = drawPile ds
            curHand = hand ds
            curDisc = discardPile ds
            curExh  = exhaustPile ds
            total = length curDraw + length curHand + length curDisc + length curExh
            nameOf cId = case Map.lookup cId (cardDefs (world st)) of
                Just c  -> cardName c
                Nothing -> cId
            cardCounts = Map.toList (Map.fromListWith (+) [(nameOf cId, 1 :: Int) | cId <- curDraw])
            cardLines = [ "  - " ++ name ++ (if cnt > 1 then " (x" ++ show cnt ++ ")" else "")
                        | (name, cnt) <- cardCounts ]
            header = "=== Draw Pile (" ++ show (length curDraw) ++ "/" ++ show total ++ " cards) ==="
            body = if null cardLines then ["  (Empty)"] else cardLines
            footer = "Hand: " ++ show (length curHand)
                     ++ " | Discard: " ++ show (length curDisc)
                     ++ " | Exhaust: " ++ show (length curExh)
        in (st, intercalate "\n" ([header] ++ body ++ [footer]))

-- | Display discard pile contents.
showDiscard :: GameState -> CommandResult
showDiscard st = case deckState (save st) of
    Nothing -> (st, "You don't have a deck.")
    Just ds ->
        let curDisc = discardPile ds
            nameOf cId = case Map.lookup cId (cardDefs (world st)) of
                Just c  -> cardName c
                Nothing -> cId
            cardCounts = Map.toList (Map.fromListWith (+) [(nameOf cId, 1 :: Int) | cId <- curDisc])
            cardLines = [ "  - " ++ name ++ (if cnt > 1 then " (x" ++ show cnt ++ ")" else "")
                        | (name, cnt) <- cardCounts ]
            header = "=== Discard Pile (" ++ show (length curDisc) ++ " cards) ==="
            body = if null cardLines then ["  (Empty)"] else cardLines
        in (st, intercalate "\n" ([header] ++ body))

