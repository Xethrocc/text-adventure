{-# LANGUAGE LambdaCase #-}
-- | Core game state and manipulation for the text adventure engine
--
--   Explicit export list (Phase 0.7): 27 internal helpers stay private so
--   @-Wall@ can report them as unused once they become dead.
-- | Core game state and manipulation for the text adventure engine
--
--   Explicit export list (Phase 0.7): 27 internal helpers stay private so
--   @-Wall@ can report them as unused once they become dead.
module Game
    (
      -- * State, rooms and movement
      emptyGameState
    , emptyGameWorld
    , ensureRoomExists
    , lookupRoom
    , getCurrentRoom
    , canonicalRoomId
    , moveToRoom
    , canMove
    , getExitInDirection
    , effectiveConnections
    , markCurrentRoomVisited
    , isRoomVisited
    , setRoomVisited
    , incrementTurnCount
      -- * Lookups and text normalisation
    , getItemsInLocation
    , getNPCsInRoom
    , hasItem
    , playerHasTaggedItem
    , isLivingNPCInRoom
    , isDeadNPC
    , isPlayerDead
    , itemAliases
    , npcAliases
    , matchesItemTarget
    , normalizeText
      -- * Inventory and equipment
    , pickupItem
    , dropItem
    , giveItem
    , consumeItem
    , equipItem
    , unequipItem
    , isEquipped
    , equipmentSummary
    , syncInventory
    , discoverItem
    , modifyItemProp
      -- * NPCs, skills and the party
    , updateNPCState
    , modifyNPCProp
    , clampNPCHealth
    , moveNPCToRoom
    , followParty
    , isInParty
    , partyMembersInRoom
    , getSkill
    , modifySkill
    , resolveActorNpcId
    , getEntityState
    , setEntityState
      -- * Flags, variables, conditions and predicates
    , getFlag
    , setFlag
    , getVariable
    , setVariable
    , setVariableChecked
    , hasCondition
    , applyCondition
    , applyConditionWithHidden
    , clearCondition
    , evalExpr
    , evalPredicate
    , resolveValueRef
    , formatWithVars
    , lookupVarForFormat
    , setPlayerHP
    , updatePlayerHealth
    , addDiagnostic
      -- * Combat
    , effectiveAttack
    , effectiveDefense
    , effectiveMaxHealth
    , combatRound
    , setCombatRound
    , combatRoundKey
    , combatEngagedKey
    , setCombatEngaged
    , isCombatEngaged
    , combatAbilityKey
    , combatActionKey
    , combatInitiativeKey
    , combatInitiativePlayerKey
    , combatVarPrefix
      -- * Cards and deck
    , drawCards
    , discardCard
    , discardHand
    , addCardToDeck
    , exhaustCard
    , shuffleDeck
    , shuffleList
      -- * Art, text and rendering helpers
    , asciiPlayback
    , asciiFrames
    , defaultFrameMicros
    , renderArtForLook
    , endArtFor
    , resolveAsciiArt
    , resolveCondText
    , resolveHotspotTarget
    , replaceChar
    , removeAt
      -- * Dialogue
    , setActiveDialogue
    , clearActiveDialogue
    , setDialogueNode
      -- * Game end
    , endGame
    ) where

import Types
import Messages (formatStringWith, renderMsg)
import Data.List (foldl', isPrefixOf, nub, stripPrefix)
import Data.Bits (shiftR)
import Data.Char (toLower, isDigit, isSpace)
import Data.Maybe (listToMaybe, fromMaybe)
import Control.Applicative ((<|>))
import Data.Word (Word64)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable

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
    , procDefs           = Map.empty
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
    , pendingSfx = []
    , pendingMusic = Nothing
    , diagnostics = []
    , lastVeto = Nothing
    , chosenTarget = Nothing
    , procScopes = []
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

-- | Replace coordinate placeholders in AsciiArt.
replaceAsciiCoords :: AsciiArt -> Int -> Int -> Int -> AsciiArt
replaceAsciiCoords art x y z =
    art
        { aaStatic = replaceCondTextCoords (aaStatic art) x y z
        , aaFrames = map (\ct -> replaceCondTextCoords ct x y z) (aaFrames art)
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
        rAscii = replaceAsciiCoords (btAsciiArt biome) x y z
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
            , roomDarkMsg       = Nothing
            , roomOnEnter       = Nothing
            , roomOnLook        = Nothing
            , roomOnExit        = Nothing
            , roomSearchOutcome = Nothing
            , roomAscii         = rAscii
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


-- | Check if player has an item in inventory
hasItem :: ItemID -> GameState -> Bool
hasItem iId state = case Map.lookup iId (itemStates (save state)) of
    Just itemState -> itemLocation itemState == CarriedBy ActorPlayer
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
                (Map.filter (\itemState -> itemLocation itemState == CarriedBy ActorPlayer) (itemStates saveState))
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
    canonExit (Open to)          = Open (canonicalRoomId (world state) to)
    canonExit (Locked to k)      = Locked (canonicalRoomId (world state) to) k
    canonExit (Guarded to p msg) = Guarded (canonicalRoomId (world state) to) p msg
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
pickupItem iId state = relocateItem iId (CarriedBy ActorPlayer) state

-- | Remove item from player's inventory to current room
dropItem :: ItemID -> GameState -> GameState
dropItem iId state = relocateItem iId (InRoom (currentRoom (save state))) state

-- | Give item directly to player inventory (e.g., NPC reward, loot)
giveItem :: ItemID -> GameState -> GameState
giveItem iId state = relocateItem iId (CarriedBy ActorPlayer) state


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
        Nothing -> Left (renderMsg "item.no_id" [("id", iId)])
        Just def -> case itemEquipSlot def of
            Nothing -> Left (renderMsg "equip.not_equippable" [("item", itemName def)])
            Just slot ->
                if not (hasItem iId state)
                then Left (renderMsg "equip.need_carried" [("item", itemName def)])
                else case equippedInSlot slot state of
                    Just other | other /= iId ->
                        Left (renderMsg "equip.slot_occupied" [("item", other)])
                    _ -> Right $ state { save = (save state) { equipment = Map.insert slot iId (equipment (save state)) } }

-- | Unequip an item by id
unequipItem :: ItemID -> GameState -> GameState
unequipItem iId state =
    let newEquip = Map.filter (/= iId) (equipment (save state))
    in state { save = (save state) { equipment = newEquip } }


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
    | Map.null (equipment (save state)) = renderMsg "equip.nothing" []
    | otherwise =
        renderMsg "equip.header" [] ++ unlines
            [ renderMsg "equip.line" [("slot", show slot), ("name", nameFor iId)]
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

-- | Apply (or refresh) a timed status effect (defaults to not hidden)
applyCondition :: String -> Int -> Maybe Effect -> Maybe Effect -> GameState -> GameState
applyCondition name turns tick end = applyConditionWithHidden name turns tick end False

-- | Apply (or refresh) a timed status effect with explicit hidden flag (Phase 2.1)
applyConditionWithHidden :: String -> Int -> Maybe Effect -> Maybe Effect -> Bool -> GameState -> GameState
applyConditionWithHidden name turns tick end hidden state = state
    { save = (save state)
        { conditions = Map.insert name (Condition name turns tick end hidden) (conditions (save state)) } }

-- | Remove a status effect
clearCondition :: String -> GameState -> GameState
clearCondition name state = state
    { save = (save state) { conditions = Map.delete name (conditions (save state)) } }

-- | Is a status effect active?
hasCondition :: String -> GameState -> Bool
hasCondition name state = Map.member name (conditions (save state))


-- ---------------------------------------------------------------------------
-- Variables (Phase 3b)
-- ---------------------------------------------------------------------------

-- | Read an adventure-declared variable value.
-- | Look up a name in the procedure scope stack (Phase 2.5), innermost
--   binding first: parameters and locals shadow adventure variables.
lookupProcScope :: String -> GameState -> Maybe VariableValue
lookupProcScope name state = go (procScopes state)
  where
    go [] = Nothing
    go (scope:rest) = case Map.lookup name scope of
        Just v  -> Just v
        Nothing -> go rest

getVariable :: String -> GameState -> Maybe VariableValue
getVariable name state =
    lookupProcScope name state <|> Map.lookup name (variables (save state))

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
evalPredicate (HasCondition cn) st = hasCondition cn st
-- A state predicate checks the entity's state in whichever layer stores it:
-- `entityStates` (exit locks, doors, `set_state:` targets), an NPC's status
-- ("alive"/"dead", as `killNPC` and the combat path set it) or an item's status
-- ("intact"/…). Reading only `entityStates` made `{state: <npc>, is: alive}` —
-- which `starship.yaml` and `combo.yaml` both use — always false.
evalPredicate (EntityHasState entity expected) st =
    getEntityState entity st == Just expected
        || npcStateMatches
        || itemStateMatches
        || cardStateMatches
        || varTextMatches
  where
    npcStateMatches = case Map.lookup entity (npcStates (save st)) of
        Just ns -> npcStatus ns == expected
        Nothing -> False
    itemStateMatches = case Map.lookup entity (itemStates (save st)) of
        Just is -> itemStatus is == expected
        Nothing -> False
    cardStateMatches = case deckState (save st) of
        Just ds -> case expected of
            "hand"        -> entity `elem` hand ds
            "in_hand"     -> entity `elem` hand ds
            "draw"        -> entity `elem` drawPile ds
            "discard"     -> entity `elem` discardPile ds
            "exhaust"     -> entity `elem` exhaustPile ds
            _             -> False
        Nothing -> False
    varTextMatches = case Map.lookup entity (variables (save st)) of
        Just (VVText s) -> s == expected
        _               -> False
evalPredicate (RoomHasTag rId tag) st =
    case lookupRoom rId st of
        Just room -> tag `Set.member` roomTags room
        Nothing   -> False
evalPredicate (Location actor rId) st = case actor of
    ActorPlayer     -> currentRoom (save st) == rId
    ActorNPC nId    -> checkEntityLoc nId
    ActorEntity eId -> checkEntityLoc eId
    ActorShip _     -> False
    ActorRoom _     -> False
  where
    checkEntityLoc eId =
        case Map.lookup eId (npcStates (save st)) of
            Just ns -> npcLocation ns == InRoom rId
            Nothing -> case Map.lookup eId (itemStates (save st)) of
                Just is -> itemLocation is == InRoom rId
                Nothing -> False
evalPredicate (CompareVar name op n) st =
    case getVariable name st of
        Just (VVInt v) -> fromMaybe False (compareValues op v n)
        _              -> case name of
            _ | Just cn <- stripPrefix "condition_turns." name ->
                fromMaybe False (compareValues op (resolveValueRef (VRConditionTurns cn) st) n)
            _ -> False
-- A text variable equals a literal. This is the read side of `variables:` with
-- `type: text`: the value is a `VVText`, and no other predicate can inspect it
-- (`compare_var` only compares numbers, `compare` resolves text to 0). Such a
-- variable is written by the engine itself (`combat.action` / `combat.ability`,
-- which would otherwise be write-only) and seeded by `initial:` /
-- `initial_variables:`. The authored `set_var` outcome takes integers only
-- (`AOSetVar String Int`), so it cannot produce a text value.
evalPredicate (VarIs name expected) st =
    case getVariable name st of
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
resolveValueRef (VRConditionTurns cName) st =
    case Map.lookup cName (conditions (save st)) of
        Just c  -> condRemaining c
        Nothing -> 0
resolveValueRef (VRVariable name) st =
    case getVariable name st of
        Just (VVInt n)  -> n
        Just (VVText s) -> case reads s of [(n,"")] -> n; _ -> 0
        _               -> case name of
            "player.hp"         -> playerHealth (player (save st))
            "player.health"     -> playerHealth (player (save st))
            "player.max_hp"     -> playerMaxHealth (player (save st))
            "player.max_health" -> playerMaxHealth (player (save st))
            "turn.count"        -> turnCount (save st)
            "turns"             -> turnCount (save st)
            "hand.count"        -> maybe 0 (length . hand) (deckState (save st))
            "deck.count"        -> maybe 0 (length . drawPile) (deckState (save st))
            "discard.count"     -> maybe 0 (length . discardPile) (deckState (save st))
            "exhaust.count"     -> maybe 0 (length . exhaustPile) (deckState (save st))
            _ | Just cn <- stripPrefix "condition_turns." name ->
                resolveValueRef (VRConditionTurns cn) st
              | Just rest <- stripPrefix "item." name, (itId, '.':prop) <- break (== '.') rest ->
                resolveValueRef (VRItemProp itId prop) st
              | Just rest <- stripPrefix "npc." name, (nId, '.':prop) <- break (== '.') rest ->
                resolveValueRef (VRActorProp (ActorNPC nId) (PCustom prop)) st
              | otherwise -> case reads name of
                  [(n, "")] -> n
                  _         -> 0
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
    | Just val <- getVariable name st =
        Just (varToString val)
    | Just fName <- stripPrefix "flag:" name <|> stripPrefix "flag." name =
        case Map.lookup fName (flags (save st)) of
            Just v  -> Just v
            Nothing -> Just "false"
    | Just v <- Map.lookup name (flags (save st)) =
        Just v
    | Just rest <- stripPrefix "item." name =
        case break (== '.') rest of
            (itId, '.':prop) ->
                case Map.lookup itId (itemStates (save st)) of
                    Nothing -> Just ("<error: unknown item '" ++ itId ++ "'>")
                    Just is -> case Map.lookup prop (itemProps is) of
                        Nothing -> Just ("<error: unknown prop '" ++ prop ++ "' on item '" ++ itId ++ "'>")
                        Just val -> Just (show val)
            _ -> Nothing
    | Just rest <- stripPrefix "npc." name =
        case break (== '.') rest of
            (nId, '.':prop) ->
                let actualId = resolveActorNpcId nId st
                in case Map.lookup actualId (npcStates (save st)) of
                    Nothing -> Just ("<error: unknown npc '" ++ nId ++ "'>")
                    Just ns -> case prop of
                        "health" -> Just (show (fromMaybe 0 (npcHealth ns)))
                        "hp"     -> Just (show (fromMaybe 0 (npcHealth ns)))
                        _        -> case Map.lookup prop (npcProps ns) of
                            Nothing -> Just ("<error: unknown prop '" ++ prop ++ "' on npc '" ++ nId ++ "'>")
                            Just val -> Just (show val)
            _ -> Nothing
    | Just cName <- stripPrefix "condition_turns." name =
        Just (show (resolveValueRef (VRConditionTurns cName) st))
    | name `elem` ["player.hp", "player.health"] =
        Just (show (playerHealth (player (save st))))
    | name `elem` ["player.max_hp", "player.max_health"] =
        Just (show (playerMaxHealth (player (save st))))
    | name `elem` ["turn.count", "turns"] =
        Just (show (turnCount (save st)))
    | name `elem` ["hand.count", "cards_in_hand"] =
        Just (show (maybe 0 (length . hand) (deckState (save st))))
    | name `elem` ["deck.count", "draw_pile.count"] =
        Just (show (maybe 0 (length . drawPile) (deckState (save st))))
    | name `elem` ["discard.count", "discard_pile.count"] =
        Just (show (maybe 0 (length . discardPile) (deckState (save st))))
    | name `elem` ["exhaust.count", "exhaust_pile.count"] =
        Just (show (maybe 0 (length . exhaustPile) (deckState (save st))))
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

-- ---------------------------------------------------------------------------
-- Conditional text (Phase 3g)
--
--   formatStringWith/applyVarModifier moved to Messages.hs (Phase 1.1);
--   imported there. formatWithVars stays: it is the var-resolver bridge.
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



-- | Set player HP directly (clamped to max)
setPlayerHP :: Int -> GameState -> GameState
setPlayerHP n state =
    let saveSt = save state
        p = player saveSt
        maxHp = playerMaxHealth p
        p' = p { playerHealth = max 0 (min maxHp n) }
    in state { save = saveSt { player = p' } }



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

-- | Draw up to n cards from draw pile into hand (capped by maxHandSize if > 0).
--   If draw pile runs out, discard pile is shuffled into draw pile.
--   Hand limit is checked after reshuffle so cards in discard are properly recycled.
drawCards :: Int -> GameState -> GameState
drawCards n st
    | n <= 0    = st
    | otherwise = modifyDeckState (drawCardsHelper n) st

drawCardsHelper :: Int -> DeckState -> GameState -> GameState
drawCardsHelper n ds0 currentSt = go n ds0 currentSt
  where
    handCap = maxHandSize ds0

    go remainingCards ds stAcc
        | remainingCards <= 0 = stAcc
        | handCap > 0 && length (hand ds) >= handCap =
            -- Hand is full. If drawPile is empty and discardPile has cards,
            -- reshuffle discardPile into drawPile now so cards are ready.
            if null (drawPile ds) && not (null (discardPile ds))
            then
                let (shuffled, newRng) = shuffleList (discardPile ds) (rngState (save stAcc))
                    ds' = ds { drawPile = shuffled, discardPile = [] }
                in stAcc { save = (save stAcc) { deckState = Just ds', rngState = newRng } }
            else stAcc { save = (save stAcc) { deckState = Just ds } }
        | not (null (drawPile ds)) =
            let c = head (drawPile ds)
                ds' = ds { hand = hand ds ++ [c], drawPile = tail (drawPile ds) }
                stAcc' = stAcc { save = (save stAcc) { deckState = Just ds' } }
            in go (remainingCards - 1) ds' stAcc'
        | not (null (discardPile ds)) =
            let (shuffled, newRng) = shuffleList (discardPile ds) (rngState (save stAcc))
                ds' = ds { drawPile = shuffled, discardPile = [] }
                stAcc' = stAcc { save = (save stAcc) { deckState = Just ds', rngState = newRng } }
            in go remainingCards ds' stAcc'
        | otherwise =
            stAcc { save = (save stAcc) { deckState = Just ds } }

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
                DestHand    -> if maxHandSize ds <= 0 || length (hand ds) < maxHandSize ds
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


