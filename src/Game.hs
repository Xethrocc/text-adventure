{-# LANGUAGE LambdaCase #-}
-- | Core game state and manipulation for the text adventure engine
module Game where

import Types
import Data.List (intercalate, find, elemIndex)
import Data.Char (toLower)
import Data.Maybe (listToMaybe, fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

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
        , containers         = Map.empty
        , triggerStates      = Map.empty
        }
    , pendingNarrative = Nothing
    }

-- ---------------------------------------------------------------------------
-- Lookups
-- ---------------------------------------------------------------------------

-- | Helper to get current room from game state
getCurrentRoom :: GameState -> Maybe Room
getCurrentRoom state = Map.lookup (currentRoom (save state)) (rooms (world state))

-- | Get all visible items in a location.
--   Hidden items only appear once they have been discovered via `search`.
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

-- | Check if a direction is valid from current room
canMove :: Direction -> GameState -> Bool
canMove dir state = case getCurrentRoom state of
    Just room -> dir `Map.member` roomConnections room
    Nothing   -> False

-- | Get exit in a given direction
getExitInDirection :: Direction -> GameState -> Maybe Exit
getExitInDirection dir state = case getCurrentRoom state of
    Just room -> Map.lookup dir (roomConnections room)
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

-- | Update NPC state (health, status, etc)
updateNPCState :: String -> NPCState -> GameState -> GameState
updateNPCState targetNpcId newNpcState state = state
    { save = (save state) { npcStates = Map.insert targetNpcId newNpcState (npcStates (save state)) } }

-- | Move NPC to void (dead) and fire state-change triggers, unlocking entity.
killNPC :: String -> GameState -> GameState
killNPC targetNpcId state =
    let state' = state
            { save = (save state)
                { npcStates = Map.adjust (\s -> s { npcLocation = Removed, npcStatus = "dead" }) targetNpcId (npcStates (save state))
                , entityStates = Map.insert targetNpcId "dead" (entityStates (save state))
                } }
    in fst (fireTriggers (OnStateChange targetNpcId) state')

-- | Modify an NPC's property
modifyNPCProp :: String -> String -> Int -> GameState -> GameState
modifyNPCProp nId prop delta state = state
    { save = (save state) { npcStates = Map.adjust (\s ->
        let currentVal = Map.findWithDefault 0 prop (npcProps s)
        in s { npcProps = Map.insert prop (currentVal + delta) (npcProps s) }
        ) nId (npcStates (save state)) } }

-- | Move an NPC to a different room
moveNPCToRoom :: String -> RoomID -> GameState -> GameState
moveNPCToRoom nId targetRoom state = state
    { save = (save state) { npcStates = Map.adjust (\s -> s { npcLocation = InRoom targetRoom }) nId (npcStates (save state)) } }

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
        npcIsAlive npc = case Map.lookup (npcId npc) (npcStates (save state)) of
            Just ns -> npcStatus ns /= "dead"
            Nothing -> False
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
tickConditions state = foldl step (state, []) (Map.toList (conditions (save state)))
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

-- | Evaluate a Predicate against the current game state.
evalPredicate :: Predicate -> GameState -> Bool
evalPredicate PTrue _ = True
evalPredicate (PNot p) st = not (evalPredicate p st)
evalPredicate (PAll ps) st = all (\p -> evalPredicate p st) ps
evalPredicate (PAny ps) st = any (\p -> evalPredicate p st) ps
evalPredicate (PlayerHas iId) st = hasItem iId st
evalPredicate (HasFlag f) st = getFlag f st == Just "true"
evalPredicate (EntityHasState entity expected) st =
    getEntityState entity st == Just expected
evalPredicate (RoomHasTag rId tag) st =
    case Map.lookup rId (rooms (world st)) of
        Just room -> tag `Set.member` roomTags room
        Nothing   -> False
evalPredicate (Location eId rId) st =
    case Map.lookup eId (npcStates (save st)) of
        Just ns  -> npcLocation ns == InRoom rId
        Nothing  -> case Map.lookup eId (itemStates (save st)) of
            Just is -> itemLocation is == InRoom rId
            Nothing -> False
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
        _               -> 0
resolveValueRef (VRFlag f) st =
    case getFlag f st of
        Just "true"  -> 1
        Just _       -> 0
        Nothing      -> 0
resolveValueRef (VRItemProp iId prop) st =
    case Map.lookup iId (itemStates (save st)) of
        Just is -> Map.findWithDefault 0 prop (itemProps is)
        Nothing -> 0
resolveValueRef (VRProperty eId prop) st =
    case Map.lookup eId (npcStates (save st)) of
        Just ns -> case prop of
            "hp" -> fromMaybe 0 (npcHealth ns)
            _    -> Map.findWithDefault 0 prop (npcProps ns)
        Nothing -> 0
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
-- Conditional text (Phase 3g)
-- ---------------------------------------------------------------------------

-- | Resolve a CondText: first variant whose predicate holds wins; otherwise
--   the default is returned.
resolveCondText :: CondText -> GameState -> String
resolveCondText ct state =
    case [tvText tv | tv <- ctVariants ct, evalPredicate (tvWhen tv) state] of
        (s:_) -> s
        []    -> ctDefault ct

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
    | depth > maxOutcomeDepth = (state, "[ERROR] Maximum outcome depth exceeded.", salt)
    | otherwise = case outcome of
    SendMessage msg -> (state, msg, salt)

    Sequence outcomes ->
        let (st', msg', salt') = foldl (\(st, acc, s) o ->
                let (st2, m2, s2) = applyOutcomeWith (depth + 1) s o targetId st
                in (st2, if null acc then m2 else acc ++ "\n" ++ m2, s2))
                (state, "", salt) outcomes
        in (st', msg', salt')

    SetValue vr ev ->
        let state' = applySetValue vr ev state
        in (state', "", salt)

    ModifyValue VRPlayerHealth delta ->
        let state' = if delta >= 0
                     then updatePlayerHealth (+ delta) state
                     else let st' = updatePlayerHealth (+ delta) state
                          in if isPlayerDead st' then endGame Death st' else st'
        in (state', "", salt)
    ModifyValue vr delta ->
        let state' = modifyValueProp vr delta state
        in (state', "", salt)

    MoveEntity eid (InRoom room) ->
        let state' = moveEntityToRoom eid room state
        in (state', "", salt)
    MoveEntity eid (CarriedBy _) ->
        let state' = giveItem eid state
        in (state', "", salt)
    MoveEntity eid Removed ->
        let state' = consumeItem eid state
        in (state', "", salt)
    MoveEntity eid (EquippedBy _ slot) ->
        case equipItem eid state of
            Left err    -> (state, err, salt)
            Right st'   -> (st', "", salt)
    MoveEntity eid (InContainer _) ->
        (state, "", salt)

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
            rng = rngState (save state)
            pick = fromIntegral (rng `mod` fromIntegral totalWeight)
            st' = state { save = (save state) { rngState = nextRng (rng + fromIntegral salt) } }
            go :: Int -> [(Int, Effect)] -> Effect
            go _ [(_, e)] = e
            go acc ((w, e):rest)
                | pick < acc + w = e
                | otherwise = go (acc + w) rest
            go _ [] = Noop
        in applyOutcomeWith (depth + 1) (salt + 1) (go 0 weighted) targetId st'

    GameEnd reason msg -> (endGame reason state, msg, salt)

    ApplyCondition name turns tick end -> (applyCondition name turns tick end state, "", salt)
    ClearCondition name -> (clearCondition name state, "", salt)

    ModifySkill skillId delta -> (modifySkill skillId delta state, "", salt)

    -- Narrative: store lines + follow-up for interactive display
    Narrative lines followUp ->
        (state { pendingNarrative = Just (lines, followUp) }, intercalate "\n" lines, salt)

    Noop -> (state, "", salt)

-- | Apply SetValue: set a value reference to a new value (handles flags, variables, states)
applySetValue :: ValueRef -> EffectValue -> GameState -> GameState
applySetValue (VRFlag name) val state =
    setFlag name (effectValueToString val) state
applySetValue (VRVariable name) val state =
    setVariable name (effectValToVarVal val) state
applySetValue (VRProperty eId "state") val state =
    setEntityState eId (effectValueToString val) state
applySetValue (VRProperty "player" "room") val state =
    fst (transitionToRoom (effectValueToString val) (clearActiveDialogue state))
applySetValue (VRProperty rId "visited") val state =
    let b = case val of { EVInt n -> n /= 0; _ -> False }
    in setRoomVisited rId b state
applySetValue VRPlayerHealth val state =
    let n = case val of { EVInt n -> n; _ -> 0 }
    in if n <= 0 then endGame Death (setPlayerHP n state) else setPlayerHP n state
applySetValue _ _ state = state

-- | Convert EffectValue to VariableValue
effectValToVarVal :: EffectValue -> VariableValue
effectValToVarVal (EVInt n)    = VVInt n
effectValToVarVal (EVString s) = VVText s
effectValToVarVal (EVBool b)   = VVInt (if b then 1 else 0)

-- | Apply ModifyValue to a non-player-health reference
modifyValueProp :: ValueRef -> Int -> GameState -> GameState

-- | Convert EffectValue to String (for flag/state values)
effectValueToString :: EffectValue -> String
effectValueToString (EVInt n)    = show n
effectValueToString (EVString s) = s
effectValueToString (EVBool b)   = if b then "true" else "false"

-- | Apply ModifyValue to a non-player-health reference
modifyValueProp (VRFlag name) delta state =
    let cur = case getFlag name state of
            Just "true" -> 1
            _           -> 0
        newVal = if cur + delta > 0 then "true" else "false"
    in setFlag name newVal state
modifyValueProp (VRVariable name) delta state =
    let cur = case getVariable name state of
            Just (VVInt n)  -> n
            Just (VVText s) -> case reads s of [(n,_)] -> n; _ -> 0
            _               -> 0
    in setVariable name (VVInt (cur + delta)) state
modifyValueProp (VRItemProp iId prop) delta state =
    modifyItemProp iId prop delta state
modifyValueProp (VRProperty eId "hp") delta state =
    modifyNPCHealth eId delta state
modifyValueProp (VRProperty nId prop) delta state =
    modifyNPCProp nId prop delta state
modifyValueProp _ _ state = state

-- | Modify an NPC's health, killing them if <= 0
modifyNPCHealth :: NPCID -> Int -> GameState -> GameState
modifyNPCHealth nId delta state =
    case Map.lookup nId (npcStates (save state)) of
        Nothing -> state
        Just n ->
            let oldHealth = fromMaybe 0 (npcHealth n)
                newHealth = oldHealth + delta
                state' = updateNPCState nId (n { npcHealth = Just newHealth }) state
                state'' = clampNPCHealth nId state'
            in if newHealth <= 0 then killNPC nId state'' else state''

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

-- | Apply zero or more outcomes in sequence
applyOutcomes :: [Effect] -> ItemID -> GameState -> CommandResult
applyOutcomes outcomes targetId state =
    let (st, msg, _) = foldl (\(s, acc, slt) o ->
            let (s2, m2, slt2) = applyOutcomeWith 0 slt o targetId s
            in (s2, if null acc then m2 else acc ++ "\n" ++ m2, slt2))
            (state, "", 0) outcomes
    in (st, msg)

-- | Run a room hook (onEnter / onLook / onExit) if one is defined
runRoomHook :: (Room -> Maybe Effect) -> RoomID -> GameState -> (GameState, String)
runRoomHook hook rId state =
    case Map.lookup rId (rooms (world state)) >>= hook of
        Nothing -> (state, "")
        Just outcome -> applyOutcome outcome "" state

-- | Move the player to another room, running exit/enter hooks and marking the
--   destination visited. Used by both walking (Go) and TransitionRoom outcomes
--   so data-driven teleports behave exactly like walks.
transitionToRoom :: RoomID -> GameState -> (GameState, String)
transitionToRoom dest state =
    let cur = currentRoom (save state)
        stAfterDialogue = clearActiveDialogue state
        (stAfterExit, exitMsg) = runRoomHook roomOnExit cur stAfterDialogue
        moved = moveToRoom dest stAfterExit
        visited = markCurrentRoomVisited moved
        (finalState, enterMsg) = runRoomHook roomOnEnter dest visited
        fullMsg = intercalate "\n" (filter (not . null) [exitMsg, enterMsg])
    in (finalState, fullMsg)

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
        _ -> unlines ("=== Journal ===" : []) ++
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
vehicleStopList v = Map.toList (vehicleStops v)

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
                ( state { save = (save state)
                    { currentVehicle = Just vId
                    , currentRoom = vehicleEntryRoom v
                    , visitedRooms = Set.insert (vehicleEntryRoom v) (visitedRooms (save state)) } }
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
                ( state { save = (save state)
                    { currentVehicle = Nothing
                    , currentRoom = outside
                    , visitedRooms = Set.insert outside (visitedRooms (save state)) } }
                , "You disembark from the " ++ vehicleName v ++ "." )

-- | Move a vehicle to a stop's outside room (updates its position).
--   Returns updated state; the caller decides whether the player travels with it.
moveVehicleToStop :: VehicleID -> RoomID -> GameState -> GameState
moveVehicleToStop vId stopRoom state
    | currentVehicle (save state) == Just vId =
        -- Player is aboard: travel with the vehicle
        setVehicleState vId ((getVehicleState vId state) { vsCurrentStop = stopRoom })
            state { save = (save state) { currentRoom = stopRoom } }
    | otherwise =
        setVehicleState vId ((getVehicleState vId state) { vsCurrentStop = stopRoom }) state

-- | Consume the fare for a PaidVehicle stop, if one is required.
--   Returns Left with the error message when the player cannot pay.
payStopCost :: VehicleStop -> GameState -> Either String GameState
payStopCost stop state = case stopCost stop of
    Nothing -> Right state
    Just (itemId, errMsg) ->
        if hasItem itemId state
        then Right (consumeItem itemId state)
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
    Just _ ->
        let vs = getVehicleState vId state
            maxFuel = maybe 0 snd (vehicleFuelProp =<< lookupVehicle vId state)
            cur = fromMaybe 0 (vsFuel vs)
            newFuel = min maxFuel (cur + amount)
        in Just (setVehicleState vId (vs { vsFuel = Just newFuel }) state,
                 "The " ++ maybe vId vehicleName (lookupVehicle vId state) ++ " is fuelled ("
                    ++ show newFuel ++ "/" ++ show maxFuel ++ ").")

-- | Clear a vehicle condition (e.g. after `repair`)
clearVehicleCondition :: VehicleID -> String -> GameState -> GameState
clearVehicleCondition vId condName state =
    let vs = getVehicleState vId state
    in setVehicleState vId (vs { vsActiveConditions = Set.delete condName (vsActiveConditions vs) }) state

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
                   let (st', msgs) = foldl (\(s, ms) o ->
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
                    (Just (fname, maxF), Just f) ->
                        "\n" ++ (if f <= 0 then "Out of " else "Fuel (") ++ ""
                                ++ (if f > 0 then fname ++ ": " else "") ++ show f ++ "/" ++ show maxF ++ ")"
                    _ -> ""
                statusLine = unlines (filter (not . null) [condLine]) ++
                             (if null condLine then "" else "\n") ++ fuelLine
            in if null (trim statusLine) then Nothing else Just (trim statusLine)
  where
    trim = f . f where f = reverse . dropWhile (== '\n')

-- ---------------------------------------------------------------------------
-- Trigger system (Phase 3f)
-- ---------------------------------------------------------------------------

-- | Fire triggers matching the given event type.
--   Returns updated state and accumulated messages from all triggered effects.
fireTriggers :: EventType -> GameState -> (GameState, String)
fireTriggers event state =
    let triggers = triggerDefs (world state)
        matching = filter (\t -> trEvent t == event) triggers
    in fireTriggerList matching state

-- | Fire a specific list of triggers (internal, also used by nested call from effects)
fireTriggerList :: [TriggerDef] -> GameState -> (GameState, String)
fireTriggerList triggers state =
    foldl fireOne (state, "") triggers
  where
    ts = triggerStates (save state)

    fireOne (st, acc) tr =
        let tId = trId tr
            tState = Map.lookup tId ts
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
        let (st', msgs) = foldl (\(s, a) e ->
                let (s', m, _) = applyOutcomeWith 0 0 e "" s
                in (s', a ++ m ++ "\n")) (st { save = (save st) { triggerStates = updatedTs } }, acc) (trEffects tr)
            updatedTs = Map.insert tId (TriggerState True (trCooldown tr)) (triggerStates (save st))
        in (st', msgs)
