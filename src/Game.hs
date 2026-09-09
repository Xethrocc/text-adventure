{-# LANGUAGE LambdaCase #-}
-- | Core game state and manipulation for the text adventure engine
module Game where

import Types
import Data.List (intercalate)
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
        }
    }

-- ---------------------------------------------------------------------------
-- Lookups
-- ---------------------------------------------------------------------------

-- | Helper to get current room from game state
getCurrentRoom :: GameState -> Maybe Room
getCurrentRoom state = Map.lookup (currentRoom (save state)) (rooms (world state))

-- | Get all visible items in a location.
--   Hidden items only appear once they have been discovered via `search`.
getItemsInLocation :: RoomID -> GameState -> [ItemDef]
getItemsInLocation loc state =
    [ def
    | (iId, st) <- Map.toList (itemStates (save state))
    , itemLocation st == loc
    , Just def <- [Map.lookup iId (itemDefs (world state))]
    , not (itemHidden def) || itemDiscovered st
    ]

-- | All items in a location, including hidden ones (used internally)
getAllItemsInLocation :: RoomID -> GameState -> [ItemDef]
getAllItemsInLocation loc state =
    let itemIds = Map.keys $ Map.filter (\s -> itemLocation s == loc) (itemStates (save state))
    in [def | iId <- itemIds, Just def <- [Map.lookup iId (itemDefs (world state))]]

-- | Check if player has an item in inventory
hasItem :: ItemID -> GameState -> Bool
hasItem iId state = case Map.lookup iId (itemStates (save state)) of
    Just itemState -> itemLocation itemState == "inventory"
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
                (Map.filter (\itemState -> itemLocation itemState == "inventory") (itemStates saveState))
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
pickupItem iId state =
    let saveState = save state
        updatedSave =
            saveState
                { itemStates =
                    Map.adjust (\s -> s { itemLocation = "inventory" }) iId (itemStates saveState)
                }
    in state { save = syncInventory updatedSave }

-- | Remove item from player's inventory to current room
dropItem :: ItemID -> GameState -> GameState
dropItem iId state =
    let saveState = save state
        updatedSave =
            saveState
                { itemStates =
                    Map.adjust (\s -> s { itemLocation = currentRoom saveState }) iId (itemStates saveState)
                }
    in state { save = syncInventory updatedSave }

-- | Give item directly to player inventory (e.g., NPC reward, loot)
giveItem :: ItemID -> GameState -> GameState
giveItem iId state =
    let saveState = save state
        updatedSave = saveState
            { itemStates = Map.adjust (\s -> s { itemLocation = "inventory" }) iId (itemStates saveState) }
    in state { save = syncInventory updatedSave }

-- | Move an item to a specific room (e.g., loot drop)
moveItemToRoom :: ItemID -> RoomID -> GameState -> GameState
moveItemToRoom iId targetRoom state =
    let saveState = save state
        updatedSave = saveState
            { itemStates = Map.adjust (\s -> s { itemLocation = targetRoom }) iId (itemStates saveState) }
    in state { save = syncInventory updatedSave }

-- | Consume an item, removing it from play entirely
consumeItem :: ItemID -> GameState -> GameState
consumeItem iId state =
    let saveState = save state
        updatedSave = saveState
            { itemStates = Map.adjust (\s -> s { itemLocation = "consumed" }) iId (itemStates saveState) }
    in state { save = syncInventory updatedSave }

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
    let npcIds = Map.keys $ Map.filter (\s -> npcLocation s == rId) (npcStates (save state))
    in [def | nId <- npcIds, Just def <- [Map.lookup nId (npcDefs (world state))]]

-- | Update NPC state (health, status, etc)
updateNPCState :: String -> NPCState -> GameState -> GameState
updateNPCState targetNpcId newNpcState state = state
    { save = (save state) { npcStates = Map.insert targetNpcId newNpcState (npcStates (save state)) } }

-- | Move NPC to void (dead)
killNPC :: String -> GameState -> GameState
killNPC targetNpcId state = state
    { save = (save state) { npcStates = Map.adjust (\s -> s { npcLocation = "void", npcStatus = "dead" }) targetNpcId (npcStates (save state)) } }

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
    { save = (save state) { npcStates = Map.adjust (\s -> s { npcLocation = targetRoom }) nId (npcStates (save state)) } }

-- | Set the current dialogue node for an NPC
setDialogueNode :: String -> Maybe String -> GameState -> GameState
setDialogueNode nId node state = state
    { save = (save state) { npcStates = Map.adjust (\s -> s { npcDialogueNode = node }) nId (npcStates (save state)) } }

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
applyCondition :: String -> Int -> Maybe ActionOutcome -> Maybe ActionOutcome -> GameState -> GameState
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
           then let (stEnd, mEnd) = maybe (st, "") (\o -> applyOutcomePure o st) (condEndOutcome cond)
                    st' = clearCondition name stEnd
                in (st', if null mEnd then msgs else msgs ++ [mEnd])
           else let st1 = st { save = (save st) { conditions = Map.adjust (\c -> c { condRemaining = remaining }) name (conditions (save st)) } }
                    (st2, mTick) = maybe (st1, "") (\o -> applyOutcomePure o st1) (condTickOutcome cond)
                in (st2, if null mTick then msgs else msgs ++ [mTick])
    -- local wrapper to avoid a module cycle with Parser
    applyOutcomePure = applyOutcomeFallback

-- | Outcome application for conditions. Lives in Game to avoid a Parser
--   dependency; supports the subset of outcomes that make sense for ticks.
applyOutcomeFallback :: ActionOutcome -> GameState -> (GameState, String)
applyOutcomeFallback outcome state = case outcome of
    MessageOnly msg          -> (state, msg)
    HealPlayer amount msg    -> (updatePlayerHealth (+ amount) state, msg)
    DamagePlayer amount msg  ->
        let st' = updatePlayerHealth (subtract amount) state
        in (if isPlayerDead st' then endGame Death st' else st', msg)
    SetFlag n v msg          -> (setFlag n v state, msg)
    MultipleOutcomes os ->
        let (st', msgs) = foldl (\(s, ms) o ->
                let (s2, m2) = applyOutcomeFallback o s
                in (s2, if null m2 then ms else ms ++ [m2]))
                (state, []) os
        in (st', intercalate "\n" msgs)
    _ -> (state, "")  -- unsupported in tick context, silently ignored

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
        Just outcome -> applyOutcomeFallback outcome withoutActive

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
