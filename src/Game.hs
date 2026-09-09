-- | Core game state and manipulation for the text adventure engine
module Game where

import Types
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
    }

-- | Default empty game state
emptyGameState :: GameState
emptyGameState = GameState
    { world = emptyGameWorld
    , save = SaveState
        { player             = Player 100 100 10 5
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
