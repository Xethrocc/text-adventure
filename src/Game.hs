-- | Core game state and manipulation for the text adventure engine
module Game where

import Types
import qualified Data.Map.Strict as Map

-- | Default empty game state
emptyGameState :: GameState
emptyGameState = GameState
    { world = GameWorld
        { rooms              = Map.empty
        , itemDefs           = Map.empty
        , npcDefs            = Map.empty
        , entityInteractions = Map.empty
        }
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
        }
    }

-- | Helper to get current room from game state
getCurrentRoom :: GameState -> Maybe Room
getCurrentRoom state = Map.lookup (currentRoom (save state)) (rooms (world state))

-- | Get all items currently in a specific location (e.g., room ID or "inventory")
getItemsInLocation :: RoomID -> GameState -> [ItemDef]
getItemsInLocation loc state = 
    let itemIds = Map.keys $ Map.filter (\s -> itemLocation s == loc) (itemStates (save state))
    in [def | iId <- itemIds, Just def <- [Map.lookup iId (itemDefs (world state))]]

-- | Check if player has an item in inventory
hasItem :: ItemID -> GameState -> Bool
hasItem iId state = case Map.lookup iId (itemStates (save state)) of
    Just itemState -> itemLocation itemState == "inventory"
    Nothing        -> False

syncInventory :: SaveState -> SaveState
syncInventory saveState =
    saveState
        { inventory =
            Map.keys
                (Map.filter (\itemState -> itemLocation itemState == "inventory") (itemStates saveState))
        }

-- | Move player to a different room
moveToRoom :: RoomID -> GameState -> GameState
moveToRoom destinationRoom state = state { save = (save state) { currentRoom = destinationRoom } }

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

-- | Get entity state
getEntityState :: String -> GameState -> Maybe String
getEntityState entity state = Map.lookup entity (entityStates (save state))

-- | Set entity state
setEntityState :: String -> String -> GameState -> GameState
setEntityState entity val state = state { save = (save state) { entityStates = Map.insert entity val (entityStates (save state)) } }

-- | Update player health
updatePlayerHealth :: (Int -> Int) -> GameState -> GameState
updatePlayerHealth f state = 
    let p = player (save state)
        newHealth = max 0 (min (playerMaxHealth p) (f (playerHealth p)))
    in state { save = (save state) { player = p { playerHealth = newHealth } } }

-- | Check if player is dead
isPlayerDead :: GameState -> Bool
isPlayerDead state = playerHealth (player (save state)) <= 0

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

-- | Modify an item's property
modifyItemProp :: String -> String -> Int -> GameState -> GameState
modifyItemProp iId prop delta state = state
    { save = (save state) { itemStates = Map.adjust (\s -> 
        let currentVal = Map.findWithDefault 0 prop (itemProps s)
        in s { itemProps = Map.insert prop (currentVal + delta) (itemProps s) }
        ) iId (itemStates (save state)) } }

-- | Modify an NPC's property
modifyNPCProp :: String -> String -> Int -> GameState -> GameState
modifyNPCProp nId prop delta state = state
    { save = (save state) { npcStates = Map.adjust (\s -> 
        let currentVal = Map.findWithDefault 0 prop (npcProps s)
        in s { npcProps = Map.insert prop (currentVal + delta) (npcProps s) }
        ) nId (npcStates (save state)) } }

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

-- | Move an NPC to a different room
moveNPCToRoom :: String -> RoomID -> GameState -> GameState
moveNPCToRoom nId targetRoom state = state
    { save = (save state) { npcStates = Map.adjust (\s -> s { npcLocation = targetRoom }) nId (npcStates (save state)) } }

-- | Set a room's visited flag
setRoomVisitedFlag :: RoomID -> Bool -> GameState -> GameState
setRoomVisitedFlag rId visited state = state
    { world = (world state) { rooms = Map.adjust (\r -> r { roomVisited = visited }) rId (rooms (world state)) } }

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

-- | Clamp NPC health to its max health from the definition
clampNPCHealth :: String -> GameState -> GameState
clampNPCHealth nId state = case Map.lookup nId (npcDefs (world state)) of
    Just nDef -> case npcMaxHealth nDef of
        Just maxHp -> state
            { save = (save state)
                { npcStates = Map.adjust (\s -> s { npcHealth = fmap (min maxHp) (npcHealth s) }) nId (npcStates (save state)) } }
        Nothing -> state
    Nothing -> state

-- | Check if a target string matches any living NPC in the room (for weapon routing)
isLivingNPCInRoom :: String -> GameState -> Bool
isLivingNPCInRoom target state =
    let currentRoomId = currentRoom (save state)
        roomNPCs = getNPCsInRoom currentRoomId state
        matchTarget npc = any (\kw -> map (\c -> if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c) target == map (\c -> if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c) kw) (npcId npc : npcName npc : npcKeywords npc)
        npcIsAlive npc = case Map.lookup (npcId npc) (npcStates (save state)) of
            Just ns -> npcStatus ns /= "dead"
            Nothing -> False
    in any (\npc -> matchTarget npc && npcIsAlive npc) roomNPCs
