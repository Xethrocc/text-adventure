-- | Core game state and manipulation for the text adventure engine
module Game where

import Types
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.List (intercalate)

-- | Default empty game state
emptyGameState :: GameState
emptyGameState = GameState
    { rooms              = Map.empty
    , player             = Player 100 100 10 5
    , currentRoom        = "start"
    , inventory          = []
    , itemStates         = Map.empty
    , itemDefs           = Map.empty
    , npcStates          = Map.empty
    , npcDefs            = Map.empty
    , entityStates       = Map.empty
    , entityInteractions = Map.empty
    , gameOver           = False
    }

-- | Helper to get current room from game state
getCurrentRoom :: GameState -> Maybe Room
getCurrentRoom state = Map.lookup (currentRoom state) (rooms state)

-- | Get all items currently in a specific location (e.g., room ID or "inventory")
getItemsInLocation :: RoomID -> GameState -> [ItemDef]
getItemsInLocation loc state = 
    let itemIds = Map.keys $ Map.filter (\s -> itemLocation s == loc) (itemStates state)
    in [def | iId <- itemIds, Just def <- [Map.lookup iId (itemDefs state)]]

-- | Check if player has an item in inventory
hasItem :: ItemID -> GameState -> Bool
hasItem iId state = iId `elem` inventory state

-- | Move player to a different room
moveToRoom :: RoomID -> GameState -> GameState
moveToRoom roomName state = state { currentRoom = roomName }

-- | Add item to player's inventory
pickupItem :: ItemID -> GameState -> GameState
pickupItem iId state = state 
    { inventory = iId : inventory state 
    , itemStates = Map.adjust (\s -> s { itemLocation = "inventory" }) iId (itemStates state)
    }

-- | Remove item from player's inventory to current room
dropItem :: ItemID -> GameState -> GameState
dropItem iId state = state 
    { inventory = filter (/= iId) (inventory state) 
    , itemStates = Map.adjust (\s -> s { itemLocation = currentRoom state }) iId (itemStates state)
    }

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
getEntityState entity state = Map.lookup entity (entityStates state)

-- | Set entity state
setEntityState :: String -> String -> GameState -> GameState
setEntityState entity val state = state { entityStates = Map.insert entity val (entityStates state) }

-- | Update player health
updatePlayerHealth :: (Int -> Int) -> GameState -> GameState
updatePlayerHealth f state = 
    let p = player state
        newHealth = max 0 (min (playerMaxHealth p) (f (playerHealth p)))
    in state { player = p { playerHealth = newHealth } }

-- | Check if player is dead
isPlayerDead :: GameState -> Bool
isPlayerDead state = playerHealth (player state) <= 0

-- | Get all NPCs in a specific room
getNPCsInRoom :: RoomID -> GameState -> [NPCDef]
getNPCsInRoom rId state = 
    let npcIds = Map.keys $ Map.filter (\s -> npcLocation s == rId) (npcStates state)
    in [def | nId <- npcIds, Just def <- [Map.lookup nId (npcDefs state)]]

-- | Update NPC state (health, status, etc)
updateNPCState :: String -> NPCState -> GameState -> GameState
updateNPCState targetNpcId newNpcState state = state
    { npcStates = Map.insert targetNpcId newNpcState (npcStates state) }

-- | Move NPC to void (dead)
killNPC :: String -> GameState -> GameState
killNPC targetNpcId state = state
    { npcStates = Map.adjust (\s -> s { npcLocation = "void", npcStatus = "dead" }) targetNpcId (npcStates state) }