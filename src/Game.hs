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
    , entityStates       = Map.empty
    , entityInteractions = Map.empty
    , gameOver           = False
    }

-- | Helper to get current room from game state
getCurrentRoom :: GameState -> Maybe Room
getCurrentRoom state = Map.lookup (currentRoom state) (rooms state)

-- | Check if player has an item in inventory
hasItem :: String -> GameState -> Bool
hasItem target state = any ((target `elem`) . itemKeywords) (inventory state)

-- | Move player to a different room
moveToRoom :: String -> GameState -> GameState
moveToRoom roomName state = state { currentRoom = roomName }

-- | Add item to player's inventory
pickupItem :: Item -> GameState -> GameState
pickupItem item state = state { inventory = item : inventory state }

-- | Remove item from player's inventory
dropItem :: Item -> GameState -> GameState
dropItem item state = state { inventory = filter (/= item) (inventory state) }

-- | Remove item from current room
removeItemFromRoom :: String -> GameState -> GameState
removeItemFromRoom itemName state = state
    { rooms = Map.adjust removeItem (currentRoom state) (rooms state) }
    where
        removeItem room = room { roomItems = filter (not . matchItem itemName) (roomItems room) }
        matchItem name item = name `elem` itemKeywords item

-- | Add item to current room
addItemToRoom :: Item -> GameState -> GameState
addItemToRoom item state = state
    { rooms = Map.adjust addItem (currentRoom state) (rooms state) }
    where
        addItem room = room { roomItems = item : roomItems room }

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

-- | Update NPC in room
updateNPCInRoom :: String -> NPC -> GameState -> GameState
updateNPCInRoom roomName newNpc state = state
    { rooms = Map.adjust updateRoom roomName (rooms state) }
    where
        updateRoom room = room { roomNPCs = replaceNPC (roomNPCs room) }
        replaceNPC [] = []
        replaceNPC (n:ns)
            | npcName n == npcName newNpc = newNpc : ns
            | otherwise = n : replaceNPC ns

-- | Remove NPC from room
removeNPCFromRoom :: String -> String -> GameState -> GameState
removeNPCFromRoom roomName targetNpcName state = state
    { rooms = Map.adjust updateRoom roomName (rooms state) }
    where
        updateRoom room = room { roomNPCs = filter (\n -> npcName n /= targetNpcName) (roomNPCs room) }