-- | Core data types and game state for the text adventure engine
module Game where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.List (intercalate)

-- | Direction enumeration for movement

data Direction = North | South | East | West | Up | Down
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Game item with name and description

data Item = Item
    { itemName        :: String
    , itemDescription :: String
    , itemKeywords    :: [String]  -- Words that can refer to this item
    } deriving (Show, Eq)

-- | Room with connections, items, and description

data Room = Room
    { roomName        :: String
    , roomDescription :: String
    , roomItems       :: [Item]
    , roomConnections :: Map.Map Direction String  -- Direction -> Room name
    , roomVisited     :: Bool
    } deriving (Show, Eq)

-- | Non-player character with dialogue and behavior

data NPC = NPC
    { npcName        :: String
    , npcDescription :: String
    , npcDialogue    :: String
    , npcKeywords    :: [String]  -- Words that can refer to this NPC
    } deriving (Show, Eq)

-- | Player inventory

type Inventory = [Item]

-- | Game state containing all rooms, current location, and player inventory

data GameState = GameState
    { rooms        :: Map.Map String Room
    , currentRoom  :: String
    , inventory    :: Inventory
    , gameOver     :: Bool
    } deriving (Show, Eq)

-- | Default empty game state

emptyGameState :: GameState
emptyGameState = GameState
    { rooms       = Map.empty
    , currentRoom = "start"
    , inventory   = []
    , gameOver    = False
    }

-- | Helper to get current room from game state

getCurrentRoom :: GameState -> Maybe Room
getCurrentRoom state = Map.lookup (currentRoom state) (rooms state)

-- | Check if player has an item in inventory

hasItem :: String -> GameState -> Bool
hasItem itemName state = any ((itemName `elem`) . itemKeywords) (inventory state)

-- | Move player to a different room

moveToRoom :: String -> GameState -> GameState
moveToRoom roomName state = state { currentRoom = roomName }

-- | Add item to player's inventory

pickupItem :: Item -> GameState -> GameState
pickupItem item state = state { inventory = item : inventory state }

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

-- | Get room name in a given direction

getRoomInDirection :: Direction -> GameState -> Maybe String
getRoomInDirection dir state = case getCurrentRoom state of
    Just room -> Map.lookup dir (roomConnections room)
    Nothing   -> Nothing