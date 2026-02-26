-- Core data types for the text adventure engine
module Types where

import Data.Map (Map)
import qualified Data.Map as Map

-- Direction types for navigation
data Direction = North | South | East | West | Up | Down
    deriving (Eq, Ord, Show, Read)

-- Item representation
data Item = Item
    { itemName        :: String
    , itemDescription :: String
    , itemPortable    :: Bool
    , itemUsable      :: Bool
    }
    deriving (Show, Eq)

-- Room representation
data Room = Room
    { roomName        :: String
    , roomDescription :: String
    , roomItems       :: [Item]
    , roomExits       :: Map Direction String  -- Direction -> Destination room name
    , roomVisited     :: Bool
    }
    deriving (Show, Eq)

-- Player state
data Player = Player
    { playerName   :: String
    , playerHealth :: Int
    , playerInventory :: [Item]
    , playerLocation :: String  -- Current room name
    }
    deriving (Show, Eq)

-- Game state
data GameState = GameState
    { rooms      :: Map String Room
    , player     :: Player
    , gameOver   :: Bool
    }
    deriving (Show, Eq)