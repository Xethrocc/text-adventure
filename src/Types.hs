-- | Core data types for the text adventure engine
module Types where

import qualified Data.Map.Strict as Map

-- | Direction enumeration for movement
data Direction = North | South | East | West | Up | Down
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Exit connection between rooms
data Exit
    = Open String             -- ^ Destination room name
    | Locked String String    -- ^ Destination room name, Entity name
    deriving (Show, Eq)

-- | Game item with name, description, recognizable keywords, valid actions, and pickability
data Item = Item
    { itemName        :: String
    , itemDescription :: String
    , itemKeywords    :: [String]
    , itemActions     :: [String]
    , itemPickable    :: Bool
    } deriving (Show, Eq)

-- | Non-player character with dialogue and behavior
data NPC = NPC
    { npcName        :: String
    , npcDescription :: String
    , npcDialogue    :: String
    , npcKeywords    :: [String]
    } deriving (Show, Eq)

-- | Room with connections, items, NPCs, and description
data Room = Room
    { roomName        :: String
    , roomDescription :: String
    , roomItems       :: [Item]
    , roomNPCs        :: [NPC]
    , roomConnections :: Map.Map Direction Exit
    , roomVisited     :: Bool
    } deriving (Show, Eq)

-- | Player inventory
type Inventory = [Item]

-- | Game state containing all rooms, current location, inventory, and entity states
data GameState = GameState
    { rooms              :: Map.Map String Room
    , currentRoom        :: String
    , inventory          :: Inventory
    , entityStates       :: Map.Map String String  -- ^ EntityName -> State (e.g., "door" -> "locked")
    , entityInteractions :: Map.Map (String, String) (String, String) -- ^ (Item, Entity) -> (NewState, Message)
    , gameOver           :: Bool
    } deriving (Show, Eq)