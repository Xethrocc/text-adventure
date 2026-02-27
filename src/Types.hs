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

-- | Verb enumeration for dynamic actions
data Verb = VGo | VLook | VLookAt | VTake | VDrop | VInventory | VUse | VUseOn | VTalk | VAttack | VHelp | VQuit | VUnknown
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Action Outcome representing the result of an interaction
data ActionOutcome 
    = MessageOnly String
    | ChangeItemState String String -- ^ New State, Message
    | ChangeNPCState String String  -- ^ New State, Message
    | TransitionRoom String String  -- ^ New RoomID, Message
    deriving (Show, Eq)

type ItemID = String
type RoomID = String

-- | Static item definition (loaded from JSON)
data ItemDef = ItemDef
    { itemId          :: ItemID
    , itemName        :: String
    , itemDescription :: String
    , itemKeywords    :: [String]
    , itemVerbMap     :: Map.Map (Verb, String) ActionOutcome -- ^ (Verb, CurrentState) -> Outcome
    } deriving (Show, Eq)

-- | Dynamic item state
data ItemState = ItemState
    { itemLocation :: RoomID  -- ^ The room ID, or "inventory", or "consumed"
    , itemStatus   :: String  -- ^ e.g., "intact", "burned", "open"
    } deriving (Show, Eq)

-- | Static NPC definition
data NPCDef = NPCDef
    { npcId            :: String
    , npcName          :: String
    , npcDescription   :: String
    , npcDialogue      :: Map.Map String String -- ^ CurrentStatus -> Dialogue String
    , npcKeywords      :: [String]
    , npcMaxHealth     :: Maybe Int
    , npcAttackBase    :: Int
    , npcDefenseBase   :: Int
    , npcVerbMap       :: Map.Map (Verb, String) ActionOutcome
    } deriving (Show, Eq)

-- | Dynamic NPC state
data NPCState = NPCState
    { npcLocation :: RoomID
    , npcStatus   :: String    -- ^ e.g., "alive", "dead", "sleeping"
    , npcHealth   :: Maybe Int -- ^ Current health
    } deriving (Show, Eq)

-- | Player with combat statistics
data Player = Player
    { playerHealth    :: Int
    , playerMaxHealth :: Int
    , playerAttack    :: Int
    , playerDefense   :: Int
    } deriving (Show, Eq)

-- | Room with connections and static data, no longer holding dynamic items/NPCs
data Room = Room
    { roomId          :: RoomID
    , roomName        :: String
    , roomDescription :: String
    , roomConnections :: Map.Map Direction Exit
    , roomVisited     :: Bool
    } deriving (Show, Eq)

-- | Player inventory
type Inventory = [ItemID]

-- | Game state containing all rooms, current location, inventory, and entity states
data GameState = GameState
    { rooms              :: Map.Map RoomID Room
    , player             :: Player
    , currentRoom        :: RoomID
    , inventory          :: Inventory
    , itemStates         :: Map.Map ItemID ItemState
    , itemDefs           :: Map.Map ItemID ItemDef
    , npcStates          :: Map.Map String NPCState
    , npcDefs            :: Map.Map String NPCDef
    , entityStates       :: Map.Map String String  -- ^ EntityName -> State (e.g., "door" -> "locked")
    , entityInteractions :: Map.Map (String, String) (String, String) -- ^ (Item, Entity) -> (NewState, Message)
    , gameOver           :: Bool
    } deriving (Show, Eq)