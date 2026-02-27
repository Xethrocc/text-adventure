{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Core data types for the text adventure engine
module Types where

import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Data.Aeson
import Data.Aeson.Types (Parser)

-- | Direction enumeration for movement
data Direction = North | South | East | West | Up | Down
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON Direction
instance FromJSON Direction
instance ToJSONKey Direction
instance FromJSONKey Direction

-- | Exit connection between rooms
data Exit
    = Open String             -- ^ Destination room name
    | Locked String String    -- ^ Destination room name, Entity name
    deriving (Show, Eq, Generic)

instance ToJSON Exit
instance FromJSON Exit

-- | Verb enumeration for dynamic actions
data Verb = VGo | VLook | VLookAt | VTake | VDrop | VInventory | VUse | VUseOn | VTalk | VAttack | VHelp | VQuit | VUnknown
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON Verb
instance FromJSON Verb

-- | Action Outcome representing the result of an interaction
data ActionOutcome 
    = MessageOnly String
    | ChangeItemState String String -- ^ New State, Message
    | ChangeNPCState String String  -- ^ New State, Message
    | TransitionRoom String String  -- ^ New RoomID, Message
    deriving (Show, Eq, Generic)

instance ToJSON ActionOutcome
instance FromJSON ActionOutcome

type ItemID = String
type RoomID = String

-- Helper: encode a Map with (Verb, String) keys as a JSON object with "VTake:intact" style keys
verbStateMapToJSON :: Map.Map (Verb, String) ActionOutcome -> Value
verbStateMapToJSON = toJSON . Map.mapKeys (\(v, s) -> show v ++ ":" ++ s)

verbStateMapFromJSON :: Value -> Parser (Map.Map (Verb, String) ActionOutcome)
verbStateMapFromJSON v = do
    m <- parseJSON v :: Parser (Map.Map String ActionOutcome)
    let parsePair k = case break (== ':') k of
            (vStr, ':':sStr) -> case reads vStr of
                [(verb, "")] -> Right ((verb, sStr), ())
                _            -> Left $ "Bad verb: " ++ vStr
            _                -> Left $ "Bad key format: " ++ k
    case mapM (\(k, val) -> case parsePair k of
                Right ((verb, st), _) -> Right ((verb, st), val)
                Left err              -> Left err
              ) (Map.toList m) of
        Right pairs -> pure $ Map.fromList pairs
        Left err    -> fail err

-- | Static item definition (loaded from JSON)
data ItemDef = ItemDef
    { itemId          :: ItemID
    , itemName        :: String
    , itemDescription :: String
    , itemKeywords    :: [String]
    , itemVerbMap     :: Map.Map (Verb, String) ActionOutcome -- ^ (Verb, CurrentState) -> Outcome
    } deriving (Show, Eq)

instance ToJSON ItemDef where
    toJSON def = object
        [ "itemId"          .= itemId def
        , "itemName"        .= itemName def
        , "itemDescription" .= itemDescription def
        , "itemKeywords"    .= itemKeywords def
        , "itemVerbMap"     .= verbStateMapToJSON (itemVerbMap def)
        ]

instance FromJSON ItemDef where
    parseJSON = withObject "ItemDef" $ \o -> ItemDef
        <$> o .: "itemId"
        <*> o .: "itemName"
        <*> o .: "itemDescription"
        <*> o .: "itemKeywords"
        <*> (o .: "itemVerbMap" >>= verbStateMapFromJSON)

-- | Dynamic item state
data ItemState = ItemState
    { itemLocation :: RoomID  -- ^ The room ID, or "inventory", or "consumed"
    , itemStatus   :: String  -- ^ e.g., "intact", "burned", "open"
    } deriving (Show, Eq, Generic)

instance ToJSON ItemState
instance FromJSON ItemState

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

instance ToJSON NPCDef where
    toJSON def = object
        [ "npcId"          .= npcId def
        , "npcName"        .= npcName def
        , "npcDescription" .= npcDescription def
        , "npcDialogue"    .= npcDialogue def
        , "npcKeywords"    .= npcKeywords def
        , "npcMaxHealth"   .= npcMaxHealth def
        , "npcAttackBase"  .= npcAttackBase def
        , "npcDefenseBase" .= npcDefenseBase def
        , "npcVerbMap"     .= verbStateMapToJSON (npcVerbMap def)
        ]

instance FromJSON NPCDef where
    parseJSON = withObject "NPCDef" $ \o -> NPCDef
        <$> o .: "npcId"
        <*> o .: "npcName"
        <*> o .: "npcDescription"
        <*> o .: "npcDialogue"
        <*> o .: "npcKeywords"
        <*> o .: "npcMaxHealth"
        <*> o .: "npcAttackBase"
        <*> o .: "npcDefenseBase"
        <*> (o .: "npcVerbMap" >>= verbStateMapFromJSON)

-- | Dynamic NPC state
data NPCState = NPCState
    { npcLocation :: RoomID
    , npcStatus   :: String    -- ^ e.g., "alive", "dead", "sleeping"
    , npcHealth   :: Maybe Int -- ^ Current health
    } deriving (Show, Eq, Generic)

instance ToJSON NPCState
instance FromJSON NPCState

-- | Player with combat statistics
data Player = Player
    { playerHealth    :: Int
    , playerMaxHealth :: Int
    , playerAttack    :: Int
    , playerDefense   :: Int
    } deriving (Show, Eq, Generic)

instance ToJSON Player
instance FromJSON Player

-- | Room with connections and static data, no longer holding dynamic items/NPCs
data Room = Room
    { roomId          :: RoomID
    , roomName        :: String
    , roomDescription :: String
    , roomConnections :: Map.Map Direction Exit
    , roomVisited     :: Bool
    } deriving (Show, Eq, Generic)

instance ToJSON Room
instance FromJSON Room

-- | Player inventory
type Inventory = [ItemID]

-- Helper: encode a Map with (String, String) tuple keys
tupleMapToJSON :: Map.Map (String, String) (String, String) -> Value
tupleMapToJSON = toJSON . Map.mapKeys (\(a, b) -> a ++ "|" ++ b) . Map.map (\(a, b) -> [a, b])

tupleMapFromJSON :: Value -> Parser (Map.Map (String, String) (String, String))
tupleMapFromJSON v = do
    m <- parseJSON v :: Parser (Map.Map String [String])
    let parsePair k = case break (== '|') k of
            (a, '|':b) -> Right (a, b)
            _          -> Left $ "Bad key format: " ++ k
    case mapM (\(k, val) -> case (parsePair k, val) of
                (Right (a, b), [v1, v2]) -> Right ((a, b), (v1, v2))
                (Left err, _)            -> Left err
                _                        -> Left $ "Bad value for key: " ++ k
              ) (Map.toList m) of
        Right pairs -> pure $ Map.fromList pairs
        Left err    -> fail err

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

instance ToJSON GameState where
    toJSON gs = object
        [ "rooms"              .= rooms gs
        , "player"             .= player gs
        , "currentRoom"        .= currentRoom gs
        , "inventory"          .= inventory gs
        , "itemStates"         .= itemStates gs
        , "itemDefs"           .= itemDefs gs
        , "npcStates"          .= npcStates gs
        , "npcDefs"            .= npcDefs gs
        , "entityStates"       .= entityStates gs
        , "entityInteractions" .= tupleMapToJSON (entityInteractions gs)
        , "gameOver"           .= gameOver gs
        ]

instance FromJSON GameState where
    parseJSON = withObject "GameState" $ \o -> GameState
        <$> o .: "rooms"
        <*> o .: "player"
        <*> o .: "currentRoom"
        <*> o .: "inventory"
        <*> o .: "itemStates"
        <*> o .: "itemDefs"
        <*> o .: "npcStates"
        <*> o .: "npcDefs"
        <*> o .: "entityStates"
        <*> (o .: "entityInteractions" >>= tupleMapFromJSON)
        <*> o .: "gameOver"