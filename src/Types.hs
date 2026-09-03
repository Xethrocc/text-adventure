{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Core data types for the text adventure engine
module Types where

import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Bits (xor, shiftR)

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

-- | Reason the game ended
data GameOverReason = Victory | Death | Custom String
    deriving (Show, Eq, Generic)

instance ToJSON GameOverReason
instance FromJSON GameOverReason

-- | Action Outcome representing the result of an interaction
data ActionOutcome 
    = MessageOnly String
    | ChangeItemState String String       -- ^ New State, Message
    | ChangeNPCState String String        -- ^ New State, Message
    | TransitionRoom String String        -- ^ New RoomID, Message
    | HealPlayer Int String               -- ^ Health to add, Message
    | DamagePlayer Int String             -- ^ Damage to deal, Message
    | UpdateNPCHealth String Int String   -- ^ NPC ID, health delta (+/-), Message
    | ModifyItemProp String String Int String -- ^ Item ID, Prop Name, delta (+/-), Message
    | ModifyNPCProp String String Int String  -- ^ NPC ID, Prop Name, delta (+/-), Message
    | SetEntityState String String String -- ^ Entity, New State, Message
    | MultipleOutcomes [ActionOutcome]
    -- New outcome constructors
    | GiveItem ItemID String              -- ^ Add item to player inventory, Message
    | MoveItem ItemID RoomID String       -- ^ Move item to a room (loot drops), Message
    | ConsumeItem ItemID String           -- ^ Remove item from play entirely (location → "consumed"), Message
    | MoveNPC String RoomID String        -- ^ Move NPC to a different room, Message
    | SetRoomVisited RoomID Bool String   -- ^ Mark room visited/unvisited, Message
    | SetFlag String String String        -- ^ Flag name, value, Message
    | CheckFlag String String ActionOutcome ActionOutcome -- ^ Flag, expected value, then-branch, else-branch
    | RandomChoice [ActionOutcome]        -- ^ Pick one outcome deterministically via game-state hash
    | GameEnd GameOverReason String       -- ^ End the game with a reason, Message
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
        Right parsedPairs -> pure $ Map.fromList parsedPairs
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
    , itemProps    :: Map.Map String Int -- ^ Generic integer properties (e.g. "uses_left" -> 3) 
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
    , npcProps    :: Map.Map String Int -- ^ Generic properties
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
        Right parsedPairs -> pure $ Map.fromList parsedPairs
        Left err    -> fail err

-- | Static world definition containing blueprint/map data
data GameWorld = GameWorld
    { rooms              :: Map.Map RoomID Room
    , itemDefs           :: Map.Map ItemID ItemDef
    , npcDefs            :: Map.Map String NPCDef
    , entityInteractions :: Map.Map (String, String) (String, String) -- ^ (Item, Entity) -> (NewState, Message)
    } deriving (Show, Eq)

instance ToJSON GameWorld where
    toJSON gw = object
        [ "rooms"              .= rooms gw
        , "itemDefs"           .= itemDefs gw
        , "npcDefs"            .= npcDefs gw
        , "entityInteractions" .= tupleMapToJSON (entityInteractions gw)
        ]

instance FromJSON GameWorld where
    parseJSON = withObject "GameWorld" $ \o -> GameWorld
        <$> o .: "rooms"
        <*> o .: "itemDefs"
        <*> o .: "npcDefs"
        <*> (o .: "entityInteractions" >>= tupleMapFromJSON)

-- | Dynamic state of an active playthrough
data SaveState = SaveState
    { player             :: Player
    , currentRoom        :: RoomID
    , inventory          :: Inventory
    , itemStates         :: Map.Map ItemID ItemState
    , npcStates          :: Map.Map String NPCState
    , entityStates       :: Map.Map String String  -- ^ EntityName -> State (e.g., "door" -> "locked")
    , flags              :: Map.Map String String   -- ^ General-purpose flags for data-driven conditionals
    , turnCount          :: Int                     -- ^ Number of commands executed (drives deterministic RNG)
    , gameOver           :: Bool
    , gameOverReason     :: Maybe GameOverReason    -- ^ Why the game ended (Nothing while playing)
    } deriving (Show, Eq, Generic)

-- Custom JSON instances for backward compatibility with old saves
instance ToJSON SaveState where
    toJSON ss = object
        [ "player"         .= player ss
        , "currentRoom"    .= currentRoom ss
        , "inventory"      .= inventory ss
        , "itemStates"     .= itemStates ss
        , "npcStates"      .= npcStates ss
        , "entityStates"   .= entityStates ss
        , "flags"          .= flags ss
        , "turnCount"      .= turnCount ss
        , "gameOver"       .= gameOver ss
        , "gameOverReason" .= gameOverReason ss
        ]

instance FromJSON SaveState where
    parseJSON = withObject "SaveState" $ \o -> SaveState
        <$> o .:  "player"
        <*> o .:  "currentRoom"
        <*> o .:  "inventory"
        <*> o .:  "itemStates"
        <*> o .:  "npcStates"
        <*> o .:  "entityStates"
        <*> o .:? "flags"          .!= Map.empty
        <*> o .:? "turnCount"      .!= 0
        <*> o .:  "gameOver"
        <*> o .:? "gameOverReason" .!= Nothing

-- | Save file wrapper with metadata for save slots
data SaveFile = SaveFile
    { saveVersion    :: Int          -- ^ Schema version for migration
    , saveTimestamp  :: String       -- ^ ISO 8601 timestamp
    , worldChecksum  :: String       -- ^ Hash of serialized GameWorld for compatibility check
    , saveName       :: String       -- ^ User-chosen slot name
    , saveData       :: SaveState    -- ^ The actual save data
    } deriving (Show, Eq, Generic)

instance ToJSON SaveFile
instance FromJSON SaveFile

-- | Combined game state holding both world and save
data GameState = GameState
    { world :: GameWorld
    , save  :: SaveState
    } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Deterministic pseudo-random number generation from game state
-- ---------------------------------------------------------------------------
-- Instead of threading a StdGen, we derive "randomness" from a hash of
-- observable game-state variables.  The turnCount provides the primary
-- varying input; room, inventory size, and player HP add extra entropy
-- so identical turn numbers across different playthroughs diverge.
--
-- A Murmur3-style finalizer mixes the bits.  The salt parameter lets
-- multiple RandomChoice outcomes in the same turn produce different values.

-- | Mix an integer through a Murmur3-style finalizer for good bit distribution
mixHash :: Int -> Int
mixHash x0 =
    let x1 = (x0 `xor` (x0 `shiftR` 16)) * 0x45d9f3b
        x2 = (x1 `xor` (x1 `shiftR` 16)) * 0x45d9f3b
    in x2 `xor` (x2 `shiftR` 16)

-- | Derive a deterministic pseudo-random non-negative Int from the current game state.
--   The salt parameter differentiates multiple random draws within the same turn.
gameRandom :: GameState -> Int -> Int
gameRandom state salt =
    let tc      = turnCount (save state)
        roomVal = foldl (\acc c -> acc * 31 + fromEnum c) 0 (currentRoom (save state))
        invCnt  = length (inventory (save state))
        hp      = playerHealth (player (save state))
        -- Combine inputs with large coprime multipliers to spread bits
        combined = tc      * 2654435761   -- golden ratio constant
                 + roomVal * 1442695040888963407
                 + invCnt  * 31
                 + hp      * 17
                 + salt
    in abs (mixHash combined)

-- | Pick an index from [0 .. n-1] using the game-state-derived RNG
gameRandomIndex :: GameState -> Int -> Int -> Int
gameRandomIndex state salt n
    | n <= 0    = 0
    | otherwise = gameRandom state salt `mod` n
