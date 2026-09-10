{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Core data types for the text adventure engine
module Types where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Aeson
import Data.Aeson.Types (Parser, toJSONKeyText, FromJSONKeyFunction (..))
import Data.Bits (xor, shiftR)

-- ---------------------------------------------------------------------------
-- ID aliases
-- ---------------------------------------------------------------------------

type ItemID    = String
type RoomID    = String
type NPCID     = String
type EntityID  = String
type VehicleID = String
type QuestID   = String
type SkillID   = String
type FlagID    = String
type FactionID = String

-- ---------------------------------------------------------------------------
-- Basic enumerations
-- ---------------------------------------------------------------------------

-- | Direction enumeration for movement
data Direction = North | South | East | West | Up | Down | Southeast
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
data Verb = VGo | VLook | VLookAt | VTake | VDrop | VInventory | VUse | VUseOn
          | VTalk | VAttack | VSearch | VHelp | VQuit | VUnknown
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON Verb
instance FromJSON Verb

-- | Reason the game ended
data GameOverReason = Victory | Death | Custom String
    deriving (Show, Eq, Generic)

instance ToJSON GameOverReason
instance FromJSON GameOverReason

-- ---------------------------------------------------------------------------
-- Equipment
-- ---------------------------------------------------------------------------

-- | Slots an item can be equipped in. One item per slot.
data EquipSlot
    = Head
    | Body
    | Hands
    | Feet
    | Weapon
    | Offhand
    | Accessory
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON EquipSlot
instance FromJSON EquipSlot

instance ToJSONKey EquipSlot where
    toJSONKey = toJSONKeyText (T.pack . show)

instance FromJSONKey EquipSlot where
    fromJSONKey = FromJSONKeyTextParser $ \t ->
        case reads (T.unpack t) of
            [(s, "")] -> pure s
            _         -> fail ("Unknown EquipSlot: " ++ T.unpack t)

-- | Passive bonus granted by an equipped item
data EquipEffect
    = AttackBonus    Int
    | DefenseBonus   Int
    | MaxHealthBonus Int
    deriving (Show, Eq, Generic)

instance ToJSON EquipEffect
instance FromJSON EquipEffect

-- ---------------------------------------------------------------------------
-- Action outcomes
-- ---------------------------------------------------------------------------

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
    | GiveItem ItemID String              -- ^ Add item to player inventory, Message
    | MoveItem ItemID RoomID String       -- ^ Move item to a room (loot drops), Message
    | ConsumeItem ItemID String           -- ^ Remove item from play entirely (location → "consumed"), Message
    | MoveNPC String RoomID String        -- ^ Move NPC to a different room, Message
    | SetRoomVisited RoomID Bool String   -- ^ Mark room visited/unvisited, Message
    | SetFlag String String String        -- ^ Flag name, value, Message
    | CheckFlag String String ActionOutcome ActionOutcome -- ^ Flag, expected value, then-branch, else-branch
    | RandomChoice [ActionOutcome]        -- ^ Pick one outcome deterministically via game-state hash
    | GameEnd GameOverReason String       -- ^ End the game with a reason, Message
    -- Equipment (Phase 0)
    | EquipItem ItemID String             -- ^ Equip an item the player carries, Message
    | UnequipItem ItemID String           -- ^ Unequip an item, Message
    -- Skills (Phase 2)
    | CheckSkill SkillID Int ActionOutcome ActionOutcome -- ^ Skill, DC, Pass-branch, Fail-branch
    | ModifySkill SkillID Int String      -- ^ Skill, delta (+/-), Message
    -- Conditions (Phase 2)
    | ApplyCondition String Int (Maybe ActionOutcome) (Maybe ActionOutcome) -- ^ Name, turns, tick, end
    | ClearCondition String String        -- ^ Condition name, Message
    | HasCondition String ActionOutcome ActionOutcome -- ^ Condition name, then-branch, else-branch
    -- Quests (Phase 2)
    | StartQuest QuestID String           -- ^ Quest, Message
    | AdvanceQuest QuestID String         -- ^ Quest, Message (moves to next stage)
    | CompleteQuest QuestID String        -- ^ Quest, Message (fires questReward)
    -- Narratives (Phase 4.4)
    | Narrative [String] ActionOutcome    -- ^ Lines to show, then follow-up outcome
    deriving (Show, Eq, Generic)

instance ToJSON ActionOutcome
instance FromJSON ActionOutcome

-- ---------------------------------------------------------------------------
-- JSON helpers for compound Map keys
-- ---------------------------------------------------------------------------

-- | Encode a Map with (Verb, String) keys as "VTake:intact" style keys
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

-- | Encode a Map with (String, String) tuple keys using "a|b"
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

-- ---------------------------------------------------------------------------
-- Items
-- ---------------------------------------------------------------------------

-- | Static item definition (loaded from JSON)
data ItemDef = ItemDef
    { itemId            :: ItemID
    , itemName          :: String
    , itemDescription   :: String
    , itemKeywords      :: [String]
    , itemTags          :: Set.Set String        -- ^ e.g. "lightsource", "weapon", "key"
    , itemEquipSlot     :: Maybe EquipSlot       -- ^ Nothing = not equippable
    , itemEquipEffects  :: [EquipEffect]         -- ^ Passive bonuses while equipped
    , itemHidden        :: Bool                  -- ^ Hidden until discovered via `search`
    , itemDiscoverText  :: Maybe String          -- ^ Message shown on discovery
    , itemVerbMap       :: Map.Map (Verb, String) ActionOutcome
    } deriving (Show, Eq)

instance ToJSON ItemDef where
    toJSON def = object
        [ "itemId"           .= itemId def
        , "itemName"         .= itemName def
        , "itemDescription"  .= itemDescription def
        , "itemKeywords"     .= itemKeywords def
        , "itemTags"         .= itemTags def
        , "itemEquipSlot"    .= itemEquipSlot def
        , "itemEquipEffects" .= itemEquipEffects def
        , "itemHidden"       .= itemHidden def
        , "itemDiscoverText" .= itemDiscoverText def
        , "itemVerbMap"      .= verbStateMapToJSON (itemVerbMap def)
        ]

instance FromJSON ItemDef where
    parseJSON = withObject "ItemDef" $ \o -> ItemDef
        <$> o .:  "itemId"
        <*> o .:  "itemName"
        <*> o .:  "itemDescription"
        <*> o .:  "itemKeywords"
        <*> o .:? "itemTags"         .!= Set.empty
        <*> o .:? "itemEquipSlot"    .!= Nothing
        <*> o .:? "itemEquipEffects" .!= []
        <*> o .:? "itemHidden"       .!= False
        <*> o .:? "itemDiscoverText" .!= Nothing
        <*> (o .: "itemVerbMap" >>= verbStateMapFromJSON)

-- | Dynamic item state
data ItemState = ItemState
    { itemLocation    :: RoomID  -- ^ Room ID, "inventory", "consumed" or "container:<id>"
    , itemStatus      :: String  -- ^ e.g., "intact", "burned", "open"
    , itemProps       :: Map.Map String Int
    , itemDiscovered  :: Bool    -- ^ Has a hidden item been found?
    } deriving (Show, Eq, Generic)

instance ToJSON ItemState
instance FromJSON ItemState where
    parseJSON = withObject "ItemState" $ \o -> ItemState
        <$> o .:  "itemLocation"
        <*> o .:  "itemStatus"
        <*> o .:? "itemProps"      .!= Map.empty
        <*> o .:? "itemDiscovered" .!= False

-- ---------------------------------------------------------------------------
-- Dialogue
-- ---------------------------------------------------------------------------

-- | A single selectable reply inside a dialogue node
data DialogueChoice = DialogueChoice
    { dcText     :: String          -- ^ What the player says (shown as option)
    , dcNextNode :: Maybe String    -- ^ Next node in current tree (Nothing = exit dialogue)
    , dcOutcome  :: ActionOutcome   -- ^ Outcome applied when chosen
    } deriving (Show, Eq, Generic)

instance ToJSON DialogueChoice where
    toJSON c = object
        [ "dcText"     .= dcText c
        , "dcNextNode" .= dcNextNode c
        , "dcOutcome"  .= dcOutcome c
        ]

instance FromJSON DialogueChoice where
    parseJSON = withObject "DialogueChoice" $ \o -> DialogueChoice
        <$> o .:  "dcText"
        <*> o .:? "dcNextNode" .!= Nothing
        <*> o .:? "dcOutcome"  .!= MessageOnly ""

-- | One node (= one NPC utterance plus replies)
data DialogueNode = DialogueNode
    { dnId      :: String
    , dnText    :: String
    , dnChoices :: [DialogueChoice]
    } deriving (Show, Eq, Generic)

instance ToJSON DialogueNode
instance FromJSON DialogueNode

-- | A complete branching conversation
data DialogueTree = DialogueTree
    { dtEntry :: String                        -- ^ Starting node id
    , dtNodes :: Map.Map String DialogueNode
    } deriving (Show, Eq, Generic)

instance ToJSON DialogueTree
instance FromJSON DialogueTree

-- ---------------------------------------------------------------------------
-- NPCs
-- ---------------------------------------------------------------------------

-- | Static NPC definition
data NPCDef = NPCDef
    { npcId            :: NPCID
    , npcName          :: String
    , npcDescription   :: String
    , npcDialogue      :: Map.Map String String        -- ^ Legacy: Status -> single line
    , npcDialogueTrees :: Map.Map String DialogueTree  -- ^ Status -> branching dialogue
    , npcKeywords      :: [String]
    , npcMaxHealth     :: Maybe Int
    , npcAttackBase    :: Int
    , npcDefenseBase   :: Int
    , npcVerbMap       :: Map.Map (Verb, String) ActionOutcome
    } deriving (Show, Eq)

instance ToJSON NPCDef where
    toJSON def = object
        [ "npcId"            .= npcId def
        , "npcName"          .= npcName def
        , "npcDescription"   .= npcDescription def
        , "npcDialogue"      .= npcDialogue def
        , "npcDialogueTrees" .= npcDialogueTrees def
        , "npcKeywords"      .= npcKeywords def
        , "npcMaxHealth"     .= npcMaxHealth def
        , "npcAttackBase"    .= npcAttackBase def
        , "npcDefenseBase"   .= npcDefenseBase def
        , "npcVerbMap"       .= verbStateMapToJSON (npcVerbMap def)
        ]

instance FromJSON NPCDef where
    parseJSON = withObject "NPCDef" $ \o -> NPCDef
        <$> o .:  "npcId"
        <*> o .:  "npcName"
        <*> o .:  "npcDescription"
        <*> o .:? "npcDialogue"      .!= Map.empty
        <*> o .:? "npcDialogueTrees" .!= Map.empty
        <*> o .:  "npcKeywords"
        <*> o .:  "npcMaxHealth"
        <*> o .:  "npcAttackBase"
        <*> o .:  "npcDefenseBase"
        <*> (o .: "npcVerbMap" >>= verbStateMapFromJSON)

-- | Dynamic NPC state
data NPCState = NPCState
    { npcLocation  :: RoomID
    , npcStatus    :: String    -- ^ e.g., "alive", "dead", "sleeping"
    , npcHealth    :: Maybe Int
    , npcProps     :: Map.Map String Int
    , npcDialogueNode :: Maybe String  -- ^ Current node inside the active tree
    } deriving (Show, Eq, Generic)

instance ToJSON NPCState
instance FromJSON NPCState where
    parseJSON = withObject "NPCState" $ \o -> NPCState
        <$> o .:  "npcLocation"
        <*> o .:  "npcStatus"
        <*> o .:  "npcHealth"
        <*> o .:? "npcProps"         .!= Map.empty
        <*> o .:? "npcDialogueNode"  .!= Nothing

-- ---------------------------------------------------------------------------
-- Player
-- ---------------------------------------------------------------------------

-- | Player with combat statistics and skills.
--   The health/attack/defense fields are *base* values; equipment bonuses are
--   applied on lookup. Skills are plain named values (lockpick, stealth, ...).
data Player = Player
    { playerHealth    :: Int
    , playerMaxHealth :: Int
    , playerAttack    :: Int
    , playerDefense   :: Int
    , playerSkills    :: Map.Map SkillID Int
    } deriving (Show, Eq, Generic)

instance ToJSON Player
instance FromJSON Player where
    parseJSON = withObject "Player" $ \o -> Player
        <$> o .:  "playerHealth"
        <*> o .:  "playerMaxHealth"
        <*> o .:  "playerAttack"
        <*> o .:  "playerDefense"
        <*> o .:? "playerSkills" .!= Map.empty

-- ---------------------------------------------------------------------------
-- Conditions (status effects)
-- ---------------------------------------------------------------------------

-- | A timed status effect on the player.
--   Ticks once per turn; when the remaining turns reach 0 the effect ends.
data Condition = Condition
    { condName        :: String
    , condRemaining   :: Int                -- ^ Turns until it expires
    , condTickOutcome :: Maybe ActionOutcome -- ^ Fired every turn while active
    , condEndOutcome  :: Maybe ActionOutcome -- ^ Fired once when it expires
    } deriving (Show, Eq, Generic)

instance ToJSON Condition
instance FromJSON Condition

-- ---------------------------------------------------------------------------
-- Quests
-- ---------------------------------------------------------------------------

-- | One stage of a quest. Stages advance in order.
data QuestStage = QuestStage
    { qsId   :: String
    , qsText :: String                          -- ^ Shown in the journal
    , qsHint :: Maybe String                    -- ^ Optional nudge for the player
    } deriving (Show, Eq, Generic)

instance ToJSON QuestStage
instance FromJSON QuestStage

-- | A quest: an ordered list of stages plus a completion reward.
data Quest = Quest
    { questId          :: QuestID
    , questName        :: String
    , questDescription :: String
    , questPrereqs     :: Map.Map FlagID String -- ^ Flags that must match before StartQuest works
    , questStages      :: [QuestStage]
    , questReward      :: Maybe ActionOutcome
    } deriving (Show, Eq, Generic)

instance ToJSON Quest
instance FromJSON Quest

-- ---------------------------------------------------------------------------
-- Vehicles (Phase 3)
-- ---------------------------------------------------------------------------

-- | How a vehicle moves between its stops
data VehicleType
    = PlayerControlled   -- ^ The player steers it (from the cockpit, `drive to`)
    | AutomaticRoute     -- ^ Follows its route (`wait` advances to next stop)
    | PaidVehicle        -- ^ AutomaticRoute, but each stop costs an item
    deriving (Show, Eq, Generic)

instance ToJSON VehicleType
instance FromJSON VehicleType

-- | One stop on a vehicle's route: the outside room it docks at
data VehicleStop = VehicleStop
    { stopExternalRoom :: RoomID                     -- ^ Outside room at this stop
    , stopLabel        :: String                     -- ^ e.g. "Köln Hbf, Gleis 3"
    , stopCost         :: Maybe (ItemID, String)     -- ^ (item consumed per stop, error msg) for PaidVehicle
    } deriving (Show, Eq, Generic)

instance ToJSON VehicleStop
instance FromJSON VehicleStop

-- | Static vehicle definition. The vehicle's interior rooms live in the
--   world's global `rooms` map (so hooks, tags and lighting work there too);
--   `vehicleRooms` lists which room ids belong to this vehicle.
data VehicleDef = VehicleDef
    { vehicleId               :: VehicleID
    , vehicleName             :: String
    , vehicleDescription      :: String
    , vehicleType             :: VehicleType
    , vehicleRooms            :: [RoomID]                       -- ^ interior room ids
    , vehicleEntryRoom        :: RoomID                         -- ^ where `enter` puts the player
    , vehicleCockpitRoom      :: Maybe RoomID                   -- ^ required for `drive` (PlayerControlled)
    , vehicleStops            :: Map.Map RoomID VehicleStop     -- ^ outside room -> stop
    , vehicleKeywords         :: [String]
    , vehicleFuelProp         :: Maybe (String, Int)            -- ^ (fuel name, max units)
    , vehicleConditionEffects :: Map.Map String ActionOutcome   -- ^ condition -> outcome fired vehicle-wide
    } deriving (Show, Eq)

instance ToJSON VehicleDef where
    toJSON v = object
        [ "vehicleId"               .= vehicleId v
        , "vehicleName"             .= vehicleName v
        , "vehicleDescription"      .= vehicleDescription v
        , "vehicleType"             .= vehicleType v
        , "vehicleRooms"            .= vehicleRooms v
        , "vehicleEntryRoom"        .= vehicleEntryRoom v
        , "vehicleCockpitRoom"      .= vehicleCockpitRoom v
        , "vehicleStops"            .= vehicleStops v
        , "vehicleKeywords"         .= vehicleKeywords v
        , "vehicleFuelProp"         .= vehicleFuelProp v
        , "vehicleConditionEffects" .= vehicleConditionEffects v
        ]

instance FromJSON VehicleDef where
    parseJSON = withObject "VehicleDef" $ \o -> VehicleDef
        <$> o .:  "vehicleId"
        <*> o .:  "vehicleName"
        <*> o .:  "vehicleDescription"
        <*> o .:  "vehicleType"
        <*> o .:? "vehicleRooms"            .!= []
        <*> o .:  "vehicleEntryRoom"
        <*> o .:? "vehicleCockpitRoom"      .!= Nothing
        <*> o .:? "vehicleStops"            .!= Map.empty
        <*> o .:? "vehicleKeywords"         .!= []
        <*> o .:? "vehicleFuelProp"         .!= Nothing
        <*> o .:? "vehicleConditionEffects" .!= Map.empty

-- | Dynamic vehicle state
data VehicleState = VehicleState
    { vsCurrentStop      :: RoomID                    -- ^ outside room the vehicle is currently at
    , vsFuel             :: Maybe Int                 -- ^ remaining fuel units (if fuelled)
    , vsActiveConditions :: Set.Set String            -- ^ e.g. "hull_breach", "derailed"
    , vsRoomOverrides    :: Map.Map RoomID String     -- ^ condition-flavoured room descriptions
    } deriving (Show, Eq, Generic)

instance ToJSON VehicleState
instance FromJSON VehicleState where
    parseJSON = withObject "VehicleState" $ \o -> VehicleState
        <$> o .:  "vsCurrentStop"
        <*> o .:? "vsFuel"             .!= Nothing
        <*> o .:? "vsActiveConditions" .!= Set.empty
        <*> o .:? "vsRoomOverrides"    .!= Map.empty

-- ---------------------------------------------------------------------------
-- Rooms
-- ---------------------------------------------------------------------------

-- | Room with connections and static data.
--   Note: `roomVisited` lives in SaveState (dynamic), not here.
data Room = Room
    { roomId              :: RoomID
    , roomName            :: String
    , roomDescription     :: String
    , roomConnections     :: Map.Map Direction Exit
    , roomTags            :: Set.Set String            -- ^ "dark", "safe", "vehicle", ...
    , roomAltDescriptions :: Map.Map FlagID String     -- ^ flag -> alternative description
    , roomLightFlag       :: Maybe FlagID              -- ^ when "true", a "dark" room is lit
    , roomOnEnter         :: Maybe ActionOutcome
    , roomOnLook          :: Maybe ActionOutcome
    , roomOnExit          :: Maybe ActionOutcome
    , roomSearchOutcome   :: Maybe ActionOutcome
    , roomAscii           :: Maybe String              -- ^ Optional ASCII art banner (Phase 4.6)
    } deriving (Show, Eq, Generic)

instance ToJSON Room where
    toJSON r = object
        [ "roomId"              .= roomId r
        , "roomName"            .= roomName r
        , "roomDescription"     .= roomDescription r
        , "roomConnections"     .= roomConnections r
        , "roomTags"            .= roomTags r
        , "roomAltDescriptions" .= roomAltDescriptions r
        , "roomLightFlag"       .= roomLightFlag r
        , "roomOnEnter"         .= roomOnEnter r
        , "roomOnLook"          .= roomOnLook r
        , "roomOnExit"          .= roomOnExit r
        , "roomSearchOutcome"   .= roomSearchOutcome r
        , "roomAscii"           .= roomAscii r
        ]

instance FromJSON Room where
    parseJSON = withObject "Room" $ \o -> Room
        <$> o .:  "roomId"
        <*> o .:  "roomName"
        <*> o .:  "roomDescription"
        <*> o .:  "roomConnections"
        <*> o .:? "roomTags"            .!= Set.empty
        <*> o .:? "roomAltDescriptions" .!= Map.empty
        <*> o .:? "roomLightFlag"       .!= Nothing
        <*> o .:? "roomOnEnter"         .!= Nothing
        <*> o .:? "roomOnLook"          .!= Nothing
        <*> o .:? "roomOnExit"          .!= Nothing
        <*> o .:? "roomSearchOutcome"   .!= Nothing
        <*> o .:? "roomAscii"           .!= Nothing

-- | Player inventory
type Inventory = [ItemID]

-- ---------------------------------------------------------------------------
-- World & save state
-- ---------------------------------------------------------------------------

-- | Static world definition containing blueprint/map data
data GameWorld = GameWorld
    { rooms              :: Map.Map RoomID Room
    , itemDefs           :: Map.Map ItemID ItemDef
    , npcDefs            :: Map.Map NPCID NPCDef
    , entityInteractions :: Map.Map (String, String) (String, String)
    , itemInteractions   :: Map.Map (String, String) ActionOutcome  -- ^ (Item, Item) -> outcome
    , questDefs          :: Map.Map QuestID Quest                    -- ^ Static quest definitions
    , vehicleDefs        :: Map.Map VehicleID VehicleDef             -- ^ Static vehicle definitions (Phase 3)
    } deriving (Show, Eq)

instance ToJSON GameWorld where
    toJSON gw = object
        [ "rooms"              .= rooms gw
        , "itemDefs"           .= itemDefs gw
        , "npcDefs"            .= npcDefs gw
        , "entityInteractions" .= tupleMapToJSON (entityInteractions gw)
        , "itemInteractions"   .= Map.mapKeys (\(a, b) -> a ++ "|" ++ b) (itemInteractions gw)
        , "questDefs"          .= questDefs gw
        , "vehicleDefs"        .= vehicleDefs gw
        ]

instance FromJSON GameWorld where
    parseJSON = withObject "GameWorld" $ \o -> GameWorld
        <$> o .:  "rooms"
        <*> o .:  "itemDefs"
        <*> o .:  "npcDefs"
        <*> (o .: "entityInteractions" >>= tupleMapFromJSON)
        <*> (o .:? "itemInteractions" .!= Map.empty >>= parseItemInteractions)
        <*> o .:? "questDefs" .!= Map.empty
        <*> o .:? "vehicleDefs" .!= Map.empty

parseItemInteractions :: Map.Map String ActionOutcome -> Parser (Map.Map (String, String) ActionOutcome)
parseItemInteractions m =
    case mapM parseKey (Map.toList m) of
        Right pairs -> pure (Map.fromList pairs)
        Left err    -> fail err
  where
    parseKey (k, v) = case break (== '|') k of
        (a, '|':b) -> Right ((a, b), v)
        _          -> Left $ "Bad item interaction key: " ++ k

-- | Dynamic state of an active playthrough
data SaveState = SaveState
    { player             :: Player
    , currentRoom        :: RoomID
    , inventory          :: Inventory
    , itemStates         :: Map.Map ItemID ItemState
    , npcStates          :: Map.Map NPCID NPCState
    , entityStates       :: Map.Map String String
    , flags              :: Map.Map FlagID String
    , turnCount          :: Int
    , gameOver           :: Bool
    , gameOverReason     :: Maybe GameOverReason
    , visitedRooms       :: Set.Set RoomID
    , equipment          :: Map.Map EquipSlot ItemID
    , conditions         :: Map.Map String Condition          -- ^ Active status effects
    , activeQuests       :: Map.Map QuestID Int               -- ^ Quest -> current stage index (0-based)
    , completedQuests    :: Set.Set QuestID
    , vehicleStates      :: Map.Map VehicleID VehicleState    -- ^ Dynamic vehicle state (Phase 3)
    , currentVehicle     :: Maybe VehicleID                   -- ^ Vehicle the player is inside (Phase 3)
    , activeDialogue     :: Maybe NPCID                       -- ^ Currently engaged dialogue NPC (Phase 4.6)
    } deriving (Show, Eq)

instance ToJSON SaveState where
    toJSON ss = object
        [ "player"          .= player ss
        , "currentRoom"     .= currentRoom ss
        , "inventory"       .= inventory ss
        , "itemStates"      .= itemStates ss
        , "npcStates"       .= npcStates ss
        , "entityStates"    .= entityStates ss
        , "flags"           .= flags ss
        , "turnCount"       .= turnCount ss
        , "gameOver"        .= gameOver ss
        , "gameOverReason"  .= gameOverReason ss
        , "visitedRooms"    .= visitedRooms ss
        , "equipment"       .= equipment ss
        , "conditions"      .= conditions ss
        , "activeQuests"    .= activeQuests ss
        , "completedQuests" .= completedQuests ss
        , "vehicleStates"   .= vehicleStates ss
        , "currentVehicle"  .= currentVehicle ss
        , "activeDialogue"  .= activeDialogue ss
        ]

instance FromJSON SaveState where
    parseJSON = withObject "SaveState" $ \o -> SaveState
        <$> o .:  "player"
        <*> o .:  "currentRoom"
        <*> o .:  "inventory"
        <*> o .:  "itemStates"
        <*> o .:  "npcStates"
        <*> o .:  "entityStates"
        <*> o .:? "flags"           .!= Map.empty
        <*> o .:? "turnCount"       .!= 0
        <*> o .:  "gameOver"
        <*> o .:? "gameOverReason"  .!= Nothing
        <*> o .:? "visitedRooms"    .!= Set.empty
        <*> o .:? "equipment"       .!= Map.empty
        <*> o .:? "conditions"      .!= Map.empty
        <*> o .:? "activeQuests"    .!= Map.empty
        <*> o .:? "completedQuests" .!= Set.empty
        <*> o .:? "vehicleStates"   .!= Map.empty
        <*> o .:? "currentVehicle"  .!= Nothing
        <*> o .:? "activeDialogue"  .!= Nothing

-- | Save file wrapper with metadata for save slots
data SaveFile = SaveFile
    { saveVersion    :: Int
    , saveTimestamp  :: String
    , worldChecksum  :: String
    , saveName       :: String
    , saveData       :: SaveState
    } deriving (Show, Eq, Generic)

instance ToJSON SaveFile
instance FromJSON SaveFile

-- | Combined game state holding both world and save
data GameState = GameState
    { world :: GameWorld
    , save  :: SaveState
    , pendingNarrative :: Maybe ([String], ActionOutcome)  -- ^ Narrative lines + follow-up (Phase 4.4)
    } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Deterministic pseudo-random number generation from game state
-- ---------------------------------------------------------------------------

-- | Mix an integer through a Murmur3-style finalizer for good bit distribution
mixHash :: Int -> Int
mixHash x0 =
    let x1 = (x0 `xor` (x0 `shiftR` 16)) * 0x45d9f3b
        x2 = (x1 `xor` (x1 `shiftR` 16)) * 0x45d9f3b
    in x2 `xor` (x2 `shiftR` 16)

-- | Derive a deterministic pseudo-random non-negative Int from the current game state.
gameRandom :: GameState -> Int -> Int
gameRandom state salt =
    let tc      = turnCount (save state)
        roomVal = foldl (\acc c -> acc * 31 + fromEnum c) 0 (currentRoom (save state))
        invCnt  = length (inventory (save state))
        hp      = playerHealth (player (save state))
        combined = tc      * 2654435761
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
