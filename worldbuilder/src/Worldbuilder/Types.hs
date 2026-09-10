{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Authoring schema for the worldbuilder — clean YAML types
--   that get compiled into the engine's GameWorld + SaveState.
module Worldbuilder.Types where

import Data.Aeson
import Control.Applicative ((<|>))
import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map

-- ---------------------------------------------------------------------------
-- Top-level adventure
-- ---------------------------------------------------------------------------

data Adventure = Adventure
    { advName        :: Maybe String
    , advStartRoom   :: String
    , advRooms       :: [ARoom]
    , advItems       :: [AItem]
    , advNPCs        :: [ANPC]
    , advQuests      :: [AQuest]
    , advVehicles    :: [AVehicle]
    , advInteractions :: Maybe AInteractions
    } deriving (Show, Eq, Generic)

instance FromJSON Adventure where
    parseJSON = withObject "Adventure" $ \o -> Adventure
        <$> o .:? "name"
        <*> o .:  "start_room"
        <*> o .:? "rooms"           .!= []
        <*> o .:? "items"           .!= []
        <*> o .:? "npcs"            .!= []
        <*> o .:? "quests"          .!= []
        <*> o .:? "vehicles"        .!= []
        <*> o .:? "interactions"

-- ---------------------------------------------------------------------------
-- Rooms
-- ---------------------------------------------------------------------------

data ARoom = ARoom
    { arId          :: String
    , arName        :: String
    , arDesc        :: String
    , arExits       :: Map.Map String AExitRef
    , arTags        :: [String]
    , arAltDesc     :: Map.Map String String
    , arLightFlag   :: Maybe String
    , arOnEnter     :: Maybe [AActionOutcome]
    , arOnLook      :: Maybe [AActionOutcome]
    , arOnExit      :: Maybe [AActionOutcome]
    , arSearch      :: Maybe [AActionOutcome]
    , arAscii       :: Maybe String
    } deriving (Show, Eq, Generic)

instance FromJSON ARoom where
    parseJSON = withObject "ARoom" $ \o -> ARoom
        <$> o .:  "id"
        <*> o .:  "name"
        <*> o .:? "desc"      .!= ""
        <*> o .:? "exits"     .!= Map.empty
        <*> o .:? "tags"      .!= []
        <*> o .:? "alt_desc"  .!= Map.empty
        <*> o .:? "light_flag"
        <*> o .:? "on_enter"
        <*> o .:? "on_look"
        <*> o .:? "on_exit"
        <*> o .:? "search"
        <*> o .:? "ascii"

-- | Exit reference: target room + optional lock entity
data AExitRef = AExitRef
    { aeTarget :: String
    , aeLocked :: Maybe String
    } deriving (Show, Eq, Generic)

instance FromJSON AExitRef where
    -- Plain string "hallway" -> Open "hallway"
    parseJSON (String s) = pure (AExitRef (show s) Nothing)
    -- Object { to: ..., locked_by: ... }
    parseJSON v = withObject "AExitRef" (\o -> AExitRef
        <$> o .:  "to"
        <*> o .:? "locked_by") v

-- ---------------------------------------------------------------------------
-- Items
-- ---------------------------------------------------------------------------

data AItem = AItem
    { aiId         :: String
    , aiName       :: String
    , aiDesc       :: String
    , aiKeywords   :: [String]
    , aiTags       :: [String]
    , aiLocation   :: String           -- room id or "inventory"
    , aiState      :: String
    , aiEquipSlot  :: Maybe String
    , aiEquipEffects :: [String]       -- "attack+3", "defense+2", "maxhp+10"
    , aiHidden     :: Bool
    , aiDiscover   :: Maybe String
    , aiProps      :: Map.Map String Int
    , aiOnTake     :: Maybe [AActionOutcome]
    , aiVerbMap    :: Map.Map String [AActionOutcome]  -- "verb,state" -> outcomes
    } deriving (Show, Eq, Generic)

instance FromJSON AItem where
    parseJSON = withObject "AItem" $ \o -> AItem
        <$> o .:  "id"
        <*> o .:  "name"
        <*> o .:? "desc"      .!= ""
        <*> o .:? "keys"      .!= []
        <*> o .:? "tags"      .!= []
        <*> o .:? "location"  .!= "start"
        <*> o .:? "state"     .!= "intact"
        <*> o .:? "slot"
        <*> o .:? "effects"   .!= []
        <*> o .:? "hidden"    .!= False
        <*> o .:? "discover"
        <*> o .:? "props"     .!= Map.empty
        <*> o .:? "on_take"
        <*> o .:? "verb_map"  .!= Map.empty

-- ---------------------------------------------------------------------------
-- NPCs
-- ---------------------------------------------------------------------------

data ANPC = ANPC
    { anId          :: String
    , anName        :: String
    , anDesc        :: String
    , anKeywords    :: [String]
    , anLocation    :: String
    , anState       :: String
    , anMaxHealth   :: Maybe Int
    , anAttack      :: Int
    , anDefense     :: Int
    , anDialogue    :: Map.Map String ADialogueTree
    , anVerbMap     :: Map.Map String [AActionOutcome]
    } deriving (Show, Eq, Generic)

instance FromJSON ANPC where
    parseJSON = withObject "ANPC" $ \o -> ANPC
        <$> o .:  "id"
        <*> o .:  "name"
        <*> o .:? "desc"      .!= ""
        <*> o .:? "keys"      .!= []
        <*> o .:? "location"  .!= "start"
        <*> o .:? "state"     .!= "alive"
        <*> o .:? "max_hp"
        <*> o .:? "attack"    .!= 0
        <*> o .:? "defense"   .!= 0
        <*> o .:? "dialogue"  .!= Map.empty
        <*> o .:? "verb_map"  .!= Map.empty

-- | Dialogue tree: { entry: ..., nodes: { ... } }
data ADialogueTree = ADialogueTree
    { adtEntry :: String
    , adtNodes :: Map.Map String ADialogueNode
    } deriving (Show, Eq, Generic)

instance FromJSON ADialogueTree where
    parseJSON = withObject "ADialogueTree" $ \o -> ADialogueTree
        <$> o .:? "entry" .!= "start"
        <*> o .:? "nodes" .!= Map.empty

data ADialogueNode = ADialogueNode
    { adnText    :: String
    , adnChoices :: [ADialogueChoice]
    } deriving (Show, Eq, Generic)

instance FromJSON ADialogueNode where
    parseJSON = withObject "ADialogueNode" $ \o -> ADialogueNode
        <$> o .:  "text"
        <*> o .:? "choices" .!= []

-- | Choice: either a string (just text, no next/outcome) or an object
data ADialogueChoice = ADialogueChoice
    { adcText     :: String
    , adcNext     :: Maybe String
    , adcOutcomes :: [AActionOutcome]
    } deriving (Show, Eq, Generic)

instance FromJSON ADialogueChoice where
    parseJSON (String s) = pure (ADialogueChoice (show s) Nothing [])
    parseJSON v = withObject "ADialogueChoice" (\o -> ADialogueChoice
        <$> o .:  "text"
        <*> o .:? "next"
        <*> o .:? "outcomes" .!= []) v

-- ---------------------------------------------------------------------------
-- Quests
-- ---------------------------------------------------------------------------

data AQuest = AQuest
    { aqId      :: String
    , aqName    :: String
    , aqDesc    :: String
    , aqPrereqs :: [String]
    , aqStages  :: [AQuestStage]
    , aqReward  :: Maybe [AActionOutcome]
    } deriving (Show, Eq, Generic)

instance FromJSON AQuest where
    parseJSON = withObject "AQuest" $ \o -> AQuest
        <$> o .:  "id"
        <*> o .:  "name"
        <*> o .:? "desc"    .!= ""
        <*> o .:? "prereqs" .!= []
        <*> o .:? "stages"  .!= []
        <*> o .:? "reward"

data AQuestStage = AQuestStage
    { aqsId   :: String
    , aqsDesc :: String
    , aqsHint :: Maybe String
    } deriving (Show, Eq, Generic)

instance FromJSON AQuestStage where
    parseJSON = withObject "AQuestStage" $ \o -> AQuestStage
        <$> o .:  "id"
        <*> o .:? "desc" .!= ""
        <*> o .:? "hint"

-- ---------------------------------------------------------------------------
-- Vehicles
-- ---------------------------------------------------------------------------

data AVehicle = AVehicle
    { avId         :: String
    , avName       :: String
    , avDesc       :: String
    , avType       :: String              -- "player", "auto", "paid"
    , avInterior   :: [ARoom]
    , avEntryRoom  :: String
    , avCockpit    :: Maybe String
    , avStops      :: Map.Map String String
    , avKeywords   :: [String]
    , avFuel       :: Maybe (String, Int)
    , avConditions :: Map.Map String [AActionOutcome]
    } deriving (Show, Eq, Generic)

instance FromJSON AVehicle where
    parseJSON = withObject "AVehicle" $ \o -> AVehicle
        <$> o .:  "id"
        <*> o .:  "name"
        <*> o .:? "desc"       .!= ""
        <*> o .:? "type"       .!= "player"
        <*> o .:? "interior"   .!= []
        <*> o .:  "entry_room"
        <*> o .:? "cockpit"
        <*> o .:? "stops"      .!= Map.empty
        <*> o .:? "keys"       .!= []
        <*> o .:? "fuel"
        <*> o .:? "conditions" .!= Map.empty

-- ---------------------------------------------------------------------------
-- Interactions
-- ---------------------------------------------------------------------------

data AInteractions = AInteractions
    { aiEntity  :: Map.Map (String, String) String              -- (item, target) -> new state
    , aiItem    :: Map.Map (String, String) [AActionOutcome]    -- (item, item) -> outcomes
    } deriving (Show, Eq, Generic)

instance FromJSON AInteractions where
    parseJSON = withObject "AInteractions" $ \o -> AInteractions
        <$> o .:? "entity" .!= Map.empty
        <*> o .:? "item"   .!= Map.empty

-- ---------------------------------------------------------------------------
-- Action outcomes (YAML-friendly — each has exactly one key)
-- ---------------------------------------------------------------------------

data AActionOutcome
    = AOMessage String
    | AOHealPlayer Int
    | AODamagePlayer Int
    | AOGiveItem String
    | AOConsumeItem String
    | AOSetFlag String String
    | AOCheckFlag String String AActionOutcome AActionOutcome
    | AOStartQuest String
    | AOAdvanceQuest String
    | AOCompleteQuest String
    | AOEquipItem String
    | AORoomTransition String
    | AONarrative [String]
    deriving (Show, Eq, Generic)

-- Parse an outcome from an object with a single recognized key
instance FromJSON AActionOutcome where
    parseJSON (String s) = pure (AOMessage (show s))
    parseJSON v = withObject "AActionOutcome" (\o ->
            (AOMessage <$> o .: "msg")
        <|> (AOHealPlayer <$> o .: "heal")
        <|> (AODamagePlayer <$> o .: "damage")
        <|> (AOGiveItem <$> o .: "give")
        <|> (AOConsumeItem <$> o .: "consume")
        <|> (AOSetFlag <$> o .: "set_flag" <*> o .:? "val" .!= "true")
        <|> (AOStartQuest <$> o .: "start_quest")
        <|> (AOAdvanceQuest <$> o .: "advance_quest")
        <|> (AOCompleteQuest <$> o .: "complete_quest")
        <|> (AOEquipItem <$> o .: "equip")
        <|> (AORoomTransition <$> o .: "move")
        <|> (AONarrative <$> o .: "narrative")
        <|> fail "Unknown outcome type. Use one of: msg, heal, damage, give, consume, set_flag, start_quest, etc."
        ) v