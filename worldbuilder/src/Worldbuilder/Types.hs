{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Authoring schema for the worldbuilder — clean YAML types
--   that get compiled into the engine's GameWorld + SaveState.
module Worldbuilder.Types where

import Data.Aeson
import Data.Aeson.Types (Parser, Object)
import Control.Applicative ((<|>))
import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Types as E

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
    , advVerbs       :: [AVerb]
    , advVariables   :: [AVariable]
    , advTriggers    :: [ATrigger]
    , advPlayer      :: Maybe AAdventurePlayer   -- ^ Player stats override (Phase 4d)
    , advInitialVariables :: Map.Map String Value -- ^ initial_variables overrides (Phase 4d)
    , advInitialFlags     :: Map.Map String String       -- ^ initial_flags (Phase 4d)
    , advActiveQuests     :: [String]                    -- ^ active_quests (Phase 4d)
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
        <*> o .:? "verbs"           .!= []
        <*> o .:? "variables"       .!= []
        <*> o .:? "rules"           .!= []
        <*> o .:? "player"
        <*> o .:? "initial_variables" .!= Map.empty
        <*> o .:? "initial_flags"     .!= Map.empty
        <*> o .:? "active_quests"     .!= []

-- | Optional player stats block in YAML (Phase 4d).
--   `player: { max_hp: 50, attack: 8, defense: 3, skills: { lockpick: 5 } }`
data AAdventurePlayer = AAdventurePlayer
    { apMaxHealth :: Maybe Int
    , apAttack    :: Maybe Int
    , apDefense   :: Maybe Int
    , apSkills    :: Map.Map String Int
    } deriving (Show, Eq, Generic)

instance FromJSON AAdventurePlayer where
    parseJSON = withObject "AAdventurePlayer" $ \o -> AAdventurePlayer
        <$> o .:? "max_hp"
        <*> o .:? "attack"
        <*> o .:? "defense"
        <*> o .:? "skills" .!= Map.empty

-- | A declared adventure verb: canonical name + input aliases (Phase 3a).
data AVerb = AVerb
    { avbName    :: String
    , avbAliases :: [String]
    } deriving (Show, Eq, Generic)

instance FromJSON AVerb where
    parseJSON = withObject "AVerb" $ \o -> AVerb
        <$> o .:  "name"
        <*> o .:? "aliases" .!= []

-- | A trigger rule as authored in YAML (Phase 3f).
--   `on` is a string like "enter loc_3", "take crystal", "turn", "custom foo".
data ATrigger = ATrigger
    { atId        :: String
    , atOn        :: String
    , atWhen      :: Maybe E.Predicate     -- ^ optional condition (reuses engine predicate parsing)
    , atEffects   :: [AActionOutcome]
    , atOnce      :: Bool
    , atCooldown  :: Int
    } deriving (Show, Eq, Generic)

instance FromJSON ATrigger where
    parseJSON = withObject "ATrigger" $ \o -> ATrigger
        <$> o .:  "id"
        <*> o .:  "on"
        <*> o .:? "when"
        <*> o .:? "effects" .!= []
        <*> o .:? "once"    .!= False
        <*> o .:? "cooldown" .!= 0

-- | A conditional text variant in YAML: `when:` predicate gates `text:`.
data ATextVariant = ATextVariant
    { atvWhen :: E.Predicate
    , atvText :: String
    } deriving (Show, Eq, Generic)

instance FromJSON ATextVariant where
    parseJSON = withObject "ATextVariant" $ \o -> ATextVariant
        <$> o .:  "when"
        <*> o .:  "text"

-- | YAML shorthand for the engine's CondText: plain string or {default, variants}.
data ACondText = ACondText
    { actDefault  :: String
    , actVariants :: [ATextVariant]
    } deriving (Show, Eq, Generic)

instance FromJSON ACondText where
    parseJSON v = case v of
        String s -> pure (ACondText (T.unpack s) [])
        _ -> withObject "ACondText" (\o -> ACondText
                <$> o .:? "default"  .!= ""
                <*> o .:? "variants" .!= []) v

-- | Parse a `description` field: prefers the new CondText object, falls back
--   to legacy `desc:` string.
textField :: Object -> Parser ACondText
textField o = do
    mNew <- o .:? "description"
    case mNew of
        Just ct -> pure ct
        Nothing -> ACondText <$> o .:? "desc" .!= "" <*> pure []

-- ---------------------------------------------------------------------------
-- Rooms
-- ---------------------------------------------------------------------------

data ARoom = ARoom
    { arId          :: String
    , arName        :: String
    , arTexts       :: ACondText
    , arExits       :: Map.Map String AExitRef
    , arTags        :: [String]
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
        <*> textField o
        <*> o .:? "exits"     .!= Map.empty
        <*> o .:? "tags"      .!= []
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
    parseJSON (String s) = pure (AExitRef (T.unpack s) Nothing)
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
    , aiTexts      :: ACondText
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
    , aiPortable   :: Maybe Bool       -- default Nothing → True (backwards compat)
    , aiTakeFailure :: Maybe String
    , aiInContainer :: Maybe String    -- ^ container item id; when set, item starts inside it
    } deriving (Show, Eq, Generic)

instance FromJSON AItem where
    parseJSON = withObject "AItem" $ \o -> AItem
        <$> o .:  "id"
        <*> o .:  "name"
        <*> textField o
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
        <*> o .:? "portable"
        <*> o .:? "take_failure"
        <*> o .:? "in_container"

-- ---------------------------------------------------------------------------
-- NPCs
-- ---------------------------------------------------------------------------

data ANPC = ANPC
    { anId          :: String
    , anName        :: String
    , anTexts       :: ACondText
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
        <*> textField o
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
    , adcVisible  :: Maybe E.Predicate   -- ^ `visible_when` gate (optional)
    , adcOutcomes :: [AActionOutcome]
    } deriving (Show, Eq, Generic)

instance FromJSON ADialogueChoice where
    parseJSON (String s) = pure (ADialogueChoice (T.unpack s) Nothing Nothing [])
    parseJSON v = withObject "ADialogueChoice" (\o -> ADialogueChoice
        <$> o .:  "text"
        <*> o .:? "next"
        <*> o .:? "visible_when"
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
-- Variables (Phase 3b)
-- ---------------------------------------------------------------------------

-- | An adventure-declared variable in the YAML schema.
data AVariable = AVariable
    { avbVarName    :: String
    , avbVarType    :: String
    , avbInitial    :: Maybe Value
    , avbMin        :: Maybe Int
    , avbMax        :: Maybe Int
    } deriving (Show, Eq, Generic)

instance FromJSON AVariable where
    parseJSON = withObject "AVariable" $ \o -> AVariable
        <$> o .:  "name"
        <*> o .:  "type"
        <*> o .:? "initial"
        <*> o .:? "min"
        <*> o .:? "max"

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
    | AOMoveNPC String String          -- ^ npc id, target room (move_npc + to)
    | AOGameEnd String (Maybe String)  -- ^ reason (victory/death/custom), optional msg
    | AOConditional E.Predicate [AActionOutcome] [AActionOutcome]  -- ^ if/then/else
    | AONarrative [String]
    deriving (Show, Eq, Generic)

-- Parse an outcome from an object with a single recognized key
instance FromJSON AActionOutcome where
    parseJSON (String s) = pure (AOMessage (T.unpack s))
    parseJSON v = withObject "AActionOutcome" (\o ->
            -- NOTE: game_end must be tried before msg: an object may carry both
            -- "game_end" and a "msg" for the end screen.
            (AOGameEnd <$> o .: "game_end" <*> o .:? "msg")
        <|> (AOConditional <$> o .: "if" <*> o .:? "then" .!= [] <*> o .:? "else" .!= [])
        <|> (AOMessage <$> o .: "msg")
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
        <|> (AOMoveNPC <$> o .: "move_npc" <*> o .: "to")
        <|> (AONarrative <$> o .: "narrative")
        <|> fail "Unknown outcome type. Use one of: msg, heal, damage, give, consume, set_flag, start_quest, etc."
        ) v