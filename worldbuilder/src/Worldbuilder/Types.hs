{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Authoring schema for the worldbuilder — clean YAML types
--   that get compiled into the engine's GameWorld + SaveState.
module Worldbuilder.Types where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.Foldable as Foldable
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
    , advFactions         :: [AFaction]                  -- ^ factions + standing (Phase 7a)
    , advEncounterTables  :: [AEncounterTable]           -- ^ encounter tables (Phase 7c)
    , advEnvironment      :: Maybe AEnvironment          -- ^ weather + drains (Phase 7d)
    , advStealth          :: Maybe AStealth              -- ^ noise + observers (Phase 7e)
    , advPatrol           :: Maybe APatrol               -- ^ patrouillierende/angreifende NPCs (Modul 7i)
    , advCombat           :: Maybe ACombat               -- ^ combat profile (Phase 7f)
    , advAbilities        :: [AAbility]                  -- ^ player abilities (Phase 7f-3 A3/A4)
    , advEndArt           :: Map.Map String AAscii       -- ^ end banners per reason (Phase G)
    , advTitleArt         :: AAscii                      -- ^ optional title banner (Phase G)
    , advClips            :: [AClip]                     -- ^ cutscene clips (Phase H/H4)
    , advGame             :: Maybe AGamePolicy           -- ^ roguelike policy (Rogue Phase 1)
    , advCards            :: [ACard]                     -- ^ card definitions (Phase 2D)
    , advDeck             :: Maybe [String]              -- ^ starting deck (Phase 2D)
    , advHandLimit        :: Maybe Int                   -- ^ maximum cards in hand (Phase 2 / S2)
    , advSandboxZones     :: [ASandboxZone]              -- ^ procedural sandbox zones (Schritt 3 / Phase 3E)
    , advRawValue         :: Maybe Value                 -- ^ raw parsed JSON/YAML value for schema validation
    } deriving (Show, Eq, Generic)

-- | Rogue Phase 1: the authored `game:` block. Every field is optional so
--   authors opt in field by field; absent fields keep their engine default
--   (permadeath False, allow_undo True, ironman False, save_zones []).
data AGamePolicy = AGamePolicy
    { agpPermadeath :: Maybe Bool
    , agpAllowUndo  :: Maybe Bool
    , agpIronman    :: Maybe Bool
    , agpSaveZones  :: [String]
    , agpMetaSlug   :: Maybe String               -- ^ explicit slug override (Rogue Phase 2, M8)
    } deriving (Show, Eq, Generic)

instance FromJSON AGamePolicy where
    parseJSON = withObject "AGamePolicy" $ \o -> AGamePolicy
        <$> o .:? "permadeath"
        <*> o .:? "allow_undo"
        <*> o .:? "ironman"
        <*> o .:? "save_zones" .!= []
        <*> o .:? "meta_slug"

-- | A declared cutscene clip (Phase H/H4). Frames are inline (small scenes)
--   or live in a companion file (`file: art/pan.json`, D14) that the
--   Worldbuilder embeds at compile time — the runtime never reads files.
data AClip = AClip
    { acId     :: String
    , acFile   :: Maybe FilePath
    , acFrames :: [String]
    , acFps    :: Int
    } deriving (Show, Eq, Generic)

instance FromJSON AClip where
    parseJSON = withObject "AClip" $ \o -> AClip
        <$> o .: "id"
        <*> o .:? "file"
        <*> o .:? "frames" .!= []
        <*> o .: "fps"

instance FromJSON Adventure where
    parseJSON v@(Object o) = Adventure
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
        <*> o .:? "factions"          .!= []
        <*> o .:? "encounter_tables"  .!= []
        <*> o .:? "environment"
        <*> o .:? "stealth"
        <*> o .:? "patrol"
        <*> o .:? "combat"
        <*> o .:? "abilities"       .!= []
        <*> o .:? "end_art"         .!= Map.empty
        <*> o .:? "title_art"       .!= AAscii (ACondText "" []) [] 1 [] Nothing
        <*> o .:? "clips"           .!= []
        <*> o .:? "game"
        <*> parseCardsField o
        <*> parseDeckField o
        <*> parseHandLimitField o
        <*> parseSandboxZonesField o
        <*> pure (Just v)
    parseJSON _ = fail "Expected Adventure to be an object"

-- | Parse 'cards' field: supports both a map (`cards: { strike: { ... } }`) and a list (`cards: [ { id: "strike", ... } ]`).
parseCardsField :: Object -> Parser [ACard]
parseCardsField o = do
    mVal <- o .:? "cards"
    case mVal of
        Nothing -> pure []
        Just (Array arr) -> mapM parseJSON (Foldable.toList arr)
        Just (Object obj) ->
            mapM (\(k, v) -> do
                    card <- parseJSON v
                    let cid = if null (acdId card) then K.toString k else acdId card
                        cname = if null (acdName card) then cid else acdName card
                    pure card { acdId = cid, acdName = cname }
                 ) (KM.toList obj)
        Just _ -> fail "Expected 'cards' to be an object (map) or array (list)"

-- | Parse 'handLimit' / 'hand_limit' field: supports top-level or inside 'deck' block.
parseHandLimitField :: Object -> Parser (Maybe Int)
parseHandLimitField o = do
    topHL <- (o .:? "handLimit") <|> (o .:? "hand_limit")
    case topHL of
        Just hl -> pure (Just hl)
        Nothing -> do
            mDeckVal <- o .:? "deck"
            case mDeckVal of
                Just (Object dObj) -> (dObj .:? "handLimit") <|> (dObj .:? "hand_limit")
                _                  -> pure Nothing

-- | Parse 'deck' field: supports list of card IDs (`[strike, defend]`) or map with counts (`{ strike: 4, defend: 4 }`).
parseDeckField :: Object -> Parser (Maybe [String])
parseDeckField o = do
    mVal <- o .:? "deck"
    case mVal of
        Nothing -> pure Nothing
        Just v  -> Just <$> parseDeckValue v

-- | Parse deck representation from JSON Value (list or map with counts)
parseDeckValue :: Value -> Parser [String]
parseDeckValue (Array arr) = mapM parseJSON (Foldable.toList arr)
parseDeckValue (Object obj) = do
    case KM.lookup "cards" obj of
        Just cardsVal -> parseDeckValue cardsVal
        Nothing -> do
            let cardPairs = filter (\(k, _) -> K.toString k `notElem` ["handLimit", "hand_limit"]) (KM.toList obj)
            cardLists <- mapM (\(k, v) -> do
                                 count <- parseJSON v :: Parser Int
                                 pure (replicate count (K.toString k))
                              ) cardPairs
            pure (concat cardLists)
parseDeckValue _ = fail "Expected 'deck' to be a list of card IDs or a map of card ID to count"

-- | Parse 'sandbox_zones' field: supports both a map (`sandbox_zones: { wildnis: { ... } }`) and a list (`sandbox_zones: [ { id: "wildnis", ... } ]`).
parseSandboxZonesField :: Object -> Parser [ASandboxZone]
parseSandboxZonesField o = do
    mVal <- o .:? "sandbox_zones"
    case mVal of
        Nothing -> pure []
        Just (Array arr) -> mapM parseJSON (Foldable.toList arr)
        Just (Object obj) ->
            mapM (\(k, v) -> do
                    sz <- parseJSON v
                    let zid = if null (aszId sz) then K.toString k else aszId sz
                    pure sz { aszId = zid }
                 ) (KM.toList obj)
        Just _ -> fail "Expected 'sandbox_zones' to be an object (map) or array (list)"

-- ---------------------------------------------------------------------------
-- Cards (Schritt 2 / Phase 2D)
-- ---------------------------------------------------------------------------

-- | A card as authored in YAML.
data ACard = ACard
    { acdId          :: String
    , acdName        :: String
    , acdCost        :: Map.Map String Int
    , acdType        :: String               -- ^ "attack", "skill", "power", "curse", "status"
    , acdTarget      :: String               -- ^ "self", "single_enemy", "all_enemies", "none"
    , acdDescription :: String
    , acdExhaust     :: Bool
    , acdOutcomes    :: [AActionOutcome]
    } deriving (Show, Eq, Generic)

instance FromJSON ACard where
    parseJSON = withObject "ACard" $ \o -> ACard
        <$> o .:? "id"          .!= ""
        <*> o .:? "name"        .!= ""
        <*> o .:? "cost"        .!= Map.empty
        <*> o .:? "type"        .!= "skill"
        <*> o .:? "target"      .!= "none"
        <*> (do mDesc <- o .:? "description"
                case mDesc of
                    Just d -> pure d
                    Nothing -> o .:? "desc" .!= "")
        <*> o .:? "exhaust"     .!= False
        <*> (do mOutcomes <- o .:? "outcomes"
                case mOutcomes of
                    Just os -> pure os
                    Nothing -> o .:? "effects" .!= [])

-- | Optional player stats block in YAML (Phase 4d).
--   `player: { max_hp: 50, attack: 8, defense: 3, skills: { lockpick: 5 }, deck: [strike, defend] }`
data AAdventurePlayer = AAdventurePlayer
    { apMaxHealth :: Maybe Int
    , apAttack    :: Maybe Int
    , apDefense   :: Maybe Int
    , apSkills    :: Map.Map String Int
    , apDeck      :: Maybe [String]
    , apHandLimit :: Maybe Int
    } deriving (Show, Eq, Generic)

instance FromJSON AAdventurePlayer where
    parseJSON = withObject "AAdventurePlayer" $ \o -> AAdventurePlayer
        <$> o .:? "max_hp"
        <*> o .:? "attack"
        <*> o .:? "defense"
        <*> o .:? "skills" .!= Map.empty
        <*> (o .:? "deck" >>= maybe (pure Nothing) (fmap Just . parseDeckValue))
        <*> (o .:? "handLimit" <|> o .:? "hand_limit")

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

-- | YAML form of the engine's `AsciiArt` (Phase B/D/H): a plain string, a
--   `{default, variants}` CondText, an animated `{frames, every, ...}` object,
--   or one carrying an `ambient` loop (Phase H/H1).
data AAscii = AAscii
    { asaStatic   :: ACondText
    , asaFrames   :: [ACondText]
    , asaEvery    :: Int
    , asaHotspots :: [E.Hotspot]
    , asaAmbient  :: Maybe E.Ambient
    } deriving (Show, Eq, Generic)

instance FromJSON AAscii where
    parseJSON v = case v of
        String s -> pure (AAscii (ACondText (T.unpack s) []) [] 0 [] Nothing)
        _ -> withObject "AAscii" (\o -> do
                stat <- ACondText <$> o .:? "default" .!= "" <*> o .:? "variants" .!= []
                frames <- o .:? "frames" .!= []
                every <- o .:? "every" .!= 1
                spots <- o .:? "hotspots" .!= []
                amb <- o .:? "ambient" .!= Nothing
                pure (AAscii stat frames every spots amb)) v

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
    , arAscii       :: AAscii
    , arIntro       :: Maybe String               -- ^ clip id played once on enter (Phase H/H4)
    , arFloor       :: Maybe Int                  -- ^ optional floor / dungeon level index (Phase 4b)
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
        <*> o .:? "ascii"     .!= AAscii (ACondText "" []) [] 0 [] Nothing
        <*> o .:? "intro"     .!= Nothing
        <*> o .:? "floor"     .!= Nothing

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
-- Procedural Sandbox Zones (Schritt 3 / Phase 3E)
-- ---------------------------------------------------------------------------

-- | Biome template as authored in YAML.
data ABiomeTemplate = ABiomeTemplate
    { abtId           :: String
    , abtWeight       :: Maybe Int
    , abtNamePattern  :: Maybe String
    , abtDescription  :: ACondText
    , abtTags         :: [String]
    , abtAsciiArt     :: Maybe AAscii
    , abtPassableDirs :: [String]
    } deriving (Show, Eq, Generic)

instance FromJSON ABiomeTemplate where
    parseJSON = withObject "ABiomeTemplate" $ \o -> ABiomeTemplate
        <$> o .:? "id" .!= ""
        <*> o .:? "weight"
        <*> o .:? "name_pattern"
        <*> textField o
        <*> o .:? "tags" .!= []
        <*> (o .:? "ascii_art" <|> o .:? "ascii")
        <*> o .:? "passable_dirs" .!= []

-- | Sandbox zone as authored in YAML.
data ASandboxZone = ASandboxZone
    { aszId     :: String
    , aszOrigin :: (Int, Int, Int)
    , aszFloor  :: Maybe Int
    , aszBiomes :: [ABiomeTemplate]
    } deriving (Show, Eq, Generic)

instance FromJSON ASandboxZone where
    parseJSON = withObject "ASandboxZone" $ \o -> ASandboxZone
        <$> o .:? "id" .!= ""
        <*> parseOriginField o
        <*> o .:? "floor"
        <*> parseBiomesField o

parseOriginField :: Object -> Parser (Int, Int, Int)
parseOriginField o = do
    mOrig <- o .:? "origin"
    case mOrig of
        Nothing -> pure (0, 0, 0)
        Just (Array arr) -> case Foldable.toList arr of
            [Number x, Number y, Number z] -> pure (round x, round y, round z)
            [Number x, Number y]           -> pure (round x, round y, 0)
            _                              -> fail "origin array must have 2 or 3 numbers"
        Just (Object obj) -> do
            x <- obj .:? "x" .!= 0
            y <- obj .:? "y" .!= 0
            z <- obj .:? "z" .!= 0
            pure (x, y, z)
        Just _ -> fail "origin must be array or object"

parseBiomesField :: Object -> Parser [ABiomeTemplate]
parseBiomesField o = do
    mVal <- o .:? "biomes"
    case mVal of
        Nothing -> pure []
        Just (Array arr) -> mapM parseJSON (Foldable.toList arr)
        Just (Object obj) ->
            mapM (\(k, v) -> do
                    bt <- parseJSON v
                    let bid = if null (abtId bt) then K.toString k else abtId bt
                    pure bt { abtId = bid }
                 ) (KM.toList obj)
        Just _ -> fail "Expected 'biomes' to be an object (map) or array (list)"

-- ---------------------------------------------------------------------------
-- Items
-- ---------------------------------------------------------------------------

data AItem = AItem
    { aiId         :: String
    , aiName       :: String
    , aiTexts      :: ACondText
    , aiAscii      :: AAscii
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
        <*> o .:? "ascii"     .!= AAscii (ACondText "" []) [] 0 [] Nothing
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
    , anAscii       :: AAscii
    , anKeywords    :: [String]
    , anLocation    :: String
    , anState       :: String
    , anMaxHealth   :: Maybe Int
    , anAttack      :: Int
    , anDefense     :: Int
    , anDialogue    :: Map.Map String ADialogueTree
    , anVerbMap     :: Map.Map String [AActionOutcome]
    , anParty       :: Maybe AParty          -- ^ party / companion block (Phase 7g)
    } deriving (Show, Eq, Generic)

-- | The `party:` block on an NPC (Phase 7g): the NPC can be recruited,
--   follows the player and fights alongside them.
--
--     party: { can_join: true, order_verb: follow, hp_tracked: true }
--
--   Compiles to the follow variable `party.<npcId>` plus one order verb that
--   toggles membership — no engine-side party state of its own.
data AParty = AParty
    { aptCanJoin   :: Bool    -- ^ false: never recruitable (the block is inert)
    , aptOrderVerb :: String  -- ^ custom verb that toggles follow / stay
    , aptHpTracked :: Bool    -- ^ true: companion HP is tracked (needs max_hp)
    , aptFollowMsg :: Maybe String  -- ^ message when the companion joins
    , aptStayMsg   :: Maybe String  -- ^ message when the companion stays behind
    } deriving (Show, Eq, Generic)

instance FromJSON AParty where
    parseJSON = withObject "AParty" $ \o -> AParty
        <$> o .:? "can_join"   .!= True
        <*> o .:? "order_verb" .!= "follow"
        <*> o .:? "hp_tracked" .!= True
        <*> o .:? "follow_msg"
        <*> o .:? "stay_msg"

instance FromJSON ANPC where
    parseJSON = withObject "ANPC" $ \o -> ANPC
        <$> o .:  "id"
        <*> o .:  "name"
        <*> textField o
        <*> o .:? "ascii"     .!= AAscii (ACondText "" []) [] 0 [] Nothing
        <*> o .:? "keys"      .!= []
        <*> o .:? "location"  .!= "start"
        <*> o .:? "state"     .!= "alive"
        <*> o .:? "max_hp"
        <*> o .:? "attack"    .!= 0
        <*> o .:? "defense"   .!= 0
        <*> o .:? "dialogue"  .!= Map.empty
        <*> o .:? "verb_map"  .!= Map.empty
        <*> o .:? "party"

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
    , avStops      :: Map.Map String AStop
    , avKeywords   :: [String]
    , avFuel       :: Maybe AFuel
    , avConditions :: Map.Map String [AActionOutcome]
    , avStartStop  :: Maybe String        -- ^ label of the stop the vehicle starts at
    , avSystems    :: Map.Map String ASystem  -- ^ ship systems -> VarMap (Phase 7h)
    , avStations   :: [AStation]          -- ^ interior-room verbs (Phase 7h)
    } deriving (Show, Eq, Generic)

-- | A vehicle stop (P1-19). Short form `label: room_id`, or long form
--   `label: { room: room_id, cost: { item: ticket, refused: "…" } }`. A
--   declared cost makes boarding consume the item — the engine's `paid`
--   vehicle fare (`VehicleStop.stopCost`), previously unreachable from YAML.
data AStop = AStop
    { asRoom :: String
    , asCost :: Maybe (String, String)   -- ^ (item consumed per stop, refusal message)
    } deriving (Show, Eq, Generic)

instance FromJSON AStop where
    parseJSON (String s) = pure (AStop (T.unpack s) Nothing)
    parseJSON v = withObject "AStop" (\o -> AStop
        <$> o .:  "room"
        <*> (o .:? "cost" >>= traverse
                (\c -> (,) <$> c .: "item"
                           <*> c .:? "refused" .!= "You cannot pay the fare."))) v

-- | Vehicle fuel (P2-21): `fuel: { item: hay, max: 10 }` or the legacy
--   two-element form `fuel: [hay, 10]`.
data AFuel = AFuel
    { afItem :: String
    , afMax  :: Int
    } deriving (Show, Eq, Generic)

instance FromJSON AFuel where
    parseJSON v =
        (do xs <- parseJSON v :: Parser [Value]
            case xs of
                [i, m] -> AFuel <$> parseJSON i <*> parseJSON m
                _      -> fail "fuel: expected [item, max] or {item, max}")
        <|> withObject "AFuel" (\o -> AFuel <$> o .: "item" <*> o .: "max") v

-- | A ship system (Phase 7h): compiled into the VarMap entry
--   `ship.<vehicleId>.<name>`. `power`, `shields`, `hull` and `weapons` carry
--   combat meaning in the 7f resolver; every other name is free for rules.
data ASystem = ASystem
    { asInitial :: Int   -- ^ starting value
    , asMax     :: Int   -- ^ upper bound (0 = no declared bound)
    } deriving (Show, Eq, Generic)

instance FromJSON ASystem where
    parseJSON = withObject "ASystem" $ \o -> ASystem
        <$> o .:? "initial" .!= 0
        <*> o .:? "max"     .!= 0

-- | A station on a ship (Phase 7h): an interior room plus a verb that only
--   works while the player stands there.
data AStation = AStation
    { astRoom    :: String                    -- ^ interior room id
    , astVerb    :: String                    -- ^ declared custom verb
    , astEffects :: [AActionOutcome]
    , astWhen    :: Maybe E.Predicate         -- ^ extra gate
    } deriving (Show, Eq, Generic)

instance FromJSON AStation where
    parseJSON = withObject "AStation" $ \o -> AStation
        <$> o .:  "room"
        <*> o .:  "verb"
        <*> o .:? "effects" .!= []
        <*> o .:? "when"

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
        <*> o .:? "start_stop"
        <*> o .:? "systems"    .!= Map.empty
        <*> o .:? "stations"   .!= []

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
-- Factions (Phase 7a)
-- ---------------------------------------------------------------------------

-- | A declared faction: standing lives as the VarMap entry `faction.<id>`.
--   `levels` are well-formedness-checked authoring hints (threshold metadata);
--   runtime gating uses the numeric standing predicates (`at_least`/`at_most`).
data AFaction = AFaction
    { afId     :: String
    , afName   :: String
    , afInitial :: Int
    , afLevels :: [AFactionLevel]   -- ^ threshold metadata (authoring hints)
    } deriving (Show, Eq, Generic)

-- | A named standing threshold: at `at` points the relation is called `name`.
data AFactionLevel = AFactionLevel
    { aflAt   :: Int
    , aflName :: String
    } deriving (Show, Eq, Generic)

instance FromJSON AFactionLevel where
    parseJSON = withObject "AFactionLevel" $ \o -> AFactionLevel
        <$> o .:  "at"
        <*> o .:  "name"

instance FromJSON AFaction where
    parseJSON = withObject "AFaction" $ \o -> AFaction
        <$> o .:  "id"
        <*> o .:? "name"    .!= ""
        <*> o .:? "initial" .!= 0
        <*> o .:? "levels"  .!= []

-- ---------------------------------------------------------------------------
-- Encounter tables (Phase 7c)
-- ---------------------------------------------------------------------------

-- | A weighted random-event table: compiles to one trigger whose effects are a
--   single `RandomChoice [(weight, Effect)]`. The `on`/`cooldown`/`when` reuse
--   the regular trigger machinery.
data AEncounterTable = AEncounterTable
    { ertId       :: String
    , ertOn       :: String                       -- ^ e.g. "turn" or "enter wilds"
    , ertWhen     :: Maybe E.Predicate            -- ^ table-level gate
    , ertCooldown :: Int                          -- ^ turns between draws
    , ertEntries  :: [AEncounterEntry]
    } deriving (Show, Eq, Generic)

-- | One weighted row; `weight` is relative. Optional `when` gates the row.
data AEncounterEntry = AEncounterEntry
    { eneWeight  :: Int
    , eneWhen    :: Maybe E.Predicate
    , eneEffects :: [AActionOutcome]
    } deriving (Show, Eq, Generic)

instance FromJSON AEncounterEntry where
    parseJSON = withObject "AEncounterEntry" $ \o -> AEncounterEntry
        <$> o .:  "weight"
        <*> o .:? "when"    .!= Nothing
        <*> o .:? "effects" .!= []

instance FromJSON AEncounterTable where
    parseJSON = withObject "AEncounterTable" $ \o -> AEncounterTable
        <$> o .:  "id"
        <*> o .:? "on"        .!= "turn"
        <*> o .:? "when"      .!= Nothing
        <*> o .:? "cooldown"  .!= 0
        <*> o .:? "entries"   .!= []

-- ---------------------------------------------------------------------------
-- Environment (Phase 7d): weather + drains
-- ---------------------------------------------------------------------------

-- | The `environment:` segment: an optional weather machine and a list of
--   per-turn variable drains (hunger, temperature, ...).
data AEnvironment = AEnvironment
    { envWeather :: Maybe AWeatherDef
    , envDrains  :: [ADrainDef]
    } deriving (Show, Eq, Generic)

instance FromJSON AEnvironment where
    parseJSON = withObject "AEnvironment" $ \o -> AEnvironment
        <$> o .:? "weather"
        <*> o .:? "drains" .!= []

-- | Weather: a finite set of states (clear, storm, ...) stored as the int
--   variable `env.weather` (index into `states`). Transitions fire on
--   `on: turn`, gated by `when`; when several apply, the later one wins.
data AWeatherDef = AWeatherDef
    { weaStates      :: [String]
    , weaInitial     :: String
    , weaTransitions :: [AWeatherTransition]
    } deriving (Show, Eq, Generic)

instance FromJSON AWeatherDef where
    parseJSON = withObject "AWeatherDef" $ \o -> AWeatherDef
        <$> o .:  "states"
        <*> o .:? "initial"      .!= ""
        <*> o .:? "transitions"  .!= []

-- | One guarded weather transition: if `when` holds on a turn, the weather
--   moves to `to` (and the extra effects run).
data AWeatherTransition = AWeatherTransition
    { wtWhen    :: Maybe E.Predicate
    , wtTo      :: String
    , wtEffects :: [AActionOutcome]
    } deriving (Show, Eq, Generic)

instance FromJSON AWeatherTransition where
    parseJSON = withObject "AWeatherTransition" $ \o -> AWeatherTransition
        <$> o .:? "when"    .!= Nothing
        <*> o .:  "to"
        <*> o .:? "effects" .!= []

-- | One drain: every turn (while `when` holds) the variable moves by
--   `per_turn`; once it reaches 0 or below, `at_zero` fires.
data ADrainDef = ADrainDef
    { drVar     :: String
    , drPerTurn :: Int
    , drWhen    :: Maybe E.Predicate
    , drAtZero  :: [AActionOutcome]
    } deriving (Show, Eq, Generic)

instance FromJSON ADrainDef where
    parseJSON = withObject "ADrainDef" $ \o -> ADrainDef
        <$> o .:  "var"
        <*> o .:  "per_turn"
        <*> o .:? "when"    .!= Nothing
        <*> o .:? "at_zero" .!= []

-- ---------------------------------------------------------------------------
-- Stealth (Phase 7e): noise + observers
-- ---------------------------------------------------------------------------

-- | The `stealth:` segment: a noise variable that rises on movement and
--   decays per turn, plus observers (NPCs) that react once noise crosses a
--   threshold. Pure schema-sugar on triggers/variables — no core change.
data AStealth = AStealth
    { stNoise    :: ANoiseSpec
    , stObservers :: [AObserver]
    } deriving (Show, Eq, Generic)

instance FromJSON AStealth where
    parseJSON = withObject "AStealth" $ \o -> AStealth
        <$> o .:  "noise"
        <*> o .:? "observers" .!= []

-- | Noise machine: variable name (default "noise"), gain per move, decay
--   per turn, and an upper clamp. The compiler emits one `on: enter` trigger
--   per room (gain) plus one `on: turn` trigger (decay).
data ANoiseSpec = ANoiseSpec
    { nsVar      :: String
    , nsOnMove   :: Int
    , nsDecay    :: Int
    , nsMax      :: Int
    } deriving (Show, Eq, Generic)

instance FromJSON ANoiseSpec where
    parseJSON = withObject "ANoiseSpec" $ \o -> ANoiseSpec
        <$> o .:? "var"         .!= "noise"
        <*> o .:? "on_move"     .!= 0
        <*> o .:? "decay_per_turn" .!= 0
        <*> o .:? "max"         .!= 100

-- | One observer: an NPC who reacts when noise >= `hears_at`. `cooldown`
--   re-arms the guard after N turns (0 = react every turn while loud).
data AObserver = AObserver
    { obNPC     :: String
    , obHearsAt :: Int
    , obCooldown :: Int
    , obOnHear  :: [AActionOutcome]
    } deriving (Show, Eq, Generic)

instance FromJSON AObserver where
    parseJSON = withObject "AObserver" $ \o -> AObserver
        <$> o .:  "npc"
        <*> o .:  "hears_at"
        <*> o .:? "cooldown" .!= 0
        <*> o .:? "on_hear"  .!= []

-- ---------------------------------------------------------------------------
-- Patrol (Modul 7i): umherziehende und angreifende NPCs
-- ---------------------------------------------------------------------------

-- | Das `patrol:`-Segment: NPCs, die auf einem Rundkurs umherziehen und
--   zuschlagen, sobald sie mit dem Spieler im selben Raum stehen. Reiner
--   Schema-Zucker auf Triggern/Variablen — kein Kern-Eingriff.
data APatrol = APatrol
    { ptHostiles :: [AHostile]
    } deriving (Show, Eq, Generic)

instance FromJSON APatrol where
    parseJSON = withObject "APatrol" $ \o -> APatrol
        <$> o .:? "hostiles" .!= []

-- | Ein umherziehender oder stehender Feind. `path` ist der Rundkurs in
--   Raum-IDs, `start_index` zeigt auf den Startraum (`path !! start_index`).
--   `guardian: true` lässt den NPC stehen (dann listet `path` nur noch die
--   Räume, in denen er angreift). `warn` ist eine Meldung, die feuert, sobald
--   der Feind mit dem Spieler im selben Raum steht (Anwesenheits-Warnung);
--   `attack` sind gewöhnliche Outcome-Effekte für denselben Fall.
data AHostile = AHostile
    { ahNPC        :: String
    , ahPath       :: [String]
    , ahStartIndex :: Int
    , ahGuardian   :: Bool
    , ahWarn       :: Maybe String
    , ahAttack     :: [AActionOutcome]
    } deriving (Show, Eq, Generic)

instance FromJSON AHostile where
    parseJSON = withObject "AHostile" $ \o -> AHostile
        <$> o .:  "npc"
        <*> o .:? "path"        .!= []
        <*> o .:? "start_index" .!= 0
        <*> o .:? "guardian"    .!= False
        <*> o .:? "warn"
        <*> o .:? "attack"      .!= []

-- ---------------------------------------------------------------------------
-- Combat (Phase 7f): authored combat profile
-- ---------------------------------------------------------------------------

-- | The `combat:` segment: `profile` is one of off | narrative | classic | tactical.
--   - off:       attack is refused (attack_refused message), no HP spent
--   - narrative: opposed roll (player effective attack vs defense +
--     difficulty offset); on_win / on_lose effects decide everything
--   - classic:   exactly today's behaviour — and the default without a block
--   - tactical:  round-based tactical combat with player actions and enemy reactions (Phase 7f-3)
data ACombat = ACombat
    { acProfile        :: String
    , acAttackRefused  :: Maybe String
    , acDifficulty     :: Int
    , acOnWin          :: [AActionOutcome]
    , acOnLose         :: [AActionOutcome]
    , acInitiative     :: Maybe String
    , acFleeAllowed    :: Maybe Bool
    , acMaxRounds      :: Maybe Int
    , acSpeedAttribute :: Maybe String
    , acScreen         :: Maybe ACombatScreen
    } deriving (Show, Eq, Generic)

instance FromJSON ACombat where
    parseJSON = withObject "ACombat" $ \o -> ACombat
        <$> o .:? "profile"         .!= "classic"
        <*> o .:? "attack_refused"
        <*> o .:? "difficulty"      .!= 0
        <*> o .:? "on_win"          .!= []
        <*> o .:? "on_lose"         .!= []
        <*> o .:? "initiative"
        <*> o .:? "flee_allowed"
        <*> o .:? "max_rounds"
        <*> o .:? "speed_attribute"
        <*> o .:? "screen"

-- | Das `combat.screen`-Segment (nur Profil `classic`): der Kampfbildschirm,
--   den das Original vor jedem Schlag ausgab. Ohne Block bleibt die Ausgabe
--   unverändert.
data ACombatScreen = ACombatScreen
    { asArt      :: AAscii          -- ^ optionale Kampfkunst ueber dem Block
    , asBarWidth :: Int             -- ^ Zellenbreite beider KP-Balken (Default 10)
    , asScene    :: Maybe String    -- ^ Szenenzeile; Nothing = Original-Wortlaut
    , asFooter   :: Maybe String    -- ^ Flucht-Hinweis; Nothing = Original-Wortlaut
    } deriving (Show, Eq, Generic)

instance FromJSON ACombatScreen where
    parseJSON = withObject "ACombatScreen" $ \o -> ACombatScreen
        <$> o .:? "art"       .!= AAscii (ACondText "" []) [] 0 [] Nothing
        <*> o .:? "bar_width" .!= 10
        <*> o .:? "scene"
        <*> o .:? "footer"

-- ---------------------------------------------------------------------------
-- Abilities (Phase 7f-3 A3/A4)
-- ---------------------------------------------------------------------------

data AAbility = AAbility
    { aabId       :: String
    , aabName     :: Maybe String
    , aabCostVar  :: Maybe String
    , aabCost     :: Maybe Int
    , aabCooldown :: Maybe Int
    , aabEffects  :: [AActionOutcome]
    } deriving (Show, Eq, Generic)

instance FromJSON AAbility where
    parseJSON = withObject "AAbility" $ \o -> AAbility
        <$> o .:  "id"
        <*> o .:? "name"
        <*> o .:? "cost_var"
        <*> o .:? "cost"
        <*> o .:? "cooldown"
        <*> o .:? "effects" .!= []

-- ---------------------------------------------------------------------------
-- Interactions
-- ---------------------------------------------------------------------------

data AInteractions = AInteractions
    { aiEntity  :: [AEntityInteraction]
    , aiItem    :: [AItemInteraction]
    } deriving (Show, Eq, Generic)

-- | `use <item> on <target>` -> sets the target entity's state.
data AEntityInteraction = AEntityInteraction
    { aeiItem   :: String
    , aeiTarget :: String
    , aeiState  :: String            -- ^ new state of the target (e.g. "unlocked")
    , aeiMsg    :: Maybe String
    } deriving (Show, Eq, Generic)

-- | Item-on-item interaction (crafting): `use <item1> on <item2>`.
data AItemInteraction = AItemInteraction
    { aiiItem1   :: String
    , aiiItem2   :: String
    , aiiEffects :: [AActionOutcome]
    } deriving (Show, Eq, Generic)

instance FromJSON AEntityInteraction where
    parseJSON = withObject "AEntityInteraction" (\o -> AEntityInteraction
        <$> o .:  "item"
        <*> o .:  "target"
        <*> o .:  "state"
        <*> o .:? "msg")

instance FromJSON AItemInteraction where
    parseJSON = withObject "AItemInteraction" (\o -> AItemInteraction
        <$> o .:  "item1"
        <*> o .:  "item2"
        <*> o .:? "effects" .!= [])

instance FromJSON AInteractions where
    parseJSON = withObject "AInteractions" (\o -> AInteractions
        <$> o .:? "entity" .!= []
        <*> o .:? "item"   .!= [])

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
    | AOStartQuest String
    | AOAdvanceQuest String
    | AOCompleteQuest String
    | AOEquipItem String
    | AORoomTransition String
    | AOMoveNPC String String          -- ^ npc id, target room (move_npc + to)
    | AODamageNPC String Int           -- ^ damage_npc: { npc: id, amount: N } (Phase 7g)
    | AOGameEnd String (Maybe String)  -- ^ reason (victory/death/custom), optional msg
    | AOConditional E.Predicate [AActionOutcome] [AActionOutcome]  -- ^ if/then/else
    | AOSetVar String Int              -- ^ set a declared numeric variable
    | AOAddVar String Int              -- ^ add a delta to a declared numeric variable
    | AONarrative [String] [AActionOutcome]  -- ^ narrative: [...] + optional `then:` follow-ups
    | AOStandingAdd String Int         -- ^ standing: {faction: X, add: N} (Phase 7a)
    | AOStandingSet String Int         -- ^ standing: {faction: X, set: N} (Phase 7a)
    | AOSetEntityState String String   -- ^ set_state: <entity>, to: <state> (Phase 7a)
    -- Rogue Phase 3: dynamic exits at runtime.
    | AOSetExit String String String (Maybe String)
        -- ^ set_exit: { from: hall, dir: north, to: chamber, locked_by: seal }
    | AORemoveExit String String       -- ^ remove_exit: { from: hall, dir: south }
    -- P1-17: effects that previously had no YAML form at all.
    | AOApplyCondition String Int [AActionOutcome] [AActionOutcome]
        -- ^ condition: {name, turns, tick, end} — timed status effect
    | AOClearCondition String          -- ^ clear_condition: <name>
    | AOModifySkill String Int         -- ^ skill: {name, delta}
    | AORandomChoice [(Int, [AActionOutcome])]  -- ^ random: [[weight, [outcomes]], ...]
    | AORaiseEvent String              -- ^ raise: <name> — fires `on: custom <name>` (P1-20)
    | AOPlayClip String                -- ^ play_clip: <clip-id> — queues a cutscene (Phase H/H4)
    | AOComputeVar String E.Expr       -- ^ compute_var: { var: name, expr: "..." }
    -- Schritt 2 / Phase 2D: Card game outcomes
    | AODrawCards Int
    | AODiscardHand
    | AODiscardCard String
    | AOExhaustCard String
    | AOAddCardToDeck String String
    | AOShuffleDeck
    -- Schritt 3 / Phase 3E: Dynamic room generation
    | AOGenerateRoom String String String String String String
      -- ^ id, name, desc, connect_from, direction, return_direction
    deriving (Show, Eq, Generic)

-- Parse an outcome from an object with a single recognized key
instance FromJSON AActionOutcome where
    parseJSON (String s)
        | s == "discard_hand" = pure AODiscardHand
        | s == "shuffle_deck" = pure AOShuffleDeck
        | otherwise           = pure (AOMessage (T.unpack s))
    parseJSON v = withObject "AActionOutcome" (\o ->
            -- NOTE: game_end must be tried before msg: an object may carry both
            -- "game_end" and a "msg" for the end screen.
            (AOGameEnd <$> o .: "game_end" <*> o .:? "msg")
        <|> (AOConditional <$> o .: "if" <*> o .:? "then" .!= [] <*> o .:? "else" .!= [])
        -- P1-17: effects that previously had no YAML form. These must stay
        -- BEFORE the broad `msg` branch below — an object may carry a `msg`
        -- sibling (cf. game_end at the top).
        <|> (AONarrative <$> o .: "narrative" <*> o .:? "then" .!= [])
        <|> (do cond <- o .: "condition"
                AOApplyCondition <$> cond .: "name" <*> cond .: "turns"
                                 <*> cond .:? "tick" .!= [] <*> cond .:? "end" .!= [])
        <|> (AOClearCondition <$> o .: "clear_condition")
        <|> (do sk <- o .: "skill"
                AOModifySkill <$> sk .: "name" <*> sk .: "delta")
        <|> (AORandomChoice <$> o .: "random")
        <|> (AORaiseEvent <$> o .: "raise")
        <|> (AOPlayClip <$> o .: "play_clip")
        <|> (do cv <- o .: "compute_var"
                AOComputeVar <$> cv .: "var" <*> cv .: "expr")
        <|> (AOComputeVar <$> o .: "compute_var" <*> o .: "expr")
        <|> (do c <- o .: "compute"
                AOComputeVar <$> c .: "var" <*> c .: "expr")
        <|> (do varVal <- o .: "set_var"
                case varVal of
                    Object obj -> AOSetVar <$> (obj .: "var" <|> obj .: "variable") <*> obj .: "value"
                    String s   -> AOSetVar (T.unpack s) <$> o .: "value"
                    _          -> fail "set_var must be string or object")
        <|> (do varVal <- o .: "add_var"
                case varVal of
                    Object obj -> AOAddVar <$> (obj .: "var" <|> obj .: "variable") <*> obj .: "delta"
                    String s   -> AOAddVar (T.unpack s) <$> o .: "delta"
                    _          -> fail "add_var must be string or object")
        <|> (AODrawCards <$> o .: "draw_cards")
        <|> (AODrawCards <$> o .: "draw")
        <|> (do b <- o .: "discard_hand"
                if b then pure AODiscardHand else fail "discard_hand must be true")
        <|> (AODiscardCard <$> o .: "discard_card")
        <|> (AODiscardCard <$> o .: "discard")
        <|> (AOExhaustCard <$> o .: "exhaust_card")
        <|> (AOExhaustCard <$> o .: "exhaust")
        <|> (do ac <- o .: "add_card"
                mTo <- ac .:? "to"
                mDest <- ac .:? "destination"
                let dest = case mTo of
                        Just d  -> d
                        Nothing -> fromMaybe "draw" mDest
                AOAddCardToDeck <$> ac .: "card" <*> pure dest)
        <|> (do b <- o .: "shuffle_deck"
                if b then pure AOShuffleDeck else fail "shuffle_deck must be true")
        -- Schritt 3 / Phase 3E: dynamic room generation
        <|> (do grVal <- o .: "generate_room"
                case grVal of
                    Object gr -> do
                        rId <- gr .: "id"
                        rName <- gr .:? "name" .!= ""
                        rDesc <- gr .:? "description" >>= maybe (gr .:? "desc" >>= maybe (gr .:? "template" .!= "") pure) pure
                        rFrom <- gr .:? "connect_from" >>= maybe (gr .:? "from" .!= "current_room") pure
                        rDir <- gr .:? "direction" >>= maybe (gr .:? "dir" >>= maybe (gr .:? "to_dir" .!= "north") pure) pure
                        rRetDir <- gr .:? "return_direction" >>= maybe (gr .:? "return_dir" >>= maybe (gr .:? "ret_dir" .!= "") pure) pure
                        pure (AOGenerateRoom rId rName rDesc rFrom rDir rRetDir)
                    String s -> pure (AOGenerateRoom (T.unpack s) "" "" "current_room" "north" "")
                    _ -> fail "generate_room must be an object or string")
        <|> (AOMessage <$> o .: "msg")
        <|> (AOMessage <$> o .: "text")
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
        -- Phase 7g: damage an NPC (companion death via trap, scripted harm)
        <|> (do dn <- o .: "damage_npc"
                AODamageNPC <$> dn .: "npc" <*> dn .: "amount")
        -- Phase 2D: card damage helpers
        <|> (do dmgVal <- o .: "damage_enemy"
                case dmgVal of
                    Object de -> do
                        tgt <- de .:? "target" .!= "chosen"
                        amt <- de .: "amount"
                        pure (AODamageNPC tgt amt)
                    Number n -> pure (AODamageNPC "chosen" (truncate n))
                    _ -> fail "damage_enemy must be object or number")
        <|> (do dmgVal <- o .: "damage_all_enemies"
                case dmgVal of
                    Object de -> do
                        amt <- de .: "amount"
                        pure (AODamageNPC "all" amt)
                    Number n -> pure (AODamageNPC "all" (truncate n))
                    _ -> fail "damage_all_enemies must be object or number")
        -- Phase 7a: standing sugar + set_state
        <|> (do st <- o .: "standing"
                fid <- st .: "faction"
                (   (AOStandingAdd fid <$> st .: "add")
                 <|> (AOStandingSet fid <$> st .: "set") ))
        <|> (AOSetEntityState <$> o .: "set_state" <*> o .: "to")
        -- Rogue Phase 3: set_exit / remove_exit must be tried before the broad
        -- `msg` fallback (objects may carry sibling keys).
        <|> (do se <- o .: "set_exit"
                AOSetExit <$> se .: "from" <*> se .: "dir"
                          <*> se .: "to"  <*> se .:? "locked_by")
        <|> (do re <- o .: "remove_exit"
                AORemoveExit <$> re .: "from" <*> re .: "dir")
        <|> fail "Unknown outcome type. Use one of: msg, heal, damage, give, consume, set_flag, start_quest, etc."
        ) v

-- ---------------------------------------------------------------------------
-- Known YAML Keys per Entity Type (Compiler-Härtung)
-- ---------------------------------------------------------------------------

-- | Entity types in the YAML adventure schema that have known key specifications.
data EntityType
    = EntAdventure
    | EntRoom
    | EntExitRef
    | EntItem
    | EntNPC
    | EntQuest
    | EntQuestStage
    | EntRule
    | EntCard
    | EntSandboxZone
    | EntBiomeTemplate
    | EntVehicle
    | EntVariable
    | EntVerb
    | EntFaction
    | EntEncounterTable
    | EntAbility
    | EntClip
    | EntPlayer
    | EntGame
    | EntEnvironment
    | EntStealth
    | EntPatrol
    | EntCombat
    | EntCombatScreen
    | EntInteractions
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Single source of truth for allowed YAML mapping keys per entity type,
-- derived directly from the 'FromJSON' instances.
knownKeys :: EntityType -> Set.Set String
knownKeys EntAdventure = Set.fromList
    [ "name", "start_room", "rooms", "items", "npcs", "quests", "vehicles"
    , "interactions", "verbs", "variables", "rules", "player"
    , "initial_variables", "initial_flags", "active_quests", "factions"
    , "encounter_tables", "environment", "stealth", "patrol", "combat"
    , "abilities", "end_art", "title_art", "clips", "game", "cards", "deck"
    , "handLimit", "hand_limit", "sandbox_zones"
    ]
knownKeys EntRoom = Set.fromList
    [ "id", "name", "desc", "description", "exits", "tags", "light_flag"
    , "on_enter", "on_look", "on_exit", "search", "ascii", "intro", "floor"
    ]
knownKeys EntExitRef = Set.fromList
    [ "to", "locked_by" ]
knownKeys EntItem = Set.fromList
    [ "id", "name", "desc", "description", "ascii", "keys", "tags"
    , "location", "state", "slot", "effects", "hidden", "discover"
    , "props", "on_take", "verb_map", "portable", "take_failure", "in_container"
    ]
knownKeys EntNPC = Set.fromList
    [ "id", "name", "desc", "description", "ascii", "keys", "location"
    , "state", "max_hp", "attack", "defense", "dialogue", "verb_map", "party"
    ]
knownKeys EntQuest = Set.fromList
    [ "id", "name", "desc", "prereqs", "stages", "reward" ]
knownKeys EntQuestStage = Set.fromList
    [ "id", "desc", "hint" ]
knownKeys EntRule = Set.fromList
    [ "id", "on", "when", "effects", "once", "cooldown" ]
knownKeys EntCard = Set.fromList
    [ "id", "name", "cost", "type", "target", "description", "desc"
    , "exhaust", "outcomes", "effects"
    ]
knownKeys EntSandboxZone = Set.fromList
    [ "id", "origin", "floor", "biomes" ]
knownKeys EntBiomeTemplate = Set.fromList
    [ "id", "weight", "name_pattern", "description", "desc", "tags"
    , "ascii_art", "ascii", "passable_dirs"
    ]
knownKeys EntVehicle = Set.fromList
    [ "id", "name", "desc", "type", "interior", "entry_room", "cockpit"
    , "stops", "keys", "fuel", "conditions", "start_stop", "systems", "stations"
    ]
knownKeys EntVariable = Set.fromList
    [ "name", "type", "initial", "min", "max" ]
knownKeys EntVerb = Set.fromList
    [ "name", "aliases" ]
knownKeys EntFaction = Set.fromList
    [ "id", "name", "initial", "levels" ]
knownKeys EntEncounterTable = Set.fromList
    [ "id", "on", "when", "cooldown", "entries" ]
knownKeys EntAbility = Set.fromList
    [ "id", "name", "cost_var", "cost", "cooldown", "effects" ]
knownKeys EntClip = Set.fromList
    [ "id", "file", "frames", "fps" ]
knownKeys EntPlayer = Set.fromList
    [ "max_hp", "attack", "defense", "skills", "deck", "handLimit", "hand_limit" ]
knownKeys EntGame = Set.fromList
    [ "permadeath", "allow_undo", "ironman", "save_zones", "meta_slug" ]
knownKeys EntEnvironment = Set.fromList
    [ "weather", "drains" ]
knownKeys EntStealth = Set.fromList
    [ "noise", "observers" ]
knownKeys EntPatrol = Set.fromList
    [ "hostiles" ]
knownKeys EntCombat = Set.fromList
    [ "profile", "attack_refused", "difficulty", "on_win", "on_lose"
    , "initiative", "flee_allowed", "max_rounds", "speed_attribute", "screen"
    ]
knownKeys EntCombatScreen = Set.fromList
    [ "art", "bar_width", "scene", "footer" ]
knownKeys EntInteractions = Set.fromList
    [ "entity", "item" ]