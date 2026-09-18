{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Core data types for the text adventure engine
module Types where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Word (Word64)
import GHC.Generics (Generic)
import Data.Aeson
import Data.Aeson.Types (Parser, toJSONKeyText)
import Control.Applicative ((<|>))
import Data.Char (toLower)

-- ---------------------------------------------------------------------------
-- ID aliases
--
-- All IDs, descriptions and messages are `String`, not `Text`. Review P2-24
-- raised this ("`computeWorldChecksum` and `resolveCondText` pay for it") and
-- asked for a conscious decision rather than a silent omission:
--
--   * Every value here crosses the `world.json` boundary and the YAML authoring
--     schema. `String` keeps `Aeson`/`HsYAML` round-trips direct, and the
--     character-level work (`computeWorldChecksum`, `resolveCondText`) is
--     measured at ~12 ns per command at TheFog scale.
--   * Migrating to `Text` touches every module and every type alias at once —
--     a cross-cutting change with no user-visible effect and a real risk of
--     half-migrated APIs.
--
-- Decision (Cluster E): keep `String`. Revisit only if a profiling run shows
-- text handling dominating, which the measurement above does not.
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
-- | Combined result of executing a command
type CommandResult = (GameState, String)

-- ---------------------------------------------------------------------------
-- Variable system (Phase 3b): deklarierte Variablen für erweiterte Profile
-- ---------------------------------------------------------------------------

-- | Variable type: determines valid values and validation.
data VariableType
    = VTBool                     -- ^ true/false (stored as Int 0/1)
    | VTInt (Maybe Int) (Maybe Int)  -- ^ integer range (Nothing = unbounded)
    | VTText                     -- ^ arbitrary string
    | VTEnum [String]            -- ^ one of the listed values
    deriving (Show, Eq, Generic)

instance ToJSON VariableType
instance FromJSON VariableType

-- | A runtime variable value.
data VariableValue
    = VVBool Bool
    | VVInt  Int
    | VVText String
    deriving (Show, Eq, Generic)

instance ToJSON VariableValue
instance FromJSON VariableValue

-- | Static definition of an adventure-declared variable.
data VarDef = VarDef
    { vdVarName    :: String
    , vdVarType    :: VariableType
    , vdVarInitial :: VariableValue
    } deriving (Show, Eq, Generic)

instance ToJSON VarDef
instance FromJSON VarDef

-- | Basic enumerations
-- ---------------------------------------------------------------------------

-- | Direction enumeration for movement
data Direction = North | South | East | West | Up | Down
               | Northeast | Northwest | Southeast | Southwest
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

-- | Verb for dynamic actions. Core verbs are built in; adventures may
--   declare additional verbs (e.g. cast, hack, dock) via the world's verb
--   registry (Phase 3a). VUnknown is a parse fallback, never authored.
data Verb = VGo | VLook | VLookAt | VTake | VDrop | VInventory | VUse | VUseOn
          | VTalk | VAttack | VSearch | VHelp | VQuit | VUnknown
          | VCustom String
    deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON Verb
instance FromJSON Verb

-- | A declared adventure verb: canonical name plus input aliases.
--   The canonical name is what appears in verb_map keys ("cast,intact").
data VerbDef = VerbDef
    { vdName    :: String
    , vdAliases :: [String]
    } deriving (Show, Eq, Generic)

instance ToJSON VerbDef
instance FromJSON VerbDef

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

-- | Comparison operator for predicates
data Comparator = CEq | CNeq | CLt | CLte | CGt | CGte
    deriving (Show, Eq, Generic)

instance ToJSON Comparator
instance FromJSON Comparator

-- | Reference to a value that can be compared in a predicate or modified in an effect.
data ValueRef
    = VRFlag    FlagID                -- ^ flag value as string
    | VRVariable String                -- ^ adventure variable name (float/int)
    | VRItemProp ItemID String         -- ^ (item name, property name)
    | VRProperty String String         -- ^ (entity name, property name)
    | VRPlayerHealth                   -- ^ player hit points
    deriving (Show, Eq, Generic)

instance ToJSON ValueRef
instance FromJSON ValueRef

-- | Predicate: composable condition language for the generic rule system (Phase 3c).
--   A single outcome `Conditional Predicate a a` replaces CheckFlag, HasCondition,
--   CheckSkill, and any future bespoke check.
data Predicate
    = PTrue
    | PNot Predicate
    | PAll  [Predicate]       -- ^ logical AND
    | PAny [Predicate]       -- ^ logical OR
    | Compare ValueRef Comparator ValueRef   -- ^ compare two values/constants
    | PlayerHas ItemID                       -- ^ does the player carry this item?
    | EntityHasState String String           -- ^ entity, expected state (e.g. "wolf", "dead")
    | HasFlag FlagID                         -- ^ flag == "true"
    | RoomHasTag RoomID String               -- ^ room has a given tag
    | Location String String   -- ^ entity ID, room ID (is entity in this room?)
    | CompareVar String Comparator Int  -- ^ variable vs integer literal (mana >= 5)
    deriving (Show, Eq, Generic)

-- | Serialize to the same compact object shape that FromJSON accepts
--   (mirrors the YAML shorthand so saved worlds round-trip cleanly).
instance ToJSON Predicate where
    toJSON p = case p of
        PTrue              -> object [ "true"     .= True ]
        PNot q             -> object [ "not"      .= q ]
        PAll qs            -> object [ "all"      .= qs ]
        PAny qs            -> object [ "any"      .= qs ]
        Compare l op r     -> object [ "lhs" .= l, "op" .= op, "rhs" .= r ]
        PlayerHas i        -> object [ "has_item" .= i ]
        EntityHasState e s -> object [ "state"    .= e, "is" .= s ]
        HasFlag f          -> object [ "has_flag" .= f ]
        RoomHasTag r t     -> object [ "room"     .= r, "has_tag" .= t ]
        Location e r       -> object [ "at"       .= e, "room" .= r ]
        CompareVar n op v  -> object [ "compare_var" .= object
                                        [ "name" .= n, "op" .= comparatorName op, "value" .= v ] ]

-- | Stable string form of a comparator, used in YAML/JSON predicates.
comparatorName :: Comparator -> String
comparatorName CEq  = "eq"
comparatorName CNeq = "ne"
comparatorName CLt  = "lt"
comparatorName CLte = "lte"
comparatorName CGt  = "gt"
comparatorName CGte = "gte"

-- | Parse a comparator from its string form.
parseComparatorName :: String -> Maybe Comparator
parseComparatorName s = case map toLower s of
    "eq"  -> Just CEq
    "="   -> Just CEq
    "ne"  -> Just CNeq
    "!="  -> Just CNeq
    "lt"  -> Just CLt
    "<"   -> Just CLt
    "lte" -> Just CLte
    "<="  -> Just CLte
    "gt"  -> Just CGt
    ">"   -> Just CGt
    "gte" -> Just CGte
    ">="  -> Just CGte
    _     -> Nothing

instance FromJSON Predicate where
    parseJSON = withObject "Predicate" $ \o ->
            (PAll  <$> o .: "all")
        <|> (PAny  <$> o .: "any")
        <|> (PNot  <$> o .: "not")
        <|> (PTrue <$ (o .: "true" :: Parser Bool))
        <|> (PlayerHas <$> o .: "has_item")
        <|> (HasFlag   <$> o .: "has_flag")
        <|> (EntityHasState <$> o .: "state" <*> o .: "is")
        <|> (RoomHasTag     <$> o .: "room"  <*> o .: "has_tag")
        <|> (Location       <$> o .: "at"    <*> o .: "room")
        <|> (Compare <$> o .: "lhs" <*> o .: "op" <*> o .: "rhs")
        <|> (do cv  <- o .: "compare_var"
                n   <- cv .: "name"
                opS <- cv .: "op"
                v   <- cv .: "value"
                case parseComparatorName opS of
                    Just cmp -> pure (CompareVar n cmp v)
                    Nothing  -> fail ("Unknown comparator '" ++ opS ++ "' in compare_var"))
        -- Phase 7a: standing sugar -> CompareVar on the "faction.<id>" variable.
        --   Input-only alias: ToJSON stays the canonical compare_var form, so
        --   saved worlds round-trip through the existing CompareVar branch.
        <|> (do st   <- o .: "standing"
                fid  <- st .: "faction"
                let var = "faction." ++ fid
                (   (CompareVar var CGte <$> st .: "at_least")
                 <|> (CompareVar var CLte <$> st .: "at_most")
                 <|> (CompareVar var CEq  <$> st .: "equals")
                 <|> fail "standing: expected at_least, at_most, or equals" ))
        <|> fail "Unknown predicate"

-- | Action Outcome representing the result of an interaction
--   This is the engine-level Effect-DSL: a compact, composable set of
--   effect constructors that replace the previous 30-specific Effect
--   variants. The Worldbuilder compiles its higher-level shorthands into
--   these Effects.
data Effect
    = Sequence [Effect]                          -- ^ Do many effects in order
    | Conditional Predicate Effect Effect         -- ^ Branch: if (pred) then this else that
    | RandomChoice [(Int, Effect)]               -- ^ Weighted random pick from candidates
    | SetValue ValueRef EffectValue                     -- ^ Set any value (flag, variable, property)
    | ModifyValue ValueRef Int                    -- ^ Modify a numeric value (hp, skill, prop, etc.)
    | MoveEntity EntityID Location                -- ^ Move an entity to a location
    | SendMessage String                          -- ^ Show a message to the player
    | ApplyCondition String Int (Maybe Effect) (Maybe Effect) -- ^ Name, turns, tick, end effects
    | ClearCondition String                       -- ^ Remove a condition by name
    | RaiseEvent String                           -- ^ P1-20: fire `OnCustomEvent name`
    | ModifySkill SkillID Int                     -- ^ Change a skill by delta
    | QuestOp QuestOp String                      -- ^ Quest lifecycle operation
    | GameEnd GameOverReason String               -- ^ End the game with a reason
    | Narrative [String] Effect                   -- ^ Lines to show, then follow-up (stored as pendingNarrative)
    | Noop                                        -- ^ Do nothing
    deriving (Show, Eq, Generic)

-- | Quest operations for the Effect DSL
data QuestOp = StartQuest | AdvanceQuest | CompleteQuest
    deriving (Show, Eq, Generic)

instance ToJSON QuestOp
instance FromJSON QuestOp

-- | A runtime value that can be stored via SetValue.
data EffectValue
    = EVInt Int
    | EVString String
    | EVBool Bool
    deriving (Show, Eq, Generic)

instance ToJSON EffectValue
instance FromJSON EffectValue

-- | Typed location for MoveEntity.
data Location
    = InRoom RoomID
    | CarriedBy EntityID
    | InContainer EntityID
    | EquippedBy EntityID
    | Removed
    deriving (Show, Eq, Generic)

instance ToJSON Location
instance FromJSON Location

-- ---------------------------------------------------------------------------
-- Conditionally selected text (Phase 3g)
-- ---------------------------------------------------------------------------

-- | A text variant with a predicate condition: first matching variant wins.
data TextVariant = TextVariant
    { tvWhen :: Predicate
    , tvText :: String
    } deriving (Show, Eq, Generic)

instance ToJSON TextVariant
instance FromJSON TextVariant

-- | A text with conditional variants and a default fallback.
data CondText = CondText
    { ctDefault  :: String
    , ctVariants :: [TextVariant]
    } deriving (Show, Eq, Generic)

instance ToJSON CondText where
    toJSON (CondText def vars) = object
        [ "default"  .= def
        , "variants" .= vars
        ]

-- Accept either an object {default, variants} or a plain string (shorthand).
instance FromJSON CondText where
    parseJSON v = case v of
        String s -> pure (CondText (T.unpack s) [])
        _ -> withObject "CondText" (\o -> CondText
                <$> o .:  "default"
                <*> o .:? "variants" .!= []) v

-- | Smart constructor: a plain string becomes CondText with just a default.
plainText :: String -> CondText
plainText s = CondText s []

instance ToJSON Effect
instance FromJSON Effect

-- ---------------------------------------------------------------------------
-- JSON helpers for compound Map keys
-- ---------------------------------------------------------------------------

-- | Encode a Map with (Verb, String) keys as `[{verb, state, effect}, …]`.
verbStateMapToJSON :: Map.Map (Verb, String) Effect -> Value
verbStateMapToJSON m =
    toJSON [ object [ "verb" .= show v, "state" .= s, "effect" .= e ]
           | ((v, s), e) <- Map.toList m ]

verbStateMapFromJSON :: Value -> Parser (Map.Map (Verb, String) Effect)
verbStateMapFromJSON v =
    (do xs <- parseJSON v :: Parser [Value]
        Map.fromList <$> mapM entry xs)
    <|> verbStateMapFromLegacyJSON v
  where
    entry = withObject "verb-state entry" $ \o -> do
        vTxt <- o .: "verb"
        s    <- o .: "state"
        e    <- o .: "effect"
        case reads vTxt of
            [(verb, "")] -> pure ((verb, s), e)
            _            -> fail ("Bad verb encoding: " ++ vTxt)

-- | Legacy form: one string key per entry, `"VTake:intact"`. Only the first
--   `:` separates verb and state, so a state containing `:` used to be
--   corrupted (P2-9); kept for reading old `world.json` files.
verbStateMapFromLegacyJSON :: Value -> Parser (Map.Map (Verb, String) Effect)
verbStateMapFromLegacyJSON v = do
    m <- parseJSON v :: Parser (Map.Map String Effect)
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

-- | Encode a Map with (String, String) tuple keys as objects.
tupleMapToJSON :: Map.Map (String, String) (String, String) -> Value
tupleMapToJSON m =
    toJSON [ object [ "a" .= a, "b" .= b, "value" .= [v1, v2] ]
           | ((a, b), (v1, v2)) <- Map.toList m ]

tupleMapFromJSON :: Value -> Parser (Map.Map (String, String) (String, String))
tupleMapFromJSON v =
    (do xs <- parseJSON v :: Parser [Value]
        Map.fromList <$> mapM entry xs)
    <|> tupleMapFromLegacyJSON v
  where
    entry = withObject "interaction entry" $ \o -> do
        a   <- o .: "a"
        b   <- o .: "b"
        val <- o .: "value"
        case val of
            [v1, v2] -> pure ((a, b), (v1, v2))
            _        -> fail "interaction value must be a 2-element array"

-- | Legacy form: `"a|b"` string keys (P2-9).
tupleMapFromLegacyJSON :: Value -> Parser (Map.Map (String, String) (String, String))
tupleMapFromLegacyJSON v = do
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
        , itemDescription   :: CondText
        , itemKeywords      :: [String]
        , itemTags          :: Set.Set String        -- ^ e.g. "lightsource", "weapon", "key"
        , itemEquipSlot     :: Maybe EquipSlot       -- ^ Nothing = not equippable
        , itemEquipEffects  :: [EquipEffect]         -- ^ Passive bonuses while equipped
        , itemHidden        :: Bool                  -- ^ Hidden until discovered via `search`
        , itemDiscoverText  :: Maybe String          -- ^ Message shown on discovery
        , itemPortable      :: Bool                  -- ^ Can the player pick this up?
        , itemTakeFailure   :: Maybe String          -- ^ Message when take fails (non-portable)
        , itemVerbMap       :: Map.Map (Verb, String) Effect
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
        , "itemPortable"     .= itemPortable def
        , "itemTakeFailure"  .= itemTakeFailure def
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
        <*> o .:? "itemPortable"     .!= True
        <*> o .:? "itemTakeFailure"  .!= Nothing
        <*> (o .: "itemVerbMap" >>= verbStateMapFromJSON)

-- | Dynamic item state
data ItemState = ItemState
    { itemLocation    :: Location
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
    , dcVisible  :: Maybe Predicate -- ^ Optional gate: choice only shown if predicate holds
    , dcOutcome  :: Effect   -- ^ Outcome applied when chosen
    } deriving (Show, Eq, Generic)

instance ToJSON DialogueChoice where
    toJSON c = object
        [ "dcText"     .= dcText c
        , "dcNextNode" .= dcNextNode c
        , "dcVisible"  .= dcVisible c
        , "dcOutcome"  .= dcOutcome c
        ]

instance FromJSON DialogueChoice where
    parseJSON = withObject "DialogueChoice" $ \o -> DialogueChoice
        <$> o .:  "dcText"
        <*> o .:? "dcNextNode" .!= Nothing
        <*> o .:? "dcVisible"  .!= Nothing
        <*> o .:? "dcOutcome"  .!= SendMessage ""

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
    , npcDescription   :: CondText
    , npcDialogueTrees :: Map.Map String DialogueTree  -- ^ Status -> branching dialogue
    , npcKeywords      :: [String]
    , npcMaxHealth     :: Maybe Int
    , npcAttackBase    :: Int
    , npcDefenseBase   :: Int
    , npcVerbMap       :: Map.Map (Verb, String) Effect
    } deriving (Show, Eq)

instance ToJSON NPCDef where
    toJSON def = object
        [ "npcId"            .= npcId def
        , "npcName"          .= npcName def
        , "npcDescription"   .= npcDescription def
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
        <*> o .:? "npcDialogueTrees" .!= Map.empty
        <*> o .:  "npcKeywords"
        <*> o .:  "npcMaxHealth"
        <*> o .:  "npcAttackBase"
        <*> o .:  "npcDefenseBase"
        <*> (o .: "npcVerbMap" >>= verbStateMapFromJSON)

-- | Dynamic NPC state
data NPCState = NPCState
    { npcLocation  :: Location
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

-- | Persistent state of an openable/closeable container.
data ContainerState = ContainerState
    { containerOpen     :: Bool
    , containerLocked   :: Bool
    , containerCapacity :: Maybe Int
    } deriving (Show, Eq, Generic)

instance ToJSON ContainerState
instance FromJSON ContainerState

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
    , condTickOutcome :: Maybe Effect -- ^ Fired every turn while active
    , condEndOutcome  :: Maybe Effect -- ^ Fired once when it expires
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
    , questReward      :: Maybe Effect
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

-- | Fuel specification for a vehicle (P2-21).
--   Replaces a bare `(String, Int)` tuple; decoded from both the current
--   object form `{"item": …, "max": …}` and the legacy two-element array.
data FuelSpec = FuelSpec
    { fsItem :: ItemID   -- ^ item that refuels this vehicle
    , fsMax  :: Int      -- ^ tank capacity
    } deriving (Show, Eq, Generic)

instance ToJSON FuelSpec where
    toJSON fs = object [ "item" .= fsItem fs, "max" .= fsMax fs ]

instance FromJSON FuelSpec where
    parseJSON v =
        (do xs <- parseJSON v :: Parser [Value]
            case xs of
                [i, m] -> FuelSpec <$> parseJSON i <*> parseJSON m
                _      -> fail "fuel: expected [item, max] or {item, max}")
        <|> withObject "FuelSpec" (\o -> FuelSpec <$> o .: "item" <*> o .: "max") v

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
    , vehicleRoute            :: [RoomID]                       -- ^ AUTHORED stop order; empty = Map key order (backward compat)
    , vehicleKeywords         :: [String]
    , vehicleFuelProp         :: Maybe FuelSpec                -- ^ fuel item + tank capacity
    , vehicleConditionEffects :: Map.Map String Effect   -- ^ condition -> outcome fired vehicle-wide
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
        , "vehicleRoute"            .= vehicleRoute v
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
        <*> o .:? "vehicleRoute"            .!= []
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
    , roomDescription     :: CondText
    , roomConnections     :: Map.Map Direction Exit
    , roomTags            :: Set.Set String            -- ^ "dark", "safe", "vehicle", ...
    , roomLightFlag       :: Maybe FlagID              -- ^ when "true", a "dark" room is lit
    , roomOnEnter         :: Maybe Effect
    , roomOnLook          :: Maybe Effect
    , roomOnExit          :: Maybe Effect
    , roomSearchOutcome   :: Maybe Effect
    , roomAscii           :: Maybe String              -- ^ Optional ASCII art banner (Phase 4.6)
    } deriving (Show, Eq, Generic)

instance ToJSON Room where
    toJSON r = object
        [ "roomId"              .= roomId r
        , "roomName"            .= roomName r
        , "roomDescription"     .= roomDescription r
        , "roomConnections"     .= roomConnections r
        , "roomTags"            .= roomTags r
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

-- | Event type that triggers rules: what happened in the game world.
data EventType
    = OnEnter RoomID
    | OnLeave RoomID
    | OnLook RoomID
    | OnTake ItemID
    | OnDrop ItemID
    | OnUse ItemID
    | OnSearch RoomID
    | OnStateChange String
    | OnCustomEvent String
    | OnTurn
    | OnCommand String                 -- ^ verb name (e.g. "activate")
    deriving (Show, Eq, Generic)

instance ToJSON EventType
instance FromJSON EventType

-- | A trigger rule: when event matches conditions, fire effects.
data TriggerDef = TriggerDef
    { trId         :: String
    , trEvent      :: EventType
    , trCondition  :: Maybe Predicate
    , trEffects    :: [Effect]
    , trOnce       :: Bool
    , trCooldown   :: Int              -- ^ turns between re-firing (0 = no cooldown)
    } deriving (Show, Eq, Generic)

instance ToJSON TriggerDef
instance FromJSON TriggerDef

-- | Runtime state of a trigger rule.
data TriggerState = TriggerState
    { tsFired           :: Bool
    , tsCooldownRemaining :: Int
    } deriving (Show, Eq, Generic)

instance ToJSON TriggerState
instance FromJSON TriggerState

-- | Static world definition containing blueprint/map data
data GameWorld = GameWorld
    { rooms              :: Map.Map RoomID Room
    , itemDefs           :: Map.Map ItemID ItemDef
    , npcDefs            :: Map.Map NPCID NPCDef
    , entityInteractions :: Map.Map (String, String) (String, String)
    , itemInteractions   :: Map.Map (String, String) Effect  -- ^ (Item, Item) -> outcome
    , questDefs          :: Map.Map QuestID Quest                    -- ^ Static quest definitions
    , vehicleDefs        :: Map.Map VehicleID VehicleDef             -- ^ Static vehicle definitions (Phase 3)
    , verbDefs           :: Map.Map String VerbDef                   -- ^ Adventure-declared verbs (Phase 3a)
    , varDefs            :: Map.Map String VarDef                    -- ^ Adventure-declared variables (Phase 3b)
    , triggerDefs        :: [TriggerDef]                              -- ^ Trigger rules (Phase 3f)
    , combatProfile      :: CombatProfile                            -- ^ combat policy (Phase 7f)
    , worldName          :: String                                   -- ^ adventure title (`name:`), shown as the game banner
    , abilities          :: Map.Map String PlayerAbility             -- ^ Player abilities (Phase 7f-3, step A3)
    } deriving (Show, Eq)

-- | Player ability definition for tactical combat (Phase 7f-3, step A3).
data PlayerAbility = PlayerAbility
    { paId       :: String
    , paName     :: String
    , paCostVar  :: String   -- ^ variable to deduct cost from, e.g. "player.mana"
    , paCost     :: Int      -- ^ amount to deduct
    , paCooldown :: Int      -- ^ cooldown duration in turns
    , paEffects  :: [Effect] -- ^ effects executed on use
    } deriving (Show, Eq, Generic)

instance ToJSON PlayerAbility where
    toJSON pa = object
        [ "id"        .= paId pa
        , "name"      .= paName pa
        , "cost_var"  .= paCostVar pa
        , "cost"      .= paCost pa
        , "cooldown"  .= paCooldown pa
        , "effects"   .= paEffects pa
        ]

instance FromJSON PlayerAbility where
    parseJSON = withObject "PlayerAbility" $ \o -> PlayerAbility
        <$> (o .: "id" <|> o .: "paId")
        <*> (o .: "name" <|> o .: "paName")
        <*> (o .:? "cost_var" >>= maybe (o .:? "paCostVar" .!= "") pure)
        <*> (o .:? "cost" >>= maybe (o .:? "paCost" .!= 0) pure)
        <*> (o .:? "cooldown" >>= maybe (o .:? "paCooldown" .!= 0) pure)
        <*> (o .:? "effects" >>= maybe (o .:? "paEffects" .!= []) pure)

-- | Combat policy, chosen by authored data (Phase 7f). `CombatClassic` is
--   the exact pre-7f behaviour and the default when no `combat:` block is
--   present (bit-identical regression gate).
data CombatProfile
    = CombatOff (Maybe String)                 -- ^ attack refused; optional custom message
    | CombatNarrative NarrativeCombat          -- ^ opposed roll -> on_win/on_lose effects
    | CombatClassic                            -- ^ today's behaviour: attack vs defense, retaliation
    | CombatTactical TacticalCombat            -- ^ round-based: one action per round (Phase 7f-3)
    deriving (Show, Eq, Generic)

-- | Narrative combat: the player's effective attack is rolled against the
--   target's defense + a difficulty offset. No HP attrition — the on_win /
--   on_lose effects decide everything.
data NarrativeCombat = NarrativeCombat
    { ncDifficulty :: Int
    , ncOnWin      :: Effect
    , ncOnLose     :: Effect
    } deriving (Show, Eq, Generic)

-- | Tactical combat configuration (Phase 7f-3, step A2/A3).
--
--   One player action = one round. The enemy reacts via an `on: turn` rule
--   gated on `combat.engaged >= 1` — no second interpreter.
data TacticalCombat = TacticalCombat
    { tcInitiative     :: InitiativeRule  -- ^ who strikes first
    , tcFleeAllowed    :: Bool            -- ^ can the player flee?
    , tcMaxRounds      :: Int             -- ^ hard limit on rounds (safety net)
    , tcSpeedAttribute :: String          -- ^ skill/prop name for BySpeed initiative (default "speed")
    } deriving (Show, Eq, Generic)

instance ToJSON TacticalCombat where
    toJSON tc = object
        [ "initiative"      .= tcInitiative tc
        , "flee_allowed"    .= tcFleeAllowed tc
        , "max_rounds"      .= tcMaxRounds tc
        , "speed_attribute" .= tcSpeedAttribute tc
        ]

instance FromJSON TacticalCombat where
    parseJSON = withObject "TacticalCombat" $ \o -> TacticalCombat
        <$> (o .:? "initiative"      <|> o .:? "tcInitiative")      .!= PlayerFirst
        <*> (o .:? "flee_allowed"    <|> o .:? "tcFleeAllowed")     .!= True
        <*> (o .:? "max_rounds"      <|> o .:? "tcMaxRounds")       .!= 100
        <*> (o .:? "speed_attribute" <|> o .:? "tcSpeedAttribute")  .!= "speed"

-- | Initiative order within a tactical round.
--   `BySpeed` is defined for forward-compatibility (A3) but falls back to
--   `PlayerFirst` at runtime until A3 wires it.
data InitiativeRule = PlayerFirst | EnemyFirst | BySpeed
    deriving (Show, Eq, Generic)

instance ToJSON InitiativeRule
instance FromJSON InitiativeRule

-- | What the player does in one combat round.
--
--   Phase 7f-3 (`tactical`) step A2: every constructor is wired. `CAAttack`
--   deals damage; `CADefend` sets `combat.action` for the enemy trigger;
--   `CAFlee` ends the fight if allowed. `CAUseItem`/`CAAbility` are A3.
--   See `plan-7f3-tactical-7h2-shipduell.md`.
data CombatAction
    = CAAttack
    | CADefend
    | CAFlee
    | CAUseItem ItemID
    | CAAbility String
    deriving (Show, Eq, Generic)

instance ToJSON CombatAction
instance FromJSON CombatAction

instance ToJSON CombatProfile where
    toJSON (CombatOff mRefused) = object
        [ "profile" .= ("off" :: String)
        , "attack_refused" .= mRefused ]
    toJSON (CombatNarrative nc) = object
        [ "profile" .= ("narrative" :: String)
        , "difficulty" .= ncDifficulty nc
        , "on_win"  .= ncOnWin nc
        , "on_lose" .= ncOnLose nc ]
    toJSON CombatClassic = object [ "profile" .= ("classic" :: String) ]
    toJSON (CombatTactical tc) = object
        [ "profile"         .= ("tactical" :: String)
        , "initiative"      .= tcInitiative tc
        , "flee_allowed"    .= tcFleeAllowed tc
        , "max_rounds"      .= tcMaxRounds tc
        , "speed_attribute" .= tcSpeedAttribute tc ]

instance FromJSON CombatProfile where
    parseJSON v = withObject "CombatProfile" (\o -> do
        prof <- o .: "profile"
        case prof of
            "off"       -> CombatOff <$> o .:? "attack_refused"
            "narrative" -> CombatNarrative <$>
                (NarrativeCombat
                    <$> o .:? "difficulty" .!= 0
                    <*> o .:? "on_win"  .!= Noop
                    <*> o .:? "on_lose" .!= Noop)
            "classic"   -> pure CombatClassic
            "tactical"  -> CombatTactical <$>
                (TacticalCombat
                    <$> o .:? "initiative"      .!= PlayerFirst
                    <*> o .:? "flee_allowed"     .!= True
                    <*> o .:? "max_rounds"       .!= 100
                    <*> o .:? "speed_attribute"  .!= "speed")
            other       -> fail ("unknown combat profile '" ++ other ++ "'")) v

instance ToJSON GameWorld where
    toJSON gw = object
        [ "rooms"              .= rooms gw
        , "itemDefs"           .= itemDefs gw
        , "npcDefs"            .= npcDefs gw
        , "entityInteractions" .= tupleMapToJSON (entityInteractions gw)
        , "itemInteractions"   .= itemInteractionsToJSON (itemInteractions gw)
        , "questDefs"          .= questDefs gw
        , "vehicleDefs"        .= vehicleDefs gw
        , "verbDefs"           .= verbDefs gw
        , "varDefs"            .= varDefs gw
        , "triggerDefs"       .= triggerDefs gw
        , "combatProfile"      .= combatProfile gw
        , "worldName"          .= worldName gw
        , "abilities"          .= abilities gw
        ]

instance FromJSON GameWorld where
    parseJSON = withObject "GameWorld" $ \o -> GameWorld
        <$> o .:  "rooms"
        <*> o .:  "itemDefs"
        <*> o .:  "npcDefs"
        <*> (o .: "entityInteractions" >>= tupleMapFromJSON)
        <*> (o .:? "itemInteractions" >>= maybe (pure Map.empty) parseItemInteractions)
        <*> o .:? "questDefs" .!= Map.empty
        <*> o .:? "vehicleDefs" .!= Map.empty
        <*> o .:? "verbDefs" .!= Map.empty
        <*> o .:? "varDefs"  .!= Map.empty
        <*> o .:? "triggerDefs" .!= []
        <*> o .:? "combatProfile" .!= CombatClassic
        <*> o .:? "worldName" .!= ""
        <*> o .:? "abilities" .!= Map.empty

-- | Encode item-on-item outcomes as objects (P2-9).
itemInteractionsToJSON :: Map.Map (String, String) Effect -> Value
itemInteractionsToJSON m =
    toJSON [ object [ "a" .= a, "b" .= b, "effect" .= e ]
           | ((a, b), e) <- Map.toList m ]

parseItemInteractions :: Value -> Parser (Map.Map (String, String) Effect)
parseItemInteractions v =
    (do xs <- parseJSON v :: Parser [Value]
        Map.fromList <$> mapM entry xs)
    <|> legacy
  where
    entry = withObject "item interaction entry" $ \o -> do
        a <- o .: "a"
        b <- o .: "b"
        e <- o .: "effect"
        pure ((a, b), e)
    -- Legacy form: `"a|b"` string keys.
    legacy = do
        m <- parseJSON v :: Parser (Map.Map String Effect)
        case mapM parseKey (Map.toList m) of
            Right kvs -> pure (Map.fromList kvs)
            Left err  -> fail err
    parseKey (k, e) = case break (== '|') k of
        (a, '|':b) -> Right ((a, b), e)
        _          -> Left ("Bad item interaction key: " ++ k)

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
    , rngState           :: Word64                            -- ^ Explicit RNG state for deterministic random outcomes (Phase 1)
    , variables          :: Map.Map String VariableValue       -- ^ Adventure-declared variables (Phase 3b)
    , triggerStates      :: Map.Map String TriggerState      -- ^ Runtime state of trigger rules (fired/cooldown)
    } deriving (Show, Eq, Generic)

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
        , "rngState"        .= rngState ss
        , "variables"       .= variables ss
        , "triggerStates"   .= triggerStates ss
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
        <*> o .:? "rngState"        .!= 0
        <*> o .:? "variables"       .!= Map.empty
        <*> o .:? "triggerStates"   .!= Map.empty

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
    , pendingNarrative :: Maybe ([String], Effect)  -- ^ Narrative lines + follow-up (Phase 4.4)
    , diagnostics :: [String]                      -- ^ Engine-level findings for the author (P2-23)
    } deriving (Show, Eq)

-- NOTE: `GameState` deliberately has **no** JSON instance — only `SaveState`
-- and `GameWorld` are serialized. `diagnostics` is therefore runtime-only by
-- construction: nothing has to be excluded from a save file, and a loaded
-- session simply starts with an empty list. It carries engine findings that a
-- *content* error produced (currently the outcome-depth guard), which must reach
-- the author but never the player's text.

-- ---------------------------------------------------------------------------
-- Explicit deterministic RNG state (Phase 1f)
-- ---------------------------------------------------------------------------

-- | Initial seed for a fresh playthrough (golden-ratio constant, nonzero).
initialRngState :: Word64
initialRngState = 0x9E3779B97F4A7C15

-- | Advance the explicit RNG state (linear congruential generator).
nextRng :: Word64 -> Word64
nextRng w = w * 6364136223846793005 + 1
