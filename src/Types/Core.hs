{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Core data types for the text adventure engine
module Types.Core
    ( -- * ID aliases
      ItemID
    , RoomID
    , NPCID
    , EntityID
    , VehicleID
    , QuestID
    , SkillID
    , FlagID
    , FactionID
    , FactionLevel (..)
    , CardID
    , ClipID
      -- * Command results
    , CommandResultEv
    , CommandResult
      -- * Variables
    , VariableType (..)
    , VariableValue (..)
    , VarDef (..)
      -- * Basic enumerations
    , Direction (..)
    , Exit (..)
    , exitRoomID
    , Verb (..)
    , VerbPhase (..)
    , VerbDef (..)
    , directionDelta
    , oppositeDirection
    , GameOverReason (..)
      -- * Equipment
    , EquipSlot (..)
    , EquipEffect (..)
      -- * Predicates & Expressions
    , Comparator (..)
    , comparatorName
    , parseComparatorName
    , ActorRef (..)
    , actorId
    , parseActorString
    , DistanceTarget (..)
    , PursuitOptions (..)
    , defaultPursuitOptions
    , parseDistanceRef
    , CountWhat (..)
    , CountWhere (..)
    , CountSpec (..)
    , parseCountRef
    , PropRef (..)
    , ValueRef (..)
    , legacyVRProperty
    , Expr (..)
    , showExpr
    , ExprToken (..)
    , tokenizeExpr
    , parseExprTokens
    , parseExprAdditive
    , parseExprMultiplicative
    , parseExprUnary
    , parseExprPrimary
    , parseExpr
    , Predicate (..)
      -- * Audio / Music
    , MusicCommand (..)
      -- * Effects
    , Effect (..)
    , noopEffect
    , EffectValue (..)
    , Location (..)
    , QuestOp (..)
      -- * Text and Art
    , TextVariant (..)
    , CondText (..)
    , plainText
    , isEmptyCond
    , Hotspot (..)
    , AsciiArt (..)
    , Ambient (..)
    , emptyAscii
    , isEmptyAscii
    , asciiPair
      -- * JSON helpers
    , verbStateMapToJSON
    , verbStateMapFromJSON
    , verbStateMapFromLegacyJSON
    , tupleMapToJSON
    , tupleMapFromJSON
    , tupleMapFromLegacyJSON
      -- * Grammar metadata (4.3.5)
    , Grammar (..)
    , emptyGrammar
    , grammarEmpty
    , grammarJSONFields
    , grammarFromJSONFields
      -- * Items
    , ItemDef (..)
    , ItemState (..)
      -- * Dialogue
    , DialogueChoice (..)
    , DialogueNode (..)
    , DialogueTree (..)
      -- * NPCs
    , NPCDef (..)
    , NPCState (..)
    , ContainerState (..)
    , ContainerDef (..)
      -- * Player
    , Player (..)
    , Inventory
      -- * Conditions
    , Condition (..)
      -- * Quests
    , Quest (..)
    , QuestStage (..)
      -- * Rooms
    , Room (..)
    , MapPos (..)
      -- * Events and Triggers
    , EventType (..)
    , TriggerEvent
    , TriggerDef (..)
    , TriggerState (..)
    , ProcDef (..)
    , FactDef (..)
    , StatementDef (..)
    , CombineDef (..)
    , ChapterDef (..)
    , DeviceDef (..)
    , DeviceID
    , LevelDef (..)
    , ProgressionDef (..)
      -- * Procedural Sandbox & Cutscenes
    , BiomeTemplate (..)
    , SandboxZone (..)
    , Clip (..)
      -- * Game Policy & World
    , GamePolicy (..)
    , defaultGamePolicy
    , RecipeKey (..)
    , recipeId
    , recipeRequiresLearning
    , recipeResult
    , recipeIngredientsList
    , GameWorld (..)
    , itemInteractionsToJSON
    , parseItemInteractions
    , npcInteractionsToJSON
    , parseNpcInteractions
      -- * Save & Game State
    , SaveState (..)
    , exitOverridesToJSON
    , parseExitOverrides
    , SaveFile (..)
    , GameState (..)
      -- * RNG and Utilities
    , initialRngState
    , nextRng
    , slugify
    , splitMix64Mix
    , deriveCellSeed
    , sandboxRoomId
    , parseSandboxRoomId
    , isSandboxTarget
    ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Word (Word64)
import Data.Bits (shiftR, xor)
import GHC.Generics (Generic)
import Types.Output
import Data.Aeson
import Data.Aeson.Types (Parser, Pair, toJSONKeyText)
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Char (toLower, isDigit, isAlpha, isAlphaNum, isSpace)
import Data.List (intercalate, stripPrefix, foldl')
import Data.Maybe (isNothing, isJust)
import qualified Data.Foldable as Foldable
import Text.Read (readMaybe)

import Types.Cards
import Types.Vehicles
import Types.Combat

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
type CardID    = String

-- | A named standing threshold for a faction (Phase 7a / G9a).
--   At `flAt` points the relation is called `flName`.
data FactionLevel = FactionLevel
    { flAt   :: Int
    , flName :: String
    } deriving (Show, Eq, Generic)

instance ToJSON FactionLevel where
    toJSON (FactionLevel atVal nameVal) = object
        [ "at"   .= atVal
        , "name" .= nameVal
        ]

instance FromJSON FactionLevel where
    parseJSON = withObject "FactionLevel" $ \o -> FactionLevel
        <$> o .: "at"
        <*> o .: "name"

-- ---------------------------------------------------------------------------
-- | Combined result of executing a command
--
--   Two forms since Phase 1.2: 'CommandResultEv' is the primary, structured
--   form (an ordered 'OutputEvent' stream — catalog messages keep their key,
--   art travels with hotspot payloads, audio and state changes ride along).
--   'CommandResult' is the compatibility form: the same stream rendered back
--   to the flat CLI text ('Output.renderEvents'), byte-identical to the
--   pre-1.2 string pipeline. The test suite and the aux paths of the game
--   loop consume the flat form; the protocol (1.4) and the WebUI consume the
--   event form.
type CommandResultEv = (GameState, [OutputEvent])
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
    , vdOnOverflow :: [Effect]
    } deriving (Show, Eq, Generic)

-- | K7+K4: Hand-written ToJSON/FromJSON preserving the byte contract.
-- When 'vdOnOverflow' is empty (all existing adventures), the field is omitted,
-- keeping world.json encoding 100% byte-identical.
instance ToJSON VarDef where
    toJSON vd = object $
        [ "vdVarInitial" .= vdVarInitial vd
        , "vdVarName"    .= vdVarName vd
        , "vdVarType"    .= vdVarType vd
        ] ++ [ "vdOnOverflow" .= vdOnOverflow vd | not (null (vdOnOverflow vd)) ]

instance FromJSON VarDef where
    parseJSON = withObject "VarDef" $ \o -> VarDef
        <$> o .:  "vdVarName"
        <*> o .:  "vdVarType"
        <*> o .:  "vdVarInitial"
        <*> o .:? "vdOnOverflow" .!= []

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


-- | Verb for dynamic actions. Core verbs are built in; adventures may
--   declare additional verbs (e.g. cast, hack, dock) via the world's verb
--   registry (Phase 3a). VUnknown is a parse fallback, never authored.
data Verb = VGo | VLook | VLookAt | VTake | VDrop | VInventory | VUse | VUseOn
          | VTalk | VAttack | VSearch | VHelp | VQuit | VUnknown
          | VCustom String
    deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON Verb
instance FromJSON Verb

-- | Phase 4.2 (Veto Stufe 2): when a verb_map entry runs relative to the
--   standard action. 'PhaseAfter' is the historical behaviour (entry without
--   a phase prefix): on `take` the entry runs in addition to the standard
--   pickup, on every other verb it replaces the standard action.
--   'PhaseBefore' runs before the standard action and may veto it via a
--   `block:` effect; 'PhaseInstead' replaces the standard action outright.
--   The standard guards (`take.already`, `take.not_portable`,
--   `inventory.full`) are checked before any phase and gate all of them —
--   they describe state, not the action.
data VerbPhase = PhaseAfter | PhaseBefore | PhaseInstead
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

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
instance FromJSON Comparator where
    parseJSON (String s) = case parseComparatorName (T.unpack s) of
        Just c  -> pure c
        Nothing -> fail ("Unknown comparator: " ++ T.unpack s)
    parseJSON v = genericParseJSON defaultOptions v

-- | Reference to an entity/actor whose property is inspected or modified.
data ActorRef
    = ActorPlayer
    | ActorNPC NPCID
    | ActorShip VehicleID
    | ActorRoom RoomID
    | ActorEntity EntityID
    deriving (Show, Eq, Generic)

instance ToJSON ActorRef

instance FromJSON ActorRef where
    parseJSON (String s) = pure (parseActorString (T.unpack s))
    parseJSON v = genericParseJSON defaultOptions v

-- | Canonical string ID for an actor reference.
actorId :: ActorRef -> String
actorId ActorPlayer       = "player"
actorId (ActorNPC nId)    = nId
actorId (ActorShip vId)   = "ship:" ++ vId
actorId (ActorRoom rId)   = rId
actorId (ActorEntity eId) = eId

-- | Reference to a specific property of an actor or entity.
data PropRef
    = PHealth
    | PRoom
    | PVisited
    | PState
    | PCustom String
    deriving (Show, Eq, Generic)

instance ToJSON PropRef
instance FromJSON PropRef

-- | Reference to a value that can be compared in a predicate or modified in an effect.
-- | Pursuit (Tür IV): the target of a distance query / pursuit step — an
--   actor (resolved to its current room) or a fixed room.
data DistanceTarget
    = DTActor ActorRef
    | DTRoom RoomID
    deriving (Show, Eq, Generic)

instance ToJSON DistanceTarget where
    toJSON (DTActor a) = object [ "actor" .= a ]
    toJSON (DTRoom r)  = object [ "room"  .= r ]

instance FromJSON DistanceTarget where
    parseJSON (String s)
        | Just r <- stripPrefix "room:" str = pure (DTRoom r)
        | otherwise                        = pure (DTActor (parseActorString str))
      where
        str = T.unpack s
    parseJSON (Object o) =
        (DTActor <$> o .: "actor") <|> (DTRoom <$> o .: "room")
    parseJSON _ = fail "Expected object or string for DistanceTarget"

-- | Which exits a seeker may pass (pursuit, Tür IV). The default
--   ("wie der Spieler") passes exactly those edges the player could walk:
--   open exits, unlocked doors and guards whose predicate holds. Lives here
--   (not in 'Pursuit') so 'Effect' can carry it without an import cycle.
data PursuitOptions = PursuitOptions
    { poIgnoreLocked  :: Bool  -- ^ pass locked exits as if they were open
    , poIgnoreGuarded :: Bool  -- ^ pass guarded exits regardless of the guard
    } deriving (Show, Eq, Generic)

instance ToJSON PursuitOptions
instance FromJSON PursuitOptions

-- | Default seeker behaviour: exactly like the player.
defaultPursuitOptions :: PursuitOptions
defaultPursuitOptions = PursuitOptions False False

-- | B2: what a count query counts.
data CountWhat
    = CountItems        -- ^ items (optionally filtered by tag)
    | CountNpcs         -- ^ all NPCs
    | CountAliveNpcs    -- ^ living NPCs only
    deriving (Show, Eq, Generic)

-- | B2: where a count query looks.
data CountWhere
    = CountInRoom RoomID
    | CountCarriedBy ActorRef
    deriving (Show, Eq, Generic)

instance ToJSON CountWhere where
    toJSON (CountInRoom r)    = object [ "in" .= r ]
    toJSON (CountCarriedBy a) = object [ "by" .= actorId a ]

instance FromJSON CountWhere where
    parseJSON = withObject "CountWhere" $ \o ->
            (CountInRoom <$> o .: "in")
        <|> (CountCarriedBy . parseActorString <$> o .: "by")

-- | B2: a general count query — `count.<what>.<in|by>.<id>[.<tag>]` in the
--   string form, `{count: {what: …, in|by: …, tag: …}}` as an object.
data CountSpec = CountSpec
    { csWhat :: CountWhat
    , csWhere :: CountWhere
    , csTag   :: Maybe String  -- ^ optional: only items carrying this tag
    } deriving (Show, Eq, Generic)

instance ToJSON CountSpec where
    toJSON cs = object $
        [ "what" .= whatName (csWhat cs), whereKey (csWhere cs) ]
        ++ maybe [] (\t -> ["tag" .= t]) (csTag cs)
      where
        whatName CountItems     = ("items" :: String)
        whatName CountNpcs      = "npcs"
        whatName CountAliveNpcs = "alive_npcs"
        whereKey (CountInRoom r)    = ("in" :: Key) .= r
        whereKey (CountCarriedBy a) = ("by" :: Key) .= actorId a

instance FromJSON CountSpec where
    parseJSON = withObject "CountSpec" $ \o -> do
        whatStr <- o .: "what"
        what <- case whatStr :: String of
            "items"      -> pure CountItems
            "npcs"       -> pure CountNpcs
            "alive_npcs" -> pure CountAliveNpcs
            other        -> fail ("count: unknown what '" ++ other
                                  ++ "' (use items, npcs or alive_npcs)")
        wherePart <- (CountInRoom <$> o .: "in")
                  <|> (CountCarriedBy . parseActorString <$> o .: "by")
        tag <- o .:? "tag"
        pure (CountSpec what wherePart tag)

-- | String convention for actor references: "player", "ship:<id>" or an
--   NPC id (the canonical parse used by distance/step sugar and the YAML
--   string forms).
parseActorString :: String -> ActorRef
parseActorString "player" = ActorPlayer
parseActorString s = case stripPrefix "ship:" s of
    Just v  -> ActorShip v
    Nothing -> ActorNPC s

data ValueRef
    = VRFlag    FlagID                -- ^ flag value as string
    | VRVariable String                -- ^ adventure variable name (float/int)
    | VRItemProp ItemID String         -- ^ (item name, property name)
    | VRActorProp ActorRef PropRef      -- ^ (actor, property) typed reference
    | VRPlayerHealth                   -- ^ player hit points
    | VRConditionTurns String          -- ^ remaining turns of an active condition/timer (Phase 2.1)
    | VRDistance ActorRef DistanceTarget -- ^ pursuit (Tür IV): hop distance, -1 = unreachable
    | VRCount CountSpec                 -- ^ B2: count query (items/npcs in a room or carried)
    | VRStandingName FactionID         -- ^ standing level name of a faction (Phase G9a)
    deriving (Show, Eq, Generic)

instance ToJSON ValueRef where
    toJSON (VRFlag f)           = object [ "tag" .= ("VRFlag" :: T.Text), "contents" .= f ]
    toJSON (VRVariable v)       = object [ "tag" .= ("VRVariable" :: T.Text), "contents" .= v ]
    toJSON (VRItemProp i p)     = object [ "tag" .= ("VRItemProp" :: T.Text), "contents" .= [i, p] ]
    toJSON (VRActorProp a p)    = object [ "tag" .= ("VRActorProp" :: T.Text), "contents" .= [toJSON a, toJSON p] ]
    toJSON VRPlayerHealth       = object [ "tag" .= ("VRPlayerHealth" :: T.Text) ]
    toJSON (VRConditionTurns c) = object [ "tag" .= ("VRConditionTurns" :: T.Text), "contents" .= c ]
    toJSON (VRDistance a t)     = object [ "tag" .= ("VRDistance" :: T.Text), "contents" .= [toJSON a, toJSON t] ]
    toJSON (VRCount cs)         = object [ "tag" .= ("VRCount" :: T.Text), "contents" .= cs ]
    toJSON (VRStandingName f)   = object [ "tag" .= ("VRStandingName" :: T.Text), "contents" .= f ]

instance FromJSON ValueRef where
    parseJSON (Number n) = pure (VRVariable (show (round n :: Int)))
    parseJSON (String s)
        | Just rest <- stripPrefix "condition_turns." str = pure (VRConditionTurns rest)
        | Just rest <- stripPrefix "distance." str         = pure (parseDistanceRef rest)
        | Just rest <- stripPrefix "count." str            = pure (parseCountRef rest)
        | Just rest <- stripPrefix "standing_name." str    = pure (VRStandingName rest)
        | Just rest <- stripPrefix "standing_name:" str    = pure (VRStandingName (dropWhile isSpace rest))
        | Just n <- (readMaybe str :: Maybe Int)           = pure (VRVariable (show n))
        | otherwise                                        = pure (VRVariable str)
      where
        str = T.unpack s
    parseJSON (Object o) =
        (do tag <- o .: "tag" :: Parser T.Text
            case tag of
                "VRFlag"           -> VRFlag <$> o .: "contents"
                "VRVariable"       -> VRVariable <$> o .: "contents"
                "VRItemProp"       -> do
                    contents <- o .: "contents"
                    case contents of
                        [i, p] -> pure (VRItemProp i p)
                        _      -> fail "VRItemProp: expected [itemId, prop]"
                "VRActorProp"      -> do
                    contents <- o .: "contents"
                    case contents of
                        [aVal, pVal] -> VRActorProp <$> parseJSON aVal <*> parseJSON pVal
                        _            -> fail "VRActorProp: expected [actor, prop]"
                "VRPlayerHealth"   -> pure VRPlayerHealth
                "VRConditionTurns" -> VRConditionTurns <$> o .: "contents"
                "VRDistance"       -> do
                    contents <- o .: "contents"
                    case contents of
                        [aVal, tVal] -> VRDistance <$> parseJSON aVal <*> parseJSON tVal
                        _            -> fail "VRDistance: expected [seeker, target]"
                "VRCount"          -> VRCount <$> o .: "contents"
                "VRStandingName"   -> VRStandingName <$> o .: "contents"
                "VRProperty"       -> do
                    contents <- o .: "contents"
                    case contents of
                        (targetStr : propStr : _) -> pure (legacyVRProperty targetStr propStr)
                        _                         -> fail "VRProperty: expected [target, prop]"
                _                  -> fail ("Unknown ValueRef tag: " ++ T.unpack tag))
        <|> (do d <- o .: "distance"
                case d of
                    [aVal, tVal] -> VRDistance <$> parseJSON aVal <*> parseJSON tVal
                    _            -> fail "distance: expected [seeker, target]")
        <|> (VRCount <$> o .: "count")
        <|> (VRConditionTurns <$> o .: "condition_turns")
        <|> (VRFlag <$> o .: "flag")
        <|> (VRVariable <$> o .: "var")
        <|> (VRStandingName <$> o .: "standing_name")
        <|> (pure VRPlayerHealth <* (guard =<< (o .: "player_health" <|> o .: "player_hp")))
    parseJSON _ = fail "Expected object, number or string for ValueRef"

-- | Map legacy stringly-typed `VRProperty target prop` into `VRActorProp`
legacyVRProperty :: String -> String -> ValueRef
legacyVRProperty "player" "room"    = VRActorProp ActorPlayer PRoom
legacyVRProperty "player" "hp"      = VRActorProp ActorPlayer PHealth
legacyVRProperty target   "visited" = VRActorProp (ActorRoom target) PVisited
legacyVRProperty target   "state"   = VRActorProp (ActorEntity target) PState
legacyVRProperty target   "hp"      = VRActorProp (ActorNPC target) PHealth
legacyVRProperty "player" prop      = VRActorProp ActorPlayer (PCustom prop)
legacyVRProperty target   prop      = VRActorProp (ActorNPC target) (PCustom prop)

-- | String form `distance.<seeker>.<target>` where <target> is an actor id
--   or `room.<roomId>`. Pursuit (Tür IV) — see "plan-pursuit-suche.md".
parseDistanceRef :: String -> ValueRef
parseDistanceRef rest = VRDistance (parseActorString seeker) target
  where
    (seeker, targetRest) = splitOnce '.' rest
    target = case stripPrefix "room." targetRest of
        Just r  -> DTRoom r
        Nothing -> DTActor (parseActorString targetRest)

-- | Split at the first occurrence of a character (second part without it).
splitOnce :: Char -> String -> (String, String)
splitOnce c s = case break (== c) s of
    (a, _:b) -> (a, b)
    (a, _)   -> (a, "")

-- | String form `count.<what>.<in|by>.<id>` and
--   `count.<what>.tag.<tag>.<in|by>.<id>` (B2) — e.g. "items.in.halle",
--   "items.tag.light.in.halle", "alive_npcs.in.halle", "items.by.player".
parseCountRef :: String -> ValueRef
parseCountRef rest = VRCount (CountSpec what wherePart tag)
  where
    (whatStr, afterWhat) = splitOnce '.' rest
    what = case whatStr of
        "npcs"       -> CountNpcs
        "alive_npcs" -> CountAliveNpcs
        _            -> CountItems
    (tag, afterTag) = case stripPrefix "tag." afterWhat of
        Nothing -> (Nothing, afterWhat)
        Just t  -> let (tg, rest') = splitOnce '.' t in (Just tg, rest')
    (qual, ident) = splitOnce '.' afterTag
    wherePart = case qual of
        "by" -> CountCarriedBy (parseActorString ident)
        _    -> CountInRoom ident

-- ---------------------------------------------------------------------------
-- Arithmetic expressions for dynamic calculations (Phase 1A)
-- ---------------------------------------------------------------------------

-- | Arithmetic expression for dynamic calculations in outcomes
data Expr
    = ELit Int
    | EVar String
    | EAdd Expr Expr
    | ESub Expr Expr
    | EMul Expr Expr
    | EDiv Expr Expr
    | EMod Expr Expr
    | EMin Expr Expr
    | EMax Expr Expr
    | EClamp Expr Expr Expr
    deriving (Show, Eq, Generic)

-- | Convert an Expr into a readable, unambiguous string representation.
showExpr :: Expr -> String
showExpr (ELit n)
    | n < 0     = "(" ++ show n ++ ")"
    | otherwise = show n
showExpr (EVar v) = v
showExpr (EAdd a b) = "(" ++ showExpr a ++ " + " ++ showExpr b ++ ")"
showExpr (ESub a b) = "(" ++ showExpr a ++ " - " ++ showExpr b ++ ")"
showExpr (EMul a b) = "(" ++ showExpr a ++ " * " ++ showExpr b ++ ")"
showExpr (EDiv a b) = "(" ++ showExpr a ++ " / " ++ showExpr b ++ ")"
showExpr (EMod a b) = "(" ++ showExpr a ++ " % " ++ showExpr b ++ ")"
showExpr (EMin a b) = "min(" ++ showExpr a ++ ", " ++ showExpr b ++ ")"
showExpr (EMax a b) = "max(" ++ showExpr a ++ ", " ++ showExpr b ++ ")"
showExpr (EClamp mn mx v) = "clamp(" ++ showExpr mn ++ ", " ++ showExpr mx ++ ", " ++ showExpr v ++ ")"

instance ToJSON Expr where
    toJSON e = toJSON (showExpr e)

instance FromJSON Expr where
    parseJSON v = case v of
        String s -> case parseExpr (T.unpack s) of
            Right e  -> pure e
            Left err -> fail ("Failed to parse expression: " ++ err)
        Number n -> pure (ELit (round n))
        Object _ -> genericParseJSON defaultOptions v
        _        -> fail "Expected string expression, number, or object for Expr"

-- | Internal tokens for expression parsing
data ExprToken
    = TokNum Int
    | TokIdent String
    | TokPlus
    | TokMinus
    | TokMul
    | TokDiv
    | TokMod
    | TokLParen
    | TokRParen
    | TokComma
    deriving (Show, Eq)

-- | Lexer for mathematical expressions
tokenizeExpr :: String -> Either String [ExprToken]
tokenizeExpr [] = Right []
tokenizeExpr (c:cs)
    | isSpace c = tokenizeExpr cs
    | c == '('  = (TokLParen :) <$> tokenizeExpr cs
    | c == ')'  = (TokRParen :) <$> tokenizeExpr cs
    | c == ','  = (TokComma :) <$> tokenizeExpr cs
    | c == '+'  = (TokPlus :) <$> tokenizeExpr cs
    | c == '-'  = (TokMinus :) <$> tokenizeExpr cs
    | c == '*'  = (TokMul :) <$> tokenizeExpr cs
    | c == '/'  = (TokDiv :) <$> tokenizeExpr cs
    | c == '%'  = (TokMod :) <$> tokenizeExpr cs
    | isDigit c =
        let (digits, rest) = span isDigit (c:cs)
        in (TokNum (read digits) :) <$> tokenizeExpr rest
    | isAlpha c || c == '_' =
        let (ident, rest) = span (\x -> isAlphaNum x || x == '_' || x == '.') (c:cs)
        in (TokIdent ident :) <$> tokenizeExpr rest
    | otherwise = Left ("Unexpected character in expression: " ++ [c])

-- | Recursive descent parser for Expr tokens
parseExprTokens :: [ExprToken] -> Either String (Expr, [ExprToken])
parseExprTokens = parseExprAdditive

parseExprAdditive :: [ExprToken] -> Either String (Expr, [ExprToken])
parseExprAdditive tokens = do
    (left, rest) <- parseExprMultiplicative tokens
    loop left rest
  where
    loop acc (TokPlus : ts) = do
        (rhs, rest') <- parseExprMultiplicative ts
        loop (EAdd acc rhs) rest'
    loop acc (TokMinus : ts) = do
        (rhs, rest') <- parseExprMultiplicative ts
        loop (ESub acc rhs) rest'
    loop acc ts = Right (acc, ts)

parseExprMultiplicative :: [ExprToken] -> Either String (Expr, [ExprToken])
parseExprMultiplicative tokens = do
    (left, rest) <- parseExprUnary tokens
    loop left rest
  where
    loop acc (TokMul : ts) = do
        (rhs, rest') <- parseExprUnary ts
        loop (EMul acc rhs) rest'
    loop acc (TokDiv : ts) = do
        (rhs, rest') <- parseExprUnary ts
        loop (EDiv acc rhs) rest'
    loop acc (TokMod : ts) = do
        (rhs, rest') <- parseExprUnary ts
        loop (EMod acc rhs) rest'
    loop acc ts = Right (acc, ts)

parseExprUnary :: [ExprToken] -> Either String (Expr, [ExprToken])
parseExprUnary (TokMinus : ts) = do
    (sub, rest) <- parseExprUnary ts
    Right (ESub (ELit 0) sub, rest)
parseExprUnary (TokPlus : ts) = parseExprUnary ts
parseExprUnary ts = parseExprPrimary ts

parseExprPrimary :: [ExprToken] -> Either String (Expr, [ExprToken])
parseExprPrimary (TokNum n : ts) = Right (ELit n, ts)
parseExprPrimary (TokIdent "min" : TokLParen : ts) = do
    (a, r1) <- parseExprAdditive ts
    case r1 of
        TokComma : r2 -> do
            (b, r3) <- parseExprAdditive r2
            case r3 of
                TokRParen : r4 -> Right (EMin a b, r4)
                _ -> Left "Expected ')' after min(a, b)"
        _ -> Left "Expected ',' in min(a, b)"
parseExprPrimary (TokIdent "max" : TokLParen : ts) = do
    (a, r1) <- parseExprAdditive ts
    case r1 of
        TokComma : r2 -> do
            (b, r3) <- parseExprAdditive r2
            case r3 of
                TokRParen : r4 -> Right (EMax a b, r4)
                _ -> Left "Expected ')' after max(a, b)"
        _ -> Left "Expected ',' in max(a, b)"
parseExprPrimary (TokIdent "clamp" : TokLParen : ts) = do
    (lo, r1) <- parseExprAdditive ts
    case r1 of
        TokComma : r2 -> do
            (hi, r3) <- parseExprAdditive r2
            case r3 of
                TokComma : r4 -> do
                    (val, r5) <- parseExprAdditive r4
                    case r5 of
                        TokRParen : r6 -> Right (EClamp lo hi val, r6)
                        _ -> Left "Expected ')' after clamp(lo, hi, val)"
                _ -> Left "Expected second ',' in clamp(lo, hi, val)"
        _ -> Left "Expected first ',' in clamp(lo, hi, val)"
parseExprPrimary (TokIdent name : ts) = Right (EVar name, ts)
parseExprPrimary (TokLParen : ts) = do
    (inner, rest) <- parseExprAdditive ts
    case rest of
        TokRParen : rest' -> Right (inner, rest')
        _ -> Left "Missing closing parenthesis ')'"
parseExprPrimary (t : _) = Left ("Unexpected token: " ++ show t)
parseExprPrimary [] = Left "Unexpected end of expression"

-- | Parse a mathematical string expression into an Expr AST.
parseExpr :: String -> Either String Expr
parseExpr s = do
    tokens <- tokenizeExpr s
    (expr, rest) <- parseExprTokens tokens
    case rest of
        [] -> Right expr
        (t : _) -> Left ("Unexpected trailing token: " ++ show t)

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
    | Location ActorRef RoomID -- ^ actor reference, room ID (is actor in this room?)
    | CompareVar String Comparator Int  -- ^ variable vs integer literal (mana >= 5)
    | VarIs String String               -- ^ text variable equals a literal (`{ var: X, is: Y }`)
    | HasCondition String               -- ^ active condition/timer on player (Phase 2.1)
    | Knows ActorRef String             -- ^ W1: actor knows this fact (VarMap `known.<actor>.<fact>`)
    | ActorHas ActorRef ItemID          -- ^ W4: does the actor (player, NPC, device) carry/hold this item?
    | HasTaggedItem ActorRef String     -- ^ B2: does the actor carry an item with this tag?
    | RoomHasTaggedItem RoomID String   -- ^ B2: does the room hold an item with this tag?
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
        ActorHas a i       -> object [ "actor_has" .= actorId a, "item" .= i ]
        EntityHasState e s -> object [ "state"    .= e, "is" .= s ]
        HasFlag f          -> object [ "has_flag" .= f ]
        RoomHasTag r t     -> object [ "room"     .= r, "has_tag" .= t ]
        Location a r       -> object [ "at"       .= actorId a, "room" .= r ]
        CompareVar n op v  -> object [ "compare_var" .= object
                                        [ "name" .= n, "op" .= comparatorName op, "value" .= v ] ]
        VarIs n v          -> object [ "var" .= n, "is" .= v ]
        HasCondition c     -> object [ "has_condition" .= c ]
        Knows a f          -> object [ "knows" .= actorId a, "fact" .= f ]
        HasTaggedItem a t  -> object [ "actor_has_tag" .= object ["actor" .= actorId a, "tag" .= t ] ]
        RoomHasTaggedItem r t -> object [ "room" .= r, "has_item_tag" .= t ]

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
    "eq"   -> Just CEq
    "ceq"  -> Just CEq
    "="    -> Just CEq
    "ne"   -> Just CNeq
    "cneq" -> Just CNeq
    "!="   -> Just CNeq
    "lt"   -> Just CLt
    "clt"  -> Just CLt
    "<"    -> Just CLt
    "lte"  -> Just CLte
    "clte" -> Just CLte
    "<="   -> Just CLte
    "gt"   -> Just CGt
    "cgt"  -> Just CGt
    ">"    -> Just CGt
    "gte"  -> Just CGte
    "cgte" -> Just CGte
    ">="   -> Just CGte
    _      -> Nothing

instance FromJSON Predicate where
    parseJSON = withObject "Predicate" $ \o ->
            (o .: "predicate")
        <|> (PAll  <$> o .: "all")
        <|> (PAny  <$> o .: "any")
        <|> (PNot  <$> o .: "not")
        <|> (PTrue <$ (o .: "true" :: Parser Bool))
        <|> (PlayerHas <$> o .: "has_item")
        <|> (ActorHas  <$> o .: "actor_has" <*> o .: "item")
        <|> (do ah <- o .: "actor_has"
                ActorHas <$> ah .: "actor" <*> ah .: "item")
        <|> (HasFlag   <$> o .: "has_flag")
        <|> (HasCondition <$> o .: "has_condition")
        -- W1: knowledge — `knows: <fact>` (player) or `{knows: <actor>, fact: <fact>}`
        <|> (do k <- o .: "knows"
                mFact  <- o .:? "fact"
                mActor <- o .:? "actor"
                case (mFact, mActor) of
                    (Just fact, _) -> do
                        act <- parseJSON k
                        pure (Knows act fact)
                    (Nothing, Just act) -> case k of
                        String f -> pure (Knows act (T.unpack f))
                        _        -> fail "Expected fact string for knows"
                    (Nothing, Nothing) -> case k of
                        String f  -> pure (Knows ActorPlayer (T.unpack f))
                        Object ko -> Knows <$> (ko .: "actor" <|> ko .: "knows") <*> ko .: "fact"
                        _         -> fail "Expected fact string or object for knows")
        <|> (EntityHasState <$> o .: "state" <*> o .: "is")
        -- Text comparison for variables holding text (`type: text`), e.g. the
        -- engine's own `combat.action`. Distinct from `state`/`is`, which tests
        -- an entity's state layer.
        <|> (VarIs <$> o .: "var" <*> o .: "is")
        <|> (RoomHasTag     <$> o .: "room"  <*> o .: "has_tag")
        <|> (RoomHasTaggedItem <$> o .: "room" <*> o .: "has_item_tag")
        <|> (do aht <- o .: "actor_has_tag"
                HasTaggedItem <$> aht .: "actor" <*> aht .: "tag")
        <|> (Location       <$> o .: "at"    <*> o .: "room")
        <|> (Compare <$> o .: "lhs" <*> o .: "op" <*> o .: "rhs")
        <|> (do cmpObj <- o .: "compare"
                (Compare <$> cmpObj .: "lhs" <*> cmpObj .: "op" <*> cmpObj .: "rhs")
                  <|> (do cName <- cmpObj .: "condition_turns"
                          opVal <- cmpObj .: "op"
                          vVal  <- cmpObj .: "value" <|> cmpObj .: "rhs"
                          pure (Compare (VRConditionTurns cName) opVal vVal)))
        <|> (do ct <- o .: "condition_turns"
                cName <- ct .: "name" <|> ct .: "condition"
                opVal <- ct .: "op"
                vVal  <- ct .: "value" <|> ct .: "rhs"
                pure (Compare (VRConditionTurns cName) opVal vVal))
        <|> (do cv  <- o .: "compare_var"
                n   <- cv .: "name"
                opS <- cv .: "op"
                cmp <- case parseComparatorName opS of
                    Just c  -> pure c
                    Nothing -> fail ("Unknown comparator '" ++ opS ++ "' in compare_var")
                (do v <- cv .: "value"
                    pure (CompareVar n cmp v))
                  <|> (do otherVar <- cv .: "var" <|> cv .: "other_var"
                          pure (Compare (VRVariable n) cmp (VRVariable otherVar))))
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
        -- Card game synergy sugar: card_in_hand / cards_in_hand / combo
        <|> (do c <- o .: "card_in_hand"
                pure (EntityHasState c "in_hand"))
        <|> (do cs <- o .: "cards_in_hand"
                pure (PAll [EntityHasState c "in_hand" | c <- cs]))
        <|> (do val <- o .: "combo"
                case val of
                    Array arr -> do
                        cs <- mapM parseJSON (Foldable.toList arr)
                        pure (PAll [EntityHasState c "in_hand" | c <- cs])
                    String s -> pure (EntityHasState (T.unpack s) "in_hand")
                    _        -> fail "Expected card id or list of card ids for combo")
        <|> fail "Unknown predicate"

-- | Exit connection between rooms
data Exit
    = Open String                             -- ^ Destination room name
    | Locked String String                    -- ^ Destination room name, Entity name
    | Guarded String Predicate (Maybe String) -- ^ Destination room name, condition to pass, optional failure message (Phase 2.2)
    deriving (Show, Eq, Generic)

instance ToJSON Exit
instance FromJSON Exit

-- | Target RoomID of an Exit (Open, Locked, or Guarded).
exitRoomID :: Exit -> RoomID
exitRoomID (Open r) = r
exitRoomID (Locked r _) = r
exitRoomID (Guarded r _ _) = r

-- | Music command queued by the pure engine for the frontend (Audio Phase 2).
data MusicCommand = MusicStart FilePath | MusicStop
    deriving (Show, Eq, Generic)

instance ToJSON MusicCommand
instance FromJSON MusicCommand

-- | Action Outcome representing the result of an interaction
--   This is the engine-level Effect-DSL: a compact, composable set of
--   effect constructors that replace the previous 30-specific Effect
--   variants. The Worldbuilder compiles its higher-level shorthands into
--   these Effects.
data Effect
    = Sequence [Effect]                          -- ^ Do many effects in order
    | Conditional Predicate Effect Effect         -- ^ Branch: if (pred) then this else that
    | RandomChoice [(Int, Effect)]               -- ^ Weighted random pick from candidates
    | RandomChoiceOn String [(Int, Effect)]      -- ^ B8: weighted pick on a named RNG stream (`rng.<name>` in the VarMap)
    | RollDice Int Int String Int                -- ^ K1: roll dice pool (pool, die, stream, keep)
    | SetValue ValueRef EffectValue                     -- ^ Set any value (flag, variable, property)
    | ModifyValue ValueRef Int                    -- ^ Modify a numeric value (hp, skill, prop, etc.)
    | MoveEntity EntityID Location                -- ^ Move an entity to a location
    | SendMessage String                          -- ^ Show a message to the player
    | ApplyCondition String Int (Maybe Effect) (Maybe Effect) Bool -- ^ Name, turns, tick, end effects, hidden
    | ClearCondition String                       -- ^ Remove a condition by name
    | RaiseEvent String                           -- ^ P1-20: fire `OnCustomEvent name`
    | ModifySkill SkillID Int                     -- ^ Change a skill by delta
    | QuestOp QuestOp String                      -- ^ Quest lifecycle operation
    | GameEnd GameOverReason String               -- ^ End the game with a reason
    | Narrative [String] Effect                   -- ^ Lines to show, then follow-up (stored as pendingNarrative)
    | PlayClip ClipID                             -- ^ Phase H/H4: queue a cutscene clip (pendingCutscene)
    | PlaySfx FilePath                            -- ^ Audio Phase 1: queue a sound effect for the frontend
    | PlayMusic FilePath                          -- ^ Audio Phase 2: start/switch background music loop
    | StopMusic                                   -- ^ Audio Phase 2: stop background music
    | SetExit RoomID Direction Exit               -- ^ Rogue Phase 3: open/rewire a dynamic exit
    | RemoveExit RoomID Direction                 -- ^ Rogue Phase 3: close a dynamic exit
    | ComputeValue ValueRef Expr                  -- ^ Phase 1A: dynamically compute an expression and assign to ValueRef
    | DrawCards Int                               -- ^ Phase 2A: draw n cards into hand
    | DiscardHand                                 -- ^ Phase 2A: discard active hand
    | DiscardCard CardID                          -- ^ Phase 2A: discard specific card from hand
    | ExhaustCard CardID                          -- ^ Phase 2A: exhaust card from hand/play
    | AddCardToDeck CardID DeckDestination        -- ^ Phase 2A: add card to draw/discard/hand
    | ShuffleDeck                                 -- ^ Phase 2A: shuffle draw pile
    | GenerateRoom RoomID String String RoomID Direction Direction -- ^ Phase 3A: id, name, description, fromRoom, toDir, returnDir
    | Block (Maybe String) Bool                   -- ^ Phase 2.2: veto command execution (optional message, consumesTurn)
    | CallProc String [EffectValue]               -- ^ Phase 2.5: run procedure `name` with literal args (D2)
    | Learn ActorRef String                       -- ^ W1: actor learns a fact (idempotent, fires OnLearn)
    | LearnRecipe String                          -- ^ K11d: player learns a recipe (idempotent, fires OnLearnRecipe)
    | Forget ActorRef String                      -- ^ W1: actor forgets a fact (explicit only, never automatic)
    | ShowNotes                                   -- ^ W1: render the player's notes book
    | NextChapter                                  -- ^ W3: to the next chapter (declaration order)
    | GotoChapter String                           -- ^ W3: to a named chapter (no backward jumps)
    | Mount ItemID ActorRef                        -- ^ W4: place an item into a device/actor
    | Unmount ItemID                               -- ^ W4: remove an item from its device/actor to the room
    | GainXp Int                                   -- ^ W2: add XP (clamped at 0), run level loop
    | StepToward ActorRef DistanceTarget PursuitOptions (Maybe String)   -- ^ pursuit: one edge toward the target (opt. author msg)
    | StepAwayFrom ActorRef DistanceTarget PursuitOptions (Maybe String) -- ^ pursuit: one edge away (opt. author msg)
    | DamageAll CountSpec Int                 -- ^ B3: damage every NPC in the set
    | MoveAll CountSpec CountWhere            -- ^ B3: move every member of the set to
    | RevealAll CountSpec                     -- ^ B3: reveal every hidden item in the set
    | ConsumeAll CountSpec                    -- ^ B3: remove every member of the set
    | SetStateAll CountSpec String            -- ^ B3: set the state of every member
    | Noop                                        -- ^ Do nothing
    deriving (Show, Eq, Generic)

-- | Identity effect for defaulted parsers.
noopEffect :: Effect
noopEffect = Noop


-- ---------------------------------------------------------------------------
-- Procedural Sandbox & Runtime Worldgen (Schritt 3 / Phase 3A)
-- ---------------------------------------------------------------------------

-- | Definition of a biome template for procedural sandbox generation.
data BiomeTemplate = BiomeTemplate
    { btId            :: String               -- ^ Unique biome identifier (e.g. "forest")
    , btWeight        :: Int                  -- ^ Relative frequency weight
    , btNamePattern   :: String               -- ^ e.g. "Dichter Nadelwald ({x}, {y})"
    , btDescription   :: CondText             -- ^ Room description with conditional variants
    , btTags          :: [String]             -- ^ e.g. ["forest", "outdoor"]
    , btAsciiArt      :: AsciiArt             -- ^ ASCII art landscape (emptyAscii if none)
    , btPassableDirs  :: [Direction]          -- ^ Open directions (e.g. [North, South, East, West])
    } deriving (Show, Eq, Generic)

instance ToJSON BiomeTemplate where
    toJSON bt = object $
        [ "id"            .= btId bt
        , "weight"        .= btWeight bt
        , "name_pattern"  .= btNamePattern bt
        , "description"   .= btDescription bt
        , "tags"          .= btTags bt
        ] ++ asciiPair "ascii_art" (btAsciiArt bt)
          ++ [ "passable_dirs" .= btPassableDirs bt ]

instance FromJSON BiomeTemplate where
    parseJSON = withObject "BiomeTemplate" $ \o -> BiomeTemplate
        <$> o .: "id"
        <*> o .:? "weight" .!= 1
        <*> o .:? "name_pattern" .!= "Wildnis ({x}, {y})"
        <*> o .:? "description" .!= plainText "Unberührte Wildnis."
        <*> o .:? "tags" .!= []
        <*> o .:? "ascii_art" .!= emptyAscii
        <*> o .:? "passable_dirs" .!= [North, South, East, West]

-- | Definition of an infinite procedural sandbox zone.
data SandboxZone = SandboxZone
    { szId            :: String               -- ^ Zone id (e.g. "wildnis")
    , szOrigin        :: (Int, Int, Int)      -- ^ Entry coordinates (x, y, z)
    , szBiomes        :: [BiomeTemplate]      -- ^ Available biomes in this zone
    , szDefaultFloor  :: Maybe Int            -- ^ Floor index for minimap (default: Just 1)
    } deriving (Show, Eq, Generic)

instance ToJSON SandboxZone where
    toJSON sz = object $
        [ "id"     .= szId sz
        , "origin" .= szOrigin sz
        , "biomes" .= szBiomes sz
        ] ++ [ "floor" .= fl | Just fl <- [szDefaultFloor sz] ]

instance FromJSON SandboxZone where
    parseJSON = withObject "SandboxZone" $ \o -> SandboxZone
        <$> o .: "id"
        <*> o .:? "origin" .!= (0, 0, 0)
        <*> o .:? "biomes" .!= []
        <*> o .:? "floor"

-- | Clip IDs are plain strings; the compiled world carries the clip map.
type ClipID = String

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
    | CarriedBy ActorRef
    | InContainer EntityID
    | EquippedBy ActorRef
    | Removed
    deriving (Show, Eq, Generic)

instance ToJSON Location where
    toJSON (InRoom r)       = object [ "tag" .= ("InRoom" :: T.Text), "contents" .= r ]
    toJSON (CarriedBy a)    = object [ "tag" .= ("CarriedBy" :: T.Text), "contents" .= toJSON a ]
    toJSON (InContainer c)  = object [ "tag" .= ("InContainer" :: T.Text), "contents" .= c ]
    toJSON (EquippedBy a)   = object [ "tag" .= ("EquippedBy" :: T.Text), "contents" .= toJSON a ]
    toJSON Removed          = object [ "tag" .= ("Removed" :: T.Text) ]

instance FromJSON Location where
    parseJSON (String s) = pure (InRoom (T.unpack s))
    parseJSON v = withObject "Location" (\o -> do
        tag <- o .: "tag" :: Parser T.Text
        case tag of
            "InRoom"      -> InRoom <$> o .: "contents"
            "CarriedBy"   -> do
                c <- o .: "contents"
                CarriedBy <$> parseJSON c
            "InContainer" -> InContainer <$> o .: "contents"
            "EquippedBy"  -> do
                c <- o .: "contents"
                EquippedBy <$> parseJSON c
            "Removed"     -> pure Removed
            _             -> fail ("Unknown Location tag: " ++ T.unpack tag)) v

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

-- | Is a CondText empty (no default, no variants)?
isEmptyCond :: CondText -> Bool
isEmptyCond ct = null (ctDefault ct) && null (ctVariants ct)

-- | A marker in ASCII art that binds to a target entity (Phase E). The glyph
--   is what the player sees; the target is an item or NPC id. Numbers (the
--   index in `aaHotspots`, 1-based) address the marker, so a glyph must not be
--   a digit (see the worldbuilder validation).
data Hotspot = Hotspot
    { hsGlyph  :: Char      -- ^ marker character in the art
    , hsTarget :: String    -- ^ item or NPC id
    } deriving (Show, Eq, Generic)

instance ToJSON Hotspot where
    toJSON h = object
        [ "glyph"  .= hsGlyph h
        , "target" .= hsTarget h
        ]

instance FromJSON Hotspot where
    parseJSON = withObject "Hotspot" $ \o -> Hotspot
        <$> o .: "glyph"
        <*> o .: "target"

-- | ASCII art: state-dependent base text plus an optional animation (Phase D)
--   and optional hotspots (Phase E).
--
--   * `aaStatic` is the conditional art of Phase B. It is rendered when there
--     are no frames (or when passive animation is disabled).
--   * `aaFrames` are animation frames, each itself a `CondText` so a frame can
--     depend on the game state as well.
--   * `aaEvery` is the number of turns per passive frame. `0` (or fewer)
--     disables the passive tick; the frames are then only shown by `watch`.
--   * `aaAmbient` is the timed loop of Phase H (H1): frames plus the art's own
--     playback rate (`fps`). The pure core hands both to the frontend; nothing
--     in the core waits.
--   * `aaHotspots` are marker glyphs bound to targets, addressed by number.
data AsciiArt = AsciiArt
    { aaStatic   :: CondText
    , aaFrames   :: [CondText]
    , aaEvery    :: Int
    , aaHotspots :: [Hotspot]
    , aaAmbient  :: Maybe Ambient
    } deriving (Show, Eq, Generic)

-- | An ambient loop (Phase H, H1): inline frames plus the rate they repeat
--   at. Frames are plain strings (no condition per frame); 4–30 frames per
--   the plan.
data Ambient = Ambient
    { ambFrames :: [String]
    , ambFps    :: Int
    } deriving (Show, Eq, Generic)

instance ToJSON Ambient where
    toJSON (Ambient frames fps) = object
        [ "frames" .= frames
        , "fps"    .= fps
        ]
instance FromJSON Ambient where
    parseJSON = withObject "Ambient" (\o ->
        Ambient <$> o .: "frames" <*> o .: "fps")

-- | No art at all. `aaEvery = 1` matches the decoder default so a static art
--   round-trips through JSON unchanged.
emptyAscii :: AsciiArt
emptyAscii = AsciiArt (CondText "" []) [] 1 [] Nothing

-- | Is the art empty (no static text, no frames, no ambient loop)?
isEmptyAscii :: AsciiArt -> Bool
isEmptyAscii a =
    null (aaFrames a) && isEmptyCond (aaStatic a)
    && case aaAmbient a of
        Nothing          -> True
        Just amb -> null (ambFrames amb)

-- Accept a plain string (shorthand), a `{default, variants}` CondText (Phase B
-- shape) or an animated `{default?, variants?, frames, every}` object.
instance FromJSON AsciiArt where
    parseJSON v = case v of
        String s -> pure (AsciiArt (CondText (T.unpack s) []) [] 0 [] Nothing)
        _ -> withObject "AsciiArt" (\o -> do
                stat <- CondText <$> o .:? "default" .!= "" <*> o .:? "variants" .!= []
                frames <- o .:? "frames" .!= []
                every <- o .:? "every" .!= 1
                spots <- o .:? "hotspots" .!= []
                amb <- o .:? "ambient" .!= Nothing
                pure (AsciiArt stat frames every spots amb)) v

-- A frame- and hotspot-less art keeps the exact Phase B JSON (a CondText
-- object), so worlds without animation/hotspots keep their historical encoding
-- and checksum.
instance ToJSON AsciiArt where
    toJSON a
        | null (aaFrames a), null (aaHotspots a), isNothing (aaAmbient a)
        = toJSON (aaStatic a)
        | otherwise = object $
            [ "default"  .= ctDefault (aaStatic a)
            , "variants" .= ctVariants (aaStatic a)
            , "frames"   .= aaFrames a
            , "every"    .= aaEvery a
            ]
            ++ [ "ambient" .= amb | Just amb <- [aaAmbient a] ]
            ++ [ "hotspots" .= aaHotspots a | not (null (aaHotspots a)) ]

-- | Encode an ASCII art field only when it carries something. An empty art is
--   omitted, so worlds without art keep their historical JSON and therefore
--   their `computeWorldChecksum` (no spurious "different world version" warning
--   when loading an old save).
asciiPair :: Key -> AsciiArt -> [Pair]
asciiPair k art
    | isEmptyAscii art = []
    | otherwise        = [k .= art]

instance ToJSON Effect where
    toJSON (RollDice pool die stream keep) = object
        [ "roll_dice" .= object
            [ "pool"   .= pool
            , "die"    .= die
            , "stream" .= stream
            , "keep"   .= keep
            ]
        ]
    toJSON other = genericToJSON defaultOptions other

    toEncoding (RollDice pool die stream keep) = pairs
        ( "roll_dice" .= object
            [ "pool"   .= pool
            , "die"    .= die
            , "stream" .= stream
            , "keep"   .= keep
            ]
        )
    toEncoding other = genericToEncoding defaultOptions other

instance FromJSON Effect where
    parseJSON v = parseRollDice v <|> genericParseJSON defaultOptions v <|> parseLegacyEffect v
      where
        parseRollDice = withObject "Effect" $ \o -> do
            rd <- o .: "roll_dice"
            flip (withObject "roll_dice") rd $ \ro -> do
                p <- ro .: "pool"
                d <- ro .: "die"
                s <- ro .:? "stream" .!= ""
                k <- ro .:? "keep" .!= p
                pure (RollDice p d s k)
        parseLegacyEffect = withObject "Effect" $ \o -> do
            tag <- o .: "tag" :: Parser T.Text
            case tag of
                "ApplyCondition" -> do
                    contents <- o .: "contents"
                    case contents of
                        [n, t, tick, end] ->
                            ApplyCondition <$> parseJSON n
                                           <*> parseJSON t
                                           <*> parseJSON tick
                                           <*> parseJSON end
                                           <*> pure False
                        _ -> fail "ApplyCondition legacy contents mismatch"
                _ -> fail ("Unsupported legacy effect tag: " ++ T.unpack tag)

-- ---------------------------------------------------------------------------
-- JSON helpers for compound Map keys
-- ---------------------------------------------------------------------------

-- | Encode a Map with (VerbPhase, Verb, String) keys as
--   `[{verb, state, phase?, effect}, …]`. The `phase` field is omitted for
--   'PhaseAfter' so every legacy entry (and every pre-4.2 world.json) encodes
--   byte-identically. Entry order is the map order; for an all-'PhaseAfter'
--   map that is the old (verb, state) order.
verbStateMapToJSON :: Map.Map (VerbPhase, Verb, String) Effect -> Value
verbStateMapToJSON m =
    toJSON [ object (["verb" .= show v, "state" .= s] ++ phaseFields ph ++ ["effect" .= e])
           | ((ph, v, s), e) <- Map.toList m ]
  where
    phaseFields PhaseAfter   = []
    phaseFields PhaseBefore  = ["phase" .= ("before" :: String)]
    phaseFields PhaseInstead = ["phase" .= ("instead" :: String)]

verbStateMapFromJSON :: Value -> Parser (Map.Map (VerbPhase, Verb, String) Effect)
verbStateMapFromJSON v =
    (do xs <- parseJSON v :: Parser [Value]
        Map.fromList <$> mapM entry xs)
    <|> verbStateMapFromLegacyJSON v
  where
    entry = withObject "verb-state entry" $ \o -> do
        vTxt <- o .: "verb"
        s    <- o .: "state"
        e    <- o .: "effect"
        ph   <- o .:? "phase" .!= ("after" :: String)
        case (reads vTxt, ph) of
            ([(verb, "")], "after")   -> pure ((PhaseAfter, verb, s), e)
            ([(verb, "")], "before")  -> pure ((PhaseBefore, verb, s), e)
            ([(verb, "")], "instead") -> pure ((PhaseInstead, verb, s), e)
            (_, "after")              -> fail ("Bad verb encoding: " ++ vTxt)
            (_, other)                -> fail ("Bad phase encoding: " ++ other)

-- | Legacy form: one string key per entry, `"VTake:intact"`. Only the first
--   `:` separates verb and state, so a state containing `:` used to be
--   corrupted (P2-9); kept for reading old `world.json` files.
verbStateMapFromLegacyJSON :: Value -> Parser (Map.Map (VerbPhase, Verb, String) Effect)
verbStateMapFromLegacyJSON v = do
    m <- parseJSON v :: Parser (Map.Map String Effect)
    let parsePair k = case break (== ':') k of
            (vStr, ':':sStr) -> case reads vStr of
                [(verb, "")] -> Right ((PhaseAfter, verb, sStr), ())
                _            -> Left $ "Bad verb: " ++ vStr
            _                -> Left $ "Bad key format: " ++ k
    case mapM (\(k, val) -> case parsePair k of
                Right (key3, _) -> Right (key3, val)
                Left err        -> Left err
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
-- Grammar metadata (4.3.5, plan-sprachpakete.md "Variante A")
-- ---------------------------------------------------------------------------

-- | Optional grammar metadata for localized message templates (4.3.5): the
--   author supplies article forms and a gender tag per item/NPC; templates
--   consume them as the placeholders @{article_nom}@/@{article_acc}@/
--   @{article_dat}@/@{gender}@ (plus @{<slot>_article_*}@ per entity slot, see
--   'Messages.grammarArgs'). No declension tables and no grammar logic in the
--   core — the author steers every form exactly (including specials);
--   case-free phrasing stays possible.
data Grammar = Grammar
    { gNom    :: Maybe String  -- ^ article in nominative case (the short form @article: "der"@ fills only this)
    , gAcc    :: Maybe String  -- ^ article in accusative case
    , gDat    :: Maybe String  -- ^ article in dative case
    , gGender :: Maybe String  -- ^ gender tag, closed set @"m"@ | @"f"@ | @"n"@ (compiler-checked)
    } deriving (Show, Eq)

-- | No grammar metadata at all (default).
emptyGrammar :: Grammar
emptyGrammar = Grammar Nothing Nothing Nothing Nothing

-- | True when no field is set — such grammars never add JSON fields.
grammarEmpty :: Grammar -> Bool
grammarEmpty g = all isNothing [gNom g, gAcc g, gDat g, gGender g]

-- | The @article@/@gender@ fields appended to the ItemDef/NPCDef JSON objects.
--   Empty grammars add NO fields (byte contract, rule 5); a grammar with only
--   the nominative set encodes the short string form.
grammarJSONFields :: Grammar -> [Pair]
grammarJSONFields g =
    [ "article" .= articleValue g | any isJust [gNom g, gAcc g, gDat g] ]
    ++ [ "gender" .= gen | Just gen <- [gGender g] ]

-- | The @article@ JSON value: short string form when only the nominative is
--   set, otherwise an object with the set cases in nom/acc/dat order.
articleValue :: Grammar -> Value
articleValue g = case (gNom g, gAcc g, gDat g) of
    (Just n, Nothing, Nothing) -> toJSON n
    _ -> object $     [ "nom" .= n | Just n <- [gNom g] ]
                  ++ [ "acc" .= a | Just a <- [gAcc g] ]
                  ++ [ "dat" .= d | Just d <- [gDat g] ]

-- | Parse the @article@/@gender@ fields (engine @world.json@ and the
--   worldbuilder YAML parser share this). @article@ accepts both encodings:
--   the short string form fills the nominative only.
grammarFromJSONFields :: Object -> Parser Grammar
grammarFromJSONFields o = do
    art <- o .:? "article"
    (n, a, d) <- parseArticleForms art
    Grammar n a d <$> o .:? "gender"

parseArticleForms :: Maybe Value -> Parser (Maybe String, Maybe String, Maybe String)
parseArticleForms Nothing           = pure (Nothing, Nothing, Nothing)
parseArticleForms (Just (String t)) = pure (Just (T.unpack t), Nothing, Nothing)
parseArticleForms (Just v)          = withObject "article"
    (\ao -> (,,) <$> ao .:? "nom" <*> ao .:? "acc" <*> ao .:? "dat") v

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
        , itemVerbMap       :: Map.Map (VerbPhase, Verb, String) Effect
        , itemCapacity      :: Maybe Int          -- ^ 4.4: container capacity (count of items), Nothing = not a container
        , itemAscii         :: AsciiArt              -- ^ Optional state-dependent, animated ASCII art
        , itemGrammar       :: Grammar               -- ^ 4.3.5: optional article/gender metadata (empty = none)
        , itemRepeatable    :: Bool                  -- ^ K15: repeatable/infinite world item or tool (default: False)
        , itemHomeLocation  :: Maybe RoomID          -- ^ K15: authored starting room for repeatable lookup/respawn
        } deriving (Show, Eq)

instance ToJSON ItemDef where
    toJSON def = object $
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
        ] ++ maybe [] (\c -> ["itemCapacity" .= c]) (itemCapacity def)
          ++ asciiPair "itemAscii" (itemAscii def)
          ++ grammarJSONFields (itemGrammar def)
          ++ (if itemRepeatable def then ["itemRepeatable" .= True] else [])
          ++ (if itemRepeatable def then maybe [] (\h -> ["itemHomeLocation" .= h]) (itemHomeLocation def) else [])

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
        <*> o .:? "itemCapacity"     .!= Nothing
        <*> o .:? "itemAscii"        .!= emptyAscii
        <*> grammarFromJSONFields o
        <*> o .:? "itemRepeatable"   .!= False
        <*> o .:? "itemHomeLocation" .!= Nothing

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
    , npcVerbMap       :: Map.Map (VerbPhase, Verb, String) Effect
    , npcDropsOnDeath  :: Bool                  -- ^ B9: the corpse lets go of what it carried/wore (default False = keeps everything)
    , npcAscii         :: AsciiArt              -- ^ Optional state-dependent, animated ASCII art
    , npcTopics        :: Map.Map String Effect    -- ^ 4.5: `ask`/`tell` X about <topic>
    , npcGrammar       :: Grammar               -- ^ 4.3.5: optional article/gender metadata (empty = none)
    } deriving (Show, Eq)

instance ToJSON NPCDef where
    toJSON def = object $
        [ "npcId"            .= npcId def
        , "npcName"          .= npcName def
        , "npcDescription"   .= npcDescription def
        , "npcDialogueTrees" .= npcDialogueTrees def
        , "npcKeywords"      .= npcKeywords def
        , "npcMaxHealth"     .= npcMaxHealth def
        , "npcAttackBase"    .= npcAttackBase def
        , "npcDefenseBase"   .= npcDefenseBase def
        , "npcVerbMap"       .= verbStateMapToJSON (npcVerbMap def)
        ] ++ (if Map.null (npcTopics def) then [] else ["topics" .= npcTopics def])
          -- B9: byte contract — the flag is written only when it is set, so
          -- every existing world.json stays byte-identical
          ++ (if npcDropsOnDeath def then ["npcDropsOnDeath" .= True] else [])
          ++ asciiPair "npcAscii" (npcAscii def)
          ++ grammarJSONFields (npcGrammar def)

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
        <*> o .:? "npcDropsOnDeath"  .!= False
        <*> o .:? "npcAscii"         .!= emptyAscii
        <*> o .:? "topics" .!= Map.empty
        <*> grammarFromJSONFields o

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

-- | 4.4: a stationary container (chest, coffin, cabinet) authored under
--   `containers:`. Portable containers are `ItemDef`s with `itemCapacity`.
--   The live state (open/locked) lives in `entityStates` — no new save field.
data ContainerDef = ContainerDef
    { conId         :: EntityID
    , conName       :: String
    , conLocation   :: RoomID
    , conState      :: ContainerState  -- ^ initial state (open/locked/capacity)
    , conRepeatable :: Bool            -- ^ K15: repeatable container (default: False)
    } deriving (Show, Eq, Generic)

instance ToJSON ContainerDef where
    toJSON c = object $
        [ "conId"       .= conId c
        , "conName"     .= conName c
        , "conLocation" .= conLocation c
        , "conState"    .= conState c
        ] ++ if conRepeatable c then ["conRepeatable" .= True] else []

instance FromJSON ContainerDef where
    parseJSON = withObject "ContainerDef" $ \o -> ContainerDef
        <$> o .:  "conId"
        <*> o .:  "conName"
        <*> o .:  "conLocation"
        <*> o .:  "conState"
        <*> o .:? "conRepeatable" .!= False

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
    , condTickOutcome :: Maybe Effect       -- ^ Fired every turn while active
    , condEndOutcome  :: Maybe Effect       -- ^ Fired once when it expires
    , condHidden      :: Bool               -- ^ Hidden from status and HUD display (Phase 2.1)
    } deriving (Show, Eq, Generic)

instance ToJSON Condition where
    toJSON c = object $
        [ "condName"        .= condName c
        , "condRemaining"   .= condRemaining c
        , "condTickOutcome" .= condTickOutcome c
        , "condEndOutcome"  .= condEndOutcome c
        ] ++ [ "condHidden" .= True | condHidden c ]

instance FromJSON Condition where
    parseJSON = withObject "Condition" $ \o -> Condition
        <$> o .: "condName"
        <*> o .: "condRemaining"
        <*> o .:? "condTickOutcome"
        <*> o .:? "condEndOutcome"
        <*> (do mh <- o .:? "condHidden"
                case mh of
                    Just h  -> pure h
                    Nothing -> o .:? "hidden" .!= False)

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
    , questOnComplete  :: Maybe QuestID          -- ^ 4.6: quest started when this one completes
    } deriving (Show, Eq, Generic)

-- | Hand-written, not derived: 'questOnComplete' is written only when set (the
--   byte contract, same pattern as 'roomIntro'/'roomFloor'). Every key the
--   derived instance wrote keeps its name and stays unconditional — notably
--   'questReward', which the pinned bytes carry as @null@ for the four shipped
--   quests without a reward.
instance ToJSON Quest where
    toJSON q = object $
        [ "questId"          .= questId q
        , "questName"        .= questName q
        , "questDescription" .= questDescription q
        , "questPrereqs"     .= questPrereqs q
        , "questStages"      .= questStages q
        , "questReward"      .= questReward q
        ]
      ++ [ "questOnComplete" .= c | Just c <- [questOnComplete q] ]

-- | Derived: the generic parser treats a @Maybe@ field as optional, so a world
--   without 'questOnComplete' decodes to 'Nothing'.
instance FromJSON Quest


-- ---------------------------------------------------------------------------
-- Rooms
-- ---------------------------------------------------------------------------

-- | Position of a room on the author's map grid (4.6). @x@ is the column,
--   @y@ the row -- the layering the compiler's auto-layout uses (row = BFS
--   depth, column = index within that layer). The third dimension is
--   'roomFloor', which already exists, so a multi-level world keeps its floors
--   apart without a new field here.
--
--   Only *authored* positions live in the world ('roomMapPos'); the computed
--   layout stays a worldbuilder-side function, so a world without @map:@ is
--   byte-identical to one written before this field existed.
data MapPos = MapPos
    { mapPosX :: Int
    , mapPosY :: Int
    } deriving (Show, Eq, Ord, Generic)

-- Hand-written, not derived: the author-facing keys are @x@/@y@ (the YAML form
--   @map: {x: 2, y: 3}@), not the record field names. The same instance pair
--   serves the YAML front end and the compiled world, so a position survives a
--   round trip through either without renaming.
instance ToJSON MapPos where
    toJSON p = object [ "x" .= mapPosX p, "y" .= mapPosY p ]

instance FromJSON MapPos where
    parseJSON = withObject "MapPos" $ \o -> MapPos
        <$> o .: "x"
        <*> o .: "y"

-- | Room with connections and static data.
--   Note: `roomVisited` lives in SaveState (dynamic), not here.
data Room = Room
    { roomId              :: RoomID
    , roomName            :: String
    , roomDescription     :: CondText
    , roomConnections     :: Map.Map Direction Exit
    , roomTags            :: Set.Set String            -- ^ "dark", "safe", "vehicle", ...
    , roomLightFlag       :: Maybe FlagID              -- ^ when "true", a "dark" room is lit
    , roomDarkMsg         :: Maybe String              -- ^ optional override for dark message (Phase 0.3)
    , roomOnEnter         :: Maybe Effect
    , roomOnLook          :: Maybe Effect
    , roomOnExit          :: Maybe Effect
    , roomSearchOutcome   :: Maybe Effect
    , roomAscii           :: AsciiArt                  -- ^ Optional ASCII art banner (state-dependent since Phase B, animated since Phase D)
    , roomIntro           :: Maybe String              -- ^ Clip id played once when entering (Phase H/H4)
    , roomFloor           :: Maybe Int                 -- ^ Optional floor / dungeon level index (Phase 4b)
    , roomMapPos          :: Maybe MapPos              -- ^ Author-set map position (4.6); written only when set
    } deriving (Show, Eq, Generic)

instance ToJSON Room where
    toJSON r = object $
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
        ] ++ asciiPair "roomAscii" (roomAscii r)
          ++ [ "roomIntro" .= i | Just i <- [roomIntro r] ]
          ++ [ "roomFloor" .= fl | Just fl <- [roomFloor r] ]
          ++ [ "roomMapPos" .= p | Just p <- [roomMapPos r] ]
          ++ [ "roomDarkMsg" .= dm | Just dm <- [roomDarkMsg r] ]

instance FromJSON Room where
    parseJSON = withObject "Room" $ \o -> Room
        <$> o .:  "roomId"
        <*> o .:  "roomName"
        <*> o .:  "roomDescription"
        <*> o .:  "roomConnections"
        <*> o .:? "roomTags"            .!= Set.empty
        <*> o .:? "roomLightFlag"       .!= Nothing
        <*> (do m1 <- o .:? "roomDarkMsg"
                case m1 of
                    Just _  -> pure m1
                    Nothing -> do
                        m2 <- o .:? "dark_msg"
                        case m2 of
                            Just _  -> pure m2
                            Nothing -> o .:? "dark_message")
        <*> o .:? "roomOnEnter"         .!= Nothing
        <*> o .:? "roomOnLook"          .!= Nothing
        <*> o .:? "roomOnExit"          .!= Nothing
        <*> o .:? "roomSearchOutcome"   .!= Nothing
        <*> o .:? "roomAscii"           .!= emptyAscii
        <*> o .:? "roomIntro"           .!= Nothing
        <*> (do mf <- o .:? "floor"
                case mf of
                    Just _  -> pure mf
                    Nothing -> o .:? "roomFloor")
        <*> o .:? "roomMapPos" .!= Nothing

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
    | OnStandingChange FactionID       -- ^ G9c: fired when standing level changes
    | OnCustomEvent String
    | OnTurn
    | OnCommand String                 -- ^ verb name (e.g. "activate")
    | OnBefore String                  -- ^ verb name before execution (Phase 2.2)
    | OnLearn String                   -- ^ W1: fired once per newly learned fact, in learning order
    | OnLearnRecipe String             -- ^ K11d: fired once per newly learned recipe, in learning order
    | OnChapter String                 -- ^ W3: fired when entering chapter <id>
    | OnLevelUp Int                    -- ^ W2: fired when player reaches level <n>
    | OnCombatStart                    -- ^ K7/K4: fired once when combat starts (combat.engaged == 0)
    | OnTalk String String             -- ^ 4.5: fired on ask/tell (npc id, topic)
    deriving (Show, Eq, Generic)

instance ToJSON EventType
instance FromJSON EventType

-- | G9c alias for EventType
type TriggerEvent = EventType

-- | A trigger rule: when event matches conditions, fire effects.
data TriggerDef = TriggerDef
    { trId         :: String
    , trEvent      :: EventType
    , trCondition  :: Maybe Predicate
    , trEffects    :: [Effect]
    , trOnce       :: Bool
    , trCooldown   :: Int              -- ^ turns between re-firing (0 = no cooldown)
    , trWeight     :: Int              -- ^ weight for weighted random selection (default 1)
    , trRequires   :: [FlagID]         -- ^ all flags must be set to fire (default [])
    , trChainsTo   :: [String]         -- ^ custom event names to raise after firing (default [])
    } deriving (Show, Eq, Generic)

-- | Hand-written, not derived: 'trWeight' (when 1), 'trRequires' (when empty)
--   and 'trChainsTo' (when empty) are omitted to preserve the byte contract
--   for existing adventures (same pattern as 'roomIntro'/'roomFloor').
instance ToJSON TriggerDef where
    toJSON t = object $
        [ "trId"        .= trId t
        , "trEvent"     .= trEvent t
        , "trCondition" .= trCondition t
        , "trEffects"   .= trEffects t
        , "trOnce"      .= trOnce t
        , "trCooldown"  .= trCooldown t
        ]
        ++ [ "trWeight"   .= trWeight t   | trWeight t /= 1 ]
        ++ [ "trRequires" .= trRequires t | not (null (trRequires t)) ]
        ++ [ "trChainsTo" .= trChainsTo t | not (null (trChainsTo t)) ]

instance FromJSON TriggerDef where
    parseJSON = withObject "TriggerDef" $ \o -> TriggerDef
        <$> o .:  "trId"
        <*> o .:  "trEvent"
        <*> o .:? "trCondition"
        <*> o .:? "trEffects"  .!= []
        <*> o .:? "trOnce"     .!= False
        <*> o .:? "trCooldown" .!= 0
        <*> (do mw <- o .:? "trWeight"
                case mw of
                    Just w  -> pure w
                    Nothing -> o .:? "weight" .!= 1)
        <*> (do mr <- o .:? "trRequires"
                case mr of
                    Just r  -> pure r
                    Nothing -> o .:? "requires" .!= [])
        <*> (do mc <- o .:? "trChainsTo"
                case mc of
                    Just c  -> pure c
                    Nothing -> o .:? "chains_to" .!= [])

-- | Runtime state of a trigger rule.
data TriggerState = TriggerState
    { tsFired           :: Bool
    , tsCooldownRemaining :: Int
    } deriving (Show, Eq, Generic)

instance ToJSON TriggerState
instance FromJSON TriggerState

-- | A named, parameterized effect bundle (Phase 2.5, D2 "procedures").
--   Authors declare `procedures:` entries and call them with `call:`; the
--   call site is literal (name + literal args) so the compiler can check it
--   statically (no dynamic dispatch).
data ProcDef = ProcDef
    { procId      :: String
    , procParams  :: [String]
    , procEffects :: [Effect]
    } deriving (Show, Eq, Generic)

instance ToJSON ProcDef
instance FromJSON ProcDef

-- | A knowledge fact (W1): declared under `facts:`. The notes book shows
--   learned player facts in **declaration order** — Gameplay-Vertrag:
--   nicht umsortieren (dieselbe Ordnungs-Falle wie `Direction`s `Ord`).
data FactDef = FactDef
    { factId       :: String
    , factKeys     :: [String]        -- ^ words the player can use to refer to it
    , factText     :: String          -- ^ notes-book text
    , factSource   :: Maybe String    -- ^ optional origin note ("found in the library")
    , factTag      :: Maybe String    -- ^ optional grouping tag ("evidence" / "guess")
    , factLearnMsg :: Maybe String    -- ^ message on learning (default: learn.default)
    , factSilent   :: Maybe Bool      -- ^ per-fact override of the adventure journal mode
    } deriving (Show, Eq, Generic)

instance ToJSON FactDef
instance FromJSON FactDef

-- | A statement (K9): authored under `statements:` with speaker, claims,
--   truth value, text, optional condition and tag.
data StatementDef = StatementDef
    { stDefId      :: String
    , stDefSpeaker :: String
    , stDefClaims  :: String
    , stDefTruth   :: Bool
    , stDefText    :: String
    , stDefWhen    :: Maybe Predicate
    , stDefTag     :: Maybe String
    } deriving (Show, Eq, Generic)

instance ToJSON StatementDef where
    toJSON st = object $
        [ "id"      .= stDefId st
        , "speaker" .= stDefSpeaker st
        , "claims"  .= stDefClaims st
        , "truth"   .= stDefTruth st
        , "text"    .= stDefText st
        ]
        ++ [ "when" .= w | Just w <- [stDefWhen st] ]
        ++ [ "tag"  .= t | Just t <- [stDefTag st] ]

instance FromJSON StatementDef where
    parseJSON = withObject "StatementDef" $ \o -> StatementDef
        <$> o .:  "id"
        <*> o .:? "speaker" .!= ""
        <*> o .:? "claims"  .!= ""
        <*> o .:? "truth"   .!= True
        <*> o .:? "text"    .!= ""
        <*> o .:? "when"
        <*> o .:? "tag"

-- | A derivation rule (W1, `combine:`): when an actor knows **all** premises,
--   the yields fact follows — the auto-cascade applies this table as a pure
--   fixpoint in the Learn application (never over trigger recursion).
data CombineDef = CombineDef
    { cdFacts  :: [String]            -- ^ premises (all must be known)
    , cdYields :: String              -- ^ the derived fact
    , cdMsg    :: Maybe String        -- ^ confirmation message on derivation
    } deriving (Show, Eq, Generic)

instance ToJSON CombineDef
instance FromJSON CombineDef

-- | A chapter (W3): authored under `chapters:` in narrative order. The list
--   order is the auto-gate tie-break and the `next_chapter` direction —
--   Gameplay-Vertrag: nicht umsortieren.
data ChapterDef = ChapterDef
    { chId    :: String
    , chIntro :: Maybe String        -- ^ shown on entering the chapter
    , chWhen  :: Maybe Predicate     -- ^ auto-gate: switch when this holds
    } deriving (Show, Eq, Generic)

instance ToJSON ChapterDef
instance FromJSON ChapterDef

-- | A device / fixture (W4): an interactive entity with state and/or item mounting.
data DeviceDef = DeviceDef
    { devId          :: DeviceID
    , devName        :: String
    , devKeys        :: [String]
    , devLocation    :: RoomID
    , devDescription :: Maybe String
    , devFitsTag     :: Maybe String
    , devFits        :: [ItemID]
    , devInsertMsg   :: Maybe String
    , devRemoveMsg   :: Maybe String
    , devOnInsert    :: [Effect]
    , devOnRemove    :: [Effect]
    , devFlipVerb    :: Maybe String
    , devFlipStates  :: [String]
    , devOnFlip      :: Map.Map String [Effect]
    } deriving (Show, Eq, Generic)

type DeviceID = String

instance ToJSON DeviceDef
instance FromJSON DeviceDef

-- | A single level threshold and its benefits (W2).
data LevelDef = LevelDef
    { lvlNumber  :: Int               -- ^ 1-based level index (1, 2, 3...)
    , lvlXp      :: Int               -- ^ XP threshold required to reach this level
    , lvlName    :: String            -- ^ Display name / rank title
    , lvlMsg     :: Maybe String      -- ^ Optional message shown upon reaching this level
    , lvlEffects :: [Effect]          -- ^ Effects executed when reaching this level
    } deriving (Show, Eq, Generic)

instance ToJSON LevelDef
instance FromJSON LevelDef

-- | Player progression table (W2). Declared under `progression:`.
data ProgressionDef = ProgressionDef
    { progLevels :: [LevelDef]
    } deriving (Show, Eq, Generic)

instance ToJSON ProgressionDef
instance FromJSON ProgressionDef

-- | Recipe key for item interactions (crafting).
--
-- Historical note (K11c):
-- Originally, 'itemInteractions' used a pair key @(String, String)@
-- (@Map.fromList [((i1, i2), eff)]@). This structure was fundamentally
-- limited to 2-ingredient recipes and could not represent multi-ingredient
-- recipes (e.g. 3 or more ingredients in alchemy or crafting).
--
-- K11c generalizes the map key from a pair to 'RecipeKey':
--   * 'RecipePair a b': exact pair of items (backwards compatible, binds
--     {item1}/{item2}).
--   * 'RecipeIngredients mId ings': N ingredients as an authored sequence / set
--     (binds {ingredient1..N}).
--
-- Why this structure?
-- 1. Dual semantics: Pair recipes match the exact pair (independent of other
--    inventory contents), while list recipes match by subset inclusion
--    (R ⊆ reachableItems) plus command reference.
-- 2. Backwards compatibility: Existing pair recipes serialize to the exact same
--    @{"a": a, "b": b, "effect": ...}@ JSON objects, keeping world.json
--    byte-identical for all existing adventures.
-- 3. Sequence preservation: Even though multi-ingredient matching tests subset
--    inclusion as a set, the authored list order is preserved so that
--    dynamic variables ({ingredient1..N}) and ordered effects (consume) are
--    deterministic.
--
-- K11d adds a leading (id, requires_learning) pair to BOTH constructors:
--   * `id` is the stable reference for `learn_recipe:`, `known_recipe.<id>`,
--     `on: learn_recipe <id>` triggers and the worldbuilder validation.
--   * `requires_learning: true` locks the recipe behind learning (only this
--     flag locks; a recipe without it stays usable as before).
--   Both fields are written to world.json only when set, so existing worlds
--   stay byte-identical (the K11c pattern). Because they are part of the key,
--   two recipes with equal ingredients but different ids no longer collide in
--   the map — that is the "several variants of one product" case.
data RecipeKey
    = RecipePair (Maybe String) Bool (Maybe ItemID) String String
      -- ^ id (K11d), requires_learning (K11d), result (K11b), item1, item2 (K11a)
    | RecipeIngredients (Maybe String) Bool (Maybe ItemID) [String]
      -- ^ id (K11c/K11d), requires_learning (K11d), result (K11b), ingredients (K11c)
    deriving (Show, Eq, Ord)

-- | Optional stable recipe id (K11d).
recipeId :: RecipeKey -> Maybe String
recipeId (RecipePair mId _ _ _ _)      = mId
recipeId (RecipeIngredients mId _ _ _) = mId

-- | K11d: is this recipe locked behind `learn_recipe`?
recipeRequiresLearning :: RecipeKey -> Bool
recipeRequiresLearning (RecipePair _ req _ _ _)      = req
recipeRequiresLearning (RecipeIngredients _ req _ _) = req

-- | Optional target item produced by a recipe (K11b).
recipeResult :: RecipeKey -> Maybe ItemID
recipeResult (RecipePair _ _ mRes _ _)      = mRes
recipeResult (RecipeIngredients _ _ mRes _) = mRes

-- | Ingredients of a recipe in declaration order (K11b).
recipeIngredientsList :: RecipeKey -> [ItemID]
recipeIngredientsList (RecipePair _ _ _ i1 i2)       = [i1, i2]
recipeIngredientsList (RecipeIngredients _ _ _ ings) = ings

-- | Static world definition containing blueprint/map data
data GameWorld = GameWorld
    { rooms              :: Map.Map RoomID Room
    , itemDefs           :: Map.Map ItemID ItemDef
    , npcDefs            :: Map.Map NPCID NPCDef
    , entityInteractions :: Map.Map (String, String) (String, String)
    , itemInteractions   :: Map.Map RecipeKey Effect  -- ^ Recipe -> outcome
    , npcInteractions    :: Map.Map (String, String) Effect  -- ^ (Item, NPC) -> outcome (B9); empty map is omitted
    , questDefs          :: Map.Map QuestID Quest                    -- ^ Static quest definitions
    , vehicleDefs        :: Map.Map VehicleID VehicleDef             -- ^ Static vehicle definitions (Phase 3)
    , verbDefs           :: Map.Map String VerbDef                   -- ^ Adventure-declared verbs (Phase 3a)
    , varDefs            :: Map.Map String VarDef                    -- ^ Adventure-declared variables (Phase 3b)
    , triggerDefs        :: [TriggerDef]                              -- ^ Trigger rules (Phase 3f)
    , combatProfile      :: CombatProfile                            -- ^ combat policy (Phase 7f)
    , worldName          :: String                                   -- ^ adventure title (`name:`), shown as the game banner
    , abilities          :: Map.Map String PlayerAbility             -- ^ Player abilities (Phase 7f-3, step A3)
    , worldEndArt        :: Map.Map String AsciiArt                  -- ^ "death"/"victory"/custom reason -> banner (Phase G)
    , worldTitleArt      :: AsciiArt                                 -- ^ Optional title banner replacing the `bannerFor` default (Phase G)
    , worldClips         :: Map.Map String Clip                      -- ^ Cutscene clips, embedded at compile time (Phase H/H4, D14)
    , worldGamePolicy    :: GamePolicy                               -- ^ roguelike policy (Rogue Phase 1; default = unchanged behaviour)
    , cardDefs           :: Map.Map CardID Card                      -- ^ Card definitions for deckbuilder / card games (Genre 5)
    , sandboxZones       :: Map.Map String SandboxZone               -- ^ Procedural infinite sandbox zones (Genre 3)
    , procDefs           :: Map.Map String ProcDef                   -- ^ Named procedures (Phase 2.5); empty map is omitted from world.json
    , factDefs           :: [FactDef]                                -- ^ Knowledge facts (W1), in declaration order; empty list is omitted
    , statementDefs      :: [StatementDef]                           -- ^ Knowledge statements (K9), in declaration order; empty list is omitted
    , combineDefs        :: [CombineDef]                             -- ^ Derivation rules (W1); empty list is omitted
    , chapterDefs        :: [ChapterDef]                             -- ^ Chapters (W3) in narrative order; empty list is omitted
    , deviceDefs         :: Map.Map DeviceID DeviceDef               -- ^ Interactive devices/fixtures (W4); empty map is omitted
    , containerDefs      :: Map.Map EntityID ContainerDef            -- ^ Stationary containers (4.4); empty map is omitted
    , progressionDef     :: Maybe ProgressionDef                     -- ^ Player progression (W2); Nothing omitted from world.json
    , startRoom          :: Maybe RoomID                             -- ^ Start room compiled from YAML `start_room:`; Nothing = arbitrary fallback
    , worldLanguage      :: Maybe String                             -- ^ `language:` (4.3): language pack code ("de" …); Nothing = plain English default
    , worldMessages      :: Map.Map String String                    -- ^ `messages:` (4.3): per-adventure catalog overrides (non-empty values only); empty map omitted from world.json
    , factions           :: Map.Map FactionID [FactionLevel]         -- ^ Faction standing levels (G9a); empty map is omitted from world.json
    } deriving (Show, Eq)

-- | A cutscene clip (Phase H/H4): a frame sequence played once at its own

-- | A cutscene clip (Phase H/H4): a frame sequence played once at its own
--   rate. Clips live in the compiled world (D14: companion files are embedded
--   by the Worldbuilder, so runtime needs no extra file access).
data Clip = Clip
    { clipFrames :: [String]
    , clipFps    :: Int
    } deriving (Show, Eq)

instance ToJSON Clip where
    toJSON (Clip frames fps) = object
        [ "frames" .= frames
        , "fps"    .= fps
        ]

instance FromJSON Clip where
    parseJSON = withObject "Clip" (\o ->
        Clip <$> o .: "frames" <*> o .: "fps")



instance ToJSON GameWorld where
    toJSON gw = object $
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
        ] ++ endArtPair ++ titleArtPair ++ clipPair ++ policyPair ++ cardPair ++ sandboxPair
          ++ procPair ++ factPair ++ statementPair ++ combinePair ++ chapterPair ++ devicePair ++ containerPair ++ progPair
          ++ startRoomPair ++ langPair ++ msgPair ++ npcInteractionPair ++ factionPair
      where
        endArtPair = [ "endArt" .= endArt | not (Map.null endArt) ]
        titleArtPair = [ "titleArt" .= titleArt | not (isEmptyAscii titleArt) ]
        clipPair = [ "clips" .= worldClips gw | not (Map.null (worldClips gw)) ]
        -- Rogue Phase 1 (M2): the policy field is only emitted when it differs
        -- from the default, otherwise every world's checksum would change and
        -- all existing saves would report "world mismatch!".
        policyPair = [ "game" .= worldGamePolicy gw
                     | worldGamePolicy gw /= defaultGamePolicy ]
        cardPair = [ "cards" .= cardDefs gw | not (Map.null (cardDefs gw)) ]
        sandboxPair = [ "sandboxZones" .= sandboxZones gw | not (Map.null (sandboxZones gw)) ]
        -- Phase 2.5 (M2): omitted when empty so world.json (and with it the
        -- world checksum) of every existing adventure stays bit-identical.
        procPair = [ "procDefs" .= procDefs gw | not (Map.null (procDefs gw)) ]
        factPair = [ "factDefs" .= factDefs gw | not (null (factDefs gw)) ]
        statementPair = [ "statementDefs" .= statementDefs gw | not (null (statementDefs gw)) ]
        combinePair = [ "combineDefs" .= combineDefs gw | not (null (combineDefs gw)) ]
        chapterPair = [ "chapterDefs" .= chapterDefs gw | not (null (chapterDefs gw)) ]
        devicePair = [ "deviceDefs" .= deviceDefs gw | not (Map.null (deviceDefs gw)) ]
        containerPair = [ "containerDefs" .= containerDefs gw | not (Map.null (containerDefs gw)) ]
        progPair = [ "progressionDef" .= p | Just p <- [progressionDef gw] ]
        startRoomPair = [ "startRoom" .= rid | Just rid <- [startRoom gw] ]
        -- Phase 4.3 (D4): language + message overrides only when set, so every
        -- existing world.json stays bit-identical (same contract as procDefs).
        langPair = [ "language" .= l | Just l <- [worldLanguage gw] ]
        msgPair  = [ "messages" .= worldMessages gw | not (Map.null (worldMessages gw)) ]
        -- B9: item-on-NPC outcomes share the (item, target) -> outcome shape of
        -- `itemInteractions`; the field is omitted when empty so world.json of
        -- every existing adventure stays bit-identical (same contract as
        -- procDefs).
        npcInteractionPair =
            [ "npcInteractions" .= npcInteractionsToJSON (npcInteractions gw)
            | not (Map.null (npcInteractions gw)) ]
        -- Phase G9a: omitted when empty so world.json of every adventure without
        -- factions stays bit-identical.
        factionPair = [ "factions" .= factions gw | not (Map.null (factions gw)) ]
        endArt = Map.filter (not . isEmptyAscii) (worldEndArt gw)
        titleArt = worldTitleArt gw

-- | Author-defined game policy for roguelike/roguelite adventures. All
--   defaults preserve today's behaviour ('defaultGamePolicy'), and the
--   JSON encoding emits the field only when it differs from the default
--   (M2: keeps the world checksum — and with it every existing save file —
--   bit-identical).
data GamePolicy = GamePolicy
    { gpPermadeath :: Bool      -- ^ death offers no undo/load, only restart/quit
    , gpAllowUndo  :: Bool      -- ^ 'undo' command globally disabled
    , gpIronman    :: Bool       -- ^ saves only in savezones (one checkpoint slot,
                                 -- ^ deleted on death); load disabled
    , gpSaveZones  :: [RoomID]  -- ^ rooms where ironman saves are allowed
    , gpMetaSlug   :: Maybe String -- ^ explicit slug for the meta file (Rogue Phase 2, M8)
    } deriving (Show, Eq, Generic)

-- | Today's behaviour: nothing changes unless the author opts in.
defaultGamePolicy :: GamePolicy
defaultGamePolicy = GamePolicy False True False [] Nothing

instance ToJSON GamePolicy where
    toJSON p = object $
        [ "permadeath" .= gpPermadeath p
        , "allow_undo" .= gpAllowUndo p
        , "ironman"    .= gpIronman p
        , "save_zones" .= gpSaveZones p
        ] ++ [ "meta_slug" .= s | Just s <- [gpMetaSlug p] ]

instance FromJSON GamePolicy where
    parseJSON = withObject "GamePolicy" $ \o -> GamePolicy
        <$> o .:? "permadeath" .!= False
        <*> o .:? "allow_undo" .!= True
        <*> o .:? "ironman"    .!= False
        <*> o .:? "save_zones" .!= []
        <*> o .:? "meta_slug"

instance FromJSON GameWorld where
    parseJSON = withObject "GameWorld" $ \o -> GameWorld
        <$> o .:  "rooms"
        <*> o .:  "itemDefs"
        <*> o .:  "npcDefs"
        <*> (o .: "entityInteractions" >>= tupleMapFromJSON)
        <*> (o .:? "itemInteractions" >>= maybe (pure Map.empty) parseItemInteractions)
        <*> (o .:? "npcInteractions" >>= maybe (pure Map.empty) parseNpcInteractions)
        <*> o .:? "questDefs" .!= Map.empty
        <*> o .:? "vehicleDefs" .!= Map.empty
        <*> o .:? "verbDefs" .!= Map.empty
        <*> o .:? "varDefs"  .!= Map.empty
        <*> o .:? "triggerDefs" .!= []
        <*> o .:? "combatProfile" .!= CombatClassic Nothing
        <*> o .:? "worldName" .!= ""
        <*> o .:? "abilities" .!= Map.empty
        <*> o .:? "endArt" .!= Map.empty
        <*> o .:? "titleArt" .!= emptyAscii
        <*> o .:? "clips" .!= Map.empty
        <*> o .:? "game" .!= defaultGamePolicy
        <*> o .:? "cards" .!= Map.empty
        <*> o .:? "sandboxZones" .!= Map.empty
        <*> o .:? "procDefs" .!= Map.empty
        <*> o .:? "factDefs" .!= []
        <*> o .:? "statementDefs" .!= []
        <*> o .:? "combineDefs" .!= []
        <*> o .:? "chapterDefs" .!= []
        <*> o .:? "deviceDefs" .!= Map.empty
        <*> o .:? "containerDefs" .!= Map.empty
        <*> o .:? "progressionDef" .!= Nothing
        <*> o .:? "startRoom" .!= Nothing
        <*> o .:? "language" .!= Nothing
        <*> o .:? "messages" .!= Map.empty
        <*> o .:? "factions" .!= Map.empty

-- | Encode item-on-item outcomes as objects (P2-9, K11c).
--   Pair recipes emit historical {"a": ..., "b": ..., "effect": ...} objects.
--   Multi-ingredient recipes emit {"ingredients": [...], "effect": ...} objects,
--   with optional "id".  K11d: "id" (both shapes) and "requires_learning: true"
--   are written only when set — existing worlds stay byte-identical, and the
--   historical field order is untouched (new fields append at the end).
itemInteractionsToJSON :: Map.Map RecipeKey Effect -> Value
itemInteractionsToJSON m =
    toJSON [ encodeEntry k e | (k, e) <- Map.toList m ]
  where
    encodeEntry (RecipePair mId req mRes a b) e =
        object $ [ "a" .= a, "b" .= b, "effect" .= e ]
               ++ [ "result" .= r | Just r <- [mRes] ]
               ++ [ "id" .= i | Just i <- [mId] ]
               ++ [ "requires_learning" .= True | req ]
    encodeEntry (RecipeIngredients mId req mRes ings) e =
        object $ [ "ingredients" .= ings, "effect" .= e ]
               ++ [ "id" .= i | Just i <- [mId] ]
               ++ [ "result" .= r | Just r <- [mRes] ]
               ++ [ "requires_learning" .= True | req ]

parseItemInteractions :: Value -> Parser (Map.Map RecipeKey Effect)
parseItemInteractions v =
    (do xs <- parseJSON v :: Parser [Value]
        Map.fromList <$> mapM entry xs)
    <|> legacy
  where
    entry = withObject "item interaction entry" $ \o -> do
        mRes  <- o .:? "result"
        mId   <- o .:? "id"
        req   <- o .:? "requires_learning" .!= False
        mIngs <- o .:? "ingredients"
        case mIngs of
            Just ings -> do
                e <- o .: "effect"
                pure (RecipeIngredients mId req mRes ings, e)
            Nothing -> do
                a <- o .: "a"
                b <- o .: "b"
                e <- o .: "effect"
                pure (RecipePair mId req mRes a b, e)
    -- Legacy form: `"a|b"` string keys.
    legacy = do
        m <- parseJSON v :: Parser (Map.Map String Effect)
        case mapM parseKey (Map.toList m) of
            Right kvs -> pure (Map.fromList kvs)
            Left err  -> fail err
    parseKey (k, e) = case break (== '|') k of
        (a, '|':b) -> Right (RecipePair Nothing False Nothing a b, e)
        _          -> Left ("Bad item interaction key: " ++ k)

-- | Encode item-on-NPC outcomes as objects (B9).
npcInteractionsToJSON :: Map.Map (String, String) Effect -> Value
npcInteractionsToJSON m =
    toJSON [ object [ "a" .= a, "b" .= b, "effect" .= e ]
           | ((a, b), e) <- Map.toList m ]

parseNpcInteractions :: Value -> Parser (Map.Map (String, String) Effect)
parseNpcInteractions v =
    (do xs <- parseJSON v :: Parser [Value]
        Map.fromList <$> mapM entry xs)
    <|> legacy
  where
    entry = withObject "npc interaction entry" $ \o -> do
        a <- o .: "a"
        b <- o .: "b"
        e <- o .: "effect"
        pure ((a, b), e)
    legacy = do
        m <- parseJSON v :: Parser (Map.Map String Effect)
        case mapM parseKey (Map.toList m) of
            Right kvs -> pure (Map.fromList kvs)
            Left err  -> fail err
    parseKey (k, e) = case break (== '|') k of
        (a, '|':b) -> Right ((a, b), e)
        _          -> Left ("Bad npc interaction key: " ++ k)

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
    , exitOverrides      :: Map.Map (RoomID, Direction) (Maybe Exit)
        -- ^ Rogue Phase 3: dynamic exits written by `SetExit`/`RemoveExit`
        -- ^ effects. `Just exit` = replacement connection, `Nothing` = exit
        -- ^ removed (even if statically present). Empty = static world (M2:
        -- ^ the ToJSON instance omits it entirely, keeping every existing
        -- ^ save's encoding bit-identical).
    , deckState          :: Maybe DeckState                  -- ^ Runtime deckbuilder state (Genre 5)
    , dynamicRooms       :: Map.Map RoomID Room              -- ^ Dynamically generated runtime rooms (Genre 3)
    } deriving (Show, Eq, Generic)

instance ToJSON SaveState where
    toJSON ss = object $
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
        ] ++ exitOverridePair ++ deckPair ++ dynamicRoomsPair
      where
        -- Rogue Phase 3 (M2): only emitted when non-empty — the encoding of
        -- untouched adventures stays bit-identical.
        exitOverridePair =
            [ "exitOverrides" .= exitOverridesToJSON (exitOverrides ss)
            | not (Map.null (exitOverrides ss)) ]
        deckPair = case deckState ss of
            Nothing -> []
            Just ds -> [ "deckState" .= ds ]
        dynamicRoomsPair =
            [ "dynamicRooms" .= dynamicRooms ss
            | not (Map.null (dynamicRooms ss)) ]

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
        <*> (o .:? "exitOverrides" >>= maybe (pure Map.empty) parseExitOverrides)
        <*> o .:? "deckState"
        <*> o .:? "dynamicRooms"   .!= Map.empty

-- | Encode exit overrides as a list of {room, dir, exit} objects — the same
--   shape as 'itemInteractionsToJSON': tuple-keyed maps have no JSON object
--   form, so we write (and read) an object list instead. `exit: null` encodes
--   a removed exit ('Nothing').
exitOverridesToJSON :: Map.Map (RoomID, Direction) (Maybe Exit) -> Value
exitOverridesToJSON m =
    toJSON [ object [ "room" .= r, "dir" .= d, "exit" .= me ]
           | ((r, d), me) <- Map.toList m ]

parseExitOverrides :: Value -> Parser (Map.Map (RoomID, Direction) (Maybe Exit))
parseExitOverrides v = do
    xs <- parseJSON v :: Parser [Value]
    Map.fromList <$> mapM entry xs
  where
    entry = withObject "exit override" $ \o -> do
        r <- o .: "room"
        d <- o .: "dir"
        e <- o .:? "exit"  -- Nothing = removed exit
        pure ((r, d), e)

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
    , pendingAnimation :: Maybe ([String], Int)    -- ^ Frames + rate in µs (Phase D/H1, runtime only)
    , pendingCutscene :: Maybe ([String], Int)      -- ^ Cutscene frames + rate in µs, played once (Phase H/H4, runtime only)
    , pendingSfx :: [FilePath]  -- ^ SFX files queued for playback (Audio Phase 1, runtime only)
    , pendingMusic :: Maybe MusicCommand           -- ^ Music command for the frontend (Audio Phase 2, runtime only)
    , diagnostics :: [String]                      -- ^ Engine-level findings for the author (P2-23)
    , lastVeto :: Maybe Bool                       -- ^ Phase 2.2: Nothing = not vetoed, Just consumesTurn = vetoed
    , chosenTarget :: Maybe String                 -- ^ Phase 2.3: entity id the player picked in a
                                                   --   disambiguation answer; overrides target
                                                   --   resolution for the replayed command (runtime only)
    , procScopes :: [Map.Map String VariableValue] -- ^ Phase 2.5: procedure parameter/locals stack,
                                                   --   innermost first (runtime only, like `diagnostics`:
                                                   --   GameState has no JSON instance, nothing to save)
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

-- ---------------------------------------------------------------------------
-- Adventure slug (Rogue Phase 0)
-- ---------------------------------------------------------------------------

-- | Deterministic file-system slug for adventure-bound file names. The
--   Roguelike/Roguelite extension (Rogue Phase 2) will use it for
--   @saves/<slug>_meta.json@: lowercase ASCII letters, digits, @-@ and @_@
--   survive; every other character becomes @_@ (runs of them collapse via
--   'words'); a name without a single surviving character falls back to
--   @"default"@ so file names stay well-formed even for worlds with an empty
--   or purely non-ASCII @worldName@.
--
--   Deliberately simple: no transliteration (umlauts become @_@), so the
--   result is stable across locales — and remember M8: adventures that want
--   meta-progression to survive a title change will get an explicit
--   @game.meta_slug@ override later.
slugify :: String -> String
slugify name
    | null slug = "default"
    | otherwise = slug
  where
    slug = intercalate "_" (words (map mapChar (map toLower name)))
    mapChar c | isKept c  = c
              | otherwise = ' '
    isKept c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')
               || c == '_' || c == '-'

-- | Advance the explicit RNG state (linear congruential generator).
nextRng :: Word64 -> Word64
nextRng w = w * 6364136223846793005 + 1

-- ---------------------------------------------------------------------------
-- Sandbox & Runtime-Worldgen Primitives (Genre 3 / Phase 3B)
-- ---------------------------------------------------------------------------

-- | SplitMix64 64-bit mixer.
splitMix64Mix :: Word64 -> Word64
splitMix64Mix s =
    let z1 = (s `xor` (s `shiftR` 30)) * 0xBF58476D1CE4E5B9
        z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94D049BB133111EB
    in z2 `xor` (z2 `shiftR` 31)

-- | Derive a deterministic cell seed from base seed, zone name, and coordinates.
deriveCellSeed :: Word64 -> String -> Int -> Int -> Int -> Word64
deriveCellSeed baseSeed zone x y z =
    let h1 = splitMix64Mix (baseSeed + fromIntegral x * 73856093)
        h2 = splitMix64Mix (h1 + fromIntegral y * 19349663)
        h3 = splitMix64Mix (h2 + fromIntegral z * 83492791)
        zoneHash = foldl' (\h c -> h * 31 + fromIntegral (fromEnum c)) (fromIntegral (length zone)) zone
    in splitMix64Mix (h3 + zoneHash)

-- | Construct canonical sandbox room ID: "sandbox_<zone>_<x>_<y>_<z>"
sandboxRoomId :: String -> Int -> Int -> Int -> RoomID
sandboxRoomId zone x y z = "sandbox_" ++ zone ++ "_" ++ show x ++ "_" ++ show y ++ "_" ++ show z

-- | Parse a sandbox room ID into (zone, x, y, z).
parseSandboxRoomId :: RoomID -> Maybe (String, Int, Int, Int)
parseSandboxRoomId rid = do
    rest <- stripPrefix "sandbox_" rid
    let parts = splitOnChar '_' rest
    guard (length parts >= 4)
    let zPart = last parts
        yPart = parts !! (length parts - 2)
        xPart = parts !! (length parts - 3)
        zoneParts = take (length parts - 3) parts
        zone = intercalate "_" zoneParts
    guard (not (null zone))
    x <- readSignedInt xPart
    y <- readSignedInt yPart
    z <- readSignedInt zPart
    pure (zone, x, y, z)
  where
    splitOnChar _ [] = [""]
    splitOnChar c (x:xs)
        | x == c    = "" : splitOnChar c xs
        | otherwise = case splitOnChar c xs of
            []    -> [[x]]
            (h:t) -> (x:h) : t

    readSignedInt ('-':ds) | not (null ds) && all isDigit ds = negate <$> readMaybe ds
    readSignedInt ds       | not (null ds) && all isDigit ds = readMaybe ds
    readSignedInt _                                          = Nothing

-- | Direction vector delta in (dx, dy, dz).
directionDelta :: Direction -> (Int, Int, Int)
directionDelta North     = (0, 1, 0)
directionDelta South     = (0, -1, 0)
directionDelta East      = (1, 0, 0)
directionDelta West      = (-1, 0, 0)
directionDelta Up        = (0, 0, 1)
directionDelta Down      = (0, 0, -1)
directionDelta Northeast = (1, 1, 0)
directionDelta Northwest = (-1, 1, 0)
directionDelta Southeast = (1, -1, 0)
directionDelta Southwest = (-1, -1, 0)

-- | Opposite compass direction.
oppositeDirection :: Direction -> Direction
oppositeDirection North     = South
oppositeDirection South     = North
oppositeDirection East      = West
oppositeDirection West      = East
oppositeDirection Up        = Down
oppositeDirection Down      = Up
oppositeDirection Northeast = Southwest
oppositeDirection Southwest = Northeast
oppositeDirection Northwest = Southeast
oppositeDirection Southeast = Northwest

-- | Check whether an exit destination refers to a declared sandbox zone.
isSandboxTarget :: RoomID -> GameWorld -> Bool
isSandboxTarget target gw =
    Map.member target (sandboxZones gw)
    || case stripPrefix "sandbox_" target of
        Just zone | Map.member zone (sandboxZones gw) -> True
        _ -> case parseSandboxRoomId target of
            Just (zone, _, _, _) -> Map.member zone (sandboxZones gw)
            Nothing              -> False

