{-# LANGUAGE OverloadedStrings #-}

-- | Phase 4 (Rogue): pre-run world generator — template schema, parsing and
--   validation. Detail plan sections 1 and 4.
--
--   This module (4b) defines the dungeon template types (@DTemplate@), their
--   'FromJSON' decoding from the same YAML/JSON bridge the adventure schema
--   uses, and 'validateTemplate' — the schema rules from detail plan section 1.
--   The pure generation algorithm lands here from 4c on ('generateDungeon');
--   the CLI wiring follows in 4d.
--
--   Design commitments from the detail plan:
--
--   * Authoring reuses the existing worldbuilder types wherever possible:
--     @room_templates[].room@ is a full @ARoom@ fragment (parsed through the
--     existing FromJSON instance), pool items/NPCs are @AItem@/@ANPC@
--     fragments, @combat:@ is a full @ACombat@ passthrough (combination lever
--     with plan-kampfbildschirm.md), @variables:@ reuses @AVariable@. There is
--     no parallel authoring schema.
--   * Room ids inside @room_templates@ are generator-assigned
--     (@junction_1@, @crypt_3@, ...); an author-set @id@ is rejected
--     ('RoomTemplateIdForbidden').
--   * Validation diagnostics reuse the 'CompileIssue' shape (the detail plan's
--     @TemplateIssue@): paths point into the template file, so the CLI can
--     render them with "Worldbuilder.Locate" exactly like compile issues.
--     Generation errors (@GEUnreachable@ and friends) will be reported with
--     parameters instead of template lines (detail plan section 4).
module Worldbuilder.Generate
    ( DMeta (..)
    , DTemplate (..)
    , DLayout (..)
    , DRoomTemplate (..)
    , DSpecial (..)
    , DStartSpec (..)
    , DBossSpec (..)
    , DTreasureSpec (..)
    , DOnewayHint (..)
    , DItemPoolEntry (..)
    , DNpcPoolEntry (..)
    , DRange (..)
    , parseTemplate
    , validateTemplate
    ) where

import Worldbuilder.Types
    ( AAdventurePlayer
    , ACombat
    , AItem
    , ANPC
    , ARoom
    , AVariable
    , aiId
    , anId
    )
import Worldbuilder.Compile (CompileIssue (..), Severity (..))

import Data.Aeson
    ( FromJSON (..)
    , Value (..)
    , withObject
    , (.!=)
    , (.:)
    , (.:?)
    )
import Data.Aeson.Types (parseEither)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.List (group, sort)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Word (Word64)

-- ---------------------------------------------------------------------------
-- Template types
-- ---------------------------------------------------------------------------

-- | Inclusive integer range, authored as @{min: n, max: m}@, a bare integer
--   (@n@ = @n..n@) or a two-element array (@[a, b]@). @drMax == 'maxBound'@
--   means \"unbounded\" (the @depth_range@ default).
data DRange = DRange
    { drMin :: Int
    , drMax :: Int
    } deriving (Show, Eq)

-- | @template:@ metadata block — the name becomes the worldName (slugged per
--   the Phase 2 rule), the description is copied verbatim.
data DMeta = DMeta
    { dmName        :: String
    , dmDescription :: Maybe String
    } deriving (Show, Eq)

-- | Structural parameters of the dungeon graph.
data DLayout = DLayout
    { dlRoomsMin  :: Int     -- ^ hard floor; below this, generation is GENoSpace
    , dlRoomsMax  :: Int     -- ^ generator may stop earlier (early abort warning)
    , dlDepth     :: Int     -- ^ maximum graph depth (start = depth 1)
    , dlBranching :: Double  -- ^ P(branch) per new room, 0.0 = chain, 1.0 = tree
    , dlLoops     :: Int     -- ^ extra non-tree edges after the spanning tree
    } deriving (Show, Eq)

-- | One room archetype: the building block the generator instantiates.
--   @drtRoomIdSet@ records whether the author illegally set @room.id@.
data DRoomTemplate = DRoomTemplate
    { drtId        :: String
    , drtWeight    :: Int
    , drtDepth     :: DRange      -- ^ depth_range; must lie in 1..layout.depth
    , drtSavezone  :: Bool        -- ^ Phase 1 synergy: instance is a savezone
    , drtRoom      :: ARoom       -- ^ full ARoom fragment, placeholder id injected
    , drtRoomIdSet :: Bool        -- ^ True = author set room.id (validation error)
    } deriving (Show, Eq)

data DStartSpec = DStartSpec
    { dstTemplate :: Maybe String -- ^ absent: generator picks a savezone archetype
    } deriving (Show, Eq)

data DBossSpec = DBossSpec
    { dbsTemplate :: String
    , dbsDepthRaw :: Maybe Value   -- ^ @depth:@ as authored; only @max@ is legal
    } deriving (Show, Eq)

data DTreasureSpec = DTreasureSpec
    { dtsTemplate :: String
    , dtsLocked   :: Bool          -- ^ requires one boss_lock-marked key in the item pool
    } deriving (Show, Eq)

data DSpecial = DSpecial
    { dspStart    :: Maybe DStartSpec
    , dspBoss     :: Maybe DBossSpec
    , dspTreasure :: Maybe DTreasureSpec
    } deriving (Show, Eq)

-- | One-way edge hint (Review-Frage 1): @from@/@to@ are archetype ids
--   (@to@ may be @\"any\"@); the generator converts exactly ONE edge of the
--   archetype into a one-way in @dohDir@ — never all of them (deterministic
--   choice, detail plan section 1).
data DOnewayHint = DOnewayHint
    { dohFrom   :: String
    , dohTo     :: String
    , dohDir    :: String
    , dohOneway :: Bool
    } deriving (Show, Eq)

data DItemPoolEntry = DItemPoolEntry
    { dipItem     :: AItem
    , dipCount    :: DRange
    , dipBossLock :: Maybe String  -- ^ marker: reserved as key; must equal the item id
    } deriving (Show, Eq)

data DNpcPoolEntry = DNpcPoolEntry
    { dnpNpc        :: ANPC
    , dnpCount      :: DRange
    , dnpDepthRange :: Maybe DRange
    , dnpBoss       :: Bool         -- ^ at most one such entry (Review-Frage 4)
    } deriving (Show, Eq)

data DTemplate = DTemplate
    { dtName          :: String
    , dtDescription   :: Maybe String
    , dtLayout        :: DLayout
    , dtRoomTemplates :: [DRoomTemplate]
    , dtSpecial       :: DSpecial
    , dtOnewayHints   :: [DOnewayHint]
    , dtItemPool      :: [DItemPoolEntry]
    , dtNpcPool       :: [DNpcPoolEntry]
    , dtCombat        :: Maybe ACombat  -- ^ 1:1 passthrough to advCombat
    , dtPlayer        :: Maybe AAdventurePlayer
    , dtVariables     :: [AVariable]
    , dtSeed          :: Maybe Word64   -- ^ CLI --seed overrides; absence errors in 4d
    } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- FromJSON instances
-- ---------------------------------------------------------------------------

instance FromJSON DRange where
    parseJSON (Number n)  = pure (DRange (round n) (round n))
    parseJSON v@(Array _) = do
        xs <- parseJSON v
        case xs :: [Value] of
            [x, y] -> DRange <$> parseJSON x <*> parseJSON y
            _      -> fail "range must have exactly two elements"
    parseJSON v = withObject "range" (\o -> DRange
        <$> o .: "min"
        <*> o .: "max") v

instance FromJSON DLayout where
    parseJSON = withObject "layout" $ \o -> do
        rooms <- o .:? "rooms" .!= DRange 12 20
        DLayout (drMin rooms) (drMax rooms)
            <$> o .:? "depth"     .!= 4
            <*> o .:? "branching" .!= 0.35
            <*> o .:? "loops"     .!= 2

-- | A room fragment is an ordinary @ARoom@ object — except that @id@ is
--   generator-assigned. The existing FromJSON requires @id@, so a placeholder
--   is injected before decoding; whether the author set one is reported
--   separately ('drtRoomIdSet').
instance FromJSON DRoomTemplate where
    parseJSON = withObject "room_template" $ \o -> do
        roomVal <- o .: "room"
        (room, roomIdSet) <- case roomVal of
            Object om -> pure
                ( Object (KM.insert (K.fromString "id") (String "<generator>") om)
                , KM.member (K.fromString "id") om )
            _ -> fail "room_templates[].room must be a mapping"
        room' <- parseJSON room
        DRoomTemplate
            <$> o .:? "id"          .!= ""
            <*> o .:? "weight"      .!= 1
            <*> o .:? "depth_range" .!= DRange 1 maxBound
            <*> o .:? "savezone"    .!= False
            <*> pure room'
            <*> pure roomIdSet

instance FromJSON DStartSpec where
    parseJSON = withObject "start" $ \o -> DStartSpec
        <$> o .:? "template"

instance FromJSON DBossSpec where
    parseJSON = withObject "boss" $ \o -> DBossSpec
        <$> o .:  "template"
        <*> o .:? "depth"

instance FromJSON DTreasureSpec where
    parseJSON = withObject "treasure" $ \o -> DTreasureSpec
        <$> o .:  "template"
        <*> o .:? "locked" .!= False

instance FromJSON DSpecial where
    parseJSON = withObject "special" $ \o -> DSpecial
        <$> o .:? "start"
        <*> o .:? "boss"
        <*> o .:? "treasure"

instance FromJSON DOnewayHint where
    parseJSON = withObject "oneway_hint" $ \o -> DOnewayHint
        <$> o .:  "from"
        <*> o .:? "to"     .!= "any"
        <*> o .:  "dir"
        <*> o .:? "oneway" .!= True

instance FromJSON DItemPoolEntry where
    parseJSON = withObject "item_pool_entry" $ \o -> DItemPoolEntry
        <$> o .:  "item"
        <*> o .:? "count"     .!= DRange 1 1
        <*> o .:? "boss_lock"

instance FromJSON DNpcPoolEntry where
    parseJSON = withObject "npc_pool_entry" $ \o -> DNpcPoolEntry
        <$> o .:  "npc"
        <*> o .:? "count"       .!= DRange 1 1
        <*> o .:? "depth_range"
        <*> o .:? "boss"        .!= False

instance FromJSON DMeta where
    parseJSON = withObject "template_meta" $ \o -> DMeta
        <$> o .:? "name"        .!= ""
        <*> o .:? "description"

instance FromJSON DTemplate where
    parseJSON = withObject "dungeon_template" $ \o -> do
        meta <- o .:? "template" .!= DMeta "" Nothing
        seed <- o .:? "seed"
        DTemplate (dmName meta) (dmDescription meta)
            <$> o .:? "layout"         .!= DLayout 12 20 4 0.35 2
            <*> o .:? "room_templates" .!= []
            <*> o .:? "special"        .!= DSpecial Nothing Nothing Nothing
            <*> o .:? "oneway_hints"   .!= []
            <*> o .:? "item_pool"      .!= []
            <*> o .:? "npc_pool"       .!= []
            <*> o .:? "combat"
            <*> o .:? "player"
            <*> o .:? "variables"      .!= []
            <*> pure (seedToWord64 <$> seed)

-- | The seed may be authored as a JSON number or a decimal string.
seedToWord64 :: Value -> Word64
seedToWord64 (Number n) = fromIntegral (round n :: Integer)
seedToWord64 (String s) = fromIntegral (readSeed (T.unpack s))
seedToWord64 _          = 0

-- | Decimal string to integer, 0 on garbage (nonsensical seeds are rejected
--   by the CLI rules in 4d, not here — parsing stays total).
readSeed :: String -> Integer
readSeed s = case reads s of
    [(n, "")] -> n
    _         -> 0

-- ---------------------------------------------------------------------------
-- Entry points
-- ---------------------------------------------------------------------------

-- | Decode a template value (produced by the same @Data.YAML.Aeson@ bridge
--   the adventure parser uses). Parse failures are plain strings — the YAML
--   error already carries position information.
parseTemplate :: Value -> Either String DTemplate
parseTemplate = parseEither parseJSON

-- | Schema rules (detail plan section 1). An empty result means the template
--   is well-formed; generation may still early-abort with a warning, and the
--   compiled output is validated by @compileAdventure@ as usual.
--
--   Codes (stable, like all worldbuilder codes):
--
--   * @TemplateNameMissing@ — @template.name@ empty
--   * @LayoutTooSmall@ / @LayoutBounds@ / @LayoutDepth@ / @LayoutBranching@ /
--     @LayoutLoops@ — @layout:@ range rules (incl. @rooms.min >= 3@)
--   * @NoRoomTemplates@ / @MissingRoomTemplateId@ / @DuplicateRoomTemplateId@
--   * @RoomTemplateIdForbidden@ — author set @room.id@ (generator assigns)
--   * @InvalidWeight@ — @weight < 1@
--   * @DepthRangeOutside@ — @depth_range@ not within 1..layout.depth
--   * @UnknownRoomTemplate@ — @special.*@ / @oneway_hints[]@ reference an
--     archetype id that does not exist
--   * @BossDepthSpec@ — @special.boss.depth@ present but not @max@
--   * @BossLockMissingKey@ / @BossLockMismatch@ — treasure lock without a
--     reserved key / marker does not match its own item id
--   * @InvalidCount@ — pool @count@ with @min > max@
--   * @DuplicatePoolItemId@ / @DuplicatePoolNpcId@
--   * @MultipleBossNpcs@ — more than one @boss: true@ pool entry (Review-
--     Frage 4: exactly one)
--   * @UnknownDirection@ — @oneway_hints[].dir@ outside the direction set
validateTemplate :: DTemplate -> [CompileIssue]
validateTemplate t = concat
    [ metaIssues
    , layoutIssues
    , archetypeIssues
    , specialIssues
    , onewayIssues
    , itemPoolIssues
    , npcPoolIssues
    ]
  where
    archIds = map drtId (dtRoomTemplates t)
    archExists a = a `elem` archIds
    layout = dtLayout t
    sp = dtSpecial t

    ti path code msg = CompileIssue path SError code msg

    metaIssues =
        [ ti "template.name" "TemplateNameMissing"
            "template.name must not be empty (becomes worldName)"
        | null (dtName t)
        ]

    layoutIssues = concat
        [ [ ti "layout.rooms" "LayoutTooSmall"
                "layout.rooms.min must be at least 3 (start + 1 room + boss)"
          | dlRoomsMin layout < 3 ]
        , [ ti "layout.rooms" "LayoutBounds"
                "layout.rooms.min must not exceed layout.rooms.max"
          | dlRoomsMin layout > dlRoomsMax layout ]
        , [ ti "layout.depth" "LayoutDepth"
                "layout.depth must be at least 1 (start = depth 1)"
          | dlDepth layout < 1 ]
        , [ ti "layout.branching" "LayoutBranching"
                "layout.branching must lie in 0.0..1.0 (0 = chain, 1 = tree)"
          | let b = dlBranching layout, b < 0 || b > 1 ]
        , [ ti "layout.loops" "LayoutLoops"
                "layout.loops must not be negative"
          | dlLoops layout < 0 ]
        ]

    archetypeIssues
        | null (dtRoomTemplates t) =
            [ ti "room_templates" "NoRoomTemplates"
                "at least one room_templates entry is required" ]
        | otherwise = concat
            [ [ ti "room_templates" "MissingRoomTemplateId"
                    "every room_templates entry needs an id"
              | any (null . drtId) (dtRoomTemplates t) ]
            , [ ti "room_templates" "DuplicateRoomTemplateId"
                    ("duplicate room_templates id '" ++ a ++ "'")
              | a <- nubDup (map drtId (dtRoomTemplates t)) ]
            , concatMap perTemplate (dtRoomTemplates t)
            ]
      where
        perTemplate rt = concat
            [ [ ti ("room_templates." ++ drtId rt ++ ".id") "RoomTemplateIdForbidden"
                    ("room_templates." ++ drtId rt
                     ++ ": room.id is generator-assigned — remove it from the room fragment")
              | drtRoomIdSet rt ]
            , [ ti ("room_templates." ++ drtId rt ++ ".weight") "InvalidWeight"
                    "weight must be at least 1"
              | drtWeight rt < 1 ]
            , [ ti ("room_templates." ++ drtId rt ++ ".depth_range") "DepthRangeOutside"
                    ("depth_range must lie within 1.." ++ show (dlDepth layout)
                     ++ " (layout.depth)")
              | not (depthInRange (drtDepth rt)) ]
            ]

        -- depth_range within 1..layout.depth; drMax == maxBound = unbounded
        depthInRange r =
            drMin r >= 1
            && drMin r <= drMax r
            && (if drMax r == maxBound
                    then drMin r <= dlDepth layout
                    else drMax r <= dlDepth layout)

    specialIssues = concat
        [ [ ti "special.start.template" "UnknownRoomTemplate"
                ("special.start references unknown room_template '" ++ a ++ "'")
          | Just st <- [dspStart sp]
          , Just a <- [dstTemplate st]
          , not (archExists a) ]
        , [ ti "special.boss.template" "UnknownRoomTemplate"
                ("special.boss references unknown room_template '" ++ a ++ "'")
          | Just bs <- [dspBoss sp]
          , let a = dbsTemplate bs
          , not (archExists a) ]
        , [ ti "special.boss.depth" "BossDepthSpec"
                "special.boss.depth must be 'max' (the boss always sits at maximum depth)"
          | Just bs <- [dspBoss sp]
          , Just d <- [dbsDepthRaw bs]
          , d /= String "max" ]
        , [ ti "special.treasure.template" "UnknownRoomTemplate"
                ("special.treasure references unknown room_template '" ++ a ++ "'")
          | Just ts <- [dspTreasure sp]
          , let a = dtsTemplate ts
          , not (archExists a) ]
        , [ ti "special.treasure.locked" "BossLockMissingKey"
                ("special.treasure is locked but no item_pool entry carries "
                 ++ "boss_lock: <item-id> (pairing rule)")
          | Just ts <- [dspTreasure sp]
          , dtsLocked ts
          , null (mapMaybe dipBossLock (dtItemPool t)) ]
        ]

    onewayIssues = concatMap perHint (zip [0 :: Int ..] (dtOnewayHints t))
      where
        perHint (i, h) =
            let base = "oneway_hints." ++ show i
            in concat
                [ [ ti (base ++ ".from") "UnknownRoomTemplate"
                        ("oneway hint 'from' references unknown room_template '"
                         ++ dohFrom h ++ "'")
                  | not (archExists (dohFrom h)) ]
                , [ ti (base ++ ".to") "UnknownRoomTemplate"
                        ("oneway hint 'to' references unknown room_template '"
                         ++ dohTo h ++ "'")
                  | dohTo h /= "any", not (archExists (dohTo h)) ]
                , [ ti (base ++ ".dir") "UnknownDirection"
                        ("unknown direction '" ++ dohDir h ++ "'")
                  | not (knownDirection (dohDir h)) ]
                ]

    itemPoolIssues = concat
        [ [ ti "item_pool" "DuplicatePoolItemId"
                ("duplicate item id '" ++ i ++ "' in item_pool")
          | i <- nubDup (map (aiId . dipItem) (dtItemPool t)) ]
        , concatMap perEntry (dtItemPool t)
        ]
      where
        perEntry e =
            let iid = aiId (dipItem e)
                base = "item_pool." ++ iid
            in concat
                [ [ ti (base ++ ".count") "InvalidCount"
                        "count.min must not exceed count.max"
                  | badRange (dipCount e) ]
                , [ ti (base ++ ".boss_lock") "BossLockMismatch"
                        ("boss_lock marker '" ++ fromMaybeMaybe (dipBossLock e)
                         ++ "' must equal this entry's item id '" ++ iid ++ "'")
                  | Just lock <- [dipBossLock e], lock /= iid ]
                ]

    npcPoolIssues = concat
        [ [ ti "npc_pool" "DuplicatePoolNpcId"
                ("duplicate npc id '" ++ n ++ "' in npc_pool")
          | n <- nubDup (map (anId . dnpNpc) (dtNpcPool t)) ]
        , [ ti ("npc_pool." ++ anId (dnpNpc e) ++ ".count") "InvalidCount"
                "count.min must not exceed count.max"
          | e <- dtNpcPool t, badRange (dnpCount e) ]
        , [ ti "npc_pool" "MultipleBossNpcs"
                "at most one npc_pool entry may carry boss: true (Review-Frage 4)"
          | length (filter dnpBoss (dtNpcPool t)) > 1 ]
        ]

    -- helpers ---------------------------------------------------------------

    badRange r = drMin r < 0 || drMin r > drMax r

    -- Known directions (mirror of Worldbuilder.Compile.parseDir — not
    -- exported there). Grid generation only uses N/S/E/W; 'down' is the
    -- classic fall-trap direction.
    knownDirection d = d `elem`
        [ "north", "south", "east", "west", "up", "down"
        , "northeast", "northwest", "southeast", "southwest"
        , "ne", "nw", "se", "sw" ]

-- | Ids occurring more than once, in deterministic (sorted) order.
nubDup :: Ord a => [a] -> [a]
nubDup = map head . filter ((> 1) . length) . group . sort

fromMaybeMaybe :: Show a => Maybe a -> String
fromMaybeMaybe (Just x) = show x
fromMaybeMaybe Nothing  = "<nothing>"
