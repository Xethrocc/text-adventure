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
    , DLevel (..)
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
    , GenerateError (..)
    , Cell3
    , Cell
    , PlannedRoom (..)
    , DEdge (..)
    , DungeonPlan (..)
    , generateDungeonLayout
    , genMaxRetries
    , ResolvedLevel (..)
    , resolveLevels
    , placeLevelRooms
    , placeRooms
    , assignArchetypes
    , connectRooms
    , ensureReachable
    , cellRoomIds
    , generateDungeon
    ) where

import Worldbuilder.Types
    ( AAdventurePlayer
    , ACombat
    , AItem (..)
    , ANPC (..)
    , ARoom
    , AVariable
    , AActionOutcome (..)
    , AExitRef (..)
    , AGamePolicy (..)
    , Adventure (..)
    , AAscii (..)
    , ACondText (..)
    , aiId
    , aiLocation
    , aiOnTake
    , anId
    , anLocation
    , arId
    , arName
    , arExits
    , arFloor
    )
import Worldbuilder.Compile (CompileIssue (..), Severity (..))
import Worldbuilder.Rng (Rng, newRng, pickWeighted, randInt, shuffle, deriveRuntimeSeed)

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
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (group, sort, sortOn)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
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

data DLevel = DLevel
    { dlvRooms        :: Maybe DRange   -- ^ room count range for this level (or absent: auto-distribution)
    , dlvDepth        :: Maybe Int      -- ^ maximum tree depth within this level (levels[i].depth)
    , dlvBranching    :: Maybe Double   -- ^ P(branch) override for this level
    , dlvReturnStairs :: Bool           -- ^ True = generate upward stairs back to level i-1 (default False)
    } deriving (Show, Eq)

-- | Resolved layout parameters for a single dungeon level / floor (Phase 4b).
data ResolvedLevel = ResolvedLevel
    { rlIndex        :: Int      -- ^ 1..N
    , rlRoomsMin     :: Int
    , rlRoomsMax     :: Int
    , rlBranching    :: Double
    , rlDepthLimit   :: Int
    , rlReturnStairs :: Bool
    } deriving (Show, Eq)

-- | Resolves the level configurations from a template's @layout:@ and @levels:@.
--   When @levels:@ is absent, returns a single level matching the Phase 4 layout.
--   When @levels:@ is present, missing entries are filled by distributing the
--   remaining room budget deterministically across the unspecified levels.
resolveLevels :: DTemplate -> [ResolvedLevel]
resolveLevels t
    | null (dtLevels t) =
        [ ResolvedLevel
            1
            (dlRoomsMin layout)
            (dlRoomsMax layout)
            (dlBranching layout)
            (dlDepth layout)
            False
        ]
    | otherwise =
        let numLevels = dlDepth layout
            explicitLevels = dtLevels t
            levelCfg i = if i <= length explicitLevels
                         then Just (explicitLevels !! (i - 1))
                         else Nothing
            sumExplicitMin = sum [ drMin r | i <- [1 .. numLevels]
                                 , Just lv <- [levelCfg i]
                                 , Just r  <- [dlvRooms lv] ]
            sumExplicitMax = sum [ drMax r | i <- [1 .. numLevels]
                                 , Just lv <- [levelCfg i]
                                 , Just r  <- [dlvRooms lv] ]
            unspecifiedCount = length [ () | i <- [1 .. numLevels]
                                      , case levelCfg i of
                                          Just lv -> dlvRooms lv == Nothing
                                          Nothing -> True ]
            remMin = max 0 (dlRoomsMin layout - sumExplicitMin)
            remMax = max remMin (dlRoomsMax layout - sumExplicitMax)
            
            calcRooms i kUnspec = case levelCfg i of
                Just lv | Just r <- dlvRooms lv -> (drMin r, drMax r, kUnspec)
                _ ->
                    let minR = max 1 (remMin `div` max 1 unspecifiedCount
                               + if kUnspec < (remMin `mod` max 1 unspecifiedCount) then 1 else 0)
                        maxR = max minR (remMax `div` max 1 unspecifiedCount
                               + if kUnspec < (remMax `mod` max 1 unspecifiedCount) then 1 else 0)
                    in (minR, maxR, kUnspec + 1)
            
            build [] _ = []
            build (i : is) k =
                let (minR, maxR, k') = calcRooms i k
                    cfg = levelCfg i
                    br  = fromMaybe (dlBranching layout) (cfg >>= dlvBranching)
                    dp  = fromMaybe maxBound (cfg >>= dlvDepth)
                    ret = maybe False dlvReturnStairs cfg
                    rl  = ResolvedLevel i minR maxR br dp ret
                in rl : build is k'
        in build [1 .. numLevels] 0
  where
    layout = dtLayout t

data DTemplate = DTemplate
    { dtName          :: String
    , dtDescription   :: Maybe String
    , dtLayout        :: DLayout
    , dtLevels        :: [DLevel]       -- ^ optional per-level layouts (Phase 4b)
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

instance FromJSON DLevel where
    parseJSON = withObject "level" $ \o -> DLevel
        <$> o .:? "rooms"
        <*> o .:? "depth"
        <*> o .:? "branching"
        <*> o .:? "return_stairs" .!= False

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
            <*> o .:? "levels"         .!= []
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
    , levelsIssues
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

    levelsIssues = concat
        [ [ ti "levels" "LevelsExceedDepth"
                ("levels count (" ++ show (length (dtLevels t))
                 ++ ") must not exceed layout.depth (" ++ show (dlDepth layout) ++ ")")
          | length (dtLevels t) > dlDepth layout ]
        , concatMap perLevel (zip [0 :: Int ..] (dtLevels t))
        ]
      where
        perLevel (i, lv) =
            let base = "levels." ++ show i
            in concat
                [ [ ti (base ++ ".rooms") "InvalidCount"
                        "rooms.min must be at least 1 and not exceed rooms.max"
                  | Just r <- [dlvRooms lv], drMin r < 1 || drMin r > drMax r ]
                , [ ti (base ++ ".branching") "LayoutBranching"
                        "branching must lie in 0.0..1.0 (0 = chain, 1 = tree)"
                  | Just b <- [dlvBranching lv], b < 0 || b > 1 ]
                , [ ti (base ++ ".depth") "LayoutDepth"
                        "depth must be at least 1"
                  | Just d <- [dlvDepth lv], d < 1 ]
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
        , [ ti ("npc_pool." ++ anId (dnpNpc e) ++ ".depth_range") "DepthRangeOutside"
                ("depth_range must lie within 1.." ++ show (dlDepth layout)
                 ++ " (layout.depth)")
          | e <- dtNpcPool t, Just r <- [dnpDepthRange e], not (depthInRange r) ]
        ]

    -- helpers ---------------------------------------------------------------

    badRange r = drMin r < 0 || drMin r > drMax r

    -- depth_range within 1..layout.depth; drMax == maxBound = unbounded
    depthInRange r =
        drMin r >= 1
        && drMin r <= drMax r
        && (if drMax r == maxBound
                then drMin r <= dlDepth layout
                else drMax r <= dlDepth layout)

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

-- ---------------------------------------------------------------------------
-- Rogue Phase 4c: pure generation core (detail plan section 3, steps 1-4)
-- ---------------------------------------------------------------------------

-- | Generation errors (detail plan section 3). @GENoSpace@ is only produced
--   when not even @layout.rooms.min@ cells are placeable — anything beyond
--   that is the early-abort warning path (@GeneratorEarlyAbort@), never a
--   hard error (detail plan clarification).
data GenerateError
    = GETemplate [CompileIssue]   -- ^ schema violations (from 'validateTemplate')
    | GEUnreachable [Cell]        -- ^ cells that could not be connected (generator bug)
    | GENoSpace                   -- ^ fewer than @rooms.min@ cells placeable
    deriving (Show, Eq)

-- | One placed grid cell: archetype id + generation depth (start = 1).
data PlannedRoom = PlannedRoom
    { prArch  :: String
    , prDepth :: Int
    } deriving (Show, Eq)

-- | A 3D grid cell: @(x, y, level)@. Startebene = 1 (Phase 4b).
--   @Data.Map@ iteration order (lexicographic) is the single
--   source of determinism — never @HashMap@ (detail plan section 7).
type Cell3 = (Int, Int, Int)

-- | Grid cell type (Phase 4b: 3D grid coordinate).
type Cell = Cell3

-- | One connection between two cells. Bidirectional edges (see 'deOneway')
--   emit two @AExitRef@s; one-way edges emit only the forward direction
--   (fall traps — Review decision 1). 'deDir' is the forward direction name.
data DEdge = DEdge
    { deFrom   :: Cell
    , deTo     :: Cell
    , deDir    :: String
    , deOneway :: Bool
    } deriving (Show, Eq)

-- | The pure result of layout generation: everything 'emitAdventure' (4d)
--   needs to build the @Adventure@ record.
data DungeonPlan = DungeonPlan
    { dpGrid         :: Map Cell PlannedRoom
    , dpEdges        :: [DEdge]
    , dpWarnings     :: [CompileIssue]  -- ^ e.g. GeneratorEarlyAbort, OnewayHintUnmatched
    , dpStartCell    :: Cell            -- ^ always @(0, 0, 1)@, depth 1
    , dpBossCell     :: Cell
    , dpTreasureCell :: Maybe Cell
    , dpEarlyAbort   :: Bool            -- ^ reached depth < layout.depth
    , dpMaxDepth     :: Int             -- ^ deepest placed cell (= boss depth)
    } deriving (Show, Eq)

-- | The four cardinal directions in deterministic draw order (north first —
--   the order is part of the generation algorithm's definition).
gridDirNames :: [String]
gridDirNames = ["north", "south", "east", "west"]

-- | Offset of a cardinal or vertical direction (screen coordinates: north = -y; down = +z level).
stepCell :: Cell -> String -> Cell
stepCell (x, y, z) dir = case dir of
    "north" -> (x, y - 1, z)
    "south" -> (x, y + 1, z)
    "east"  -> (x + 1, y, z)
    "west"  -> (x - 1, y, z)
    "down"  -> (x, y, z + 1)
    "up"    -> (x, y, z - 1)
    _       -> (x, y, z)  -- guarded by callers

-- | Direction name between two cells (cardinal, diagonal, or vertical).
gridDirBetween :: Cell -> Cell -> String
gridDirBetween (x1, y1, z1) (x2, y2, z2)
    | z2 > z1 && x2 == x1 && y2 == y1 = "down"
    | z2 < z1 && x2 == x1 && y2 == y1 = "up"
    | z2 == z1 && x2 == x1 && y2 == y1 - 1 = "north"
    | z2 == z1 && x2 == x1 && y2 == y1 + 1 = "south"
    | z2 == z1 && x2 == x1 + 1 && y2 == y1 = "east"
    | z2 == z1 && x2 == x1 - 1 && y2 == y1 = "west"
    | z2 == z1 && x2 == x1 + 1 && y2 == y1 - 1 = "northeast"
    | z2 == z1 && x2 == x1 - 1 && y2 == y1 - 1 = "northwest"
    | z2 == z1 && x2 == x1 + 1 && y2 == y1 + 1 = "southeast"
    | z2 == z1 && x2 == x1 - 1 && y2 == y1 + 1 = "southwest"
    | z2 > z1 = "down"
    | z2 < z1 = "up"
    | otherwise = "east"  -- unreachable: callers only pair neighbour cells

-- | Step 1 — place rooms on grid coordinates. Start at @(0,0)@ (depth 1);
--   iteratively expand the front-most frontier cell in a random free cardinal
--   direction; @branching@ decides whether the new cell *joins* the frontier
--   (tree growth) or *replaces* the current one (chain). Stops at
--   @rooms.max@ or when the frontier runs dry.
--
--   Pure geometry: archetypes are assigned separately ('assignArchetypes') so
--   this step is testable as pure layout logic. Result: cell -> depth.
-- | Step 1 (per level) — place rooms on 2D grid coordinates for a specific level.
--   Start at @(0, 0, level)@ (depth 1); iteratively expand the front-most frontier
--   cell in a random free cardinal direction; @branching@ decides whether the new
--   cell *joins* the frontier (tree growth) or *replaces* the current one (chain).
--   Stops at @maxRooms@ or when the frontier runs dry. Result: cell -> depth within level.
placeLevelRooms :: Int -> Int -> Double -> Int -> Rng -> (Map Cell Int, Rng)
placeLevelRooms level maxRooms branching depthLimit r0 =
    go [(0, 0, level)] (Map.singleton (0, 0, level) 1) 1 r0
  where
    go frontier cells count r
        | count >= maxRooms = (cells, r)
        | otherwise = case frontier of
            [] -> (cells, r)
            (h : rest) ->
                let d = cells Map.! h
                in if d >= depthLimit
                    then go rest cells count r  -- head exhausted (max depth)
                    else
                        let free = [ stepCell h dir
                                   | dir <- gridDirNames
                                   , not (Map.member (stepCell h dir) cells) ]
                        in case free of
                            [] -> go rest cells count r  -- head is walled in
                            _  ->
                                let (pick, r1) = randInt 0 (length free - 1) r
                                    target = free !! pick
                                    bp = floor (branching * 10000) :: Int
                                    (branch, r2)
                                        | bp <= 0    = (False, r1)
                                        | bp >= 10000 = (True, r1)
                                        | otherwise =
                                            let (draw, r') = randInt 1 10000 r1
                                            in (draw <= bp, r')
                                    cells' = Map.insert target (d + 1) cells
                                    childSterile = d + 1 >= depthLimit
                                    frontier'
                                        | bp <= 0 =
                                            if childSterile then rest else target : rest
                                        | branch =
                                            h : (if childSterile then rest else rest ++ [target])
                                        | otherwise =
                                            (if childSterile then rest else rest ++ [target]) ++ [h]
                                in go frontier' cells' (count + 1) r2

-- | Step 1 — place rooms on grid coordinates (Phase 4 compatibility).
placeRooms :: DLayout -> Rng -> (Map Cell Int, Rng)
placeRooms layout = placeLevelRooms 1 (dlRoomsMax layout) (dlBranching layout) (dlDepth layout)

-- | Step 1 (multi-level) — place rooms across all resolved levels of a template.
placeTemplateRooms :: DTemplate -> Rng -> (Map Cell Int, Rng)
placeTemplateRooms t r0
    | null (dtLevels t) = placeRooms (dtLayout t) r0
    | otherwise         =
        let levels = resolveLevels t
            step (accMap, r) rl =
                let (lvlMap, r') = placeLevelRooms
                        (rlIndex rl)
                        (rlRoomsMax rl)
                        (rlBranching rl)
                        (rlDepthLimit rl)
                        r
                in (Map.union accMap lvlMap, r')
        in foldl step (Map.empty, r0) levels

-- | Step 2 — assign archetypes. Start gets @special.start.template@ (or the
--   first sorted savezone archetype, or the first archetype), the boss cell
--   gets @special.boss.template@; everything else is drawn weighted, filtered
--   by the archetype's @depth_range@ (fallback: full weighted set).
--   The treasure cell is the deepest free cell (tie: smallest coordinate) —
--   no RNG needed.
assignArchetypes :: DTemplate -> Map Cell Int -> Rng
                 -> (Map Cell PlannedRoom, Cell, Maybe Cell, Rng)
assignArchetypes t depths r0 =
    let isMultiLevel = not (null (dtLevels t))
        maxLevel = maximum [ z | (_, _, z) <- Map.keys depths ]
        lastLevelCells = [ c | c@(_, _, z) <- Map.keys depths, z == maxLevel ]
        bossCell = head (sortByDepthDesc depths lastLevelCells)
        archIds  = map drtId (dtRoomTemplates t)
        startArch = case dstTemplate =<< dspStart (dtSpecial t) of
            Just a  -> a
            Nothing -> case [ drtId rt | rt <- dtRoomTemplates t, drtSavezone rt ] of
                (a : _) -> a
                []      -> case archIds of
                    (a : _) -> a
                    []      -> "room"  -- NoRoomTemplates rejects this earlier
        treasureCell = case dspTreasure (dtSpecial t) of
            Nothing -> Nothing
            Just _  -> case [ c | c <- Map.keys depths, c /= (0, 0, 1), c /= bossCell ] of
                [] -> Nothing
                cs -> Just (head (sortByDepthDesc depths cs))
        bossArch = maybe "room" dbsTemplate (dspBoss (dtSpecial t))
        treasureArch = maybe "room" dtsTemplate (dspTreasure (dtSpecial t))
        depthFits rng d = d >= drMin rng
                          && (drMax rng == maxBound || d <= drMax rng)
        -- weighted draw for a non-special cell at depth d (or level z in multi-level mode)
        drawArch c treeDepth r =
            let targetDepth = if isMultiLevel then (\(_, _, z) -> z) c else treeDepth
                fitting = [ (drtWeight rt, drtId rt) | rt <- dtRoomTemplates t
                          , depthFits (drtDepth rt) targetDepth ]
                pool = case fitting of
                    [] -> [ (drtWeight rt, drtId rt) | rt <- dtRoomTemplates t ]
                    _  -> fitting
            in case pool of
                []       -> (startArch, r)  -- no archetypes at all (rejected earlier)
                nonempty -> pickWeighted nonempty r
        build r = foldl step (Map.empty, r) (Map.keys depths)
        step (acc, r) c
            | c == (0, 0, 1)   = (Map.insert c (PlannedRoom startArch 1) acc, r)
            | c == bossCell    = (Map.insert c (PlannedRoom bossArch (depths Map.! bossCell)) acc, r)
            | Just c == treasureCell =
                (Map.insert c (PlannedRoom treasureArch (depths Map.! c)) acc, r)
            | otherwise =
                let (a, r') = drawArch c (depths Map.! c) r
                in (Map.insert c (PlannedRoom a (depths Map.! c)) acc, r')
        (grid, r1) = build r0
    in (grid, bossCell, treasureCell, r1)

-- | Sort cells by level (descending), then depth (descending), then coordinate (ascending).
sortByDepthDesc :: Map Cell Int -> [Cell] -> [Cell]
sortByDepthDesc depths cs =
    map (\(_, _, c) -> c)
        (sort [ (negate z, negate (depths Map.! c), c) | c@(_, _, z) <- cs ])

-- | Step 3 — edges from grid adjacency (cardinal, bidirectional), then
--   @layout.loops@ extra diagonal edges (Chebyshev-1 pairs are never directly
--   connected, so they add real cycles while staying grid-consistent via the
--   engine's ne/nw/se/sw directions), then one-way hints (exactly one edge
--   per hint, forward direction renamed to the authored direction).
connectRooms :: DTemplate -> Map Cell PlannedRoom -> Rng
             -> ([DEdge], Rng, [CompileIssue])
connectRooms t grid r0 =
    let cardinalEdges =
            [ DEdge c c' (gridDirBetween c c') False
            | c <- Map.keys grid
            , dir <- ["east", "south"]
            , let c' = stepCell c dir
            , Map.member c' grid ]
        diagonalPairs =
            [ (c, c')
            | c@(x, y, z) <- Map.keys grid
            , (dx, dy) <- [(1, 1), (-1, 1)]  -- southeast / southwest offsets
            , let c' = (x + dx, y + dy, z)
            , Map.member c' grid ]
        (loopPairs, r1) = chooseLoops (dlLoops (dtLayout t)) diagonalPairs r0
        loopEdges =
            [ DEdge a b (gridDirBetween a b) False | (a, b) <- loopPairs ]
        (intraEdges, warns) = foldl applyHint (cardinalEdges ++ loopEdges, [])
                                (zip [0 :: Int ..] (dtOnewayHints t))
        applyHint (es, ws) (i, h)
            | not (dohOneway h) = (es, ws)   -- hint without oneway is inert
            | otherwise =
                let archOf c = maybe "" prArch (Map.lookup c grid)
                    matches e =
                        not (deOneway e)
                        && archOf (deFrom e) == dohFrom h
                        && (dohTo h == "any" || archOf (deTo e) == dohTo h)
                in case filter matches es of
                    [] -> (es, ws ++ [ CompileIssue
                            ("oneway_hints." ++ show i) SWarning
                            "OnewayHintUnmatched"
                            ("no bidirectional edge from archetype '" ++ dohFrom h
                             ++ "' to convert — hint ignored") ])
                    (e : _) ->
                        -- exactly one edge per hint, deterministic (first in
                        -- sorted edge order); the forward direction is renamed
                        -- to the authored direction (e.g. a fall trap 'down')
                        let converted = e { deDir = dohDir h, deOneway = True }
                        in (map (\x -> if x == e then converted else x) es, ws)

        -- Inter-level stair edges (Phase 4b)
        levelsInGrid = Set.toAscList (Set.fromList [ z | (_, _, z) <- Map.keys grid ])
        resolved = resolveLevels t
        stairEdges = concat
            [ let cellsA = [ c | c@(_, _, z) <- Map.keys grid, z == lvlA ]
                  deepestA = head (sortOn (\c -> (negate (prDepth (grid Map.! c)), c)) cellsA)
                  startB = if Map.member (0, 0, lvlB) grid
                           then (0, 0, lvlB)
                           else head (sortOn (\c -> (prDepth (grid Map.! c), c))
                                       [ c | c@(_, _, z) <- Map.keys grid, z == lvlB ])
                  downEdge = DEdge deepestA startB "down" True
                  hasReturn = (lvlA <= length resolved && rlReturnStairs (resolved !! (lvlA - 1)))
                              || (lvlB <= length resolved && rlReturnStairs (resolved !! (lvlB - 1)))
                  upEdge = DEdge startB deepestA "up" True
              in if null cellsA then []
                 else if hasReturn then [downEdge, upEdge]
                 else [downEdge]
            | (lvlA, lvlB) <- zip levelsInGrid (drop 1 levelsInGrid)
            , lvlB == lvlA + 1
            ]
        edges = intraEdges ++ stairEdges
    in (edges, r1, warns)

-- | Choose up to @n@ loop pairs: prefer equal-depth pairs, then any.
chooseLoops :: Int -> [(Cell, Cell)] -> Rng -> ([(Cell, Cell)], Rng)
chooseLoops n cands r0
    | n <= 0 || null cands = ([], r0)
    | otherwise =
        let (shuffled, r1) = shuffle cands r0
        in (take n shuffled, r1)

-- | Step 4 — BFS over the *directed* graph (one-way edges followed in their
--   direction only, locks ignored); unconnected cells are docked to the next
--   reachable grid neighbour with a fresh bidirectional edge. Returns the
--   repaired edge list, or the cells that could not be fixed (a generator
--   bug — the pure grid is always connected).
ensureReachable :: Map Cell PlannedRoom -> [DEdge] -> ([DEdge], Maybe [Cell])
ensureReachable grid edges0 = go edges0
  where
    start = (0, 0, 1)
    go edges =
        let reach = bfsReach (successorsOf edges) start
            bad = sort [ c | c <- Map.keys grid, not (Set.member c reach) ]
        in case bad of
            [] -> (edges, Nothing)
            _  ->
                let docks = [ (c, n)
                            | c <- bad
                            , n <- take 1 [ n' | dir <- gridDirNames
                                           , let n' = stepCell c dir
                                           , Map.member n' grid
                                           , Set.member n' reach ] ]
                in case docks of
                    [] -> (edges, Just bad)
                    _  ->
                        let newEdges =
                                [ DEdge n c (gridDirBetween n c) False | (c, n) <- docks ]
                        in go (edges ++ newEdges)

-- | Directed successor map: bidirectional edges contribute both directions,
--   one-way edges only the forward one.
successorsOf :: [DEdge] -> Map Cell [Cell]
successorsOf edges = Map.fromListWith (++) (concatMap f edges)
  where
    f e | deOneway e = [(deFrom e, [deTo e])]
        | otherwise  = [(deFrom e, [deTo e]), (deTo e, [deFrom e])]

-- | BFS over a successor map, deterministic by construction.
bfsReach :: Map Cell [Cell] -> Cell -> Set Cell
bfsReach succ0 start = go [start] (Set.singleton start)
  where
    go [] acc = acc
    go (c : cs) acc =
        let next = [ n | n <- Map.findWithDefault [] c succ0
                   , not (Set.member n acc) ]
        in go (cs ++ next) (foldr Set.insert acc next)

-- | Entry point for 4c: template -> pure layout plan (steps 1-4), with a
--   deterministic retry pass: a probabilistic tree below @rooms.max@ can
--   extinct before reaching @rooms.min@ (sterile max-depth cells block the
--   frontier). We therefore try up to @genRetries@ sub-seeds derived from
--   the seed (seed * GOLDEN + attempt — same seed, same retries) and keep
--   the first attempt that reaches @rooms.min@; otherwise the attempt with
--   the most cells (early-abort path). @GENoSpace@ fires only when no
--   attempt can even reach @rooms.min@ — the hard floor of the detail plan
--   clarification.
generateDungeonLayout :: DTemplate -> Word64 -> Either GenerateError DungeonPlan
generateDungeonLayout t seed =
    let layout = dtLayout t
        resolved = resolveLevels t
        attempts = map (attemptSeed seed) [0 .. fromIntegral genMaxRetries - 1]
        tryAttempt k = placeTemplateRooms t (newRng k)
        placed = map tryAttempt attempts
        -- (rng, depths, attempt number) triples
        sized = [ (r, ds, fromIntegral i :: Integer)
                | (i, ((ds, r), _)) <- zip [0 :: Int ..] (zip placed attempts) ]
        levelSatisfied ds rl =
            let count = length [ () | (_, _, z) <- Map.keys ds, z == rlIndex rl ]
            in count >= rlRoomsMin rl
        attemptOk ds =
            Map.size ds >= dlRoomsMin layout && all (levelSatisfied ds) resolved
        ok = filter (\(_, ds, _) -> attemptOk ds) sized
        chosen = case ok of
            (p : _) -> p
            []      -> if null sized then error "unreachable" else
                        -- deterministic max by cell count (ties: first)
                        foldl1 (\a@(_, da, _) b@(_, db, _) ->
                            if Map.size db > Map.size da then b else a) sized
        (r1, depths, attemptNo) = chosen
    in if Map.size depths < dlRoomsMin layout
        then Left GENoSpace
        else
            let (grid, bossCell, treasureCell, r2) = assignArchetypes t depths r1
                (edges, _r3, onewayWarns) = connectRooms t grid r2
                (edges', unreachable) = ensureReachable grid edges
                maxDepth = maximum (Map.elems depths)
                maxLevel = maximum [ z | (_, _, z) <- Map.keys depths ]
                earlyAbort
                    | not (null (dtLevels t)) = maxLevel < dlDepth layout
                    | otherwise               = maxDepth < dlDepth layout
                earlyWarns =
                    [ CompileIssue "layout" SWarning "GeneratorEarlyAbort"
                        (if not (null (dtLevels t))
                            then "dungeon generator exhausted at level " ++ show maxLevel
                                 ++ " of " ++ show (dlDepth layout)
                            else "grid exhausted at depth " ++ show maxDepth
                                 ++ " of " ++ show (dlDepth layout)
                                 ++ " — dungeon delivered with reached maximum ("
                                 ++ show (Map.size depths) ++ " rooms placed, attempt "
                                 ++ show attemptNo ++ " of " ++ show genMaxRetries ++ ")")
                    | earlyAbort ]
                treasureWarns =
                    [ CompileIssue "special.treasure" SWarning "GeneratorTreasureSkipped"
                        "no free cell for the treasure room — placed without it"
                    | Just _ <- [dspTreasure (dtSpecial t)], treasureCell == Nothing ]
            in case unreachable of
                Just bad -> Left (GEUnreachable bad)
                Nothing  -> Right DungeonPlan
                    { dpGrid = grid
                    , dpEdges = edges'
                    , dpWarnings = onewayWarns ++ treasureWarns ++ earlyWarns
                    , dpStartCell = (0, 0, 1)
                    , dpBossCell = bossCell
                    , dpTreasureCell = treasureCell
                    , dpEarlyAbort = earlyAbort
                    , dpMaxDepth = depths Map.! bossCell
                    }

-- | Number of deterministic layout attempts per generation.
genMaxRetries :: Int
genMaxRetries = 12

-- | Sub-seed for attempt @k@: decorrelated from the layout stream by
--   multiplying with the golden constant (mirrors 'deriveRuntimeSeed').
attemptSeed :: Word64 -> Word64 -> Word64
attemptSeed base k = base * 0x9E3779B97F4A7C15 + k * 0x2545F4914F6CDD1D + 1

-- ---------------------------------------------------------------------------
-- Rogue Phase 4d: locks, population, adventure emission (detail plan 3.5-3.7)
-- ---------------------------------------------------------------------------

-- | Opposite direction of a cardinal/diagonal name.
reverseDir :: String -> String
reverseDir d = case d of
    "north"     -> "south"
    "south"     -> "north"
    "east"      -> "west"
    "west"      -> "east"
    "up"        -> "down"
    "down"      -> "up"
    "northeast" -> "southwest"
    "southwest" -> "northeast"
    "northwest" -> "southeast"
    "southeast" -> "northwest"
    other       -> other

-- | Deterministic room ids: @<archetype>_<n>@ with per-archetype counters in
--   sorted cell order (level first, then x, y).
cellRoomIds :: DungeonPlan -> Map Cell String
cellRoomIds plan = go sortedCells Map.empty Map.empty
  where
    grid = dpGrid plan
    sortedCells = sortOn (\(x, y, z) -> (z, x, y)) (Map.keys grid)
    go :: [Cell] -> Map Cell String -> Map String Int -> Map Cell String
    go [] acc _ = acc
    go (c : cs) acc counters =
        let arch = prArch (grid Map.! c)
            n    = Map.findWithDefault (0 :: Int) arch counters
        in go cs (Map.insert c (arch ++ "_" ++ show (n + 1)) acc)
                 (Map.insert arch (n + 1) counters)

-- | The savezone archetype ids declared in the template (Phase-1 synergy).
savezoneArchIds :: DTemplate -> [String]
savezoneArchIds t = [ drtId rt | rt <- dtRoomTemplates t, drtSavezone rt ]

-- | Build the exit maps: bidirectional edges emit both @AExitRef@s, one-way
--   edges only the forward one. Bidirectional edges ending at the locked
--   treasure cell carry @Just lockEntity@ on the approach direction
--   (@initialEntityStates@ seeds the entity as \"locked\"; the key's on_take
--   set_state opens it — standard Phase-7 mechanics, no new engine mechanism).
buildExits :: DungeonPlan -> Map Cell String -> Maybe String
           -> Map Cell (Map String AExitRef)
buildExits plan roomIds lockEntity =
    let lockFor cell = case (lockEntity, dpTreasureCell plan) of
            (Just ent, Just tc) | cell == tc -> Just ent
            _                                -> Nothing
        step acc e =
            let a = deFrom e
                b = deTo e
                idA = roomIds Map.! a
                idB = roomIds Map.! b
                dir = deDir e
            in if deOneway e
                then Map.insertWith Map.union a
                        (Map.singleton dir (AExitRef idB Nothing)) acc
                else
                    -- both ends of a bidirectional edge lead into the
                    -- treasure when that end is the treasure cell: edges are
                    -- normalized from the lexically smaller cell, so the
                    -- treasure can be either deFrom or deTo — lock whichever
                    -- AExitRef points at it (approach side); the exit inside
                    -- the treasure room itself stays open
                    let fwd  = Map.singleton dir (AExitRef idB (lockFor b))
                        back = Map.singleton (reverseDir dir) (AExitRef idA (lockFor a))
                    in Map.insertWith Map.union b back
                           (Map.insertWith Map.union a fwd acc)
    in foldl step Map.empty (dpEdges plan)

-- | Candidate cells for pool placement: everything except start, boss and
--   treasure. The optional depth range filters the set (in multi-level mode,
--   filters by level; in single-level mode, filters by tree depth).
populateCandidates :: Bool -> DungeonPlan -> Maybe DRange -> [Cell]
populateCandidates isMultiLevel plan mRange =
    let excluded = Set.fromList
            ([dpStartCell plan, dpBossCell plan] ++ maybe [] (: []) (dpTreasureCell plan))
        grid = dpGrid plan
        inRange c = case mRange of
            Nothing  -> True
            Just rng ->
                let val = if isMultiLevel
                          then let (_, _, z) = c in z
                          else prDepth (grid Map.! c)
                in val >= drMin rng && (drMax rng == maxBound || val <= drMax rng)
    in [ c | c <- Map.keys grid
       , not (Set.member c excluded)
       , inRange c ]

-- | Weighted room draw: deeper rooms weigh more (detail plan 3.6:
--   \"Räume gewichtet nach Tiefe ziehen\").
drawPopCell :: DungeonPlan -> [Cell] -> Rng -> (Cell, Rng)
drawPopCell plan cells r =
    let grid = dpGrid plan
    in pickWeighted [ (prDepth (grid Map.! c), c) | c <- cells ] r

-- | One pool item instance: unique id @<id>_<k>@, location overridden.
itemInstance :: AItem -> String -> String -> AItem
itemInstance frag iid rid = frag { aiId = iid, aiLocation = rid }

-- | One pool NPC instance: unique id @<id>_<k>@, location overridden.
npcInstance :: ANPC -> String -> String -> ANPC
npcInstance frag nid rid = frag { anId = nid, anLocation = rid }

-- | Steps 5-6: key placement + pool population. The reserved key goes into a
--   room with generation depth below the lock depth (tree depth is an upper
--   bound of BFS distance, so keyDepth < lockDepth holds in BFS order);
--   start room as fallback. Its on_take carries the standard set_state rule
--   — reusing the Phase-7 trigger mechanics, no new engine mechanism.
--   Ordinary pool entries draw a target room weighted by depth; boss NPCs go
--   directly into the boss room (Review-Frage 4).
populate :: DTemplate -> DungeonPlan -> Map Cell String -> Rng
         -> ([AItem], [ANPC], [CompileIssue], Rng)
populate t plan roomIds r0 =
    let isMultiLevel = not (null (dtLevels t))
        grid = dpGrid plan
        treasureCell = dpTreasureCell plan
        lockEntity = case (dspTreasure (dtSpecial t), treasureCell) of
            (Just ts, Just _) | dtsLocked ts -> Just ("lock_" ++ dtsTemplate ts)
            _                                -> Nothing

        keyCandidates = case treasureCell of
            Just tc ->
                let (_, _, zt) = tc
                    depT = prDepth (grid Map.! tc)
                in [ c | c <- populateCandidates isMultiLevel plan Nothing
                       , let (_, _, z) = c
                             dep = prDepth (grid Map.! c)
                         in if isMultiLevel
                            then z < zt || (z == zt && dep < depT)
                            else dep < depT ]
            Nothing -> populateCandidates isMultiLevel plan Nothing
        (keyCell, r1) = case keyCandidates of
            [] -> (dpStartCell plan, r0)
            cs -> (\(i, r) -> (cs !! i, r)) (randInt 0 (length cs - 1) r0)

        drawCount :: DRange -> Rng -> (Int, Rng)
        drawCount rng rr = randInt (drMin rng) (max (drMin rng) (drMax rng)) rr

        skipWarn :: String -> CompileIssue
        skipWarn what = CompileIssue ("template." ++ what) SWarning
            "GeneratorPopulationSkipped" "no non-special room available"

        stepItem (accItems, warns, rr) e =
            let iid = aiId (dipItem e)
            in case dipBossLock e of
                Just mark | mark == iid ->
                    -- exactly one reserved key instance (pairing rule)
                    let rid = roomIds Map.! keyCell
                        onTake = [ AOSetEntityState ent "unlocked" | Just ent <- [lockEntity] ]
                        item = (dipItem e)
                            { aiId = iid, aiLocation = rid
                            , aiOnTake = Just (fromMaybe [] (aiOnTake (dipItem e)) ++ onTake) }
                    in (accItems ++ [item], warns, rr)
                Just _ -> (accItems, warns, rr)  -- BossLockMismatch already rejected
                Nothing ->
                    let cands = populateCandidates isMultiLevel plan Nothing
                    in if null cands
                        then (accItems, warns ++ [skipWarn ("item_pool." ++ iid)], rr)
                        else
                            let (n, rr1) = drawCount (dipCount e) rr
                                go :: Int -> [AItem] -> Rng -> ([AItem], Rng)
                                go k acc rc
                                    | k >= n    = (acc, rc)
                                    | otherwise =
                                        let (c, rc') = drawPopCell plan cands rc
                                            inst = itemInstance (dipItem e)
                                                    (iid ++ "_" ++ show (k + 1))
                                                    (roomIds Map.! c)
                                        in go (k + 1) (acc ++ [inst]) rc'
                                (is, rr2) = go 0 [] rr1
                            in (accItems ++ is, warns, rr2)

        (items, itemWarns, r2) = foldl stepItem ([], [], r1) (dtItemPool t)

        stepNpc (accNpcs, warns, rr) e =
            let nid = anId (dnpNpc e)
            in if dnpBoss e
                then ( accNpcs ++ [npcInstance (dnpNpc e) nid (roomIds Map.! dpBossCell plan)]
                     , warns, rr )
                else
                    let cands = populateCandidates isMultiLevel plan (dnpDepthRange e)
                    in if null cands
                        then (accNpcs, warns ++ [skipWarn ("npc_pool." ++ nid)], rr)
                        else
                            let (n, rr1) = drawCount (dnpCount e) rr
                                go k acc rc
                                    | k >= n    = (acc, rc)
                                    | otherwise =
                                        let (c, rc') = drawPopCell plan cands rc
                                            inst = npcInstance (dnpNpc e)
                                                    (nid ++ "_" ++ show (k + 1))
                                                    (roomIds Map.! c)
                                        in go (k + 1 :: Int) (acc ++ [inst]) rc'
                                (ns, rr2) = go 0 [] rr1
                            in (accNpcs ++ ns, warns, rr2)

        (npcs, npcWarns, r3) = foldl stepNpc ([], [], r2) (dtNpcPool t)

    in (items, npcs, itemWarns ++ npcWarns, r3)

-- | The default (empty) title art, mirroring the adventure schema default.
emptyTitleArt :: AAscii
emptyTitleArt = AAscii (ACondText "" []) [] 0 [] Nothing

-- | Step 7 — build the @Adventure@ record (detail plan 3.7). Room ids are
--   generator-assigned; the fragment's exits are discarded (the generator
--   owns adjacency); item/NPC placement lands in advItems/advNPCs; the
--   @game@ block carries exactly the savezone instances (Phase-1 synergy —
--   ironman is implied, otherwise save_zones would be dead data); @combat:@
--   is passed through 1:1 to @advCombat@ (combination lever with
--   plan-kampfbildschirm.md). All maps iterate in sorted order.
emitAdventure :: DTemplate -> DungeonPlan -> Word64 -> (Adventure, [CompileIssue])
emitAdventure t plan seed =
    let grid = dpGrid plan
        roomIds = cellRoomIds plan
        lockEntity = case (dspTreasure (dtSpecial t), dpTreasureCell plan) of
            (Just ts, Just _) | dtsLocked ts -> Just ("lock_" ++ dtsTemplate ts)
            _                                -> Nothing
        exits = buildExits plan roomIds lockEntity
        (items, npcs, popWarns, _rFinal) = populate t plan roomIds (newRng (deriveRuntimeSeed seed))

        -- the lock entity itself: a hidden, non-portable door item so the
        -- engine's entity references (set_state in the key's on_take, the
        -- "locked" state from initialEntityStates) resolve against a real
        -- definition
        lockItem = case (dspTreasure (dtSpecial t), dpTreasureCell plan) of
            (Just ts, Just tc) | dtsLocked ts ->
                let ent = "lock_" ++ dtsTemplate ts
                    prevCell = case [ deFrom e | e <- dpEdges plan, deTo e == tc ] of
                        (c : _) -> c
                        []      -> dpStartCell plan
                    prevRoomId = roomIds Map.! prevCell
                in Just (AItem
                    { aiId = ent
                    , aiName = "Verschlossenes Schloss"
                    , aiTexts = ACondText "Ein schweres Schloss verschliesst den Durchgang." []
                    , aiAscii = AAscii (ACondText "" []) [] 0 [] Nothing
                    , aiKeywords = [ent, "schloss"]
                    , aiTags = ["lock"]
                    , aiLocation = prevRoomId
                    , aiState = "locked"
                    , aiEquipSlot = Nothing
                    , aiEquipEffects = []
                    , aiHidden = True
                    , aiDiscover = Nothing
                    , aiProps = Map.empty
                    , aiOnTake = Nothing
                    , aiVerbMap = Map.empty
                    , aiPortable = Just False
                    , aiTakeFailure = Nothing
                    , aiInContainer = Nothing
                    })
            _ -> Nothing

        findArch a = listToMaybe [ rt | rt <- dtRoomTemplates t, drtId rt == a ]
        roomInstance c@(_, _, z) =
            let rid = roomIds Map.! c
                arch = prArch (grid Map.! c)
                fl = if not (null (dtLevels t)) then Just z else arFloor (drtRoom (head (dtRoomTemplates t)))
            in case findArch arch of
                Just rt -> (drtRoom rt)
                    { arId = rid
                    , arExits = Map.findWithDefault Map.empty c exits
                    , arFloor = if not (null (dtLevels t)) then Just z else arFloor (drtRoom rt)
                    }
                Nothing -> (drtRoom (head (dtRoomTemplates t)))
                    { arId = rid, arName = rid
                    , arExits = Map.findWithDefault Map.empty c exits
                    , arFloor = fl }
        sortedCells = sortOn (\(x, y, z) -> (z, x, y)) (Map.keys grid)
        rooms = map roomInstance sortedCells

        savezoneIds = [ roomIds Map.! c | c <- sortedCells
                      , prArch (grid Map.! c) `elem` savezoneArchIds t ]
        generatedGamePolicy = if null savezoneIds
            then Nothing
            else Just AGamePolicy
                { agpPermadeath = Nothing
                , agpAllowUndo  = Nothing
                , agpIronman    = Just True  -- save_zones are only meaningful in ironman mode
                , agpSaveZones  = savezoneIds
                , agpMetaSlug   = Nothing
                }

        adv = Adventure
            { advName              = Just (dtName t)
            , advStartRoom         = roomIds Map.! dpStartCell plan
            , advRooms             = rooms
            , advItems             = maybe items (\li -> li : items) lockItem
            , advNPCs              = npcs
            , advQuests            = []
            , advVehicles          = []
            , advInteractions      = Nothing
            , advVerbs             = []
            , advVariables         = dtVariables t
            , advTriggers          = []
            , advPlayer            = dtPlayer t
            , advInitialVariables  = Map.empty
            , advInitialFlags      = Map.empty
            , advActiveQuests      = []
            , advFactions          = []
            , advEncounterTables   = []
            , advEnvironment       = Nothing
            , advStealth           = Nothing
            , advPatrol            = Nothing
            , advCombat            = dtCombat t
            , advAbilities         = []
            , advEndArt            = Map.empty
            , advTitleArt          = emptyTitleArt
            , advClips             = []
            , advGame              = generatedGamePolicy
            , advCards             = []
            , advDeck              = Nothing
            , advHandLimit         = Nothing
            , advSandboxZones      = []
            , advRawValue          = Nothing
            }
    in (adv, dpWarnings plan ++ popWarns)

-- | Top-level entry (detail plan section 3): validate, lay out, emit.
--   @GETemplate@ wraps 'validateTemplate' output; @GENoSpace@ fires below
--   @rooms.min@; @GEUnreachable@ would be a generator bug. Pure — the CLI
--   (4d) feeds the result through @compileAdventure@ and validation.
generateDungeon :: DTemplate -> Word64 -> Either GenerateError (Adventure, [CompileIssue])
generateDungeon t seed = case validateTemplate t of
    (i : _) -> Left (GETemplate (i : tail (validateTemplate t)))
    []      -> case generateDungeonLayout t seed of
        Left e   -> Left e
        Right plan -> Right (emitAdventure t plan seed)
