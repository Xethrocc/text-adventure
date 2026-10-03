{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The project view (4.6, S3): everything a map/quest editor needs to show a
--   whole adventure at a glance, in one value.
--
--   Shape follows 'docs/protocol-v1.md': snake_case fields, a version field,
--   deterministic encoding via 'E.encodeSorted' (keys sorted at every depth),
--   and **no world state** — no inventory, no flags, no rng. The view is
--   derived from the authored adventure plus the resolved layout, so it can be
--   regenerated at any time and diffed between commits. That is the point: a
--   later WebUI tab consumes this, and a reviewer's build consumes it to see
--   what a change did.
module Worldbuilder.ProjectView
    ( ProjectView (..)
    , PVRoom (..)
    , PVEdge (..)
    , PVQuest (..)
    , PVIssue (..)
    , PVReachability (..)
    , positionSource
    , buildProjectView
    , encodeProjectView
    , renderProjectView
    , renderMapGrid
    ) where

import Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List (intercalate, sortOn)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Types as E

import Worldbuilder.Types
import Worldbuilder.MapLayout (MapCell (..), roomLayout)
import Worldbuilder.QuestCheck (startableQuests, progressedQuests)
import qualified Worldbuilder.Compile as C

-- ---------------------------------------------------------------------------
-- Data model
-- ---------------------------------------------------------------------------

data ProjectView = ProjectView
    { pvVersion      :: Int
    , pvName         :: String
    , pvStartRoom    :: String
    , pvRooms        :: [PVRoom]
    , pvEdges        :: [PVEdge]
    , pvReachability :: PVReachability
    , pvQuests       :: [PVQuest]
    , pvIssues       :: [PVIssue]
    } deriving (Eq, Show)

data PVRoom = PVRoom
    { pvrId     :: String
    , pvrName   :: String
    , pvrX      :: Int
    , pvrY      :: Int
    , pvrFloor  :: Int
    , pvrSource :: String   -- ^ 'positionSource'
    } deriving (Eq, Show)

data PVEdge = PVEdge
    { pveFrom        :: String
    , pveTo          :: String
    , pveDirection   :: String
    , pveLocked      :: Maybe String
    , pveGuard       :: Maybe String
    , pveGuardHolds  :: Maybe Bool
    } deriving (Eq, Show)

data PVReachability = PVReachability
    { pvrReachable   :: [String]
    , pvrUnreachable :: [String]
    } deriving (Eq, Show)

data PVQuest = PVQuest
    { pvqId          :: String
    , pvqName        :: String
    , pvqPrereqs     :: [String]
    , pvqStages      :: [String]
    , pvqOnComplete  :: Maybe String
    , pvqStartable   :: Bool
    , pvqProgressed  :: Bool
    } deriving (Eq, Show)

data PVIssue = PVIssue
    { pviPath     :: String
    , pviCode     :: String
    , pviSeverity :: String
    , pviMessage  :: String
    } deriving (Eq, Show)

-- | Where a room's position came from. Worth showing: an authored position and
--   a computed one are both legitimate, and mixing them up is exactly the
--   confusion a map editor has to avoid.
positionSource :: MapCell -> String
positionSource c = if mcSet c then "authored" else "layout"

-- ---------------------------------------------------------------------------
-- JSON (protocol-v1 shape)
-- ---------------------------------------------------------------------------

instance ToJSON ProjectView where
    toJSON pv = object
        [ "version"      .= pvVersion pv
        , "name"         .= pvName pv
        , "start_room"   .= pvStartRoom pv
        , "rooms"        .= pvRooms pv
        , "edges"        .= pvEdges pv
        , "reachability" .= pvReachability pv
        , "quests"       .= pvQuests pv
        , "issues"       .= pvIssues pv
        ]

instance ToJSON PVRoom where
    toJSON r = object
        [ "id"       .= pvrId r
        , "name"     .= pvrName r
        , "x"        .= pvrX r
        , "y"        .= pvrY r
        , "floor"    .= pvrFloor r
        , "source"   .= pvrSource r
        ]

instance ToJSON PVEdge where
    toJSON e = object
        [ "from"         .= pveFrom e
        , "to"           .= pveTo e
        , "direction"    .= pveDirection e
        , "locked"       .= pveLocked e
        , "guard"        .= pveGuard e
        , "guard_holds"  .= pveGuardHolds e
        ]

instance ToJSON PVReachability where
    toJSON r = object
        [ "reachable"   .= pvrReachable r
        , "unreachable" .= pvrUnreachable r
        ]

instance ToJSON PVQuest where
    toJSON q = object
        [ "id"          .= pvqId q
        , "name"        .= pvqName q
        , "prereqs"     .= pvqPrereqs q
        , "stages"      .= pvqStages q
        , "on_complete" .= pvqOnComplete q
        , "startable"   .= pvqStartable q
        , "progressed"  .= pvqProgressed q
        ]

instance ToJSON PVIssue where
    toJSON i = object
        [ "path"     .= pviPath i
        , "code"     .= pviCode i
        , "severity" .= pviSeverity i
        , "message"  .= pviMessage i
        ]

-- | Deterministic encoding: keys sorted at every depth, like protocol v1.
encodeProjectView :: ProjectView -> BLC.ByteString
encodeProjectView = E.encodeSorted

-- ---------------------------------------------------------------------------
-- Build
-- ---------------------------------------------------------------------------

-- | Assemble the view.
--
--   Room order follows the **authored declaration order**, never a 'Map' order:
--   the view is meant to be diffed between commits, and an alphabetical order
--   would reshuffle a file that only got a room appended.
buildProjectView :: Adventure -> [C.CompileIssue] -> ProjectView
buildProjectView a issues = ProjectView
    { pvVersion      = 1
    , pvName         = fromMaybe "" (advName a)
    , pvStartRoom    = advStartRoom a
    , pvRooms        = map room rows
    , pvEdges        = concatMap edgesOf (advRooms a)
    , pvReachability = PVReachability
        { pvrReachable   = [ r | r <- roomIds, not (r `elem` unreachable) ]
        , pvrUnreachable = unreachable
        }
    , pvQuests       = map questView (advQuests a)
    , pvIssues       = map issueView (sortOn C.ciPath issues)
    }
  where
    cells  = roomLayout a
    cellOf r = case [ c | c <- cells, mcRoom c == r ] of
        (c:_) -> c
        []    -> MapCell r 0 0 0 False
    rows    = [ cellOf (arId r) | r <- advRooms a ]
    room c  = PVRoom
        { pvrId     = mcRoom c
        , pvrName   = fromMaybe "" (arName <$> findRoom (mcRoom c))
        , pvrX      = mcX c
        , pvrY      = mcY c
        , pvrFloor  = mcFloor c
        , pvrSource = positionSource c
        }
    -- the room behind a layout cell, so the name comes from the authoring side
    findRoom r = listToMaybe [ x | x <- advRooms a, arId x == r ]
    roomIds   = map arId (advRooms a)
    unreachable = C.unreachableRoomIds a

    edgesOf r =
        [ PVEdge
            { pveFrom       = arId r
            , pveTo         = aeTarget ex
            , pveDirection  = dir
            , pveLocked     = aeLocked ex
            , pveGuard      = fmap show (aeWhen ex)
            , pveGuardHolds = aeWhen ex >>= C.predicateTruth a
            }
        | (dir, ex) <- Map.toList (arExits r) ]      -- canonical direction order

    outs = C.allAOutcomes a
    startable   = startableQuests outs a
    progressed  = progressedQuests outs
    questView q = PVQuest
        { pvqId         = aqId q
        , pvqName       = aqName q
        , pvqPrereqs    = aqPrereqs q
        , pvqStages     = [ aqsId s | s <- aqStages q ]
        , pvqOnComplete = aqOnComplete q
        , pvqStartable  = Set.member (aqId q) startable
        , pvqProgressed = Set.member (aqId q) progressed
        }
    issueView i = PVIssue
        { pviPath     = C.ciPath i
        , pviCode     = C.ciCode i
        , pviSeverity = case C.ciSeverity i of
                            C.SError   -> "error"
                            C.SWarning -> "warning"
        , pviMessage  = C.ciMessage i
        }

-- ---------------------------------------------------------------------------
-- Text rendering
-- ---------------------------------------------------------------------------

-- | Human view: one grid per floor, then edges, quests and issues.
renderProjectView :: Int -> ProjectView -> String
renderProjectView width pv = unlines $
    [ "== " ++ pvName pv ++ " (project view v" ++ show (pvVersion pv) ++ ") ==" ]
    ++ [ "start_room: " ++ pvStartRoom pv ]
    ++ [ "" ]
    ++ renderMapGrid width (pvRooms pv)
    ++ [ "rooms: " ++ show (length (pvRooms pv))
         ++ "   edges: " ++ show (length (pvEdges pv))
         ++ "   quests: " ++ show (length (pvQuests pv)) ]
    ++ [ "unreachable: " ++ (if null (pvrUnreachable (pvReachability pv))
                                then "none" else intercalate ", " (pvrUnreachable (pvReachability pv))) ]
    ++ [ "" ]
    ++ edgeLines
    ++ [ "" ]
    ++ questLines
    ++ [ "" ]
    ++ issueLines
  where
    edgeLines
        | null (pvEdges pv) = ["edges: none"]
        | otherwise = "edges:" : map edgeLine (pvEdges pv)
    edgeLine e = "  " ++ pad 12 (pveFrom e) ++ " -" ++ pad 8 (pveDirection e ++ ">") ++ "-> "
        ++ pveTo e ++ guardNote e
    guardNote e = concat
        [ maybe "" (\l -> " [locked: " ++ l ++ "]") (pveLocked e)
        , maybe "" (\g -> " [guard: " ++ g ++ holdNote ++ "]") (pveGuard e) ]
      where holdNote = case pveGuardHolds e of
                Just True  -> ""
                Just False -> " (never holds)"
                Nothing    -> " (unknown)"
    questLines
        | null (pvQuests pv) = ["quests: none"]
        | otherwise = "quests:" : map questLine (pvQuests pv)
    questLine q = "  " ++ pad 14 (pvqId q) ++ " stages: " ++ show (length (pvqStages q))
        ++ "  startable: " ++ yesNo (pvqStartable q)
        ++ "  progresses: " ++ yesNo (pvqProgressed q)
        ++ chainNote q
    chainNote q = case pvqOnComplete q of
        Nothing -> ""
        Just nx -> "  -> " ++ nx
    issueLines
        | null (pvIssues pv) = ["issues: none"]
        | otherwise = "issues:" : map issueLine (pvIssues pv)
    issueLine i = "  [" ++ pviSeverity i ++ "] " ++ pviCode i ++ " at " ++ pviPath i
        ++ "\n      " ++ pviMessage i
    yesNo = \b -> if b then "yes" else "NO"

-- | The grid itself: one block per floor, rows are @y@, columns are @x@.
--
--   Rooms are rendered by id (shortened), because a name is prose and an id is
--   what every other line of the view refers to. Authored positions are marked
--   with a leading @*@ so a reader can see at a glance which cells are pins and
--   which are computed.
renderMapGrid :: Int -> [PVRoom] -> [String]
renderMapGrid requested rooms
    -- A cell must never be too narrow to tell two rooms apart. `loc_1` and
    -- `loc_17` both cut to `loc_`, which would make the grid quietly lie about
    -- where a room sits — so the effective width grows to the longest id plus
    -- one for the marker. Past a sane width it is capped and ids are cut with
    -- a visible marker instead.
    | width > maxCellWidth = concatMap (floorBlock width) blocks ++ truncationNote
    | otherwise           = concatMap (floorBlock width) blocks
  where
    longest = maximum (0 : map (length . pvrId) rooms)
    maxCellWidth = 28
    -- +1 for the authored marker in front of the id
    width = max requested (min (longest + 1) maxCellWidth)
    truncationNote
        | longest > maxCellWidth =
            [ "  (ids longer than " ++ show maxCellWidth
                ++ " chars are cut, marked with ~)" ]
        | otherwise = []
    groups = Map.toList (Map.fromListWith (++) [ (pvrFloor r, [r]) | r <- rooms ])
    blocks = byFloor groups

    -- 'sortOn' is deliberately not used: the explicit insertion sort keeps the
    -- floor order obvious to a reader.
    byFloor :: [(Int, [PVRoom])] -> [(Int, [PVRoom])]
    byFloor = foldr insertFloor []
    insertFloor x [] = [x]
    insertFloor x@(k, _) ((k', y) : ys)
        | k <= k'   = x : (k', y) : ys
        | otherwise = (k', y) : insertFloor x ys

    floorBlock :: Int -> (Int, [PVRoom]) -> [String]
    floorBlock cellWidth (flr, rs) =
        let (x0, x1) = spanOf pvrX rs
            (y0, y1) = spanOf pvrY rs
            at x y = case [ r | r <- rs, pvrX r == x, pvrY r == y ] of
                (r:_) -> Just r
                []    -> Nothing
            header = "  -- floor " ++ show flr ++ " --"
            ruler  = gutter ++ concat [ padTo cellWidth ("x=" ++ show x) ++ " " | x <- [x0 .. x1] ]
            grid   = [ "  y=" ++ pad 3 (show y) ++ " "
                       ++ concat [ padTo cellWidth (cellText cellWidth (at x y)) ++ " " | x <- [x0 .. x1] ]
                     | y <- [y0 .. y1] ]
        in header : ruler : grid ++ [""]
    -- Rows carry their y coordinate: without it a reader cannot tell which grid
    -- line is y=3 and which y=4.
    gutter = "         "
    spanOf :: (PVRoom -> Int) -> [PVRoom] -> (Int, Int)
    spanOf f rs = if null rs then (0, 0) else (minimum (map f rs), maximum (map f rs))
    cellText :: Int -> Maybe PVRoom -> String
    cellText _ Nothing = "."
    cellText cellWidth (Just r) = markerText r ++ cut (cellWidth - 1) (pvrId r)
    -- cut with a visible marker, so a shortened id never reads as a whole one
    cut n t | length t <= n = t
            | otherwise      = take (n - 1) t ++ "~"
    markerText r = if pvrSource r == "authored" then "*" else " "

padTo :: Int -> String -> String
padTo n s = s ++ replicate (n - length s) ' '

pad :: Int -> String -> String
pad n s = s ++ replicate (n - length s) ' '