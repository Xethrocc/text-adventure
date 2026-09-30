-- | Pursuit and path queries over the live world graph (Tür IV).
--
-- A pure, stateless search core: hop distances over a supplied room graph,
-- and the deterministic choice of a single next edge (toward or away from a
-- goal). The engine supplies the graph (passable exits from
-- 'Game.effectiveConnections'); this module never touches 'GameState', never
-- consumes @rngState@ and uses only ordered containers.
--
-- Reproduzierbarkeits-Vertrag (siehe \"plan-pursuit-suche.md\"):
--
--   * stateless and pure — the result is a function of (graph, goal) only;
--   * the tie-break is explicit and independent of 'Direction's derived
--     'Ord': candidates are sorted by target room id first, then
--     'directionPriority'. Do not reorder 'directionPriority' (Gameplay-
--     Vertrag, nicht umsortieren) — reordering it silently changes runs;
--   * only 'Data.Map' containers, no hash containers.
module Pursuit
    ( directionPriority
    , bfsDistances
    , stepToward
    , stepAway
    ) where

import qualified Data.Map.Strict as Map
import Data.List (sortOn)

import Types.Core (Direction(..), RoomID)

-- | Gameplay-Vertrag, nicht umsortieren: fixed tie-break priority for
--   equally good edges. Independent of the derived 'Ord' of 'Direction' on
--   purpose — that order is a declaration accident and may be refactored.
directionPriority :: [Direction]
directionPriority =
    [ North, South, East, West, Up, Down, Northeast, Northwest, Southeast, Southwest ]

priorityIndex :: Direction -> Int
priorityIndex d = go 0 directionPriority
  where
    go _ [] = length directionPriority   -- unknown direction sorts last
    go i (x:xs)
        | x == d    = i
        | otherwise = go (i + 1) xs

-- | Hop distance from every room to @goal@ over the given forward edges
--   (one entry per room and direction). Rooms absent from the result are
--   unreachable — the callers report that as the @-1@ sentinel.
bfsDistances :: Map.Map RoomID [(Direction, RoomID)] -> RoomID -> Map.Map RoomID Int
bfsDistances edges goal = go (Map.singleton goal 0) [goal]
  where
    -- reverse adjacency: every predecessor that can walk to a room
    predecessors =
        Map.fromListWith (++)
            [ (to, [from])
            | (from, kanten) <- Map.toList edges
            , (_, to) <- kanten ]
    go dists [] = dists
    go dists (r:rs) =
        let d    = dists Map.! r
            news = [ p | p <- Map.findWithDefault [] r predecessors
                       , not (Map.member p dists) ]
            dists' = foldl (\m p -> Map.insert p (d + 1) m) dists news
        in go dists' (rs ++ news)

-- | Deterministic candidate order: smallest target room id first (a key
--   from the data, stable under any refactor), then 'directionPriority'.
sortCandidates :: [(Direction, RoomID)] -> [(Direction, RoomID)]
sortCandidates = sortOn (\(dir, to) -> (to, priorityIndex dir))

-- | The single next edge toward the goal: strictly one hop closer.
--   'Nothing' at the goal (or from an unreachable room — no edge can lead
--   closer than @-1@).
stepToward :: [(Direction, RoomID)] -> Map.Map RoomID Int -> RoomID
           -> Maybe (Direction, RoomID)
stepToward edges dists current =
    case Map.lookup current dists of
        Just d | d > 0 ->
            case sortCandidates [ (dir, to) | (dir, to) <- edges
                                            , Map.lookup to dists == Just (d - 1) ] of
                (c:_) -> Just c
                []    -> Nothing
        _ -> Nothing

-- | The single next edge away from the goal: strictly farther (where an
--   unreachable room counts as infinitely far — fleeing into a dead end the
--   seeker cannot be followed into is the point). 'Nothing' when no strictly
--   farther neighbour exists.
stepAway :: [(Direction, RoomID)] -> Map.Map RoomID Int -> RoomID
         -> Maybe (Direction, RoomID)
stepAway edges dists current =
    let d0 = rank current
    in case sortCandidates [ (dir, to) | (dir, to) <- edges, rank to > d0 ] of
        (c:_) -> Just c
        []    -> Nothing
  where
    rank r = case Map.lookup r dists of
        Nothing -> maxBound :: Int   -- unreachable = infinitely far away
        Just d  -> d
