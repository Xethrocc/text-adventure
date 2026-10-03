-- | 4.6: the map side of the map editor — room positions and the layout that
--   fills in whatever the author did not pin.
--
--   Two halves, deliberately separated:
--
--   * **authored** positions (@map: {x: n, y: m}@ at a room) travel into the
--     compiled world as 'E.MapPos' — and only when set, so a world without
--     @map:@ stays byte-identical to one written before this feature.
--   * the **layout** lives here, in the worldbuilder, as a pure function over
--     the authored adventure. It never writes into the world; it answers the
--     editor's question "where is every room?" and backs the overlap check.
--
--   Layout rule (genre-neutral, deterministic, no rng):
--
--   * the third dimension is 'E.roomFloor': each floor is laid out on its own
--     grid, so a multi-level world keeps its levels apart;
--   * authored positions are **anchors** and keep their cell;
--   * everything else is placed by BFS from @start_room@: row @y@ is the BFS
--     depth, column @x@ the index within that layer, taking the next free cell
--     that no anchor occupies;
--   * rooms the walk cannot reach get one further row below the deepest layer,
--     in **declaration order** — the authored list, never @Map@ or @Ord@ order,
--     so the result does not depend on hashing (project rule 9);
--   * within a layer, neighbours follow the exit order of the room — the
--     canonical direction keys, not a hash — so @north@ sits left of @south@.
--
--   The walk uses the authored exits ('arExits'), one-way exits included. A
--   locked exit still connects two rooms on the map — whether it is passable
--   right now is a gameplay question, not a cartographic one.
module Worldbuilder.MapLayout
    ( MapCell(..)
    , Layout
    , roomLayout
    , layoutBounds
    , mapFloorOf
    ) where

import Worldbuilder.Types
import qualified Types as E
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | One room's place on the map: grid cell plus the floor it sits on.
data MapCell = MapCell
    { mcRoom  :: String  -- ^ room id
    , mcX     :: Int
    , mcY     :: Int
    , mcFloor :: Int     -- ^ 'E.roomFloor', with 'Nothing' as 0
    , mcSet   :: Bool    -- ^ True when the author pinned it via @map:@
    } deriving (Eq, Show)

-- | Layout in **declaration order** of @rooms:@, not map order: the editor
--   prints and writes in the order the author reads the file.
type Layout = [MapCell]

-- | Deduplicate while keeping the first occurrence. Used for a room's exit
--   targets: a Set would re-sort them by room id and throw away the direction
--   order that decides the left-to-right order inside a layer.
dedupOrder :: Eq a => [a] -> [a]
dedupOrder = go []
  where
    go acc (y:ys)
        | y `elem` acc = go acc ys
        | otherwise = go (y:acc) ys
    go acc [] = reverse acc

-- | The floor of a room: 'E.roomFloor' with an unset floor as 0. Shared with the
--   overlap check in 'Worldbuilder.Compile', so both sides agree on what a
--   "same floor" means.
mapFloorOf :: ARoom -> Int
mapFloorOf r = maybe 0 id (arFloor r)

-- | Lay out every room of an adventure. Pure: the same adventure always yields
--   the same cells.
roomLayout :: Adventure -> Layout
roomLayout adv = concatMap layoutFloor floors
  where
    rooms = advRooms adv
    floorOf = mapFloorOf
    floors = Set.toAscList (Set.fromList (map floorOf rooms))
    layoutFloor f = map toCell (declarationOrder (onFloor f))
      where
        onFloor g = [ r | r <- rooms, floorOf r == g ]
        declarationOrder rs = [ (arId r, arMapPos r) | r <- rs ]
        cells = assignCells (advStartRoom adv) (adjacency f) (declarationOrder (onFloor f))
        authored = Set.fromList [ rid | (rid, Just _) <- declarationOrder (onFloor f) ]
        toCell (rid, _cell) =
            let (x, y) = Map.findWithDefault (0, 0) rid cells
            in MapCell rid x y f (rid `Set.member` authored)
        adjacency g = Map.fromList
            [ (arId r, [ t | t <- orderedTargetsOf r, Set.member t idsOfFloor ])
            | r <- onFloor g ]
        idsOfFloor = Set.fromList (map arId (onFloor f))
        -- Map.elems is ordered by the canonical direction key, not by a hash,
        -- so the exit order of the room fixes the left-to-right order in a
        -- layer. The dedup keeps that order; a Set here would re-sort the
        -- targets by room id and quietly lose it.
        orderedTargetsOf r = dedupOrder [ aeTarget ref | ref <- Map.elems (arExits r) ]

-- | Assign every room of one floor a cell. Anchors keep theirs; the rest are
--   placed layer by layer.
assignCells :: String                     -- ^ start room (ignored if not on this floor)
            -> Map.Map String [String]   -- ^ adjacency, floor-local
            -> [(String, Maybe E.MapPos)] -- ^ rooms in declaration order
            -> Map.Map String (Int, Int)
assignCells startRoom adj ordered = go frontier0 0 placed0 used0 visited0
  where
    ids = Set.fromList (map fst ordered)
    placed0 = Map.fromList [ (rid, (E.mapPosX p, E.mapPosY p)) | (rid, Just p) <- ordered ]
    used0 = Set.fromList (Map.elems placed0)
    -- An authored position is authoritative: the walk neither moves it nor
    -- spends a cell on it. (Without this the anchor was silently overwritten —
    -- the layout looked plausible and simply ignored the author's map.)
    anchored = Set.fromList [ rid | (rid, Just _) <- ordered ]
    frontier0 = [ startRoom | startRoom `Set.member` ids ]
    visited0 = Set.fromList frontier0
    -- State: the frontier of the layer being placed, its row (the start room's
    -- row is 0), the occupied cells, the placed rooms and the visited set.
    go frontier depth placed used visited = case frontier of
        [] -> finish placed used depth visited
        _ ->
            let need = [ rid | rid <- frontier, not (rid `Set.member` anchored) ]
                cells = freeCells depth used (length need)
                placed' = foldl (\acc (rid, c) -> Map.insert rid c acc) placed (zip need cells)
                used' = foldl (flip Set.insert) used cells
                visited' = visited `Set.union` Set.fromList frontier
                -- dedupOrder, not Set.toList: the next layer inherits the exit
                -- order of this one. A Set would re-sort by room id and make
                -- the drawing depend on hashing (project rule 9).
                deeper = dedupOrder [ t
                                    | rid <- frontier
                                    , t <- Map.findWithDefault [] rid adj
                                    , not (t `Set.member` visited') ]
            in go deeper (depth + 1) placed' used' visited'

    -- Rooms the walk never reached: the row right below the deepest layer, in
    -- declaration order. `depth` is already the next free row here (the walk
    -- increments it when it hands over to the next layer), which also gives a
    -- floor without a start room its stranded rooms in row 0.
    finish placed used depth visited =
        let stranded = [ rid | (rid, _) <- ordered
                       , not (rid `Set.member` visited)
                       , not (rid `Set.member` anchored) ]
            row = depth
            cells = freeCells row used (length stranded)
        in foldl (\acc (rid, c) -> Map.insert rid c acc) placed (zip stranded cells)

    -- The first @n@ free cells of row @y@, left to right. Each room of the
    -- layer takes the next one, so a layer never overlaps itself or an anchor.
    freeCells :: Int -> Set.Set (Int, Int) -> Int -> [(Int, Int)]
    freeCells y used n = take n (filter (\c -> c `Set.notMember` used) [ (x, y) | x <- [0 ..] ])
-- | Bounding box of a layout: @(minX, maxX, minY, maxY)@, or 'Nothing' for an
--   empty layout. The editor needs it to size a canvas.
layoutBounds :: Layout -> Maybe (Int, Int, Int, Int)
layoutBounds [] = Nothing
layoutBounds cells =
    Just (minimum xs, maximum xs, minimum ys, maximum ys)
  where
    xs = map mcX cells
    ys = map mcY cells
