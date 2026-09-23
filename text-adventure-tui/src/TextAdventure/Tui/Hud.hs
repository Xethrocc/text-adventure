-- | Pure HUD model for the multi-panel TUI (Rogue Phase 5).
--
--   Everything in here reads the engine state through its public accessors
--   ('Game.effectiveConnections' for the map, 'variables' for combat and
--   resources) — no second interpreter, no state silo. The TUI calls one
--   function, 'buildHud', whenever the state may have changed ('feReadInput',
--   'feReadPlain') and renders the resulting record verbatim.
module TextAdventure.Tui.Hud
  ( HudView (..)
  , MapCell (..)
  , MapGrid (..)
  , Bar (..)
  , buildHud
  , mapGrid
  , hpBar
  , condLine
  , equipmentLines
  , combatLines
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (sortOn)
import Data.Char (toUpper)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)

import Types
import Game (effectiveConnections, getVariable, combatRound, combatRoundKey,
             combatActionKey)

-- | One cell of the minimap: the room's one-letter stamp plus the row/col
--   it occupies in the grid. Drawn with box-drawing connectors between cells.
data MapCell = MapCell
    { mcRow    :: Int
    , mcCol    :: Int
    , mcStamp  :: Char          -- ^ first letter of the room name, uppercased
    , mcHere   :: Bool          -- ^ the player's current room
    , mcRoom   :: RoomID        -- ^ back-reference for connector drawing
    } deriving (Show, Eq)

-- | A rendered slice of the explored world: the cells (grid coordinates) and
--   the rows to draw — room stamps and box-drawing connectors, fog dots for
--   adjacent-but-unvisited cells. Bounded so a large explored world cannot
--   push the HUD panel off screen; the grid recentres around the player when
--   the explored span outgrows the window.
data MapGrid = MapGrid
    { mgWidth  :: Int
    , mgHeight :: Int
    , mgCells  :: [MapCell]
    , mgFog    :: [String]     -- ^ rendered rows (stamps, connectors, fog)
    } deriving (Show, Eq)

-- | A labelled gauge (HP and similar resources).
data Bar = Bar
    { barLabel :: String
    , barNow   :: Int
    , barMax   :: Int
    } deriving (Show, Eq)

-- | Everything the HUD panels show, computed in one pure pass.
data HudView = HudView
    { hvRoom       :: String        -- ^ current room name ("" = unknown room)
    , hvMap        :: Maybe MapGrid -- ^ minimap of visited rooms (Nothing = none)
    , hvBars       :: [Bar]         -- ^ HP first, then declared numeric variables
    , hvConditions :: [String]      -- ^ active status effects, "name (n)"
    , hvEquipment  :: [String]      -- ^ "slot: item" lines, slot order
    , hvCombat     :: [String]      -- ^ combat panel lines (empty = no panel)
    , hvGameOver   :: Bool          -- ^ dim the panels on the end screen
    } deriving (Show, Eq)

-- | The full HUD for a state: one entry point the TUI can call per refresh.
buildHud :: GameState -> HudView
buildHud st = HudView
    { hvRoom       = fromMaybe "" (roomName <$> Map.lookup here (rooms (world st)))
    , hvMap        = mapGrid 9 5 st
    , hvBars       = [hpBar st] ++ numericBars st
    , hvConditions = mapMaybe condLine (Map.elems (conditions (save st)))
    , hvEquipment  = equipmentLines st
    , hvCombat     = combatLines st
    , hvGameOver   = gameOver (save st)
    }
  where
    here = currentRoom (save st)

-- ---------------------------------------------------------------------------
-- Minimap (M10: visitedRooms + Phase 3 effective connections)
-- ---------------------------------------------------------------------------

exitDest :: Exit -> Maybe RoomID
exitDest ex = case ex of
    Open dest  -> Just dest
    Locked d _ -> Just d

-- | Lay the visited rooms out on their compass lattice (row grows south, col
--   grows east, the anchor room sits at (0,0)), recentre so the player's cell
--   stays inside the window, and render stamps with connectors and fog.
--   Connections are read through 'effectiveConnections', so dynamic exits
--   (Rogue Phase 3) shape the map exactly like static ones.
mapGrid :: Int -> Int -> GameState -> Maybe MapGrid
mapGrid w h st
    | null stamped = Nothing
    | otherwise    = Just $ render w h (recentre w h here stamped)
  where
    here = currentRoom (save st)
    visited = Set.insert here (visitedRooms (save st))
    -- BFS over the effective connections of *visited* rooms, stamping every
    -- reachable visited room with lattice coordinates. Two visited rooms that
    -- are only linked by an unexplored path get independent anchors — the map
    -- then shows both clusters, the second rooted at the next unplaced room.
    stamped = go [here] (Map.singleton here (0, 0))
      where
        go [] stamps = stamps
        go (x:xs) stamps =
            let conns = [ (d, dest)
                        | (d, ex) <- Map.toList (effectiveConnections st x)
                        , Just dest <- [exitDest ex]
                        , Set.member dest visited ]
                step (sm, queue) (dir, dest)
                    | Map.member dest sm = (sm, queue)
                    | otherwise = case destCell dir (sm Map.! x) of
                        Nothing -> (sm, queue)
                        Just rc -> (Map.insert dest rc sm, queue ++ [dest])
                (stamps', queue') = foldl step (stamps, xs) conns
            in go queue' stamps'
    -- compass lattice: rows/cols from the direction deltas (Up/Down would
    -- collapse onto the same cell, so they do not place a neighbour)
    destCell dir (row, col) = case dir of
        North     -> Just (row - 1, col)
        South     -> Just (row + 1, col)
        East      -> Just (row, col + 1)
        West      -> Just (row, col - 1)
        Northeast -> Just (row - 1, col + 1)
        Northwest -> Just (row - 1, col - 1)
        Southeast -> Just (row + 1, col + 1)
        Southwest -> Just (row + 1, col - 1)
        _         -> Nothing
    -- translate the lattice so the window's top-left sits at (0,0); once the
    -- explored span outgrows the window, the window slides to keep the
    -- player's cell visible (clamped to the bounding box)
    recentre w' h' hc stamps =
        [ MapCell (row - topR) (col - topC) (stampOf rm) (rm == here) rm
        | (rm, (row, col)) <- Map.toList stamps ]
      where
        spanOf f = (minimum (map f (Map.elems stamps)),
                    maximum (map f (Map.elems stamps)))
        (rmin, rmax) = spanOf fst
        (cmin, cmax) = spanOf snd
        (hr, hcol) = stamps Map.! hc
        topR | rmax - rmin + 1 <= h' = rmin
             | otherwise = max rmin (min (hr - h' `div` 2) (rmax - h' + 1))
        topC | cmax - cmin + 1 <= w' = cmin
             | otherwise = max cmin (min (hcol - w' `div` 2) (cmax - w' + 1))
    stampOf rm = case Map.lookup rm (rooms (world st)) of
        Just room | (c:_) <- roomName room -> toUpper c
        _ -> '?'
    render w' h' cells =
        let byPos = Map.fromList [ ((mcRow m, mcCol m), m) | m <- cells ]
            -- after 'recentre' every coordinate is >= 0; the window is simply
            -- the lattice's bounding box clipped to h' x w'
            maxOf f = foldr (\k acc -> max (f k) acc) 0 (Map.keys byPos)
            rows = [0 .. min (h' - 1) (maxOf fst)]
            cols = [0 .. min (w' - 1) (maxOf snd)]
            -- ◆ on the player's cell, the room's stamp elsewhere, fog dots
            --   for cells adjacent to explored ones, space beyond
            cellCh row col = case Map.lookup (row, col) byPos of
                Just m | mcHere m  -> '\x25C6'
                       | otherwise -> mcStamp m
                Nothing
                    | any (\d -> Map.member (step d (row, col)) byPos)
                          [North, South, East, West] -> '\x00B7'
                    | otherwise -> ' '
            step d (r, c) = case d of
                North -> (r - 1, c); South -> (r + 1, c)
                East  -> (r, c + 1); West  -> (r, c - 1)
                _     -> (r, c)
            -- connector row between two stamp rows: │ when the rooms above
            -- and below are actually linked, fog dot otherwise
            connCh row col
                | Map.member (row - 1, col) byPos && Map.member (row + 1, col) byPos
                , Just above <- Map.lookup (row - 1, col) byPos
                , Just (Open dest) <- Map.lookup South
                                      =<< pure (effectiveConnections st (mcRoom above))
                = if any ((== dest) . mcRoom) (Map.elems byPos)
                      then '\x2502' else '\x00B7'
                | Map.member (row - 1, col) byPos || Map.member (row + 1, col) byPos
                    = '\x00B7'
                | otherwise = ' '
            fogRows  = [ [ cellCh row col | col <- cols ] | row <- rows ]
            connRows = [ [ connCh row col | col <- cols ]
                       | row <- init rows ]
            interleave (r:rs) (c:cs) = r : c : interleave rs cs
            interleave rs []         = rs
            interleave [] cs         = cs
        in MapGrid (length cols) (length rows)
                   [ byPos Map.! (row, col) | row <- rows, col <- cols
                   , Map.member (row, col) byPos ]
                   (interleave fogRows connRows)

-- ---------------------------------------------------------------------------
-- Stats HUD
-- ---------------------------------------------------------------------------

-- | HP gauge over the player's base stats.
hpBar :: GameState -> Bar
hpBar st = Bar "HP" (playerHealth p) (max 1 (playerMaxHealth p))
  where p = player (save st)

-- | Declared numeric variables (mana, gold, ...) as secondary gauges. The
--   engine-reserved combat.* keys stay out; a gauge without a known ceiling
--   scales to double the current value so it never renders empty.
numericBars :: GameState -> [Bar]
numericBars st =
    [ Bar (take 12 name) v (max 1 (if v <= 0 then 1 else v * 2))
    | (name, val) <- sortOn fst (Map.toList (variables (save st)))
    , Just v <- [varInt val]
    , name `notElem` [combatRoundKey, combatActionKey] ]
  where varInt (VVInt n) = Just n
        varInt _         = Nothing

-- | One active status effect: "poison (3)" — remaining turns in parens.
condLine :: Condition -> Maybe String
condLine c
    | condRemaining c <= 0 = Nothing
    | otherwise            = Just (condName c ++ " (" ++ show (condRemaining c) ++ ")")

-- | Equipment as "slot: item" lines in the slot's canonical order.
equipmentLines :: GameState -> [String]
equipmentLines st =
    [ show slot ++ ": " ++ maybe iId itemName (Map.lookup iId (itemDefs (world st)))
    | slot <- [minBound .. maxBound]
    , Just iId <- [Map.lookup slot (equipment (save st))] ]

-- ---------------------------------------------------------------------------
-- Combat panel (plan: visible when combat.engaged >= 1)
-- ---------------------------------------------------------------------------

-- | Combat panel lines: round, action, enemy label + HP when an NPC with
--   remaining health is present. Empty list = no panel (the TUI omits the
--   box entirely, D21).
combatLines :: GameState -> [String]
combatLines st
    | engaged < 1 = []
    | otherwise   = concat
        [ [ "Round " ++ show (combatRound st) ]
        , [ "Action: " ++ a | Just (VVText a) <- [getVariable combatActionKey st] ]
        , maybe [] enemyLines (listToMaybe (mapMaybe living (Map.toList (npcDefs (world st)))))
        ]
  where
    engaged = case getVariable "combat.engaged" st of
        Just (VVInt n) -> n
        _              -> 0
    -- an engaged enemy: an NPC definition with a live state carrying health
    living (nid, def) = case Map.lookup nid (npcStates (save st)) of
        Just ns | npcStatus ns /= "dead"
                , Just hp <- npcHealth ns
                , hp > 0
                , npcMaxHealth def > Just 0
                    -> Just (npcName def, hp, fromMaybe hp (npcMaxHealth def))
        _           -> Nothing
    enemyLines (name, hp, maxH) =
        [ "Enemy: " ++ name
        , "HP: " ++ show hp ++ "/" ++ show maxH ]
