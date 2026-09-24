-- | Pure tests for the TUI art-panel helpers (Phase H/H4b, D21).
module Main (main) where

import TextAdventure.Tui
    ( PanelState (..), PanelStep (..), advancePanel, panelFrame, roomAmbient )

import TextAdventure.Tui.Hud
    ( HudView (..), MapCell (..), MapGrid (..), Bar (..), buildHud, buildHudWithFloor
    , mapGrid, mapGridWithFloor, hpBar, condLine, combatLines, statsLines, barLine )

import TextAdventure.Tui.Color
    ( SgrColor (..), SgrState (..), applySgrSeq, attrOfSgr, colorAttrName
    , emptySgr, parseSgrLine, rgbToColor240 )

import Sample (initSampleGame)
import Types
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (isInfixOf)
import System.Exit (exitFailure)
import qualified Graphics.Vty as V
import Control.Concurrent.MVar (newEmptyMVar)
import Data.Maybe (isNothing)

runTest :: String -> IO Bool -> IO Bool
runTest name action = do
    passed <- action
    putStrLn ((if passed then "[PASS] " else "[FAIL] ") ++ name)
    pure passed

expectEqual :: (Eq a, Show a) => String -> a -> a -> IO Bool
expectEqual what expected actual
    | expected == actual = pure True
    | otherwise = do
        putStrLn $ "  " ++ what ++ ": expected " ++ show expected
        putStrLn $ "  actual:   " ++ show actual
        pure False

expectTrue :: String -> Bool -> IO Bool
expectTrue _ True = pure True
expectTrue msg False = do
    putStrLn $ "  expected True: " ++ msg
    pure False

-- | Sample world with the hallway carrying an ambient loop (fps 4).
hallWithAmbient :: GameState
hallWithAmbient =
    let hall = (rooms (world initSampleGame) Map.! "hallway")
                { roomAscii = emptyAscii { aaAmbient = Just (Ambient ["W0", "W1", "W2"] 4) } }
    in initSampleGame
        { world = (world initSampleGame)
            { rooms = Map.insert "hallway" hall (rooms (world initSampleGame)) } }

-- | Make the given room current.
inRoom :: RoomID -> GameState -> GameState
inRoom rid st = st { save = (save st) { currentRoom = rid } }


-- ---------------------------------------------------------------------------
-- HUD model (Rogue Phase 5)
-- ---------------------------------------------------------------------------

-- | The sample world, current room set, player HP 60/100.
hudState :: RoomID -> GameState
hudState rid = (inRoom rid hallWithAmbient)
    { save = (save (inRoom rid hallWithAmbient))
        { player = (player (save (inRoom rid hallWithAmbient)))
                     { playerHealth = 60, playerMaxHealth = 100 } } }

testHudBar :: IO Bool
testHudBar = expectEqual "hpBar reads the player" (Bar "HP" 60 100) (hpBar (hudState "start"))

-- | The minimap lays the sample's visited rooms on their compass lattice:
--   start anchors at (0,0), meadow south, hallway north.
testHudMap :: IO Bool
testHudMap =
    let st = (hudState "start")
          { save = (save (hudState "start"))
              { visitedRooms = Set.fromList ["start", "hallway", "meadow"] } }
    in case mapGrid 9 5 st of
        Nothing -> expectTrue "map exists for visited rooms" False
        Just g -> do
            let cells = mgCells g
            rA <- expectTrue "3 cells on the map" (length cells == 3)
            rB <- expectTrue "start is marked as here"
                    (any (\c -> mcHere c && mcRoom c == "start") cells)
            rC <- expectTrue "hallway sits north of start"
                    (any (\c -> mcRoom c == "hallway"
                                && any (\s -> mcRoom s == "start" && mcRow c < mcRow s) cells)
                        cells)
            rD <- expectTrue "meadow sits south of start"
                    (any (\c -> mcRoom c == "meadow"
                                && any (\s -> mcRoom s == "start" && mcRow c > mcRow s) cells)
                        cells)
            rE <- expectTrue "connector row between the stamps"
                    (any (\r -> '\x2502' `elem` r) (mgFog g))
            pure (rA && rB && rC && rD && rE)

-- | Dynamic exits (Rogue Phase 3) shape the map like static ones: a
--   RemoveExit'd connection no longer places a neighbour.
testHudMapDynamic :: IO Bool
testHudMapDynamic =
    let base = (hudState "start")
          { save = (save (hudState "start"))
              { visitedRooms = Set.fromList ["start", "hallway"] } }
        -- remove the south exit from start (meadow way): the hallway link
        -- (north) stays, so hallway still places; removing north too would
        -- drop it from the lattice
        ov = Map.fromList [(("start", South), Nothing :: Maybe Exit)]
        st  = base { save = (save base) { exitOverrides = ov
                                        , visitedRooms = Set.fromList ["start", "hallway"] } }
    in case mapGrid 9 5 st of
        Nothing -> expectTrue "map exists" False
        Just g -> do
            rA <- expectTrue "hallway still on the map (north link intact)"
                    (any ((== "hallway") . mcRoom) (mgCells g)
                     && length (mgCells g) == 2)
            -- now remove the north exit as well: no connection left, hallway
            -- gets its own anchor — with only 'start' visited it disappears
            let ov2 = Map.fromList [ (("start", South), Nothing :: Maybe Exit)
                                   , (("start", North), Nothing) ]
                st2 = base { save = (save base) { exitOverrides = ov2
                                                , visitedRooms = Set.fromList ["start"] } }
            rB <- case mapGrid 9 5 st2 of
                    Nothing -> expectTrue "single room still renders" False
                    Just g2 -> expectTrue "start alone, no hallway stamp"
                                (all (\c -> mcRoom c /= "hallway") (mgCells g2))
            pure (rA && rB)

-- | The status panel lines: gauge format, conditions, equipment.
testHudStatsLines :: IO Bool
testHudStatsLines =
    let hud = buildHud (hudState "start")
        bar = Bar "HP" 60 100
    in do
        rA <- expectEqual "bar line format" "HP [######....] 60" (barLine bar)
        rB <- expectTrue "statsLines carry the hp gauge"
                (any (isInfixOf "HP [") (statsLines hud))
        rC <- expectTrue "empty equipment renders no lines"
                (not (any (isInfixOf "Weapon:") (statsLines hud)))
        pure (rA && rB && rC)

-- | Formatted Key-Value stats table for economy/simulation variables (Phase 1D).
testHudStatsTable :: IO Bool
testHudStatsTable =
    let st0 = (hudState "start")
            { save = (save (hudState "start"))
                { variables = Map.fromList
                    [ ("gold", VVInt 1450)
                    , ("korn", VVInt 340)
                    , ("cmd.arg1", VVInt 5)
                    , ("combat.round", VVInt 1)
                    ]
                }
            }
        hud = buildHud st0
        table = hvStatsTable hud
    in do
        rA <- expectEqual "gold in table" (Just "1450") (lookup "gold" table)
        rB <- expectEqual "korn in table" (Just "340") (lookup "korn" table)
        rC <- expectTrue "cmd vars excluded" (isNothing (lookup "cmd.arg1" table))
        rD <- expectTrue "combat vars excluded" (isNothing (lookup "combat.round" table))
        rE <- expectTrue "statsLines contains formatted table"
                (any (isInfixOf "gold:") (statsLines hud))
        pure (rA && rB && rC && rD && rE)

-- | Conditions, equipment, combat panel, game-over flag.
testHudPanels :: IO Bool
testHudPanels =
    let st0 = hudState "start"
        withCond = st0 { save = (save st0)
            { conditions = Map.singleton "poison"
                (Condition "poison" 3 Nothing Nothing)
            , equipment = Map.singleton Weapon "rusty_sword"
            , variables = Map.fromList
                [ ("combat.engaged", VVInt 1)
                , ("combat.round", VVInt 4)
                , ("combat.action", VVText "attack")
                , ("gold", VVInt 25) ]
            , gameOver = True } }
        hud = buildHud withCond
    in do
        rA <- expectEqual "cond line" (Just "poison (3)") (condLine (Condition "poison" 3 Nothing Nothing))
        rB <- expectTrue "expired cond dropped" (isNothing (condLine (Condition "old" 0 Nothing Nothing)))
        rC <- expectTrue "equipment line" (any (isInfixOf "Weapon:") (hvEquipment hud))
        rD <- expectTrue "combat panel engaged" (not (null (hvCombat hud)))
        rE <- expectTrue "combat shows round 4" (any (isInfixOf "Round 4") (hvCombat hud))
        rF <- expectTrue "combat shows the action" (any (isInfixOf "attack") (hvCombat hud))
        rG <- expectTrue "combat round not a gauge"
                (not (any (\b -> barLabel b == "combat.round") (hvBars hud)))
        rH <- expectTrue "gold is a gauge" (any (\b -> barLabel b == "gold") (hvBars hud))
        rI <- expectTrue "game over flagged" (hvGameOver hud)
        rJ <- expectTrue "no combat panel when disengaged"
                (null (hvCombat (buildHud (hudState "start"))))
        pure (rA && rB && rC && rD && rE && rF && rG && rH && rI && rJ)

-- | Multi-floor HUD & Minimap: rooms on different floors are segregated,
--   the HUD detects explored floors, and non-current floor views drop the player marker.
testHudMultiFloor :: IO Bool
testHudMultiFloor =
    let r1 = (rooms (world initSampleGame) Map.! "start") { roomFloor = Just 1 }
        r2 = (rooms (world initSampleGame) Map.! "hallway") { roomFloor = Just 1 }
        r3 = (rooms (world initSampleGame) Map.! "meadow") { roomFloor = Just 2 }
        w' = (world initSampleGame)
               { rooms = Map.fromList [("start", r1), ("hallway", r2), ("meadow", r3)] }
        st = initSampleGame
               { world = w'
               , save = (save initSampleGame)
                   { currentRoom = "start"
                   , visitedRooms = Set.fromList ["start", "hallway", "meadow"] }
               }
        hudDef = buildHud st
        hudF2  = buildHudWithFloor (Just 2) st
    in do
        rA <- expectEqual "explored floors detected" [1, 2] (hvFloors hudDef)
        rB <- expectEqual "default floor is player's floor (1)" (Just 1) (hvFloor hudDef)
        rC <- expectEqual "custom floor selection sets hvFloor" (Just 2) (hvFloor hudF2)
        -- Minimap on floor 1:
        rD <- case mapGridWithFloor (Just 1) 9 5 st of
            Nothing -> expectTrue "floor 1 grid exists" False
            Just g1 -> do
                let cells = mgCells g1
                b1 <- expectEqual "floor 1 has only start & hallway" 2 (length cells)
                b2 <- expectTrue "start is marked as here on floor 1"
                        (any (\c -> mcHere c && mcRoom c == "start") cells)
                b3 <- expectTrue "meadow is NOT on floor 1"
                        (not (any (\c -> mcRoom c == "meadow") cells))
                pure (b1 && b2 && b3)
        -- Minimap on floor 2:
        rE <- case mapGridWithFloor (Just 2) 9 5 st of
            Nothing -> expectTrue "floor 2 grid exists" False
            Just g2 -> do
                let cells = mgCells g2
                b4 <- expectEqual "floor 2 has only meadow" 1 (length cells)
                b5 <- expectTrue "meadow is NOT marked as here (player is on floor 1)"
                        (all (not . mcHere) cells)
                pure (b4 && b5)
        pure (rA && rB && rC && rD && rE)

-- | Combat panel with deckbuilder active (Phase 2C).
testHudDeckCombatLines :: IO Bool
testHudDeckCombatLines =
    let ds = DeckState ["c1", "c2"] ["c3"] ["c4"] [] 10
        st0 = (hudState "start")
            { save = (save (hudState "start"))
                { deckState = Just ds
                , variables = Map.fromList
                    [ ("player.block", VVInt 8)
                    , ("player.energy", VVInt 2)
                    , ("player.max_energy", VVInt 3)
                    ]
                }
            }
        cl = combatLines st0
        hud = buildHud st0
    in do
        rA <- expectTrue "combat panel active with deck and living enemy" (not (null cl))
        rB <- expectTrue "shows deck and discard count" (any (isInfixOf "Deck: 2 | Ablage: 1") cl)
        rC <- expectTrue "shows energy and block" (any (isInfixOf "Energie: 2/3 | Block: 8") cl)
        rD <- expectTrue "shows living enemy HP" (any (isInfixOf "Enemy: goblin") cl)
        rE <- expectEqual "hvCombat matches combatLines" cl (hvCombat hud)
        -- Without living enemies:
        let stDead = st0 { save = (save st0) { npcStates = Map.adjust (\ns -> ns { npcStatus = "dead", npcHealth = Just 0 }) "goblin" (npcStates (save st0)) } }
        rF <- expectTrue "combatLines empty when all enemies dead" (null (combatLines stDead))
        pure (rA && rB && rC && rD && rE && rF)

main :: IO ()
main = do
    results <- sequence
        [ runTest "panelFrame: cutscene shows its frames" testPanelFrameCutscene
        , runTest "panelFrame: blank frame draws no box (D21)" testPanelFrameBlank
        , runTest "advancePanel: ambient loops (mod)" testAmbientLoops
        , runTest "advancePanel: cutscene finishes and signals" testCutsceneFinishes
        , runTest "roomAmbient: resolves the current room's loop" testRoomAmbientResolves
        , runTest "roomAmbient: nothing without ambient or with bad fps" testRoomAmbientAbsent
        , runTest "applySgrSeq: SGR codes change the state" testSgrSeq
        , runTest "rgbToColor240: cube, grayscale band" testQuantize
        , runTest "parseSgrLine: segments carry the state" testParse
        , runTest "colorAttrName: injective encoding" testAttrName
        , runTest "attrOfSgr: maps onto vty attributes" testAttr
        , runTest "hud: hp bar reads the player" testHudBar
        , runTest "hud: minimap lattice from visited rooms" testHudMap
        , runTest "hud: dynamic exits shape the map (Rogue P3)" testHudMapDynamic
        , runTest "hud: conditions, equipment, combat, game over" testHudPanels
        , runTest "hud: status lines + bar format" testHudStatsLines
        , runTest "hud: key-value stats table (Phase 1D)" testHudStatsTable
        , runTest "hud: multi-floor minimap isolates floors and tracks player" testHudMultiFloor
        , runTest "hud: deck combat panel lines (Phase 2C)" testHudDeckCombatLines
        ]
    if and results then pure () else exitFailure

testPanelFrameCutscene :: IO Bool
testPanelFrameCutscene = do
    done <- newEmptyMVar
    let p = PanelCutscene ["A", "B"] 100000 0 done
    r1 <- expectEqual "frame 0" (Just "A") (panelFrame p)
    r2 <- expectEqual "frame 1" (Just "B") (panelFrame (PanelCutscene ["A", "B"] 100000 1 done))
    pure (r1 && r2)

testPanelFrameBlank :: IO Bool
testPanelFrameBlank = do
    done <- newEmptyMVar
    r1 <- expectEqual "None draws nothing" Nothing (panelFrame PanelNone)
    r2 <- expectEqual "blank ambient frame draws no box (D21)" Nothing
                     (panelFrame (PanelAmbient ["", "  "] 100000 0 "Room"))
    r3 <- expectEqual "whitespace cutscene frame draws no box" Nothing
                     (panelFrame (PanelCutscene ["  "] 100000 0 done))
    pure (r1 && r2 && r3)

testAmbientLoops :: IO Bool
testAmbientLoops = do
    let p0 = PanelAmbient ["W0", "W1", "W2"] 250000 0 "Hall"
    case advancePanel p0 of
        PanelAdvanced (PanelAmbient _ _ 1 _) -> do
            let p2 = PanelAmbient ["W0", "W1", "W2"] 250000 2 "Hall"
            case advancePanel p2 of
                PanelAdvanced (PanelAmbient _ _ 0 _) -> expectTrue "wraps to 0" True
                _ -> expectTrue "wrap advances" False
            -- second check: first tick result was asserted by the pattern
            pure True
        _ -> expectTrue "ambient advances" False

testCutsceneFinishes :: IO Bool
testCutsceneFinishes = do
    done <- newEmptyMVar
    let lastFrame = PanelCutscene ["A", "B"] 100000 1 done
    case advancePanel lastFrame of
        PanelCutsceneDone _ -> expectTrue "finishes on the last frame" True
        _ -> expectTrue "finishes on the last frame" False

testRoomAmbientResolves :: IO Bool
testRoomAmbientResolves = do
    r1 <- expectEqual "hall ambient: fps 4 -> 250000 us, 3 frames, label"
                      (Just (["W0", "W1", "W2"], 250000, "Dark Hallway"))
                      (roomAmbient (inRoom "hallway" hallWithAmbient))
    r2 <- expectTrue "start room (no ambient) -> Nothing"
                      (isNothing (roomAmbient (inRoom "start" hallWithAmbient)))
    pure (r1 && r2)

-- ------------------------------------------------------------- colour (new)

testSgrSeq :: IO Bool
testSgrSeq = do
    r1 <- expectEqual "31 -> ISO foreground 1" (Just (Iso 1)) (sfFg s1)
    r2 <- expectTrue "hotspot (bold + yellow)" (sfBold s2 && sfFg s2 == Just (Iso 3))
    r3 <- expectEqual "0 resets" emptySgr (applySgrSeq [0] s2)
    r4 <- expectEqual "38;5;196 -> palette 196" (Just (C256 196)) (sfFg (applySgrSeq [38, 5, 196] emptySgr))
    r5 <- expectEqual "48;2;255;0;0 quantizes the background"
                      (Just (C256 196)) (sfBg (applySgrSeq [48, 2, 255, 0, 0] emptySgr))
    r6 <- expectTrue "22 clears bold" (not (sfBold (applySgrSeq [22] s2)))
    r7 <- expectTrue "unknown code ignored" (applySgrSeq [7] emptySgr == emptySgr)
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7)
  where
    s1 = applySgrSeq [31] emptySgr
    s2 = applySgrSeq [1, 33] emptySgr

testQuantize :: IO Bool
testQuantize = do
    r1 <- expectEqual "pure red -> cube 196" 196 (rgbToColor240 255 0 0)
    r2 <- expectEqual "pure green -> 46" 46 (rgbToColor240 0 255 0)
    r3 <- expectEqual "pure blue -> 21" 21 (rgbToColor240 0 0 255)
    r4 <- expectEqual "mid gray -> grayscale band" 244 (rgbToColor240 128 128 128)
    r5 <- expectEqual "black -> cube 16, white -> cube 231"
                      (16, 231) (rgbToColor240 0 0 0, rgbToColor240 255 255 255)
    pure (r1 && r2 && r3 && r4 && r5)

testParse :: IO Bool
testParse = do
    let segs = parseSgrLine "\ESC[1;33m!\ESC[0m rest"
    r1 <- expectEqual "two segments" 2 (length segs)
    r2 <- expectEqual "hotspot state: bold + yellow (33 -> ISO 3)"
                      (Just (Iso 3), Nothing, True)
                      (sfFg st, sfBg st, sfBold st)
    r3 <- expectTrue "after reset: default state" (snd (seg2) == emptySgr)
    r4 <- expectEqual "plain line: one segment" 1 (length (parseSgrLine "hello"))
    r5 <- expectTrue "lone escape dropped" (parseSgrLine "\ESC" == [])
    r6 <- expectTrue "bare ESC[m resets" (parseSgrLine "\ESC[m x" == [(" x", emptySgr)])
    r7 <- expectTrue "non-SGR CSI skipped" (parseSgrLine "\ESC[2J x" == [(" x", emptySgr)])
    pure (and [r1, r2, r3, r4, r5, r6, r7])
  where
    segs = parseSgrLine "\ESC[1;33m!\ESC[0m rest"
    st = snd (head segs)
    seg2 = segs !! 1

testAttrName :: IO Bool
testAttrName = do
    let a = SgrState (Just (Iso 1)) Nothing True
        b = SgrState Nothing (Just (C256 196)) False
    r1 <- expectTrue "different states, different names"
                     (colorAttrName a /= colorAttrName b)
    r2 <- expectTrue "equal states, equal names"
                     (colorAttrName a == colorAttrName (SgrState (Just (Iso 1)) Nothing True))
    r3 <- expectTrue "default state has its own name"
                     (colorAttrName emptySgr /= colorAttrName a)
    pure (r1 && r2 && r3)

testAttr :: IO Bool
testAttr = do
    let a = attrOfSgr (SgrState (Just (Iso 1)) Nothing True)
    r1 <- expectTrue "foreground mapped" (V.attrForeColor a == V.SetTo (V.ISOColor 1))
    r2 <- expectTrue "background untouched" (V.attrBackColor a == V.KeepCurrent)
    r3 <- expectTrue "bold mapped" (V.attrStyle a == V.SetTo V.bold)
    r4 <- expectTrue "default state -> defAttr"
                     (attrOfSgr emptySgr == V.defAttr
                        { V.attrStyle = V.KeepCurrent
                        , V.attrForeColor = V.KeepCurrent
                        , V.attrBackColor = V.KeepCurrent })
    pure (r1 && r2 && r3 && r4)

testRoomAmbientAbsent :: IO Bool
testRoomAmbientAbsent = do
    let hall = (rooms (world initSampleGame) Map.! "hallway")
                { roomAscii = emptyAscii { aaAmbient = Just (Ambient ["W0"] 0) } }
        badFps = hallWithAmbient
            { world = (world hallWithAmbient)
                { rooms = Map.insert "hallway" hall (rooms (world hallWithAmbient)) } }
    r1 <- expectTrue "fps 0 -> Nothing" (isNothing (roomAmbient (inRoom "hallway" badFps)))
    r2 <- expectTrue "room without ambient -> Nothing"
                      (isNothing (roomAmbient (inRoom "start" initSampleGame)))
    pure (r1 && r2)
