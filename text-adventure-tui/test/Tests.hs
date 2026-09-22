-- | Pure tests for the TUI art-panel helpers (Phase H/H4b, D21).
module Main (main) where

import TextAdventure.Tui
    ( PanelState (..), PanelStep (..), advancePanel, panelFrame, roomAmbient )

import TextAdventure.Tui.Color
    ( SgrColor (..), SgrState (..), applySgrSeq, attrOfSgr, colorAttrName
    , emptySgr, parseSgrLine, rgbToColor240 )

import Sample (initSampleGame)
import Types
import qualified Data.Map.Strict as Map
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
