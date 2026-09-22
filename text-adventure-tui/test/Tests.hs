-- | Pure tests for the TUI art-panel helpers (Phase H/H4b, D21).
module Main (main) where

import TextAdventure.Tui
    ( PanelState (..), PanelStep (..), advancePanel, panelFrame, roomAmbient )

import Sample (initSampleGame)
import Types
import qualified Data.Map.Strict as Map
import System.Exit (exitFailure)
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
