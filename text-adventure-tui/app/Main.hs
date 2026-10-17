-- | CLI entry point for the brick-based terminal UI (Phase T).
--
--   Loads the same worlds as the main CLI (--world / --save, with the same
--   validation gate) and hands the game to 'runTui'. Since the main CLI
--   moved to its own package (text-adventure-cli) it offers `--tui` there;
--   this executable remains the standalone entry for the brick UI alone.
module Main where

import TextAdventure.Tui (CliOptions (..), parseArgs, resolveTuiAudio, runTuiWithAudio)
import Game (resolveAsciiArt)
import GameLoop (initRunSeed)
import Sample (initSampleGame)
import Types (GameState, world, save, worldName, worldTitleArt, isEmptyAscii)
import Validate (validateWorld, validateGameState)
import World (loadGame, siblingSavePath)
import System.Environment (getArgs, setEnv)
import System.Exit (exitFailure)
import System.IO (hSetEncoding, stdout, stderr, stdin, utf8)

usage :: String
usage = unlines
    [ "Usage: text-adventure-tui [--world FILE] [--save FILE] [--seed N] [--allow-invalid] [--saves-dir DIR] [--no-audio] [--audio-helper P] [--help]"
    , ""
    , "  --world FILE      Load a GameWorld from a JSON file (produced by the worldbuilder)."
    , "  --save FILE       Load an initial SaveState from a JSON file."
    , "  --seed N          Pin the run seed (wandering NPC routes). Without it a fresh"
    , "                    game draws a new seed; an explicit --save keeps its stored one."
    , "  --allow-invalid   Start even if the world has validation issues."
    , "  --saves-dir DIR   Redirect in-game saves (saves/<slot>.json) to DIR. Wins over"
    , "                    the TA_SAVES_DIR environment variable (Rogue Phase 0)."
    , "  --no-audio        Disable SFX playback entirely (Audio Phase 1)."
    , "  --audio-helper P  Use executable at P to play sound effects."
    , "  --help            Show this message."
    , ""
    , "Without --world the bundled sample adventure is used."
    ]

-- | Title banner: the authored `title_art` when present, otherwise the world
--   name — the same rule as the Haskeline CLI's `titleBanner`.
titleFor :: GameState -> String
titleFor st =
    let art = worldTitleArt (world st)
    in if isEmptyAscii art
       then worldName (world st)
       else resolveAsciiArt art st

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    hSetEncoding stdin utf8
    args <- getArgs
    case parseArgs args of
        Nothing -> putStr usage
        Just opts -> do
            -- Rogue Phase 0: redirect the save directory on request (wins
            -- over TA_SAVES_DIR, same rule as the main CLI).
            maybe (pure ()) (setEnv "TA_SAVES_DIR") (coSavesDir opts)
            -- OPEN-14: the run seed — `--seed` pins it, an explicit `--save`
            -- keeps its stored seed, every other start draws a fresh one.
            let prep st = initRunSeed (coSave opts /= Nothing) (coSeed opts) st
            case coWorld opts of
                Nothing -> prep initSampleGame >>= start opts
                Just worldPath -> do
                    savePath <- case coSave opts of
                        Just s  -> pure (Just s)
                        Nothing -> siblingSavePath worldPath
                    result <- loadGame worldPath savePath
                    case result of
                        Left err -> do
                            putStrLn ("Failed to load adventure: " ++ err)
                            exitFailure
                        Right loaded -> do
                            state <- prep loaded
                            let valErrors = validateWorld (world state)
                                            ++ validateGameState (world state) (save state)
                            if not (null valErrors)
                            then do
                                putStrLn "World validation reported issues:"
                                mapM_ (\err -> putStrLn ("  - " ++ show err)) valErrors
                                if coAllowInvalid opts
                                then putStrLn "(starting with --allow-invalid)" >> start opts state
                                else do
                                    putStrLn "Use --allow-invalid to start with these issues."
                                    exitFailure
                            else start opts state
  where
    -- `lines` splits the (possibly multi-line) title art into individual
    -- rows so the UI can render/protect each row on its own.
    start opts state = do
        audioCfg <- resolveTuiAudio (coNoAudio opts) (coAudioHelper opts)
        runTuiWithAudio
            audioCfg
            (concatMap lines
                [ titleFor state
                , "Type 'help' for available commands."
                , "----------------------------"
                ])
            state
