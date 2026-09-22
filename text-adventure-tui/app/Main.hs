-- | CLI entry point for the brick-based terminal UI (Phase T).
--
--   Loads the same worlds as the main CLI (--world / --save, with the same
--   validation gate) and hands the game to 'runTui'. Since the main CLI
--   moved to its own package (text-adventure-cli) it offers `--tui` there;
--   this executable remains the standalone entry for the brick UI alone.
module Main where

import TextAdventure.Tui (runTui)
import Game (resolveAsciiArt)
import Sample (initSampleGame)
import Types (GameState, world, save, worldName, worldTitleArt, isEmptyAscii)
import Validate (validateWorld, validateGameState)
import World (loadGame, siblingSavePath)
import System.Environment (getArgs, setEnv)
import System.Exit (exitFailure)
import System.IO (hSetEncoding, stdout, stderr, stdin, utf8)

usage :: String
usage = unlines
    [ "Usage: text-adventure-tui [--world FILE] [--save FILE] [--allow-invalid] [--saves-dir DIR] [--help]"
    , ""
    , "  --world FILE      Load a GameWorld from a JSON file (produced by the worldbuilder)."
    , "  --save FILE       Load an initial SaveState from a JSON file."
    , "  --allow-invalid   Start even if the world has validation issues."
    , "  --saves-dir DIR   Redirect in-game saves (saves/<slot>.json) to DIR. Wins over"
    , "                    the TA_SAVES_DIR environment variable (Rogue Phase 0)."
    , "  --help            Show this message."
    , ""
    , "Without --world the bundled sample adventure is used."
    ]

data CliOptions = CliOptions
    { coWorld        :: Maybe FilePath
    , coSave         :: Maybe FilePath
    , coAllowInvalid :: Bool
    , coSavesDir     :: Maybe FilePath
    }

parseArgs :: [String] -> Maybe CliOptions
parseArgs args = go args (CliOptions Nothing Nothing False Nothing)
  where
    go [] opts = Just opts
    go ("--help" : _) _ = Nothing
    go ("--world" : p : rest) opts = go rest opts { coWorld = Just p }
    go ("--save" : p : rest) opts = go rest opts { coSave = Just p }
    go ("--allow-invalid" : rest) opts = go rest opts { coAllowInvalid = True }
    go ("--saves-dir" : p : rest) opts = go rest opts { coSavesDir = Just p }
    go (_ : rest) opts = go rest opts

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
            case coWorld opts of
                Nothing -> start initSampleGame
                Just worldPath -> do
                    savePath <- case coSave opts of
                        Just s  -> pure (Just s)
                        Nothing -> siblingSavePath worldPath
                    result <- loadGame worldPath savePath
                    case result of
                        Left err -> do
                            putStrLn ("Failed to load adventure: " ++ err)
                            exitFailure
                        Right state -> do
                            let valErrors = validateWorld (world state)
                                            ++ validateGameState (world state) (save state)
                            if not (null valErrors)
                            then do
                                putStrLn "World validation reported issues:"
                                mapM_ (\err -> putStrLn ("  - " ++ show err)) valErrors
                                if coAllowInvalid opts
                                then putStrLn "(starting with --allow-invalid)" >> start state
                                else do
                                    putStrLn "Use --allow-invalid to start with these issues."
                                    exitFailure
                            else start state
  where
    -- `lines` splits the (possibly multi-line) title art into individual
    -- rows so the UI can render/protect each row on its own.
    start state = runTui
        (concatMap lines
            [ titleFor state
            , "Type 'help' for available commands."
            , "----------------------------"
            ])
        state
