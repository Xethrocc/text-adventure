-- | Main module for the Haskell text adventure game
module Main where

import GameLoop (runGame)
import Sample (initSampleGame)
import World (loadGame)
import System.Environment (getArgs)
import System.Exit (exitFailure)

usage :: String
usage = unlines
    [ "Usage: text-adventure [--world FILE] [--save FILE]"
    , ""
    , "  --world FILE   Load a GameWorld from a JSON file (produced by the worldbuilder)."
    , "  --save FILE    Load an initial SaveState from a JSON file."
    , "  --help         Show this message."
    , ""
    , "Without --world the bundled sample adventure is used."
    ]

-- | Minimal flag parser: --world X / --save X / --help
parseArgs :: [String] -> Maybe (Maybe FilePath, Maybe FilePath)
parseArgs args = go args Nothing Nothing
  where
    go [] w s = Just (w, s)
    go ("--help" : _) _ _ = Nothing
    go ("--world" : p : rest) _ s = go rest (Just p) s
    go ("--save" : p : rest) w _ = go rest w (Just p)
    go (_ : rest) w s = go rest w s

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Nothing -> putStr usage
        Just (Nothing, _) -> do
            putStrLn "=== Text Adventure Game ==="
            putStrLn "Type 'help' for available commands."
            putStrLn "----------------------------"
            runGame initSampleGame
        Just (Just worldPath, savePath) -> do
            result <- loadGame worldPath savePath
            case result of
                Left err -> do
                    putStrLn ("Failed to load adventure: " ++ err)
                    exitFailure
                Right state -> do
                    putStrLn "=== Text Adventure Game ==="
                    putStrLn ("Loaded world: " ++ worldPath)
                    putStrLn "Type 'help' for available commands."
                    putStrLn "----------------------------"
                    runGame state
