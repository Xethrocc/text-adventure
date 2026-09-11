-- | Main module for the Haskell text adventure game
module Main where

import GameLoop (runGame)
import Sample (initSampleGame)
import World (loadGame)
import Types (world, save)
import Validate (validateWorld, validateGameState)
import System.Environment (getArgs)
import System.Exit (exitFailure)

usage :: String
usage = unlines
    [ "Usage: text-adventure [--world FILE] [--save FILE] [--allow-invalid]"
    , ""
    , "  --world FILE      Load a GameWorld from a JSON file (produced by the worldbuilder)."
    , "  --save FILE       Load an initial SaveState from a JSON file."
    , "  --allow-invalid   Start even if the world has validation issues."
    , "  --help            Show this message."
    , ""
    , "Without --world the bundled sample adventure is used."
    ]

-- | Minimal flag parser
parseArgs :: [String] -> Maybe (Maybe FilePath, Maybe FilePath, Bool)
parseArgs args = go args Nothing Nothing False
  where
    go [] w s a = Just (w, s, a)
    go ("--help" : _) _ _ _ = Nothing
    go ("--world" : p : rest) _ s a = go rest (Just p) s a
    go ("--save" : p : rest) w _ a = go rest w (Just p) a
    go ("--allow-invalid" : rest) w s _ = go rest w s True
    go (_ : rest) w s a = go rest w s a

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Nothing -> putStr usage
        Just (Nothing, _, _) -> do
            putStrLn "=== Text Adventure Game ==="
            putStrLn "Type 'help' for available commands."
            putStrLn "----------------------------"
            runGame initSampleGame
        Just (Just worldPath, savePath, allowInvalid) -> do
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
                        if allowInvalid
                        then putStrLn "(starting with --allow-invalid)"
                        else do
                            putStrLn "Use --allow-invalid to start with these issues."
                            exitFailure
                    else return ()
                    putStrLn "=== Text Adventure Game ==="
                    putStrLn ("Loaded world: " ++ worldPath)
                    putStrLn "Type 'help' for available commands."
                    putStrLn "----------------------------"
                    runGame state