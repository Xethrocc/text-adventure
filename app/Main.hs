{-# LANGUAGE CPP #-}

-- | Main module for the Haskell text adventure game
module Main where

import GameLoop (runGameWith)
import Ansi (ansiFilter)
import Sample (initSampleGame)
import World (loadGame)
import Types (world, save, worldName)
import Validate (validateWorld, validateGameState)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hIsTerminalDevice, hSetEncoding, stdout, stderr, stdin, utf8)

#if defined(mingw32_HOST_OS)
import Data.Word (Word32)
foreign import ccall unsafe "SetConsoleCP" c_SetConsoleCP :: Word32 -> IO Bool
foreign import ccall unsafe "SetConsoleOutputCP" c_SetConsoleOutputCP :: Word32 -> IO Bool

initConsole :: IO ()
initConsole = do
    _ <- c_SetConsoleCP 65001
    _ <- c_SetConsoleOutputCP 65001
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    hSetEncoding stdin utf8
#else
initConsole :: IO ()
initConsole = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    hSetEncoding stdin utf8
#endif

usage :: String
usage = unlines
    [ "Usage: text-adventure [--world FILE] [--save FILE] [--allow-invalid] [--no-color]"
    , ""
    , "  --world FILE      Load a GameWorld from a JSON file (produced by the worldbuilder)."
    , "  --save FILE       Load an initial SaveState from a JSON file."
    , "  --allow-invalid   Start even if the world has validation issues."
    , "  --no-color        Strip ANSI colour from the output (also done automatically"
    , "                    when stdout is not a terminal)."
    , "  --color           Allow ANSI colour when stdout is a terminal (default)."
    , "  --help            Show this message."
    , ""
    , "Without --world the bundled sample adventure is used."
    ]

-- | Parsed command line.
data CliOptions = CliOptions
    { coWorld        :: Maybe FilePath
    , coSave         :: Maybe FilePath
    , coAllowInvalid :: Bool
    , coNoColor      :: Bool
    }

-- | Minimal flag parser
parseArgs :: [String] -> Maybe CliOptions
parseArgs args = go args (CliOptions Nothing Nothing False False)
  where
    go [] opts = Just opts
    go ("--help" : _) _ = Nothing
    go ("--world" : p : rest) opts = go rest opts { coWorld = Just p }
    go ("--save" : p : rest) opts = go rest opts { coSave = Just p }
    go ("--allow-invalid" : rest) opts = go rest opts { coAllowInvalid = True }
    go ("--no-color" : rest) opts = go rest opts { coNoColor = True }
    go ("--color" : rest) opts = go rest opts { coNoColor = False }
    go (_ : rest) opts = go rest opts

-- | Banner line: the adventure title when the world carries one (P2-18).
bannerFor :: String -> String
bannerFor n
    | null n    = "=== Text Adventure Game ==="
    | otherwise = "=== " ++ n ++ " ==="

main :: IO ()
main = do
    initConsole
    args <- getArgs
    case parseArgs args of
        Nothing -> putStr usage
        Just opts -> do
            tty <- hIsTerminalDevice stdout
            let outFilter = ansiFilter tty (coNoColor opts)
            case coWorld opts of
                Nothing -> do
                    putStrLn (bannerFor (worldName (world initSampleGame)))
                    putStrLn "Type 'help' for available commands."
                    putStrLn "----------------------------"
                    runGameWith outFilter initSampleGame
                Just worldPath -> do
                    result <- loadGame worldPath (coSave opts)
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
                                then putStrLn "(starting with --allow-invalid)"
                                else do
                                    putStrLn "Use --allow-invalid to start with these issues."
                                    exitFailure
                            else return ()
                            putStrLn (bannerFor (worldName (world state)))
                            putStrLn ("Loaded world: " ++ worldPath)
                            putStrLn "Type 'help' for available commands."
                            putStrLn "----------------------------"
                            runGameWith outFilter state