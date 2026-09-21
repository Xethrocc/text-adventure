{-# LANGUAGE CPP #-}

-- | Main module for the Haskell text adventure game
module Main where

import GameLoop (runGameWith)
import Ansi (ansiFilter)
import Game (resolveAsciiArt)
import Sample (initSampleGame)
import World (loadGame)
import Types (GameState, world, save, worldName, worldTitleArt, isEmptyAscii)
import Validate (validateWorld, validateGameState)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hIsTerminalDevice, hSetEncoding, stdout, stderr, stdin, utf8)

#if defined(mingw32_HOST_OS)
import Data.Bits ((.|.))
import Data.Word (Word32)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr, wordPtrToPtr)
import Foreign.Storable (peek, poke)

foreign import ccall unsafe "SetConsoleCP" c_SetConsoleCP :: Word32 -> IO Bool
foreign import ccall unsafe "SetConsoleOutputCP" c_SetConsoleOutputCP :: Word32 -> IO Bool
foreign import ccall unsafe "GetStdHandle" c_GetStdHandle :: Word32 -> IO (Ptr ())
foreign import ccall unsafe "GetConsoleMode" c_GetConsoleMode :: Ptr () -> Ptr Word32 -> IO Bool
foreign import ccall unsafe "SetConsoleMode" c_SetConsoleMode :: Ptr () -> Word32 -> IO Bool

-- | STD_OUTPUT_HANDLE = (DWORD)(-11); as a HANDLE it is sign-extended.
stdOutputHandle :: Word32
stdOutputHandle = 0xFFFFFFF5   -- (-11) truncated to DWORD width

-- | W3: try to enable ANSI escape processing on the classic Windows console
--   (ENABLE_VIRTUAL_TERMINAL_PROCESSING, 0x0004). Windows Terminal has VT on by
--   default, so this is a no-op there; on old conhosts it is what makes colour
--   art readable instead of `[38;5;…m`-mush. Preserves the existing mode bits
--   and reports failure cleanly, so the caller can fall back to monochrome
--   output instead of printing raw sequences a terminal cannot render.
enableVT :: IO Bool
enableVT = do
    h <- c_GetStdHandle stdOutputHandle
    if h == nullPtr || h == invalidHandle
        then pure False
        else
            alloca $ \buf -> do
                poke buf 0
                ok <- c_GetConsoleMode h buf
                if not ok
                    then pure False   -- redirected, or not a console at all
                    else do
                        old <- peek buf
                        c_SetConsoleMode h (old .|. vtBit)
  where
    vtBit = 0x0004 :: Word32
    -- INVALID_HANDLE_VALUE (-1) as a HANDLE: the all-ones bit pattern converted
    -- to a pointer (Foreign.Ptr.wordPtrToPtr takes a `WordPtr`).
    invalidHandle = wordPtrToPtr maxBound

-- | W3: initialise the Windows console. Returns whether ANSI escapes are safe
--   to emit on stdout (VT enabled or already on).
initConsoleColor :: IO Bool
initConsoleColor = enableVT

initConsole :: IO ()
initConsole = do
    _ <- c_SetConsoleCP 65001
    _ <- c_SetConsoleOutputCP 65001
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    hSetEncoding stdin utf8
#else
-- | Non-Windows: VT escapes always render; the policy is the caller's TTY check.
initConsoleColor :: IO Bool
initConsoleColor = pure True

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

-- | Title banner: the authored `title_art` when present, otherwise the
--   one-line `bannerFor` default (backwards compatible, Phase G).
titleBanner :: GameState -> String
titleBanner st =
    let art = worldTitleArt (world st)
    in if isEmptyAscii art
       then bannerFor (worldName (world st))
       else resolveAsciiArt art st

main :: IO ()
main = do
    initConsole
    args <- getArgs
    case parseArgs args of
        Nothing -> putStr usage
        Just opts -> do
            tty <- hIsTerminalDevice stdout
            -- W3: on Windows, VT processing must be on for escapes to render.
            -- It is attempted once here; when it fails (old conhost), colour is
            -- dropped as if the terminal could not show it — the fallback the
            -- existing filter already implements.
            vt <- initConsoleColor
            let outFilter = ansiFilter (tty && vt) (coNoColor opts)
            case coWorld opts of
                Nothing -> do
                    putStrLn (outFilter (titleBanner initSampleGame))
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
                            putStrLn (outFilter (titleBanner state))
                            putStrLn ("Loaded world: " ++ worldPath)
                            putStrLn "Type 'help' for available commands."
                            putStrLn "----------------------------"
                            runGameWith outFilter state