-- | Audio helper discovery, SFX and music playback (Audio Phase 1 + 2).
--
--   The engine never links against audio libraries (Architecture A1); instead
--   it spawns external helpers (@mpv@, @ffplay@, etc.) to play sound files.
--   Discovery follows a priority chain (CLI flag → environment variable →
--   PATH search) and is performed once at startup. When no helper is found,
--   audio is silently disabled — same degrade philosophy as VT\/colour (W3).
module Audio
  ( AudioConfig(..)
  , AudioHelper(..)
  , MusicHandle
  , discoverAudio
  , playSfx
  , newMusicHandle
  , startMusic
  , stopMusic
  , noAudio
  ) where

import System.Environment (lookupEnv)
import System.Directory   (findExecutable)
import System.Process     (createProcess, proc, ProcessHandle,
                           terminateProcess, waitForProcess,
                           std_out, std_err, std_in,
                           StdStream(..), spawnCommand)
import System.IO          (hClose)
import Control.Monad      (void)
import Control.Exception  (catch, SomeException)
import Data.IORef
import Data.Maybe         (listToMaybe, catMaybes)
import Data.Char          (toLower)
import Data.List          (isSuffixOf)

-- | Identified audio helper with its base name for argument selection.
data AudioHelper = AudioHelper
    { ahPath :: FilePath   -- ^ Full path to the executable
    , ahName :: String     -- ^ Base name ("mpv", "ffplay", etc.)
    } deriving (Show)

-- | Runtime audio configuration, resolved once at startup.
data AudioConfig
    = AudioOff                        -- ^ No helper found or --no-audio
    | AudioOn AudioHelper             -- ^ A working audio helper
    deriving (Show)

-- | Mutable handle for the currently playing music process.
data MusicHandle = MusicHandle
    { mhProcess :: IORef (Maybe (ProcessHandle, FilePath))
    }

-- | Create a fresh music handle (no music playing).
newMusicHandle :: IO MusicHandle
newMusicHandle = MusicHandle <$> newIORef Nothing

-- | Sentinel for disabled audio.
noAudio :: AudioConfig
noAudio = AudioOff

-- | Discover an audio helper.
--
--   Priority chain (first hit wins, cached for the session):
--
--   1. Explicit path from @--audio-helper@  (passed as @Maybe FilePath@)
--   2. @TEXT_ADVENTURE_AUDIO_HELPER@ environment variable
--   3. PATH search for known candidates: @mpv@, @ffplay@, @paplay@
--   4. Nothing found → 'AudioOff'
discoverAudio :: Maybe FilePath -> IO AudioConfig
discoverAudio mCliHelper = do
    mEnvHelper <- lookupEnv "TEXT_ADVENTURE_AUDIO_HELPER"
    case mCliHelper of
        Just p  -> pure (AudioOn (AudioHelper p (baseName p)))   -- 1. CLI flag wins
        Nothing -> case mEnvHelper of
            Just p  -> pure (AudioOn (AudioHelper p (baseName p)))   -- 2. Environment variable
            Nothing -> do                 -- 3. PATH search
                found <- firstHelper ["mpv", "ffplay", "paplay"]
                case found of
                    Just ah -> pure (AudioOn ah)
                    Nothing -> pure AudioOff  -- 4. Graceful degrade

-- | Find the first available helper from a list of candidate names.
firstHelper :: [String] -> IO (Maybe AudioHelper)
firstHelper [] = pure Nothing
firstHelper (name:rest) = do
    mPath <- findExecutable name
    case mPath of
        Just p  -> pure (Just (AudioHelper p name))
        Nothing -> firstHelper rest

-- | Extract the base name from a path.
baseName :: FilePath -> String
baseName = reverse . takeWhile (\c -> c /= '/' && c /= '\\') . reverse

-- | Play a sound effect file asynchronously (fire and forget).
--
--   The helper is spawned as a detached process; its exit status is ignored.
--   When audio is off, this is a silent no-op.
playSfx :: AudioConfig -> FilePath -> IO ()
playSfx AudioOff        _    = pure ()
playSfx (AudioOn ah) path = void (spawnCommand cmd)
  where
    cmd = ahPath ah ++ " " ++ sfxArgs ah path ++ " >/dev/null 2>&1 &"

-- | SFX helper arguments (one-shot, no looping).
sfxArgs :: AudioHelper -> FilePath -> String
sfxArgs ah path = case ahName ah of
    "mpv"    -> "--no-video --really-quiet " ++ shellQuote path
    "ffplay" -> "-nodisp -autoexit " ++ shellQuote path
    _        -> shellQuote path

-- | Start background music, replacing any currently playing track.
--
--   If the requested track is already playing, this is a no-op (deduplication).
--   The old process is terminated before the new one starts.
startMusic :: AudioConfig -> MusicHandle -> FilePath -> IO ()
startMusic AudioOff _ _ = pure ()
startMusic (AudioOn ah) mh path = do
    current <- readIORef (mhProcess mh)
    case current of
        Just (_, curPath) | curPath == path -> pure ()  -- already playing
        _ -> do
            stopMusic mh  -- kill old
            let args = musicLoopArgs ah path
            (_, _, _, ph) <- createProcess (proc (ahPath ah) args)
                { std_out = CreatePipe
                , std_err = CreatePipe
                , std_in  = CreatePipe
                }
            writeIORef (mhProcess mh) (Just (ph, path))

-- | Stop the currently playing music.
stopMusic :: MusicHandle -> IO ()
stopMusic mh = do
    current <- readIORef (mhProcess mh)
    case current of
        Nothing -> pure ()
        Just (ph, _) -> do
            writeIORef (mhProcess mh) Nothing
            terminateProcess ph `catch` ignoreEx
            void (waitForProcess ph) `catch` ignoreEx
  where
    ignoreEx :: SomeException -> IO ()
    ignoreEx _ = pure ()

-- | Music helper arguments: looping, no video, quiet.
musicLoopArgs :: AudioHelper -> FilePath -> [String]
musicLoopArgs ah path = case ahName ah of
    "mpv"    -> ["--no-video", "--really-quiet", "--loop=inf", path]
    "ffplay" -> ["-nodisp", "-loop", "0", path]
    _        -> [path]  -- best effort: play once

-- | Minimal shell quoting (single quotes, with escaping).
shellQuote :: String -> String
shellQuote s = "'" ++ concatMap esc s ++ "'"
  where
    esc '\'' = "'\\''"    -- end quote, literal ', restart quote
    esc c    = [c]
