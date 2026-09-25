{-# LANGUAGE ScopedTypeVariables #-}

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

import System.Environment (lookupEnv, getExecutablePath)
import System.Directory   (findExecutable, doesFileExist)
import System.FilePath    (takeDirectory, (</>))
import System.IO          (Handle)
import System.Process     (createProcess, proc, ProcessHandle,
                           terminateProcess, waitForProcess,
                           std_out, std_err, std_in,
                           StdStream(..))
import Control.Monad      (void)
import Control.Exception  (catch, try, SomeException)
import Data.IORef
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
--   3. Bundled binary in @bin\\@ (Windows release ZIP pattern)
--   4. PATH search for known candidates: @mpv@, @ffplay@, @openmpt123@, etc.
--   5. Nothing found → 'AudioOff' (graceful silent degrade)
discoverAudio :: Maybe FilePath -> IO AudioConfig
discoverAudio mCliHelper = do
    mEnvHelper <- lookupEnv "TEXT_ADVENTURE_AUDIO_HELPER"
    case mCliHelper of
        Just p  -> pure (AudioOn (AudioHelper p (baseName p)))   -- 1. CLI flag wins
        Nothing -> case mEnvHelper of
            Just p  -> pure (AudioOn (AudioHelper p (baseName p)))   -- 2. Environment variable
            Nothing -> do
                mBundled <- findBundledHelper                         -- 3. Bundled binary in bin\
                case mBundled of
                    Just ah -> pure (AudioOn ah)
                    Nothing -> do
                        mPathHelper <- firstHelper                    -- 4. PATH search
                            ["mpv", "ffplay", "openmpt123", "fluidsynth", "timidity", "paplay"]
                        case mPathHelper of
                            Just ah -> pure (AudioOn ah)
                            Nothing -> pure AudioOff                  -- 5. Graceful degrade

-- | Look for a bundled helper binary in known relative directories:
--   1. "bin" relative to current working directory
--   2. The directory containing the running executable
--   3. "bin" sibling to the executable directory (e.g. if exe is in bin/)
findBundledHelper :: IO (Maybe AudioHelper)
findBundledHelper = do
    mExeDir <- (Just . takeDirectory <$> getExecutablePath) `catch` (\(_ :: SomeException) -> pure Nothing)
    let candidateDirs = case mExeDir of
            Just d  -> ["bin", d, d </> "bin", d </> ".." </> "bin"]
            Nothing -> ["bin"]
        names = ["mpv", "ffplay", "audio-helper", "openmpt123"]
        suffixes = ["", ".exe"]
        candidateFiles = [ dir </> (n ++ s) | dir <- candidateDirs, n <- names, s <- suffixes ]
    firstExisting candidateFiles
  where
    firstExisting [] = pure Nothing
    firstExisting (p:ps) = do
        exists <- doesFileExist p `catch` (\(_ :: SomeException) -> pure False)
        if exists
            then pure (Just (AudioHelper p (baseName p)))
            else firstExisting ps

-- | Find the first available helper from a list of candidate names in PATH.
firstHelper :: [String] -> IO (Maybe AudioHelper)
firstHelper [] = pure Nothing
firstHelper (name:rest) = do
    mPath <- findExecutable name
    case mPath of
        Just p  -> pure (Just (AudioHelper p (baseName p)))
        Nothing -> firstHelper rest

-- | Extract the normalized base name from a path (stripping directory and .exe extension).
baseName :: FilePath -> String
baseName p =
    let raw = reverse . takeWhile (\c -> c /= '/' && c /= '\\') . reverse $ p
        lower = map toLower raw
    in if ".exe" `isSuffixOf` lower
       then take (length lower - 4) lower
       else lower

-- | Play a sound effect file asynchronously (fire and forget).
--
--   The helper is spawned as a detached process with redirected standard streams;
--   its exit status is ignored. When audio is off, this is a silent no-op.
playSfx :: AudioConfig -> FilePath -> IO ()
playSfx AudioOff        _    = pure ()
playSfx (AudioOn ah) path = void $ do
    _ <- try (createProcess (proc (ahPath ah) (sfxArgs ah path))
            { std_in  = NoStream
            , std_out = NoStream
            , std_err = NoStream
            }) :: IO (Either SomeException (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle))
    pure ()

-- | SFX helper arguments (one-shot, quiet, no video).
sfxArgs :: AudioHelper -> FilePath -> [String]
sfxArgs ah path = case ahName ah of
    "mpv"        -> ["--no-video", "--really-quiet", path]
    "ffplay"     -> ["-nodisp", "-autoexit", "-loglevel", "quiet", path]
    "openmpt123" -> ["--quiet", path]
    "paplay"     -> [path]
    _            -> [path]

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
            mProc <- (Just <$> createProcess (proc (ahPath ah) args)
                { std_in  = NoStream
                , std_out = NoStream
                , std_err = NoStream
                }) `catch` (\(_ :: SomeException) -> pure Nothing)
            case mProc of
                Just (_, _, _, ph) -> writeIORef (mhProcess mh) (Just (ph, path))
                Nothing            -> writeIORef (mhProcess mh) Nothing

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
    "mpv"        -> ["--no-video", "--really-quiet", "--loop=inf", path]
    "ffplay"     -> ["-nodisp", "-loop", "0", "-loglevel", "quiet", path]
    "openmpt123" -> ["--quiet", "--repeat", "-1", path]
    _            -> [path]  -- best effort: play once
