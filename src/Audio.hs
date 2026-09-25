-- | Audio helper discovery and SFX playback (Audio Phase 1).
--
--   The engine never links against audio libraries (Architecture A1); instead
--   it spawns external helpers (@mpv@, @ffplay@, etc.) to play sound files.
--   Discovery follows a priority chain (CLI flag → environment variable →
--   PATH search) and is performed once at startup. When no helper is found,
--   audio is silently disabled — same degrade philosophy as VT\/colour (W3).
module Audio
  ( AudioConfig(..)
  , discoverAudio
  , playSfx
  , noAudio
  ) where

import System.Environment (lookupEnv)
import System.Directory   (findExecutable)
import System.Process     (spawnCommand)
import Control.Monad      (void)
import Data.Maybe         (listToMaybe, catMaybes)

-- | Runtime audio configuration, resolved once at startup.
data AudioConfig
    = AudioOff                        -- ^ No helper found or --no-audio
    | AudioOn FilePath                -- ^ Path to a working audio helper
    deriving (Show)

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
        Just p  -> pure (AudioOn p)   -- 1. CLI flag wins
        Nothing -> case mEnvHelper of
            Just p  -> pure (AudioOn p)   -- 2. Environment variable
            Nothing -> do                 -- 3. PATH search
                found <- firstExecutable ["mpv", "ffplay", "paplay"]
                case found of
                    Just p  -> pure (AudioOn p)
                    Nothing -> pure AudioOff  -- 4. Graceful degrade

-- | Find the first executable from a list of candidates.
firstExecutable :: [String] -> IO (Maybe FilePath)
firstExecutable names = do
    results <- mapM findExecutable names
    pure (listToMaybe (catMaybes results))

-- | Play a sound effect file asynchronously (fire and forget).
--
--   The helper is spawned as a detached process; its exit status is ignored.
--   When audio is off, this is a silent no-op.
playSfx :: AudioConfig -> FilePath -> IO ()
playSfx AudioOff        _    = pure ()
playSfx (AudioOn helper) path = void (spawnCommand cmd)
  where
    cmd = helper ++ " " ++ shellQuote path ++ " >/dev/null 2>&1 &"

-- | Minimal shell quoting (single quotes, with escaping).
shellQuote :: String -> String
shellQuote s = "'" ++ concatMap esc s ++ "'"
  where
    esc '\'' = "'\\''"    -- end quote, literal ', restart quote
    esc c    = [c]
