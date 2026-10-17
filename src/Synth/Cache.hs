-- | Render cache of the synth (plan-synth.md §4.1, O1; stage 5.2c).
--
--   The only IO module of the synth. A 'SoundSynth' id is rendered lazily to
--   a WAV file named after a content hash (renderer version + sound id +
--   patch), so identical patches share one file, an edited patch gets a new
--   file and nothing is ever cleaned up. The frontends keep seeing plain
--   'FilePath's (S4).
--
--   Cache directory (O1): @$XDG_CACHE_HOME/text-adventure/synth/@ (fallback
--   @~/.cache@), Windows @%LOCALAPPDATA%\\text-adventure\\synth@; if none of
--   those is usable, the system temp directory.
module Synth.Cache
  ( resolveSound
  , resolveSoundIn
  , ensureRendered
  , resolveTrack
  , resolveTrackIn
  , ensureTrackRendered
  , synthCacheDir
  , cacheKey
  , cacheFileName
  , trackCacheKey
  , trackCacheFileName
  ) where

import Control.Exception (SomeException, try)
import Data.Bits (xor)
import Data.Char (ord)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import Numeric (showHex)
import System.Directory
  ( createDirectoryIfMissing, doesFileExist, getHomeDirectory
  , getTemporaryDirectory, renameFile )
import System.Environment (lookupEnv)
import System.FilePath ((</>), (<.>))
import System.Info (os)
import System.IO (hClose, openTempFile)

import Synth.Patch
import Synth.Render (renderSound, renderTrack, rendererVersion)
import Synth.Wav (encodeWav)
import Types.Core (GameWorld (..), SoundRef (..))

-- | Resolve a queue element to a playable file. Files pass through; a synth id
--   is looked up in 'worldSounds' and rendered into the cache on first use.
--   @Left@ carries a diagnostic message (unknown id, render/write failure).
resolveSound :: GameWorld -> SoundRef -> IO (Either String FilePath)
resolveSound gw ref = case ref of
    SoundFile p -> pure (Right p)
    SoundSynth _ -> do
        dir <- synthCacheDir
        resolveSoundIn dir gw ref

-- | 'resolveSound' with an explicit cache directory (tests, tools).
resolveSoundIn :: FilePath -> GameWorld -> SoundRef -> IO (Either String FilePath)
resolveSoundIn _ _ (SoundFile p) = pure (Right p)
resolveSoundIn dir gw (SoundSynth sid) =
    case Map.lookup sid (worldSounds gw) of
        Nothing -> pure (Left ("synth sound '" ++ sid ++ "' is not defined in sounds:"))
        Just sound -> do
            r <- try (ensureRendered dir sid sound) :: IO (Either SomeException FilePath)
            pure $ case r of
                Right p -> Right p
                Left e -> Left ("synth sound '" ++ sid ++ "': cannot write cache file: " ++ show e)

-- | Render @sound@ to @dir/<key>.wav@ unless that file already exists (cache
--   hit). The file is written under a temporary name and renamed, so a
--   concurrent reader never sees a half-written WAV.
ensureRendered :: FilePath -> String -> SynthSound -> IO FilePath
ensureRendered dir sid sound = do
    let path = dir </> cacheFileName sid sound
    hit <- doesFileExist path
    if hit
        then pure path
        else do
            createDirectoryIfMissing True dir
            (tmp, h) <- openTempFile dir "render.tmp"
            BL.hPut h (encodeWav sampleRate (renderSound sid sound))
            hClose h
            renameFile tmp path
            pure path

-- | Synth 5.2d: resolve a synth music track id in 'worldTracks' to a cached
--   WAV file path for the unchanged 'feStartMusic' seam (S4).
resolveTrack :: GameWorld -> String -> IO (Either String FilePath)
resolveTrack gw tid = do
    dir <- synthCacheDir
    resolveTrackIn dir gw tid

-- | 'resolveTrack' with an explicit cache directory (tests, tools).
resolveTrackIn :: FilePath -> GameWorld -> String -> IO (Either String FilePath)
resolveTrackIn dir gw tid =
    case Map.lookup tid (worldTracks gw) of
        Nothing -> pure (Left ("synth track '" ++ tid ++ "' is not defined in tracks:"))
        Just tr -> do
            r <- try (ensureTrackRendered dir tid tr) :: IO (Either SomeException FilePath)
            pure $ case r of
                Right p -> Right p
                Left e  -> Left ("synth track '" ++ tid ++ "': cannot write cache file: " ++ show e)

-- | Render @track@ to @dir/<key>.wav@ unless that file already exists (cache hit).
ensureTrackRendered :: FilePath -> String -> Track -> IO FilePath
ensureTrackRendered dir tid tr = do
    let path = dir </> trackCacheFileName tid tr
    hit <- doesFileExist path
    if hit
        then pure path
        else do
            createDirectoryIfMissing True dir
            (tmp, h) <- openTempFile dir "render.tmp"
            BL.hPut h (encodeWav sampleRate (renderTrack tid tr))
            hClose h
            renameFile tmp path
            pure path

-- | O1: where rendered sounds live.
synthCacheDir :: IO FilePath
synthCacheDir = do
    base <- if os == "mingw32" then lookupEnv "LOCALAPPDATA" else xdg
    case base of
        Just b | not (null b) -> pure (b </> "text-adventure" </> "synth")
        _ -> do
            tmp <- getTemporaryDirectory
            pure (tmp </> "text-adventure" </> "synth")
  where
    xdg = do
        x <- lookupEnv "XDG_CACHE_HOME"
        case x of
            Just d | not (null d) -> pure (Just d)
            _ -> do
                r <- try getHomeDirectory :: IO (Either SomeException FilePath)
                pure $ case r of
                    Right h | not (null h) -> Just (h </> ".cache")
                    _ -> Nothing

-- | File name (without directory) of a sound's cache entry.
cacheFileName :: String -> SynthSound -> FilePath
cacheFileName sid sound = cacheKey sid sound <.> "wav"

-- | File name (without directory) of a track's cache entry.
trackCacheFileName :: String -> Track -> FilePath
trackCacheFileName tid tr = trackCacheKey tid tr <.> "wav"

-- | Content address: 128 bits (two FNV-1a 64 passes with different offset
--   bases) over renderer version, sound id (it seeds the noise) and the
--   derived 'Show' of the patch — a canonical, order-stable rendering.
cacheKey :: String -> SynthSound -> String
cacheKey sid sound = hashPayload ("v" ++ show rendererVersion ++ "|" ++ sid ++ "|" ++ show sound)

-- | Content address for a music track (renderer version + track id + 'Show' of 'Track').
trackCacheKey :: String -> Track -> String
trackCacheKey tid tr = hashPayload ("v" ++ show rendererVersion ++ "|track|" ++ tid ++ "|" ++ show tr)

hashPayload :: String -> String
hashPayload payload = hex64 (fnv 0xcbf29ce484222325 payload) ++ hex64 (fnv 0x84222325cbf29ce4 payload)
  where
    fnv :: Word64 -> String -> Word64
    fnv = foldl (\a c -> (a `xor` fromIntegral (ord c)) * 0x100000001b3)
    hex64 w = let h = showHex w "" in replicate (16 - length h) '0' ++ h
