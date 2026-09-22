-- | Saving, loading and listing save games
module SaveLoad where

import Types
import Game (syncInventory)
import Control.Exception (try, SomeException)
import Control.Monad (when)
import Data.List (foldl', sortBy)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import System.Directory (createDirectoryIfMissing, listDirectory, doesFileExist, removeFile)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

-- | Current save schema version.
--   Bumped to 3 with Phase 7f-3 / 7h-2 (V1: ActorRef/PropRef ADT).
currentSaveVersion :: Int
currentSaveVersion = 3

-- | Compute a simple checksum of the GameWorld for save compatibility detection.
--   The accumulator is strict (P2-10): the lazy `foldl` built one thunk per
--   character of the encoded world, i.e. per byte of the whole world JSON.
computeWorldChecksum :: GameWorld -> String
computeWorldChecksum gw =
    let encoded = BLC.unpack (Aeson.encode gw)
        -- Simple DJB2 hash
        hashVal = foldl' (\acc c -> acc * 33 + fromEnum c) 5381 encoded
    in show (abs hashVal)

-- | One line of `listSaves` output. The current world checksum is a parameter
--   (P2-10): it is the same for every save file, so the caller computes it once
--   instead of once per file.
formatSaveEntry :: String -> SaveFile -> String
formatSaveEntry currentChecksum sf =
    "  " ++ saveName sf ++ " — " ++ saveTimestamp sf
        ++ " (" ++ compat ++ ")"
  where
    compat | worldChecksum sf == currentChecksum = "compatible"
           | otherwise                           = "world mismatch!"

-- | The directory save slots live in. Default: `saves` relative to the
--   working directory (unchanged behaviour, Rogue Phase 0). The environment
--   variable `TA_SAVES_DIR` overrides it — the hermetic seam for tests, E2E
--   runs and per-adventure isolation, so the game can be driven from a temp
--   directory without polluting the repo and without ordering-dependent
--   saves leaking between runs.
savesDir :: IO FilePath
savesDir = maybe "saves" id <$> lookupEnv "TA_SAVES_DIR"

-- | Directory for one specific purpose, honouring `TA_SAVES_DIR`. The variant
--   the meta-progression layer (Rogue Phase 2) will use, kept next to its
--   sibling so the path scheme stays in one place.
savesDirFor :: String -> IO FilePath
savesDirFor base = (</> base) <$> savesDir

-- | Path of one save slot. Built with `System.FilePath` so the separator is
--   the platform's: a literal "/" happens to work on Windows too, but the
--   engine is meant to run wherever its authors do.
saveSlotPath :: String -> IO FilePath
saveSlotPath slotName = (</> (slotName ++ ".json")) <$> savesDir

-- | Save game to a named slot with metadata
saveGame :: GameState -> String -> IO ()
saveGame state slotName = do
    dir <- savesDir
    createDirectoryIfMissing True dir
    now <- getCurrentTime
    let timestamp = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" now
        checksum = computeWorldChecksum (world state)
        saveFile = SaveFile
            { saveVersion   = currentSaveVersion
            , saveTimestamp  = timestamp
            , worldChecksum = checksum
            , saveName      = slotName
            , saveData      = save state
            }
    filepath <- saveSlotPath slotName
    BL.writeFile filepath (Aeson.encodePretty saveFile)
    putStrLn $ "Game saved to " ++ filepath ++ " (" ++ timestamp ++ ")."

-- | Load game from a named slot, with checksum validation
loadGame :: GameState -> String -> IO (Maybe GameState)
loadGame state slotName = do
    filepath <- saveSlotPath slotName
    exists <- doesFileExist filepath
    if not exists
    then do
        -- Try legacy path (bare SaveState without wrapper)
        let legacyPath = slotName ++ ".json"
        legacyExists <- doesFileExist legacyPath
        if legacyExists
        then loadLegacySave state legacyPath
        else do
            putStrLn $ "Error: Save file '" ++ filepath ++ "' not found."
            return Nothing
    else do
        result <- try (BL.readFile filepath) :: IO (Either SomeException BL.ByteString)
        case result of
            Left _ -> do
                putStrLn $ "Error: Could not read file '" ++ filepath ++ "'."
                return Nothing
            Right contents -> case Aeson.decode contents of
                Just sf -> do
                    let currentChecksum = computeWorldChecksum (world state)
                    if worldChecksum sf /= currentChecksum
                    then putStrLn "Warning: This save was made with a different world version. Results may be unpredictable."
                    else return ()
                    let loadedState = state { save = syncInventory (saveData sf) }
                    putStrLn $ "Game loaded from " ++ filepath ++ " (saved: " ++ saveTimestamp sf ++ ")."
                    return (Just loadedState)
                Nothing -> do
                    -- Try loading as legacy bare SaveState
                    case Aeson.decode contents of
                        Just loadedSave -> do
                            putStrLn "Game loaded (legacy format)."
                            let loadedState = state { save = syncInventory loadedSave }
                            return (Just loadedState)
                        Nothing -> do
                            putStrLn "Error: Save file is corrupted or incompatible."
                            return Nothing

-- | Delete one save slot from disk (Rogue Phase 0). Missing files count as
--   deleted (idempotent); anything else (permissions, locked file on Windows)
--   is reported instead of crashing the game over path. This is the seam the
--   Ironman mode (Rogue Phase 1) will call from 'GameLoop.handleGameOver'.
deleteSaveSlot :: String -> IO ()
deleteSaveSlot slotName = do
    filepath <- saveSlotPath slotName
    exists <- doesFileExist filepath
    when exists $ do
        result <- try (removeFile filepath) :: IO (Either SomeException ())
        case result of
            Right () -> putStrLn $ "Save slot deleted: " ++ filepath
            Left _   -> putStrLn $ "Warning: could not delete save slot " ++ filepath ++ "."

-- | Load a legacy save file (bare SaveState, no wrapper)
loadLegacySave :: GameState -> FilePath -> IO (Maybe GameState)
loadLegacySave state filepath = do
    result <- try (BL.readFile filepath) :: IO (Either SomeException BL.ByteString)
    case result of
        Left _ -> return Nothing
        Right contents -> case Aeson.decode contents of
            Just loadedSave -> do
                putStrLn "Game loaded from legacy format."
                let loadedState = state { save = syncInventory loadedSave }
                return (Just loadedState)
            Nothing -> return Nothing

-- | List all saves in the saves directory (`TA_SAVES_DIR` overrides it)
listSaves :: GameWorld -> IO ()
listSaves gw = do
    dir <- savesDir
    createDirectoryIfMissing True dir
    files <- listDirectory dir
    let jsonFiles = filter (\f -> length f > 5 && drop (length f - 5) f == ".json") files
    if null jsonFiles
    then putStrLn "No saved games found."
    else do
        putStrLn "=== Saved Games ==="
        -- P2-10: computed once for all files, not once per file inside the loop.
        let currentChecksum = computeWorldChecksum gw
        entries <- mapM (loadSaveEntry currentChecksum) jsonFiles
        let sorted = sortBy (\(_, t1) (_, t2) -> compare t2 t1)
                     [(e, t) | Just (e, t) <- entries]
        mapM_ (\(entry, _) -> putStrLn entry) sorted
  where
    loadSaveEntry :: String -> FilePath -> IO (Maybe (String, String))
    loadSaveEntry currentChecksum filename = do
        dir <- savesDir
        result <- try (BL.readFile (dir </> filename)) :: IO (Either SomeException BL.ByteString)
        case result of
            Left _ -> return Nothing
            Right contents -> case Aeson.decode contents of
                Just sf ->
                    let ts = saveTimestamp sf
                    in return (Just (formatSaveEntry currentChecksum sf, ts))
                Nothing -> return Nothing
