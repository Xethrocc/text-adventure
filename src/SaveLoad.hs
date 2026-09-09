-- | Saving, loading and listing save games
module SaveLoad where

import Types
import Game (syncInventory)
import Control.Exception (try, SomeException)
import Data.List (sortBy)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import System.Directory (createDirectoryIfMissing, listDirectory, doesFileExist)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

-- | Current save schema version.
--   Bumped to 2 with the Phase 0/1 changes (visitedRooms, equipment, room hooks).
currentSaveVersion :: Int
currentSaveVersion = 2

-- | Compute a simple checksum of the GameWorld for save compatibility detection
computeWorldChecksum :: GameWorld -> String
computeWorldChecksum gw =
    let encoded = BLC.unpack (Aeson.encode gw)
        -- Simple DJB2 hash
        hashVal = foldl (\acc c -> acc * 33 + fromEnum c) 5381 encoded
    in show (abs hashVal)

-- | Save game to a named slot with metadata
saveGame :: GameState -> String -> IO ()
saveGame state slotName = do
    createDirectoryIfMissing True "saves"
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
        filepath = "saves/" ++ slotName ++ ".json"
    BL.writeFile filepath (Aeson.encodePretty saveFile)
    putStrLn $ "Game saved to " ++ filepath ++ " (" ++ timestamp ++ ")."

-- | Load game from a named slot, with checksum validation
loadGame :: GameState -> String -> IO (Maybe GameState)
loadGame state slotName = do
    let filepath = "saves/" ++ slotName ++ ".json"
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

-- | List all saves in the saves/ directory
listSaves :: GameWorld -> IO ()
listSaves gw = do
    createDirectoryIfMissing True "saves"
    files <- listDirectory "saves"
    let jsonFiles = filter (\f -> length f > 5 && drop (length f - 5) f == ".json") files
    if null jsonFiles
    then putStrLn "No saved games found."
    else do
        putStrLn "=== Saved Games ==="
        entries <- mapM (loadSaveEntry gw) jsonFiles
        let sorted = sortBy (\(_, t1) (_, t2) -> compare t2 t1)
                     [(e, t) | Just (e, t) <- entries]
        mapM_ (\(entry, _) -> putStrLn entry) sorted
  where
    loadSaveEntry :: GameWorld -> FilePath -> IO (Maybe (String, String))
    loadSaveEntry gameWorld filename = do
        result <- try (BL.readFile ("saves/" ++ filename)) :: IO (Either SomeException BL.ByteString)
        case result of
            Left _ -> return Nothing
            Right contents -> case Aeson.decode contents of
                Just sf ->
                    let name = saveName sf
                        ts = saveTimestamp sf
                        currentChecksum = computeWorldChecksum gameWorld
                        compat = if worldChecksum sf == currentChecksum then "compatible" else "world mismatch!"
                        entry = "  " ++ name ++ " — " ++ ts ++ " (" ++ compat ++ ")"
                    in return (Just (entry, ts))
                Nothing -> return Nothing
