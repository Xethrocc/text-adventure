-- | Loading a GameWorld / SaveState from disk.
--   This is the entry point for content produced by the worldbuilder package.
--
--   A world file (GameWorld) holds the static blueprint: rooms, item
--   definitions, NPC definitions, interactions.
--   A save file (SaveState) holds where everything currently is.
--
--   The worldbuilder emits BOTH files, so `--world` alone gives you a world
--   with nothing placed yet — pass `--save` too for a playable start.
module World where

import Types
import Game (syncInventory)
import Control.Exception (try, SomeException)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Load a GameWorld from a JSON file
loadGameWorld :: FilePath -> IO (Either String GameWorld)
loadGameWorld path = do
    result <- try (BL.readFile path) :: IO (Either SomeException BL.ByteString)
    pure $ case result of
        Left err -> Left ("Could not read world file '" ++ path ++ "': " ++ show err)
        Right contents -> eitherDecode contents

-- | Load a SaveState from a JSON file
loadSaveState :: FilePath -> IO (Either String SaveState)
loadSaveState path = do
    result <- try (BL.readFile path) :: IO (Either SomeException BL.ByteString)
    pure $ case result of
        Left err -> Left ("Could not read save file '" ++ path ++ "': " ++ show err)
        Right contents -> fmap syncInventory (eitherDecode contents)

-- | Load a complete GameState: a world plus an optional initial save.
loadGame :: FilePath -> Maybe FilePath -> IO (Either String GameState)
loadGame worldPath maybeSavePath = do
    worldResult <- loadGameWorld worldPath
    case worldResult of
        Left err -> pure (Left err)
        Right gw -> case maybeSavePath of
            Nothing -> pure (Right (GameState gw (defaultSaveState gw) Nothing))
            Just savePath -> do
                saveResult <- loadSaveState savePath
                pure $ case saveResult of
                    Left err -> Left err
                    Right ss -> Right (GameState gw ss Nothing)

-- | A bare starting state for a freshly loaded world.
--   Rooms exist but nothing is placed; use an initial-save file for that.
defaultSaveState :: GameWorld -> SaveState
defaultSaveState gw = SaveState
    { player         = Player 100 100 10 5 Map.empty
    , currentRoom    = startRoomId gw
    , inventory      = []
    , itemStates     = Map.empty
    , npcStates      = Map.empty
    , entityStates   = initialEntityStates gw
    , flags          = Map.empty
    , turnCount      = 0
    , gameOver       = False
    , gameOverReason = Nothing
    , visitedRooms   = Set.empty
    , equipment      = Map.empty
    , conditions     = Map.empty
    , activeQuests   = Map.empty
    , completedQuests = Set.empty
    , vehicleStates   = Map.empty
    , currentVehicle  = Nothing
    }

-- | Preferred starting room: "start" if it exists, else the first room by key order
startRoomId :: GameWorld -> RoomID
startRoomId gw
    | Map.member "start" (rooms gw) = "start"
    | Map.null (rooms gw)           = "start"
    | otherwise                     = fst (Map.findMin (rooms gw))

-- | Every locked exit in the world starts out locked
initialEntityStates :: GameWorld -> Map.Map String String
initialEntityStates gw =
    Map.fromList
        [ (entity, "locked")
        | room <- Map.elems (rooms gw)
        , Locked _ entity <- Map.elems (roomConnections room)
        ]
