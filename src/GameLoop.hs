-- | Main game loop and user interaction for the text adventure engine
module GameLoop where

import Types
import Game
import Parser hiding (reachableExitEntities)
import Control.Exception (try, SomeException)
import Data.Char (toLower)
import Data.List (isPrefixOf, nub, sortBy)

import qualified Data.Map as Map
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import System.Console.Haskeline
import System.Directory (createDirectoryIfMissing, listDirectory, doesFileExist)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import Data.Maybe (fromMaybe)

-- | Current save schema version
currentSaveVersion :: Int
currentSaveVersion = 1

commandWords :: [String]
commandWords =
    [ "go", "move", "walk", "look", "examine", "inspect", "read", "take", "pick", "drop", "put"
    , "inventory", "inv", "i", "use", "talk", "speak", "attack", "hit", "kill"
    , "save", "load", "saves", "restart", "help", "quit", "exit", "q"
    ]

directionWords :: [String]
directionWords = ["north", "south", "east", "west", "up", "down"]

completionItems :: [String] -> String -> [Completion]
completionItems options prefix =
    let loweredPrefix = map toLower prefix
    in map simpleCompletion (filter (\opt -> loweredPrefix `isPrefixOf` map toLower opt) (nub options))

itemCompletionTerms :: [ItemDef] -> [String]
itemCompletionTerms items = nub (concatMap (\i -> itemName i : itemKeywords i) items)

npcCompletionTerms :: [NPCDef] -> [String]
npcCompletionTerms npcs = nub (concatMap (\n -> npcName n : npcKeywords n) npcs)

roomTargets :: GameState -> [String]
roomTargets state =
    let currentRoomId = currentRoom (save state)
        roomItems = getItemsInLocation currentRoomId state
        roomNpcs = getNPCsInRoom currentRoomId state
    in nub (itemCompletionTerms roomItems ++ npcCompletionTerms roomNpcs)

inventoryTargets :: GameState -> [String]
inventoryTargets state = itemCompletionTerms (getItemsInLocation "inventory" state)

reachableExitEntities :: GameState -> [String]
reachableExitEntities state = case getCurrentRoom state of
    Just room -> [entity | Locked _ entity <- Map.elems (roomConnections room)]
    Nothing   -> []

entityTargets :: GameState -> [String]
entityTargets state =
    let exits = reachableExitEntities state
        doorAliases = if null exits then [] else ["door", "locked door"]
    in nub (roomTargets state ++ exits ++ doorAliases)

contextualSuggestions :: GameState -> [String] -> [String]
contextualSuggestions state prevWords = case prevWords of
    [] -> commandWords ++ directionWords
    ("go" : _) -> directionWords
    ("move" : _) -> directionWords
    ("walk" : _) -> directionWords
    ("look" : "at" : _) -> roomTargets state
    ("talk" : "to" : _) -> npcCompletionTerms (getNPCsInRoom (currentRoom (save state)) state)
    ("speak" : "with" : _) -> npcCompletionTerms (getNPCsInRoom (currentRoom (save state)) state)
    ("pick" : "up" : _) -> roomTargets state
    ("put" : "down" : _) -> inventoryTargets state
    ("use" : _)
        | "on" `elem` prevWords || "with" `elem` prevWords -> entityTargets state
        | otherwise -> inventoryTargets state
    (verb : _)
        | verb `elem` ["take", "drop", "attack", "hit", "kill", "examine", "inspect", "read"] ->
            roomTargets state ++ inventoryTargets state
        | otherwise -> commandWords ++ directionWords ++ roomTargets state ++ entityTargets state ++ inventoryTargets state

commandCompletion :: GameState -> CompletionFunc IO
commandCompletion state (left, _) = do
    let loweredLeft = map toLower left
        tokens = words loweredLeft
        (prevWords, currentWord)
            | not (null loweredLeft) && last loweredLeft /= ' ' && not (null tokens) =
                (init tokens, last tokens)
            | otherwise = (tokens, "")
        suggestions = contextualSuggestions state prevWords
    pure (currentWord, completionItems suggestions currentWord)

haskelineSettings :: GameState -> Settings IO
haskelineSettings state =
    (defaultSettings :: Settings IO)
        { autoAddHistory = True
        , complete = commandCompletion state
        }

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
                            putStrLn $ "Game loaded from " ++ filepath ++ " (legacy format)."
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
        Left _ -> do
            putStrLn $ "Error: Could not read file '" ++ filepath ++ "'."
            return Nothing
        Right contents -> case Aeson.decode contents of
            Just loadedSave -> do
                putStrLn $ "Game loaded from " ++ filepath ++ " (legacy format)."
                let loadedState = state { save = syncInventory loadedSave }
                return (Just loadedState)
            Nothing -> do
                putStrLn "Error: Save file is corrupted or incompatible."
                return Nothing

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

-- | Main game loop function
runGame :: GameState -> IO ()
runGame state = do
    let (newState, message) = executeCommand Look state
    putStrLn message
    gameLoop newState
    
-- | Interactive game loop
gameLoop :: GameState -> IO ()
gameLoop state
    | gameOver (save state) = handleGameOver state
    | otherwise      = do
        inputResult <- runInputT (haskelineSettings state) (getInputLine "> ")
        case inputResult of
            Nothing -> do
                let (newState, message) = executeCommand Quit state
                putStrLn message
                gameLoop newState
            Just input -> do
                let command = parseCommand input
                    -- Increment turn count for every command
                    stateWithTurn = incrementTurnCount state
                case command of
                    Save name -> do
                        saveGame stateWithTurn name
                        gameLoop stateWithTurn
                    Load name -> do
                        result <- loadGame stateWithTurn name
                        case result of
                            Just loadedState -> do
                                let (s', msg) = executeCommand Look loadedState
                                putStrLn msg
                                gameLoop s'
                            Nothing -> gameLoop stateWithTurn
                    ListSaves -> do
                        listSaves (world stateWithTurn)
                        gameLoop stateWithTurn
                    Restart -> do
                        putStrLn "Starting a new game...\n"
                        putStrLn "=== Text Adventure Game ==="
                        putStrLn "Type 'help' for available commands."
                        putStrLn "----------------------------"
                        runGame initSampleGame
                    _ -> do
                        let (newState, message) = executeCommand command stateWithTurn
                        putStrLn message
                        gameLoop newState

-- | Handle game-over screen based on reason
handleGameOver :: GameState -> IO ()
handleGameOver state = do
    case gameOverReason (save state) of
        Just Death -> do
            putStrLn ""
            putStrLn "========================================="
            putStrLn "  YOU HAVE DIED"
            putStrLn "========================================="
            putStrLn ""
            putStrLn "  [L]oad last save  |  [R]estart  |  [Q]uit"
            deathLoop state
        Just Victory -> do
            putStrLn ""
            putStrLn "========================================="
            putStrLn "  VICTORY!"
            putStrLn "========================================="
            putStrLn ""
            putStrLn "  [R]estart  |  [Q]uit"
            victoryLoop
        Just (Custom msg) -> do
            putStrLn ""
            putStrLn $ "Game Over: " ++ msg
            putStrLn ""
            putStrLn "  [R]estart  |  [Q]uit"
            victoryLoop
        Nothing -> return ()  -- Quit without reason

-- | Death screen input loop
deathLoop :: GameState -> IO ()
deathLoop state = do
    inputResult <- runInputT defaultSettings (getInputLine "> ")
    case map toLower . fromMaybe "q" <$> pure inputResult of
        Just "l" -> do
            putStrLn "Enter save name to load (or press Enter for 'savegame'):"
            nameResult <- runInputT defaultSettings (getInputLine "> ")
            let name = case nameResult of
                    Just n | not (null n) -> n
                    _                     -> "savegame"
            result <- loadGame state name
            case result of
                Just loadedState -> do
                    let (s', msg) = executeCommand Look loadedState
                    putStrLn msg
                    gameLoop s'
                Nothing -> deathLoop state
        Just "r" -> do
            putStrLn "Starting a new game...\n"
            putStrLn "=== Text Adventure Game ==="
            putStrLn "Type 'help' for available commands."
            putStrLn "----------------------------"
            runGame initSampleGame
        Just "q" -> putStrLn "Thanks for playing!"
        _ -> do
            putStrLn "  [L]oad last save  |  [R]estart  |  [Q]uit"
            deathLoop state

-- | Victory/custom game-over input loop
victoryLoop :: IO ()
victoryLoop = do
    inputResult <- runInputT defaultSettings (getInputLine "> ")
    case map toLower . fromMaybe "q" <$> pure inputResult of
        Just "r" -> do
            putStrLn "Starting a new game...\n"
            putStrLn "=== Text Adventure Game ==="
            putStrLn "Type 'help' for available commands."
            putStrLn "----------------------------"
            runGame initSampleGame
        Just "q" -> putStrLn "Thanks for playing!"
        _ -> do
            putStrLn "  [R]estart  |  [Q]uit"
            victoryLoop

-- | Initialize a sample game with rooms and items
initSampleGame :: GameState
initSampleGame = GameState
    { world = GameWorld
        { rooms = Map.fromList
            [ ("start", Room "start" "Starting Room" "You are in a small stone chamber with torches on the walls. There are exits to the north and east. The east door looks sturdy and has a keyhole."
                (Map.fromList [(North, Open "hallway"), (East, Locked "treasure" "treasure_door")]) True)
            , ("hallway", Room "hallway" "Dark Hallway" "A long, dark hallway stretches before you. The air is damp and cold. There's an exit to the south."
                (Map.fromList [(South, Open "start")]) False)
            , ("treasure", Room "treasure" "Treasure Room" "You've entered a magnificent treasure room! Gold coins and jewels are scattered everywhere. There's an exit to the west."
                (Map.fromList [(West, Open "start")]) False)
            ]
        , itemDefs = Map.fromList
            [ ("torch", ItemDef "torch" "torch" "A burning torch that provides light." ["torch", "burning torch"] Map.empty)
            , ("key", ItemDef "key" "key" "A small brass key." ["key", "brass key"] Map.empty)
            , ("gold", ItemDef "gold" "gold" "A pile of shiny gold coins." ["gold", "coins", "gold coins"] Map.empty)
            , ("jewel", ItemDef "jewel" "jewel" "A sparkling ruby that catches the light." ["jewel", "ruby", "sparkling ruby"] Map.empty)
            , ("potion_healing", ItemDef "potion_healing" "healing potion" "A small vial filled with a bubbling red liquid." ["potion", "red potion", "healing potion"] (Map.singleton (VUse, "intact") (MultipleOutcomes [HealPlayer 50 "You drink the potion and feel your wounds closing!", ModifyItemProp "potion_healing" "uses" (-1) "The potion has less liquid now.", ChangeItemState "empty" "The vial is now empty."])))
            ]
        , npcDefs = Map.fromList
            [ ("oldman", NPCDef "oldman" "old man" "A withered old man in robes." (Map.singleton "alive" "It's dangerous to go alone! Take... well, I don't have anything actually.") ["man", "old man"] Nothing 0 0 Map.empty)
            , ("goblin", NPCDef "goblin" "goblin" "A nasty little green goblin." (Map.singleton "alive" "Grrr!! I will eat you!") ["goblin", "monster"] (Just 30) 8 2 Map.empty)
            ]
        , entityInteractions = Map.fromList 
            [ (("key", "door"), ("unlocked", "You insert the brass key into the door. It clicks open!"))
            , (("key", "treasure_door"), ("unlocked", "You insert the brass key into the door. It clicks open!"))
            ]
        }
    , save = SaveState
        { player = Player 100 100 10 5
        , currentRoom = "start"
        , inventory = []
        , itemStates = Map.fromList 
            [ ("torch", ItemState "start" "burning" Map.empty)
            , ("key", ItemState "hallway" "intact" Map.empty)
            , ("gold", ItemState "treasure" "intact" Map.empty)
            , ("jewel", ItemState "treasure" "intact" Map.empty)
            , ("potion_healing", ItemState "start" "intact" (Map.singleton "uses" 3))
            ]
        , npcStates = Map.fromList
            [ ("oldman", NPCState "start" "alive" Nothing Map.empty)
            , ("goblin", NPCState "hallway" "alive" (Just 30) Map.empty)
            ]
        , entityStates = Map.singleton "treasure_door" "locked"
        , flags = Map.empty
        , turnCount = 0
        , gameOver = False
        , gameOverReason = Nothing
        }
    }

-- | Main entry point for the game
main :: IO ()
main = do
    putStrLn "=== Text Adventure Game ==="
    putStrLn "Type 'help' for available commands."
    putStrLn "----------------------------"
    runGame initSampleGame
