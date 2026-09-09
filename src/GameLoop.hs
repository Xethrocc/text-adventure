-- | Main game loop and user interaction for the text adventure engine
module GameLoop
  ( runGame
  , gameLoop
  , commandCompletion
  , initSampleGame
  ) where

import Types
import Game
import Parser hiding (reachableExitEntities)
import SaveLoad
import Sample (initSampleGame)
import Data.Char (toLower)
import Data.List (isPrefixOf, nub)
import Data.Maybe (fromMaybe)

import qualified Data.Map as Map
import System.Console.Haskeline

-- ---------------------------------------------------------------------------
-- Tab completion
-- ---------------------------------------------------------------------------

commandWords :: [String]
commandWords =
    [ "go", "move", "walk", "look", "examine", "inspect", "read", "take", "pick", "drop", "put"
    , "search", "inventory", "inv", "i", "use", "talk", "speak", "attack", "hit", "kill"
    , "equip", "wear", "wield", "unequip", "remove", "stats"
    , "enter", "board", "disembark", "drive", "wait", "refuel", "repair"
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
    ("equip" : _) -> inventoryTargets state
    ("wear" : _) -> inventoryTargets state
    ("wield" : _) -> inventoryTargets state
    ("unequip" : _) -> itemCompletionTerms (getEquippedItems state)
    ("remove" : _) -> itemCompletionTerms (getEquippedItems state)
    ("use" : _)
        | "on" `elem` prevWords || "with" `elem` prevWords -> entityTargets state
        | otherwise -> inventoryTargets state
    (verb : _)
        | verb `elem` ["take", "drop", "attack", "hit", "kill", "examine", "inspect", "read"] ->
            roomTargets state ++ inventoryTargets state
        | otherwise -> commandWords ++ directionWords ++ roomTargets state ++ entityTargets state ++ inventoryTargets state

-- | ItemDefs currently worn/wielded
getEquippedItems :: GameState -> [ItemDef]
getEquippedItems state =
    [ def
    | iId <- Map.elems (equipment (save state))
    , Just def <- [Map.lookup iId (itemDefs (world state))]
    ]

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

-- ---------------------------------------------------------------------------
-- Game loop
-- ---------------------------------------------------------------------------

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
                    stateWithTurn = incrementTurnCount state
                    -- Tick conditions once per command (player + vehicle-wide)
                    (stateAfterTick, tickMsgs) = tickConditions stateWithTurn
                    (stateAfterVehicleTick, vehicleTickMsg) = vehicleConditionTick stateAfterTick
                    allTickMsgs = tickMsgs ++ (if null vehicleTickMsg then [] else [vehicleTickMsg])
                case command of
                    Save name -> do
                        saveGame stateAfterVehicleTick name
                        gameLoop stateAfterVehicleTick
                    Load name -> do
                        result <- loadGame stateAfterVehicleTick name
                        case result of
                            Just loadedState -> do
                                let (s', msg) = executeCommand Look loadedState
                                putStrLn msg
                                gameLoop s'
                            Nothing -> gameLoop stateAfterVehicleTick
                    ListSaves -> do
                        listSaves (world stateAfterVehicleTick)
                        gameLoop stateAfterVehicleTick
                    Restart -> do
                        putStrLn "Starting a new game...\n"
                        runGame initSampleGame
                    _ -> do
                        let (newState, message) = executeCommand command stateAfterVehicleTick
                            tickOutput = if null allTickMsgs then "" else unlines allTickMsgs
                        putStrLn (if null tickOutput then message else tickOutput ++ message)
                        gameLoop newState

-- ---------------------------------------------------------------------------
-- Game over screens
-- ---------------------------------------------------------------------------

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
            runGame initSampleGame
        Just "q" -> putStrLn "Thanks for playing!"
        _ -> do
            putStrLn "  [R]estart  |  [Q]uit"
            victoryLoop
