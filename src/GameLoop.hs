-- | Main game loop and user interaction for the text adventure engine
module GameLoop
  ( runGame
  , gameLoop
  , LoopState (..)
  , initLoopState
  , applyLoopCommand
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
    , "undo", "save", "load", "saves", "restart", "help", "quit", "exit", "q"
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

-- | Runtime state for undo. History is newest-first and capped at 50 states.
data LoopState = LoopState
    { lsCurrent :: GameState
    , lsHistory :: [GameState]
    } deriving (Show, Eq)

initLoopState :: GameState -> LoopState
initLoopState state = LoopState state []

maxUndoHistory :: Int
maxUndoHistory = 50

-- | Pure command transition used by both the interactive loop and tests.
--   Undo itself does not consume a turn. Other commands run the normal turn
--   ticks and save the exact pre-command state for restoration.
applyLoopCommand :: Command -> LoopState -> (LoopState, String)
applyLoopCommand Undo loopState = case lsHistory loopState of
    [] -> (loopState, "Nothing to undo.")
    previous : rest -> (LoopState previous rest, "Undone.")
applyLoopCommand Quit loopState =
    let (newState, message) = executeCommand Quit (lsCurrent loopState)
    in (loopState { lsCurrent = newState }, message)
applyLoopCommand Help loopState = (loopState, helpText)
applyLoopCommand (Save _) loopState = (loopState, "")
applyLoopCommand (Load _) loopState = (loopState, "")
applyLoopCommand ListSaves loopState = (loopState, "")
applyLoopCommand Restart loopState = (loopState, "")
applyLoopCommand command loopState =
    let oldState = lsCurrent loopState
        stateWithTurn = incrementTurnCount oldState
        (stateAfterTick, tickMsgs) = tickConditions stateWithTurn
        (stateAfterVehicleTick, vehicleTickMsg) = vehicleConditionTick stateAfterTick
        (newState, message) = executeCommand command stateAfterVehicleTick
        allTickMsgs = tickMsgs ++ (if null vehicleTickMsg then [] else [vehicleTickMsg])
        fullMessage = if null allTickMsgs then message else unlines allTickMsgs ++ message
        history' = take maxUndoHistory (oldState : lsHistory loopState)
    in (LoopState newState history', fullMessage)

-- | Main game loop function
runGame :: GameState -> IO ()
runGame state = do
    let (newState, message) = executeCommand Look state
    putStrLn message
    loopGame (initLoopState newState)

-- | Backward-compatible entry point for callers that have a plain GameState.
gameLoop :: GameState -> IO ()
gameLoop = loopGame . initLoopState

-- | Interactive game loop with an in-memory undo history.
loopGame :: LoopState -> IO ()
loopGame loopState
    | gameOver (save state) = handleGameOver loopState
    | otherwise = do
        inputResult <- runInputT (haskelineSettings state) (getInputLine "> ")
        case inputResult of
            Nothing -> do
                let (newState, message) = executeCommand Quit state
                putStrLn message
                loopGame loopState { lsCurrent = newState }
            Just input ->
                case parseCommand input of
                    Save name -> do
                        saveGame state name
                        loopGame loopState
                    Load name -> do
                        result <- loadGame state name
                        case result of
                            Just loadedState -> do
                                let (loadedState', msg) = executeCommand Look loadedState
                                putStrLn msg
                                loopGame (initLoopState loadedState')
                            Nothing -> loopGame loopState
                    ListSaves -> do
                        listSaves (world state)
                        loopGame loopState
                    Restart -> do
                        putStrLn "Starting a new game...\n"
                        runGame initSampleGame
                    Help -> do
                        putStrLn helpText
                        loopGame loopState
                    command -> do
                        let (loopState', message) = applyLoopCommand command loopState
                        case pendingNarrative (lsCurrent loopState') of
                            Nothing -> do
                                putStrLn message
                                loopGame loopState'
                            Just (lines, followUp) -> do
                                case lines of
                                    [] -> return ()
                                    [single] -> putStrLn single
                                    _ -> do
                                        mapM_ (\l -> putStrLn l >> putStr "  [Press Enter to continue]" >> getLine >> return ())
                                            (init lines)
                                        putStrLn (last lines)
                                let (finalState, followMsg) = applyOutcome followUp "" (lsCurrent loopState')
                                    clearedState = finalState { pendingNarrative = Nothing }
                                if null followMsg
                                    then loopGame (loopState' { lsCurrent = clearedState })
                                    else do putStrLn followMsg
                                            loopGame (loopState' { lsCurrent = clearedState })
  where
    state = lsCurrent loopState

-- ---------------------------------------------------------------------------
-- Game over screens
-- ---------------------------------------------------------------------------

-- | Handle game-over screen based on reason
handleGameOver :: LoopState -> IO ()
handleGameOver loopState = do
    case gameOverReason (save state) of
        Just Death -> do
            putStrLn ""
            putStrLn "========================================="
            putStrLn "  YOU HAVE DIED"
            putStrLn "========================================="
            putStrLn ""
            putStrLn "  [U]ndo  |  [L]oad last save  |  [R]estart  |  [Q]uit"
            deathLoop loopState
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
  where
    state = lsCurrent loopState

-- | Death screen input loop
deathLoop :: LoopState -> IO ()
deathLoop loopState = do
    inputResult <- runInputT defaultSettings (getInputLine "> ")
    case map toLower . fromMaybe "q" <$> pure inputResult of
        Just "u" ->
            case lsHistory loopState of
                [] -> do
                    putStrLn "Nothing to undo."
                    deathLoop loopState
                _ -> do
                    let (restored, msg) = applyLoopCommand Undo loopState
                    putStrLn msg
                    loopGame restored
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
                    loopGame (initLoopState s')
                Nothing -> deathLoop loopState
        Just "r" -> do
            putStrLn "Starting a new game...\n"
            runGame initSampleGame
        Just "q" -> putStrLn "Thanks for playing!"
        _ -> do
            putStrLn "  [U]ndo  |  [L]oad last save  |  [R]estart  |  [Q]uit"
            deathLoop loopState
  where
    state = lsCurrent loopState

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
