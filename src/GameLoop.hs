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
import qualified Data.Map.Strict as Map

import System.Console.Haskeline

-- ---------------------------------------------------------------------------
-- Tab completion
-- ---------------------------------------------------------------------------

commandWords :: [String]
commandWords =
    [ "go", "move", "walk", "look", "examine", "inspect", "read", "take", "pick", "drop", "put"
    , "search", "inventory", "inv", "i", "use", "talk", "speak", "choose", "option", "attack", "hit", "kill"
    , "equip", "wear", "wield", "unequip", "remove", "stats"
    , "enter", "board", "disembark", "drive", "wait", "refuel", "repair"
    , "undo", "save", "load", "saves", "restart", "help", "quit", "exit", "q"
    , "activate", "swim", "crawl", "dig"
    ]

directionWords :: [String]
directionWords = ["north", "south", "east", "west", "up", "down"
    , "northeast", "northwest", "southeast", "southwest"
    , "ne", "nw", "se", "sw"]

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
        roomItems = getItemsInLocation (InRoom currentRoomId) state
        roomNpcs = getNPCsInRoom currentRoomId state
    in nub (itemCompletionTerms roomItems ++ npcCompletionTerms roomNpcs)

inventoryTargets :: GameState -> [String]
inventoryTargets state = itemCompletionTerms (getItemsInLocation (CarriedBy "player") state)

reachableExitEntities :: GameState -> [String]
reachableExitEntities state = case getCurrentRoom state of
    Just room -> [entity | Locked _ entity <- Map.elems (roomConnections room)]
    Nothing   -> []

entityTargets :: GameState -> [String]
entityTargets state =
    let exits = reachableExitEntities state
        doorAliases = if null exits then [] else ["door", "locked door"]
    in nub (roomTargets state ++ exits ++ doorAliases)

-- | Words for adventure-declared custom verbs (canonical names + aliases)
customVerbWords :: GameState -> [String]
customVerbWords state =
    concatMap (\def -> vdName def : vdAliases def)
        (Map.elems (verbDefs (world state)))

contextualSuggestions :: GameState -> [String] -> [String]
contextualSuggestions state prevWords = case prevWords of
    [] -> commandWords ++ customVerbWords state ++ directionWords
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
        | otherwise -> commandWords ++ customVerbWords state ++ directionWords
                       ++ roomTargets state ++ entityTargets state ++ inventoryTargets state

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
    , lsInitial :: GameState   -- ^ pristine initial state, used by Restart
    } deriving (Show, Eq)

initLoopState :: GameState -> LoopState
initLoopState state = LoopState state [] state

maxUndoHistory :: Int
maxUndoHistory = 50

-- | Whether a command advances the game clock.  Pure informational commands
--   (look, inventory, stats, journal, help, …) and failed/unknown input cost
--   no turn and therefore do not tick conditions or pollute undo history.
consumesTurn :: Command -> Bool
consumesTurn cmd = case cmd of
    Look           -> False
    Inventory      -> False
    StatsCmd       -> False
    JournalCmd     -> False
    Help           -> False
    Quit           -> False
    Undo           -> False
    Save _         -> False
    Load _         -> False
    ListSaves      -> False
    Restart        -> False
    Unknown _      -> False
    _              -> True

-- | Pure command transition used by both the interactive loop and tests.
--   Undo itself does not consume a turn. Other commands run the normal turn
--   ticks and save the exact pre-command state for restoration.
applyLoopCommand :: Command -> LoopState -> (LoopState, String)
applyLoopCommand Undo loopState = case lsHistory loopState of
    [] -> (loopState, "Nothing to undo.")
    previous : rest -> (LoopState previous rest (lsInitial loopState), "Undone.")
applyLoopCommand Quit loopState =
    let (newState, message) = executeCommand Quit (lsCurrent loopState)
    in (loopState { lsCurrent = newState }, message)
applyLoopCommand Restart loopState =
    let (newState, message) = executeCommand Look (lsInitial loopState)
    in (initLoopState newState, message)
applyLoopCommand Help loopState = (loopState, helpText)
applyLoopCommand (Save _) loopState = (loopState, "")
applyLoopCommand (Load _) loopState = (loopState, "")
applyLoopCommand ListSaves loopState = (loopState, "")
applyLoopCommand command loopState
    | not (consumesTurn command) =
        let (newState, message) = executeCommand command (lsCurrent loopState)
            (stateAfterTriggers, triggerMsg) = fireCommandTriggers command (lsCurrent loopState) newState
            combined = combineMessages message triggerMsg
        in (loopState { lsCurrent = stateAfterTriggers }, combined)
    | otherwise =
        let oldState = lsCurrent loopState
            stateWithTurn = incrementTurnCount oldState
            (stateAfterTick, tickMsgs) = tickConditions stateWithTurn
            (stateAfterVehicleTick, vehicleTickMsg) = vehicleConditionTick stateAfterTick
            (newState, message) = executeCommand command stateAfterVehicleTick
            (stateAfterTriggers, triggerMsg) = fireCommandTriggers command stateAfterVehicleTick newState
            allTickMsgs = tickMsgs ++ (if null vehicleTickMsg then [] else [vehicleTickMsg])
            fullMessage = if null allTickMsgs then message else unlines allTickMsgs ++ message
            combined = combineMessages fullMessage triggerMsg
            history' = take maxUndoHistory (oldState : lsHistory loopState)
        in (LoopState stateAfterTriggers history' (lsInitial loopState), combined)

-- | Combine two result messages (command result + trigger messages).
combineMessages :: String -> String -> String
combineMessages base extra
    | null extra = base
    | null base  = extra
    | otherwise  = base ++ "\n" ++ extra

-- | Determine which trigger events apply to a completed command, using the
--   state before and after the command to detect room changes.
fireCommandTriggers :: Command -> GameState -> GameState -> (GameState, String)
fireCommandTriggers cmd before after =
    let events = commandEvents cmd before after
        (st, msgs) = foldl (\(s, acc) ev -> let (s', m) = fireTriggers ev s
                                            in (s', combineMessages acc m))
                           (after, "") events
    in (st, msgs)

-- | Compute the list of events raised by a command.
commandEvents :: Command -> GameState -> GameState -> [EventType]
commandEvents cmd before after = concat
    [ roomEvents
    , takeDropUseEvents
    , lookSearchEvents
    , [OnCommand (commandVerbName cmd)]
    , [OnTurn | consumesTurn cmd]
    ]
  where
    roomEvents =
        let oldRoom = currentRoom (save before)
            newRoom = currentRoom (save after)
        in if oldRoom /= newRoom
           then [OnLeave oldRoom, OnEnter newRoom]
           else []
    takeDropUseEvents = case cmd of
        Interact VTake t -> [OnTake t | Just _ <- [findItemIdByAlias t after]]
        Interact VDrop t -> [OnDrop t | Just _ <- [findItemIdByAlias t after]]
        Interact VUse t  -> [OnUse t | Just _ <- [findItemIdByAlias t after]]
        _ -> []
    lookSearchEvents = case cmd of
        Look            -> [OnLook (currentRoom (save after)) | currentRoom (save after) `elem` Map.keys (rooms (world after))]
        SearchCmd _     -> [OnSearch (currentRoom (save after)) | currentRoom (save after) `elem` Map.keys (rooms (world after))]
        _               -> []

-- | Best-effort lookup of an item ID by alias (returns the alias as fallback, since
--   triggers are matched by ID; the exact ID lookup makes favorite-item triggers work).
findItemIdByAlias :: String -> GameState -> Maybe String
findItemIdByAlias alias state =
    let allItems = Map.elems (itemDefs (world state))
    in case [itemId i | i <- allItems, normalizeText alias `elem` itemAliases i] of
        (iId:_) -> Just iId
        []      -> Just alias

-- | Extract a canonical verb name for OnCommand triggers.
commandVerbName :: Command -> String
commandVerbName cmd = case cmd of
    Go _          -> "go"
    Look          -> "look"
    Inventory     -> "inventory"
    StatsCmd      -> "stats"
    JournalCmd    -> "journal"
    SearchCmd _   -> "search"
    TakeAll       -> "take"
    DropAll       -> "drop"
    EquipCmd _    -> "equip"
    UnequipCmd _  -> "unequip"
    UnequipAllCmd -> "unequip"
    Interact v _  -> map toLower (drop 1 (show v))  -- "VTake" -> "take"
    InteractWith VUseOn _ _ -> "use"
    _             -> "unknown"

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
                case parseCommandWith (verbDefs (world state)) input of
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
                        let (restarted, msg) = applyLoopCommand Restart loopState
                        putStrLn msg
                        loopGame restarted
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
            victoryLoop loopState
        Just (Custom msg) -> do
            putStrLn ""
            putStrLn $ "Game Over: " ++ msg
            putStrLn ""
            putStrLn "  [R]estart  |  [Q]uit"
            victoryLoop loopState
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
            let (restarted, msg) = applyLoopCommand Restart loopState
            putStrLn msg
            loopGame restarted
        Just "q" -> putStrLn "Thanks for playing!"
        _ -> do
            putStrLn "  [U]ndo  |  [L]oad last save  |  [R]estart  |  [Q]uit"
            deathLoop loopState
  where
    state = lsCurrent loopState

-- | Victory/custom game-over input loop
victoryLoop :: LoopState -> IO ()
victoryLoop loopState = do
    inputResult <- runInputT defaultSettings (getInputLine "> ")
    case map toLower . fromMaybe "q" <$> pure inputResult of
        Just "r" -> do
            putStrLn "Starting a new game...\n"
            let (restarted, msg) = applyLoopCommand Restart loopState
            putStrLn msg
            loopGame restarted
        Just "q" -> putStrLn "Thanks for playing!"
        _ -> do
            putStrLn "  [R]estart  |  [Q]uit"
            victoryLoop loopState
