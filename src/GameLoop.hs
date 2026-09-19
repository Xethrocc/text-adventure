-- | Main game loop and user interaction for the text adventure engine
module GameLoop
  ( runGame
  , runGameWith
  , gameLoop
  , LoopState (..)
  , initLoopState
  , applyLoopCommand
  , commandCompletion
  , initSampleGame
  , commandEvents
  , consumesTurn
  , consumesTurnIn
  ) where

import Types
import Game
import Parser hiding (reachableExitEntities)
import Verbs (verbCanonicalName)
import SaveLoad
import Sample (initSampleGame)
import Data.Char (toLower)
import Data.List (isPrefixOf, nub, foldl')
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map

import System.Console.Haskeline
import System.IO (hPutStrLn, stderr)
import Control.Concurrent (threadDelay)

-- ---------------------------------------------------------------------------
-- Tab completion
-- ---------------------------------------------------------------------------

commandWords :: [String]
commandWords =
    [ "go", "move", "walk", "look", "examine", "inspect", "read", "take", "pick", "drop", "put"
    , "search", "watch", "inventory", "inv", "i", "use", "talk", "speak", "choose", "option", "attack", "hit", "kill"
    , "equip", "wear", "wield", "unequip", "remove", "stats"
    , "enter", "board", "disembark", "drive", "wait", "refuel", "repair"
    , "undo", "save", "load", "saves", "restart", "help", "quit", "exit", "q"
    , "activate"
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
    WatchCmd _     -> False
    Unknown _      -> False
    _              -> True

-- | Whether a command advances the game clock, evaluated against the state the
--   command runs in. P1-16: an invalid dialogue choice (`99` in a 3-option
--   prompt, or a bare number outside a conversation) is a typo, not an action,
--   so it must not tick conditions, fire `on: turn` or grow the undo history.
consumesTurnIn :: GameState -> Command -> Bool
consumesTurnIn st cmd = case cmd of
    ChooseCmd i -> isValidChoice i st
    _           -> consumesTurn cmd

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
    | not (consumesTurnIn (lsCurrent loopState) command) =
        let (newState, message) = executeCommand command (lsCurrent loopState)
            (stateAfterTriggers, triggerMsg) = fireCommandTriggers command (lsCurrent loopState) newState
            combined = combineMessages message triggerMsg
        in (loopState { lsCurrent = stateAfterTriggers }, combined)
    | otherwise =
        let oldState = lsCurrent loopState
            stateWithTurn = incrementTurnCount oldState
            (stateAfterTick, tickMsgs) = tickConditions stateWithTurn
            (stateAfterVehicleTick, vehicleTickMsg) = vehicleConditionTick stateAfterTick
            allTickMsgs = tickMsgs ++ (if null vehicleTickMsg then [] else [vehicleTickMsg])
            tickText = unlines allTickMsgs
            history' = take maxUndoHistory (oldState : lsHistory loopState)
        in if gameOver (save stateAfterVehicleTick)
           then
               -- L11: the condition tick ended the game before the command ran
               -- (the tick pipeline runs first). The player is already dead, so
               -- the command is dropped — only the tick messages are reported.
               (LoopState stateAfterVehicleTick history' (lsInitial loopState), tickText)
           else
               let (newState, message) = executeCommand command stateAfterVehicleTick
                   (stateAfterTriggers, triggerMsg) = fireCommandTriggers command stateAfterVehicleTick newState
                   fullMessage = if null allTickMsgs then message else tickText ++ message
               in (LoopState stateAfterTriggers history' (lsInitial loopState),
                   combineMessages fullMessage triggerMsg)

-- | Combine two message fragments for trigger output. Same rule as
--   'Game.joinMessages' — empty fragments contribute nothing.
combineMessages :: String -> String -> String
combineMessages = joinMessages

-- | Determine which trigger events apply to a completed command, using the
--   state before and after the command to detect room changes.
fireCommandTriggers :: Command -> GameState -> GameState -> (GameState, String)
fireCommandTriggers cmd before after =
    let events = commandEvents cmd before after
        (st, msgs) = foldl' (\(s, acc) ev -> let (s', m) = fireTriggers ev s
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
    , [OnTurn | consumesTurnIn before cmd]
    ]
  where
    roomEvents =
        let oldRoom = currentRoom (save before)
            newRoom = currentRoom (save after)
        in if oldRoom /= newRoom
           then [OnLeave oldRoom, OnEnter newRoom]
           else []
    takeDropUseEvents = case cmd of
        -- P1-15: derive take/drop events from the actual state change, not from
        -- the command. A failed `take` (not portable / already carried) or a
        -- `drop` of something not held must not fire `OnTake`/`OnDrop`.
        Interact VTake t ->
            [ OnTake iid | Just iid <- [findItemIdByAlias t after]
                         , itemLoc iid before /= Just (CarriedBy "player")
                         , itemLoc iid after  == Just (CarriedBy "player") ]
        Interact VDrop t ->
            [ OnDrop iid | Just iid <- [findItemIdByAlias t after]
                         , itemLoc iid before == Just (CarriedBy "player")
                         , itemLoc iid after  /= Just (CarriedBy "player") ]
        -- `use` has no state criterion (its effect is up to the author).
        Interact VUse t  -> [OnUse iid | Just iid <- [findItemIdByAlias t after]]
        _ -> []
    itemLoc i st = itemLocation <$> Map.lookup i (itemStates (save st))
    lookSearchEvents = case cmd of
        Look            -> [OnLook (currentRoom (save after)) | currentRoom (save after) `elem` Map.keys (rooms (world after))]
        SearchCmd _     -> [OnSearch (currentRoom (save after)) | currentRoom (save after) `elem` Map.keys (rooms (world after))]
        _               -> []

-- | Look up an item ID by alias. Returns `Nothing` when no declared item matches
--   (P1-15: the old fallback to the raw input invented events for item IDs that
--   do not exist, e.g. `take blubb` -> `OnTake "blubb"`).
findItemIdByAlias :: String -> GameState -> Maybe String
findItemIdByAlias alias state =
    let allItems = Map.elems (itemDefs (world state))
    in case [itemId i | i <- allItems, normalizeText alias `elem` itemAliases i] of
        (iId:_) -> Just iId
        []      -> Nothing

-- | Extract a canonical verb name for OnCommand triggers.
commandVerbName :: Command -> String
commandVerbName cmd = case cmd of
    Go _          -> "go"
    Look          -> "look"
    Inventory     -> "inventory"
    StatsCmd      -> "stats"
    JournalCmd    -> "journal"
    SearchCmd _   -> "search"
    WatchCmd _    -> "watch"
    TakeAll       -> "take"
    DropAll       -> "drop"
    EquipCmd _    -> "equip"
    UnequipCmd _  -> "unequip"
    UnequipAllCmd -> "unequip"
    Interact v _        -> verbCanonicalName v
    InteractWith v _ _  -> verbCanonicalName v
    _             -> "unknown"

-- | Mapping applied to every player-facing line at the I/O boundary. The pure
--   core never inspects it: `app/Main` passes `id` on a colour TTY and
--   `stripAnsi` when colour is off or stdout is redirected.
type OutputFilter = String -> String

-- | Print a line through the output filter.
emitLine :: OutputFilter -> String -> IO ()
emitLine f = putStrLn . f

-- | Print without a trailing newline through the output filter.
emitRaw :: OutputFilter -> String -> IO ()
emitRaw f = putStr . f

-- | Main game loop function (colour allowed).
runGame :: GameState -> IO ()
runGame = runGameWith id

-- | Entry point with an explicit output filter, used by the CLI to strip ANSI
--   when stdout is not a terminal or `--no-color` is set.
runGameWith :: OutputFilter -> GameState -> IO ()
runGameWith f state = do
    let (newState, message) = executeCommand Look state
    emitLine f message
    loopGame f (initLoopState newState)

-- | Print engine diagnostics that appeared while handling one command to
--   stderr (P2-23). They describe a content error the author has to fix, so they
--   must not be mixed into the game text the player sees.
emitNewDiagnostics :: GameState -> GameState -> IO ()
emitNewDiagnostics before after =
    mapM_ (hPutStrLn stderr) (drop (length (diagnostics before)) (diagnostics after))

-- | Backward-compatible entry point for callers that have a plain GameState.
gameLoop :: GameState -> IO ()
gameLoop = loopGame id . initLoopState

-- | Interactive game loop with an in-memory undo history.
loopGame :: OutputFilter -> LoopState -> IO ()
loopGame f loopState
    | gameOver (save state) = handleGameOver f loopState
    | otherwise = do
        inputResult <- runInputT (haskelineSettings state) (getInputLine "> ")
        case inputResult of
            Nothing -> do
                let (newState, message) = executeCommand Quit state
                emitLine f message
                loopGame f loopState { lsCurrent = newState }
            Just input ->
                case parseCommandWith (verbDefs (world state)) input of
                    Save name -> do
                        saveGame state name
                        loopGame f loopState
                    Load name -> do
                        result <- loadGame state name
                        case result of
                            Just loadedState -> do
                                let (loadedState', msg) = executeCommand Look loadedState
                                emitLine f msg
                                loopGame f (initLoopState loadedState')
                            Nothing -> loopGame f loopState
                    ListSaves -> do
                        listSaves (world state)
                        loopGame f loopState
                    Restart -> do
                        emitLine f "Starting a new game...\n"
                        let (restarted, msg) = applyLoopCommand Restart loopState
                        emitLine f msg
                        loopGame f restarted
                    Help -> do
                        emitLine f helpText
                        loopGame f loopState
                    command -> do
                        let (loopState', message) = applyLoopCommand command loopState
                        emitNewDiagnostics (lsCurrent loopState) (lsCurrent loopState')
                        case pendingAnimation (lsCurrent loopState') of
                            Just frames -> do
                                emitLine f message
                                playFrames f frames
                                let cleared = (lsCurrent loopState') { pendingAnimation = Nothing }
                                loopGame f (loopState' { lsCurrent = cleared })
                            Nothing ->
                                case pendingNarrative (lsCurrent loopState') of
                                    Nothing -> do
                                        emitLine f message
                                        loopGame f loopState'
                                    Just (nls, followUp) -> do
                                        case nls of
                                            [] -> return ()
                                            [single] -> emitLine f single
                                            _ -> do
                                                mapM_ (\l -> emitLine f l >> emitRaw f "  [Press Enter to continue]" >> getLine >> return ())
                                                    (init nls)
                                                emitLine f (last nls)
                                        let (finalState, followMsg) = applyOutcome followUp "" (lsCurrent loopState')
                                            clearedState = finalState { pendingNarrative = Nothing }
                                        if null followMsg
                                            then loopGame f (loopState' { lsCurrent = clearedState })
                                            else do emitLine f followMsg
                                                    loopGame f (loopState' { lsCurrent = clearedState })
  where
    state = lsCurrent loopState

-- | Milliseconds between animation frames in `watch`. The delay lives only
--   here in the IO loop; the pure core produced the plain frame list.
frameDelayMicros :: Int
frameDelayMicros = 350000

-- | Play ASCII animation frames in order, one per delay interval.
playFrames :: OutputFilter -> [String] -> IO ()
playFrames f = mapM_ (\fr -> emitLine f fr >> threadDelay frameDelayMicros)

-- ---------------------------------------------------------------------------
-- Game over screens
-- ---------------------------------------------------------------------------

-- | Handle game-over screen based on reason
handleGameOver :: OutputFilter -> LoopState -> IO ()
handleGameOver f loopState = do
    case gameOverReason (save state) of
        Just Death -> do
            emitEndArt f state Death
                [ "========================================="
                , "  YOU HAVE DIED"
                , "========================================="
                ]
            emitLine f "  [U]ndo  |  [L]oad last save  |  [R]estart  |  [Q]uit"
            deathLoop f loopState
        Just Victory -> do
            emitEndArt f state Victory
                [ "========================================="
                , "  VICTORY!"
                , "========================================="
                ]
            emitLine f "  [R]estart  |  [Q]uit"
            victoryLoop f loopState
        Just (Custom msg) -> do
            emitEndArt f state (Custom msg) [ "Game Over: " ++ msg ]
            emitLine f "  [R]estart  |  [Q]uit"
            victoryLoop f loopState
        Nothing -> return ()  -- Quit without reason
  where
    state = lsCurrent loopState

-- | Print the end screen: a blank line, the world's `end_art` for this reason
--   when present, otherwise the built-in frame, then a blank line. The control
--   hints are printed by the caller so the input loop stays reachable either way.
emitEndArt :: OutputFilter -> GameState -> GameOverReason -> [String] -> IO ()
emitEndArt f st reason fallback = do
    emitLine f ""
    case endArtFor reason st of
        Just art -> emitLine f (resolveAsciiArt art st)
        Nothing  -> mapM_ (emitLine f) fallback
    emitLine f ""

-- | Death screen input loop
deathLoop :: OutputFilter -> LoopState -> IO ()
deathLoop f loopState = do
    inputResult <- runInputT defaultSettings (getInputLine "> ")
    case map toLower . fromMaybe "q" <$> pure inputResult of
        Just "u" ->
            case lsHistory loopState of
                [] -> do
                    emitLine f "Nothing to undo."
                    deathLoop f loopState
                _ -> do
                    let (restored, msg) = applyLoopCommand Undo loopState
                    emitLine f msg
                    loopGame f restored
        Just "l" -> do
            emitLine f "Enter save name to load (or press Enter for 'savegame'):"
            nameResult <- runInputT defaultSettings (getInputLine "> ")
            let name = case nameResult of
                    Just n | not (null n) -> n
                    _                     -> "savegame"
            result <- loadGame state name
            case result of
                Just loadedState -> do
                    let (s', msg) = executeCommand Look loadedState
                    emitLine f msg
                    loopGame f (initLoopState s')
                Nothing -> deathLoop f loopState
        Just "r" -> do
            emitLine f "Starting a new game...\n"
            let (restarted, msg) = applyLoopCommand Restart loopState
            emitLine f msg
            loopGame f restarted
        Just "q" -> emitLine f "Thanks for playing!"
        _ -> do
            emitLine f "  [U]ndo  |  [L]oad last save  |  [R]estart  |  [Q]uit"
            deathLoop f loopState
  where
    state = lsCurrent loopState

-- | Victory/custom game-over input loop
victoryLoop :: OutputFilter -> LoopState -> IO ()
victoryLoop f loopState = do
    inputResult <- runInputT defaultSettings (getInputLine "> ")
    case map toLower . fromMaybe "q" <$> pure inputResult of
        Just "r" -> do
            emitLine f "Starting a new game...\n"
            let (restarted, msg) = applyLoopCommand Restart loopState
            emitLine f msg
            loopGame f restarted
        Just "q" -> emitLine f "Thanks for playing!"
        _ -> do
            emitLine f "  [R]estart  |  [Q]uit"
            victoryLoop f loopState
