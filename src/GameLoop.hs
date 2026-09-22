-- | Main game loop and user interaction for the text adventure engine
module GameLoop
  ( runGame
  , runGameWith
  , runGameWithFrontend
  , gameLoop
  , LoopState (..)
  , initLoopState
  , applyLoopCommand
  , initSampleGame
  , commandEvents
  , consumesTurn
  , consumesTurnIn
  ) where

import Types
import Game
import Parser
import Verbs (verbCanonicalName)
import SaveLoad
import Sample (initSampleGame)
import Frontend
import Data.Char (toLower)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map

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
    MapCmd         -> False
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
    MapCmd        -> "map"
    TakeAll       -> "take"
    DropAll       -> "drop"
    EquipCmd _    -> "equip"
    UnequipCmd _  -> "unequip"
    UnequipAllCmd -> "unequip"
    Interact v _        -> verbCanonicalName v
    InteractWith v _ _  -> verbCanonicalName v
    _             -> "unknown"

-- | Mapping applied to every player-facing line at the I/O boundary is now
--   part of 'Frontend' ('Frontend.OutputFilter'); the pure core never inspects
--   it: `app/Main` passes `id` on a colour TTY and `stripAnsi` when colour is
--   off or stdout is redirected.

-- | Main game loop function (colour allowed).
runGame :: GameState -> IO ()
runGame = runGameWithFrontend (haskelineFrontend id)

-- | Entry point with an explicit output filter, used by the CLI to strip ANSI
--   when stdout is not a terminal or `--no-color` is set.
runGameWith :: OutputFilter -> GameState -> IO ()
runGameWith f = runGameWithFrontend (haskelineFrontend f)

-- | Entry point for an arbitrary frontend (Phase V). The terminal today, a
--   TUI or web backend later — the loop logic does not know the difference.
runGameWithFrontend :: Frontend -> GameState -> IO ()
runGameWithFrontend fe state = do
    let (newState, message) = executeCommand Look state
    feEmitLine fe message
    loopGame fe (initLoopState newState)

-- | Print engine diagnostics that appeared while handling one command to
--   the frontend's diagnostics channel (P2-23). They describe a content error
--   the author has to fix, so they must not be mixed into the game text the
--   player sees.
emitNewDiagnostics :: Frontend -> GameState -> GameState -> IO ()
emitNewDiagnostics fe before after =
    feDiagnostics fe (drop (length (diagnostics before)) (diagnostics after))

-- | Backward-compatible entry point for callers that have a plain GameState.
gameLoop :: GameState -> IO ()
gameLoop = loopGame (haskelineFrontend id) . initLoopState

-- | Interactive game loop with an in-memory undo history. All I/O goes
--   through the frontend record (Phase V): the loop itself is presentation-
--   agnostic and drives the shared policy functions only.
loopGame :: Frontend -> LoopState -> IO ()
loopGame fe loopState
    | gameOver (save state) = handleGameOver fe loopState
    | otherwise = do
        inputResult <- feReadInput fe state "> "
        case inputResult of
            Nothing -> do
                let (newState, message) = executeCommand Quit state
                feEmitLine fe message
                loopGame fe loopState { lsCurrent = newState }
            Just input ->
                case parseCommandWith (verbDefs (world state)) input of
                    Save name -> do
                        saveGame state name
                        loopGame fe loopState
                    Load name -> do
                        result <- loadGame state name
                        case result of
                            Just loadedState -> do
                                let (loadedState', msg) = executeCommand Look loadedState
                                feEmitLine fe msg
                                loopGame fe (initLoopState loadedState')
                            Nothing -> loopGame fe loopState
                    ListSaves -> do
                        listSaves (world state)
                        loopGame fe loopState
                    Restart -> do
                        feEmitLine fe "Starting a new game...\n"
                        let (restarted, msg) = applyLoopCommand Restart loopState
                        feEmitLine fe msg
                        loopGame fe restarted
                    Help -> do
                        feEmitLine fe helpText
                        loopGame fe loopState
                    command -> do
                        let (loopState', message) = applyLoopCommand command loopState
                        emitNewDiagnostics fe (lsCurrent loopState) (lsCurrent loopState')
                        -- Message first, then any queued cutscene (Phase H/H4:
                        -- played once at the clip's own rate), then the other
                        -- pending presentations (animation, narrative).
                        feEmitLine fe message
                        curAfterCutscene <- case pendingCutscene (lsCurrent loopState') of
                            Just (frames, micros) -> do
                                fePlayFrames fe micros frames
                                pure ((lsCurrent loopState') { pendingCutscene = Nothing })
                            Nothing -> pure (lsCurrent loopState')
                        let loopState'' = loopState' { lsCurrent = curAfterCutscene }
                        case pendingAnimation (lsCurrent loopState'') of
                            Just (frames, micros) -> do
                                fePlayFrames fe micros frames
                                let cleared = (lsCurrent loopState'') { pendingAnimation = Nothing }
                                loopGame fe (loopState'' { lsCurrent = cleared })
                            Nothing ->
                                case pendingNarrative (lsCurrent loopState'') of
                                    Nothing ->
                                        loopGame fe loopState''
                                    Just (nls, followUp) -> do
                                        case nls of
                                            [] -> return ()
                                            [single] -> feEmitLine fe single
                                            _ -> do
                                                mapM_ (\l -> feEmitLine fe l >> feReadPause fe)
                                                    (init nls)
                                                feEmitLine fe (last nls)
                                        let (finalState, followMsg) = applyOutcome followUp "" (lsCurrent loopState'')
                                            clearedState = finalState { pendingNarrative = Nothing }
                                        if null followMsg
                                            then loopGame fe (loopState'' { lsCurrent = clearedState })
                                            else do feEmitLine fe followMsg
                                                    loopGame fe (loopState'' { lsCurrent = clearedState })
  where
    state = lsCurrent loopState

-- ---------------------------------------------------------------------------
-- Game over screens
-- ---------------------------------------------------------------------------

-- | Handle game-over screen based on reason
handleGameOver :: Frontend -> LoopState -> IO ()
handleGameOver fe loopState = do
    case gameOverReason (save state) of
        Just Death -> do
            emitEndArt fe state Death
                [ "========================================="
                , "  YOU HAVE DIED"
                , "========================================="
                ]
            feEmitLine fe "  [U]ndo  |  [L]oad last save  |  [R]estart  |  [Q]uit"
            deathLoop fe loopState
        Just Victory -> do
            emitEndArt fe state Victory
                [ "========================================="
                , "  VICTORY!"
                , "========================================="
                ]
            feEmitLine fe "  [R]estart  |  [Q]uit"
            victoryLoop fe loopState
        Just (Custom msg) -> do
            emitEndArt fe state (Custom msg) [ "Game Over: " ++ msg ]
            feEmitLine fe "  [R]estart  |  [Q]uit"
            victoryLoop fe loopState
        Nothing -> return ()  -- Quit without reason
  where
    state = lsCurrent loopState

-- | Print the end screen: a blank line, the world's `end_art` for this reason
--   when present, otherwise the built-in frame, then a blank line. The control
--   hints are printed by the caller so the input loop stays reachable either way.
emitEndArt :: Frontend -> GameState -> GameOverReason -> [String] -> IO ()
emitEndArt fe st reason fallback = do
    feEmitLine fe ""
    case endArtFor reason st of
        Just art -> feEmitLine fe (resolveAsciiArt art st)
        Nothing  -> mapM_ (feEmitLine fe) fallback
    feEmitLine fe ""

-- | Death screen input loop
deathLoop :: Frontend -> LoopState -> IO ()
deathLoop fe loopState = do
    inputResult <- feReadPlain fe "> "
    case map toLower . fromMaybe "q" <$> pure inputResult of
        Just "u" ->
            case lsHistory loopState of
                [] -> do
                    feEmitLine fe "Nothing to undo."
                    deathLoop fe loopState
                _ -> do
                    let (restored, msg) = applyLoopCommand Undo loopState
                    feEmitLine fe msg
                    loopGame fe restored
        Just "l" -> do
            feEmitLine fe "Enter save name to load (or press Enter for 'savegame'):"
            nameResult <- feReadPlain fe "> "
            let name = case nameResult of
                    Just n | not (null n) -> n
                    _                     -> "savegame"
            result <- loadGame state name
            case result of
                Just loadedState -> do
                    let (s', msg) = executeCommand Look loadedState
                    feEmitLine fe msg
                    loopGame fe (initLoopState s')
                Nothing -> deathLoop fe loopState
        Just "r" -> do
            feEmitLine fe "Starting a new game...\n"
            let (restarted, msg) = applyLoopCommand Restart loopState
            feEmitLine fe msg
            loopGame fe restarted
        Just "q" -> feEmitLine fe "Thanks for playing!"
        _ -> do
            feEmitLine fe "  [U]ndo  |  [L]oad last save  |  [R]estart  |  [Q]uit"
            deathLoop fe loopState
  where
    state = lsCurrent loopState

-- | Victory/custom game-over input loop
victoryLoop :: Frontend -> LoopState -> IO ()
victoryLoop fe loopState = do
    inputResult <- feReadPlain fe "> "
    case map toLower . fromMaybe "q" <$> pure inputResult of
        Just "r" -> do
            feEmitLine fe "Starting a new game...\n"
            let (restarted, msg) = applyLoopCommand Restart loopState
            feEmitLine fe msg
            loopGame fe restarted
        Just "q" -> feEmitLine fe "Thanks for playing!"
        _ -> do
            feEmitLine fe "  [R]estart  |  [Q]uit"
            victoryLoop fe loopState
