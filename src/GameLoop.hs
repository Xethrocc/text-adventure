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
  , handleGameOver
  , deathLoop
  , saveBlockedMessage
  , loadBlockedMessage
  , deathMenuText
  , carryMetaVars
  , persistMeta
  , reseedRng
  , bumpMetaRuns
  ) where

import Types
import Game
import Parser
import Verbs (verbCanonicalName)
import SaveLoad
import Sample (initSampleGame)
import Frontend
import Control.Monad (when)
import Data.Char (toLower)
import Data.List (foldl', isPrefixOf)
import Data.Maybe (isJust, fromMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word64)
import qualified Data.Map.Strict as Map

-- ---------------------------------------------------------------------------
-- Game loop
-- ---------------------------------------------------------------------------

-- | Runtime state for undo. History is newest-first and capped at 50 states.
--   'lsSaveSlot' (Rogue Phase 1) remembers the ironman checkpoint slot of the
--   current run: set by every successful save, reset by restart — the slot it
--   points at is what the death screen deletes in ironman mode.
data LoopState = LoopState
    { lsCurrent :: GameState
    , lsHistory :: [GameState]
    , lsInitial :: GameState   -- ^ pristine initial state, used by Restart
    , lsSaveSlot :: Maybe String
    } deriving (Show, Eq)

initLoopState :: GameState -> LoopState
initLoopState state = LoopState state [] state Nothing

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
    PlayCardCmd _ _ -> False
    HandCmd        -> False
    DeckCmd        -> False
    DiscardCmd     -> False
    EndTurnCmd     -> True
    Unknown _      -> False
    _              -> True

-- | Whether a command advances the game clock, evaluated against the state the
--   command runs in. P1-16: an invalid dialogue choice (`99` in a 3-option
--   prompt, or a bare number outside a conversation) is a typo, not an action,
--   so it must not tick conditions, fire `on: turn` or grow the undo history.
consumesTurnIn :: GameState -> Command -> Bool
consumesTurnIn st cmd = case cmd of
    ChooseCmd i -> isValidChoice i st
    Interact v _ | verbCanonicalName v `elem` ["status", "bilanz", "finanzen"] -> False
    ActionWithArgs v _ | verbCanonicalName v `elem` ["status", "bilanz", "finanzen"] -> False
    _           -> consumesTurn cmd

-- | Pure command transition used by both the interactive loop and tests.
--   Undo itself does not consume a turn. Other commands run the normal turn
--   ticks and save the exact pre-command state for restoration.
applyLoopCommand :: Command -> LoopState -> (LoopState, String)
applyLoopCommand Undo loopState
    -- Rogue Phase 1: the author can disable undo globally (gpAllowUndo). The
    -- state stays untouched — the command is refused, nothing to tick.
    | not (gpAllowUndo (worldGamePolicy (world (lsCurrent loopState)))) =
        (loopState, "Undo is disabled in this adventure.")
    | otherwise = case lsHistory loopState of
        [] -> (loopState, "Nothing to undo.")
        previous : rest ->
            (loopState { lsCurrent = previous, lsHistory = rest }, "Undone.")
applyLoopCommand Quit loopState =
    let (newState, message) = executeCommand Quit (lsCurrent loopState)
    in (loopState { lsCurrent = newState }, message)
applyLoopCommand Restart loopState =
    -- Rogue Phase 2: meta.* travels from the dying/finished run into the fresh
    -- one (the plan's restart semantics); lsSaveSlot resets (initLoopState).
    -- The rngState reseed + run counter happen in the IO restart path
    -- ('runRestart') — this branch stays pure for the unit tests.
    let (newState, message) = executeCommand Look (lsInitial loopState)
    in (initLoopState (carryMetaVars (lsCurrent loopState) newState), message)


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
            policy = worldGamePolicy (world oldState)
            -- Rogue Phase 1: with undo disabled the history is not tracked at
            -- all (saves memory; the command is rejected before use anyway).
            history' = if gpAllowUndo policy
                       then take maxUndoHistory (oldState : lsHistory loopState)
                       else lsHistory loopState
            stateWithTurn = incrementTurnCount oldState
            (stateAfterTick, tickMsgs) = tickConditions stateWithTurn
            (stateAfterVehicleTick, vehicleTickMsg) = vehicleConditionTick stateAfterTick
            allTickMsgs = tickMsgs ++ (if null vehicleTickMsg then [] else [vehicleTickMsg])
            tickText = unlines allTickMsgs
        in if gameOver (save stateAfterVehicleTick)
           then
               -- L11: the condition tick ended the game before the command ran
               -- (the tick pipeline runs first). The player is already dead, so
               -- the command is dropped — only the tick messages are reported.
               (loopState { lsCurrent = stateAfterVehicleTick, lsHistory = history' }, tickText)
           else
               let (newState, message) = executeCommand command stateAfterVehicleTick
                   (stateAfterTriggers, triggerMsg) = fireCommandTriggers command stateAfterVehicleTick newState
                   fullMessage = if null allTickMsgs then message else tickText ++ message
               in (loopState { lsCurrent = stateAfterTriggers, lsHistory = history' },
                   combineMessages fullMessage triggerMsg)

-- | Rogue (Empfehlung 3, pure): a restart begins a fresh run with a freshly
--   derived rngState — carrying the old stream over would replay identical
--   "randomness" in every run, which is fatal for a roguelike. The IO path
--   derives the seed from the system clock and injects it here (M6-style:
--   pure core, clock in the IO path only).
reseedRng :: Word64 -> GameState -> GameState
reseedRng seed st = st { save = (save st) { rngState = seed } }

-- | Rogue Phase 2 (Zusatz-Empfehlung 4, pure): `meta.runs` counts fresh runs.
--   Bumped at run start and on every restart (a restart is a new run). Only
--   applied to adventures that actually use meta-progression (declared
--   meta.* variable or existing meta file) — the Default-Invariante of the
--   other adventures stays untouched (no meta file is ever created for them).
bumpMetaRuns :: GameState -> GameState
bumpMetaRuns st
    | not isMetaAdventure = st
    | otherwise = st { save = (save st)
        { variables = Map.alter bump "meta.runs" (variables (save st)) } }
  where
    isMetaAdventure =
        any ("meta." `isPrefixOf`) (Map.keys (varDefs (world st)))
        || not (Map.null (metaVars (variables (save st))))
    bump (Just (VVInt n)) = Just (VVInt (n + 1))
    bump _                = Just (VVInt 1)

-- | The shared restart path (main loop, death menu, victory menu): fresh
--   rngState from the system clock, meta.runs bumped and persisted.
runRestart :: Frontend -> LoopState -> IO ()
runRestart fe loopState = do
    seed <- newRngSeedIO
    let (restarted, msg) = applyLoopCommand Restart loopState
        fresh = restarted { lsCurrent = reseedRng seed . bumpMetaRuns $ lsCurrent restarted }
    persistMeta (lsCurrent fresh)
    feEmitLine fe "Starting a new game...\n"
    feEmitLine fe msg
    loopGame fe fresh

-- | IO-side seed derivation for restarts (M6: clock stays in the IO path).
newRngSeedIO :: IO Word64
newRngSeedIO = do
    t <- getPOSIXTime
    pure (floor (t * 1000))

-- | Combine two message fragments for trigger output. Same rule as
--   'Game.joinMessages' — empty fragments contribute nothing.
combineMessages :: String -> String -> String
combineMessages = joinMessages

-- ---------------------------------------------------------------------------
-- Policy gates (Rogue Phase 1)
-- ---------------------------------------------------------------------------

-- | Rogue Phase 1: pure gate for the in-game save command.
--   'Nothing' allows saving; 'Just' carries the rejection message. Only the
--   ironman mode restricts saving (to savezone rooms); every other adventure
--   saves anywhere, as before.
saveBlockedMessage :: GameState -> Maybe String
saveBlockedMessage st
    | not (gpIronman policy) = Nothing
    | currentRoom (save st) `elem` gpSaveZones policy = Nothing
    | otherwise = Just "You can only rest at a savezone."
  where
    policy = worldGamePolicy (world st)

-- | Rogue Phase 1: loading is rejected in ironman mode — restoring a save
--   would let the player outlive a death the checkpoint deletion was supposed
--   to make final.
loadBlockedMessage :: GameState -> Maybe String
loadBlockedMessage st
    | gpIronman (worldGamePolicy (world st)) = Just "Loading is disabled in ironman mode."
    | otherwise = Nothing

-- | The menu line under the death screen. Permadeath (and ironman, which
--   deletes the checkpoint) offer no undo/load, only restart or quit.
deathMenuText :: GamePolicy -> String
deathMenuText policy
    | gpPermadeath policy || gpIronman policy = "  [R]estart  |  [Q]uit"
    | otherwise = "  [U]ndo  |  [L]oad last save  |  [R]estart  |  [Q]uit"

-- | Rogue Phase 2 (pure, testable): carry the old run's meta.* variables into
--   the fresh state — souls earned survive the restart, everything else
--   (rooms, HP, normal variables) resets to 'lsInitial'.
carryMetaVars :: GameState -> GameState -> GameState
carryMetaVars old fresh = fresh
    { save = (save fresh)
        { variables = mergeMetaVars (variables (save old)) (variables (save fresh)) } }

-- | Rogue Phase 2 (M5): overlay the disk meta file over a state — the file
--   always wins (the VarMap inside a slot save is a snapshot; the meta file
--   is the authoritative progress store).
mergeMetaFromDisk :: GameState -> IO GameState
mergeMetaFromDisk st = do
    disk <- loadMeta (world st)
    pure st { save = (save st) { variables = mergeMetaVars disk (variables (save st)) } }

-- | Rogue Phase 2: write the current meta.* variables to the per-adventure
--   meta file. Called on game over (victory, death, custom, quit) — and
--   no-op for adventures without meta.* variables.
persistMeta :: GameState -> IO ()
persistMeta st = saveMeta (world st) (variables (save st))

-- | Determine which trigger events apply to a completed command, using the
--   state before and after the command to detect room changes.
fireCommandTriggers :: Command -> GameState -> GameState -> (GameState, String)
fireCommandTriggers cmd before after =
    let events = commandEvents cmd before after
        afterWithCmdVars = bindCommandVars cmd after
        (st, msgs) = foldl' (\(s, acc) ev -> let (s', m) = fireTriggers ev s
                                            in (s', combineMessages acc m))
                           (afterWithCmdVars, "") events
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
        Look            -> [OnLook (currentRoom (save after)) | isJust (lookupRoom (currentRoom (save after)) after)]
        SearchCmd _     -> [OnSearch (currentRoom (save after)) | isJust (lookupRoom (currentRoom (save after)) after)]
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
    ActionWithArgs v _  -> verbCanonicalName v
    PlayCardCmd _ _     -> "play"
    HandCmd             -> "hand"
    DeckCmd             -> "deck"
    DiscardCmd          -> "discard"
    EndTurnCmd          -> "end_turn"
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
--   Rogue Phase 2: the per-adventure meta.* variables are loaded from
--   `saves/<slug>_meta.json` and merged over the initial state before the
--   first command (M5: the meta file wins over the initial VarMap).
runGameWithFrontend :: Frontend -> GameState -> IO ()
runGameWithFrontend fe state0 = do
    state <- mergeMetaFromDisk state0
    -- Zusatz-Empfehlung 4: every fresh run advances meta.runs (and persists it
    -- immediately — even an abandoned run counts). No-op for adventures
    -- without meta-progression (Default-Invariante).
    let state' = bumpMetaRuns state
    persistMeta state'
    let (newState, message) = executeCommand Look state'
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
                        -- Rogue Phase 1: ironman restricts saving to savezones
                        -- and routes it into one checkpoint slot; normal games
                        -- save wherever the player asks (unchanged).
                        let policy = worldGamePolicy (world state)
                            slotName = if gpIronman policy
                                       then ironmanCheckpointSlot else name
                        case saveBlockedMessage state of
                            Just msg -> do
                                feEmitLine fe msg
                                loopGame fe loopState
                            Nothing -> do
                                saveGame state slotName
                                loopGame fe loopState { lsSaveSlot = Just slotName }
                    Load name -> do
                        case loadBlockedMessage state of
                            Just msg -> do
                                feEmitLine fe msg
                                loopGame fe loopState
                            Nothing -> do
                                result <- loadGame state name
                                case result of
                                    Just loadedState -> do
                                        -- Rogue Phase 2 (M5): the meta file is
                                        -- the authoritative progress store —
                                        -- a slot's snapshot never overrides it.
                                        loadedState' <- mergeMetaFromDisk loadedState
                                        let (_, msg) = executeCommand Look loadedState'
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
                        -- Audio Phase 1: drain queued SFX (fire-and-forget).
                        mapM_ (fePlaySfx fe) (pendingSfx (lsCurrent loopState'))
                        let loopState'a = loopState' { lsCurrent = (lsCurrent loopState') { pendingSfx = [] } }
                        -- Audio Phase 2: dispatch pending music command.
                        case pendingMusic (lsCurrent loopState'a) of
                            Just (MusicStart path) -> feStartMusic fe path
                            Just MusicStop         -> feStopMusic fe
                            Nothing                -> pure ()
                        let loopState'b = loopState'a { lsCurrent = (lsCurrent loopState'a) { pendingMusic = Nothing } }
                        curAfterCutscene <- case pendingCutscene (lsCurrent loopState'b) of
                            Just (frames, micros) -> do
                                fePlayFrames fe micros frames
                                pure ((lsCurrent loopState'b) { pendingCutscene = Nothing })
                            Nothing -> pure (lsCurrent loopState'b)
                        let loopState'' = loopState'b { lsCurrent = curAfterCutscene }
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
    -- Rogue Phase 2: meta.* persists through game over — victory, death,
    -- custom end and quit alike (a quit mid-run keeps the souls collected so
    -- far). Written before anything else so the loop below cannot lose it.
    persistMeta state
    case gameOverReason (save state) of
        Just Death -> do
            emitEndArt fe state Death
                [ "========================================="
                , "  YOU HAVE DIED"
                , "========================================="
                ]
            -- Rogue Phase 1 (M1/M2-Entscheidung): in ironman mode the run's
            -- checkpoint dies with the run — exactly one slot, tracked by
            -- 'lsSaveSlot' since the last in-zone save. The `--save` start
            -- file is never touched (neutral re-entry point).
            let policy = worldGamePolicy (world state)
            when (gpIronman policy) $
                maybe (pure ()) deleteSaveSlot (lsSaveSlot loopState)
            feEmitLine fe (deathMenuText policy)
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

-- | Death screen input loop. Policy gates (Rogue Phase 1):
--   * permadeath: no undo/load at all — only restart or quit;
--   * ironman: the checkpoint was deleted by 'handleGameOver', load is
--     rejected (restoring it would defeat the deletion);
--   * 'gpAllowUndo = false' rejects undo.
--   Everything else behaves as before (undo is the only path that can
--   actually restore, and it needs an unspent history entry).
deathLoop :: Frontend -> LoopState -> IO ()
deathLoop fe loopState = do
    inputResult <- feReadPlain fe state "> "
    let policy = worldGamePolicy (world state)
    case map toLower . fromMaybe "q" <$> pure inputResult of
        Just "u"
            | gpPermadeath policy -> do
                feEmitLine fe "No undo after death (permadeath)."
                deathLoop fe loopState
            | gpIronman policy -> do
                feEmitLine fe "No undo in ironman mode."
                deathLoop fe loopState
            | not (gpAllowUndo policy) -> do
                feEmitLine fe "Undo is disabled in this adventure."
                deathLoop fe loopState
            | otherwise ->
                case lsHistory loopState of
                    [] -> do
                        feEmitLine fe "Nothing to undo."
                        deathLoop fe loopState
                    _ -> do
                        let (restored, msg) = applyLoopCommand Undo loopState
                        feEmitLine fe msg
                        loopGame fe restored
        Just "l"
            | gpPermadeath policy -> do
                feEmitLine fe "No load after death (permadeath)."
                deathLoop fe loopState
            | gpIronman policy -> do
                feEmitLine fe "Loading is disabled in ironman mode."
                deathLoop fe loopState
            | otherwise -> do
                feEmitLine fe "Enter save name to load (or press Enter for 'savegame'):"
                nameResult <- feReadPlain fe state "> "
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
        Just "r" -> runRestart fe loopState
        Just "q" -> feEmitLine fe "Thanks for playing!"
        _ -> do
            feEmitLine fe (deathMenuText policy)
            deathLoop fe loopState
  where
    state = lsCurrent loopState

-- | Victory/custom game-over input loop
victoryLoop :: Frontend -> LoopState -> IO ()
victoryLoop fe loopState = do
    inputResult <- feReadPlain fe (lsCurrent loopState) "> "
    case map toLower . fromMaybe "q" <$> pure inputResult of
        Just "r" -> runRestart fe loopState
        Just "q" -> feEmitLine fe "Thanks for playing!"
        _ -> do
            feEmitLine fe "  [R]estart  |  [Q]uit"
            victoryLoop fe loopState
