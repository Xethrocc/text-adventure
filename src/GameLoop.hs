-- | Main game loop and user interaction for the text adventure engine
module GameLoop
  ( runGame
  , runGameWith
  , runGameWithFrontend
  , gameLoop
  , LoopState (..)
  , initLoopState
  , PendingDisambiguation (..)
  , applyLoopCommand
  , applyLoopCommandEv
  , sideEvents
  , initSampleGame
  , commandEvents
  , consumesTurn
  , consumesTurnIn
  , handleGameOver
  , deathLoop
  , victoryLoop
  , saveBlockedMessage
  , loadBlockedMessage
  , deathMenuText
  , carryMetaVars
  , persistMeta
  , reseedRng
  , bumpMetaRuns
  , SessionRequest (..)
  , IoRequest
  , RequestOutcome (..)
  , SessionState (..)
  , transitionSave
  , transitionLoad
  , transitionLoadSuccess
  , transitionListSaves
  , transitionRestart
  , transitionGameOver
  , transitionDeathUndo
  , transitionDeathCanLoad
  , transitionDeathLoadSlot
  , transitionDeathInput
  , transitionVictoryInput
  , advanceNarrative
  , executeRequest
  , endScreenLines
  ) where

import Types
import Game
import Messages (renderMsg, evMsg)
import Effects
import Parser
import Verbs (verbCanonicalName)
import SaveLoad
import Sample (initSampleGame)
import Frontend
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
--   'lsPendingDisambiguation' (Phase 2.3) remembers an open "Which do you
--   mean?" question: the candidate ids in prompt order plus the command that
--   produced the ambiguity, so the player's answer can replay it. Runtime only —
--   'LoopState' is never serialized, so no save/world byte is affected.
data LoopState = LoopState
    { lsCurrent :: GameState
    , lsHistory :: [GameState]
    , lsInitial :: GameState   -- ^ pristine initial state, used by Restart
    , lsSaveSlot :: Maybe String
    , lsPendingDisambiguation :: Maybe PendingDisambiguation
    } deriving (Show, Eq)

-- | An open disambiguation question (Phase 2.3).
data PendingDisambiguation = PendingDisambiguation
    { pdCandidates :: [String]  -- ^ candidate entity ids, in prompt order
    , pdCommand    :: Command   -- ^ the command that hit the ambiguity
    } deriving (Show, Eq)

initLoopState :: GameState -> LoopState
initLoopState state = LoopState state [] state Nothing Nothing

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

-- | Phase 1.2 (primary path): run one command and return the ordered event
--   stream — catalog messages keep key + args, prose/art/audio/state changes
--   travel as their own events. 'applyLoopCommand' is the compatibility form
--   (rendered back to the flat CLI text, byte-identical).
--
--   Phase 2.3: this is also the disambiguation entry point. If a question from
--   the previous command is still open, the input is first interpreted as an
--   answer (a number or a distinguishing word); a valid answer replays the
--   original command on the chosen candidate **without advancing the clock**,
--   an invalid one falls through as a normal command (which also closes the
--   question). Otherwise the command runs normally and an 'EvDisambiguate' in
--   its stream opens a new question.
applyLoopCommandEv :: Command -> LoopState -> (LoopState, [OutputEvent])
applyLoopCommandEv command loopState
    | Just (ls', evs) <- applyPendingDisambiguation command loopState = (ls', evs)
    | otherwise =
        let (ls', evs) = applyLoopCommandCore command loopState
        in (ls' { lsPendingDisambiguation = newPending command evs }, evs)

-- | Phase 2.3: resolve an open disambiguation question with this input. The
--   chosen entity id is written into 'chosenTarget' so the replayed command
--   resolves to exactly that entity (its id may also be a keyword of another
--   candidate — the answer picks an entity, not a string).
applyPendingDisambiguation :: Command -> LoopState -> Maybe (LoopState, [OutputEvent])
applyPendingDisambiguation command loopState = do
    pending <- lsPendingDisambiguation loopState
    raw     <- disambiguationAnswerText command
    chosen  <- resolveDisambiguationAnswer (pdCandidates pending) raw (lsCurrent loopState)
    let st' = (lsCurrent loopState) { chosenTarget = Just chosen }
    pure (runCommandNoTurn (pdCommand pending)
                           loopState { lsPendingDisambiguation = Nothing, lsCurrent = st' })

-- | Phase 2.3: the command shapes that can carry an answer are the ones the
--   parser could not resolve: a bare number becomes 'ChooseCmd' (Phase 4.6),
--   anything else unknown keeps its raw input line. Every other command is a
--   normal command and closes an open question.
disambiguationAnswerText :: Command -> Maybe String
disambiguationAnswerText (Unknown raw) = Just raw
disambiguationAnswerText (ChooseCmd n)  = Just (show n)
disambiguationAnswerText _              = Nothing

-- | Phase 2.3: does this command's event stream open a disambiguation question?
--   Only commands whose target resolution can be disambiguated
--   ('disambiguableCommand') are remembered; any other command (including an
--   invalid answer) closes a pending question.
newPending :: Command -> [OutputEvent] -> Maybe PendingDisambiguation
newPending command evs
    | disambiguableCommand command
    , ids : _ <- [candidateIds | EvDisambiguate candidateIds <- evs]
    = Just (PendingDisambiguation ids command)
    | otherwise = Nothing

-- | Phase 2.3: replay the disambiguated command without a turn — the whole
--   question/answer exchange must not advance the clock (plan row 2.3). The
--   veto check still runs, but a `block: turn: true` rule cannot make the
--   answer cost a turn either. The transient 'chosenTarget' is cleared again
--   so it can never leak into a later command.
runCommandNoTurn :: Command -> LoopState -> (LoopState, [OutputEvent])
runCommandNoTurn command loopState =
    case checkBeforeVeto command (lsCurrent loopState) of
        Left (stBlocked, msgs, _) ->
            (loopState { lsCurrent = stBlocked { chosenTarget = Nothing } }
            , msgs ++ sideEvents (lsCurrent loopState) stBlocked)
        Right (stAfterBefore, beforeMsgs) ->
            let (lsDone, evsDone) = applyAfterVeto [OnTurn] command stAfterBefore beforeMsgs loopState
            in (lsDone { lsCurrent = (lsCurrent lsDone) { chosenTarget = Nothing } }, evsDone)

-- | Phase 2.3: shared tail of the non-turn dispatch path — dispatch, fire the
--   command triggers and append the additive side events. 'skipEvents' removes
--   event types from consideration: the disambiguation answer replays a
--   turn-shaped command without advancing the clock, so it passes 'OnTurn'
--   here — "no clock advance" must also mean "no `on: turn`".
applyAfterVeto :: [EventType] -> Command -> GameState -> [OutputEvent] -> LoopState -> (LoopState, [OutputEvent])
applyAfterVeto skipEvents command stAfterBefore beforeMsgs loopState =
    let (newState, message) = dispatchCommandEv command stAfterBefore
        (stateAfterTriggers, triggerMsg) =
            fireCommandTriggersSkipping skipEvents command (lsCurrent loopState) newState
        cmdMsg = joinBeforeAndCmd beforeMsgs message
        combined = joinEv cmdMsg triggerMsg ++ sideEvents (lsCurrent loopState) stateAfterTriggers
    in (loopState { lsCurrent = stateAfterTriggers }, combined)

applyLoopCommandCore :: Command -> LoopState -> (LoopState, [OutputEvent])
applyLoopCommandCore Undo loopState
    -- Rogue Phase 1: the author can disable undo globally (gpAllowUndo). The
    -- state stays untouched — the command is refused, nothing to tick.
    | not (gpAllowUndo (worldGamePolicy (world (lsCurrent loopState)))) =
        (loopState, evMsg "undo.disabled" [])
    | otherwise = case lsHistory loopState of
        [] -> (loopState, evMsg "undo.nothing" [])
        previous : rest ->
            (loopState { lsCurrent = previous, lsHistory = rest }, evMsg "undo.done" [])
applyLoopCommandCore Quit loopState =
    let (newState, message) = executeCommandEv Quit (lsCurrent loopState)
    in (loopState { lsCurrent = newState }, message)
applyLoopCommandCore Restart loopState =
    -- Rogue Phase 2: meta.* travels from the dying/finished run into the fresh
    -- one (the plan's restart semantics); lsSaveSlot resets (initLoopState).
    -- The rngState reseed + run counter happen in the IO restart path
    -- ('runRestart') — this branch stays pure for the unit tests.
    let (newState, message) = executeCommandEv Look (lsInitial loopState)
    in (initLoopState (carryMetaVars (lsCurrent loopState) newState), message)


applyLoopCommandCore Help loopState = (loopState, evMsg "help.text" [])
applyLoopCommandCore (Save _) loopState = (loopState, [])
applyLoopCommandCore (Load _) loopState = (loopState, [])
applyLoopCommandCore ListSaves loopState = (loopState, [])
applyLoopCommandCore command loopState =
    case checkBeforeVeto command (lsCurrent loopState) of
        Left (stBlocked, msgs, False) ->
            -- Vetoed without turn consumption (Phase 2.2 default)
            (loopState { lsCurrent = stBlocked }
            , msgs ++ sideEvents (lsCurrent loopState) stBlocked)

        Left (stBlocked, msgs, True) ->
            -- Vetoed with consumesTurn = True: command action dropped, but turn ticks advance!
            let oldState = lsCurrent loopState
                policy = worldGamePolicy (world oldState)
                history' = if gpAllowUndo policy
                           then take maxUndoHistory (oldState : lsHistory loopState)
                           else lsHistory loopState
                stateWithTurn = incrementTurnCount stBlocked
                (stateAfterTick, tickMsgs) = tickConditions stateWithTurn
                (stateAfterVehicleTick, vehicleTickMsg) = vehicleConditionTick stateAfterTick
                allTickMsgs = tickMsgs ++ (if null (renderEvents vehicleTickMsg) then [] else [vehicleTickMsg])
                tickText = unlinesEv allTickMsgs
                (stateAfterTurnTriggers, turnTrigMsg) = fireTriggers OnTurn stateAfterVehicleTick
                fullMsg = if null allTickMsgs then msgs else tickText ++ msgs
            in (loopState { lsCurrent = stateAfterTurnTriggers, lsHistory = history' }
               , joinEv fullMsg turnTrigMsg ++ sideEvents oldState stateAfterTurnTriggers)

        Right (stAfterBefore, beforeMsgs)
            | not (consumesTurnIn (lsCurrent loopState) command) ->
                applyAfterVeto [] command stAfterBefore beforeMsgs loopState

            | otherwise ->
                let oldState = lsCurrent loopState
                    policy = worldGamePolicy (world oldState)
                    -- Rogue Phase 1: with undo disabled the history is not tracked at
                    -- all (saves memory; the command is rejected before use anyway).
                    history' = if gpAllowUndo policy
                               then take maxUndoHistory (oldState : lsHistory loopState)
                               else lsHistory loopState
                    stateWithTurn = incrementTurnCount stAfterBefore
                    (stateAfterTick, tickMsgs) = tickConditions stateWithTurn
                    (stateAfterVehicleTick, vehicleTickMsg) = vehicleConditionTick stateAfterTick
                    allTickMsgs = tickMsgs ++ (if null (renderEvents vehicleTickMsg) then [] else [vehicleTickMsg])
                    tickText = unlinesEv allTickMsgs
                in if gameOver (save stateAfterVehicleTick)
                   then
                       -- L11: the condition tick ended the game before the command ran
                       -- (the tick pipeline runs first). The player is already dead, so
                       -- the command is dropped — only the tick messages are reported.
                       (loopState { lsCurrent = stateAfterVehicleTick, lsHistory = history' },
                           tickText ++ sideEvents oldState stateAfterVehicleTick)
                   else
                       let (newState, message) = dispatchCommandEv command stateAfterVehicleTick
                           (stateAfterTriggers, triggerMsg) = fireCommandTriggers command stateAfterVehicleTick newState
                           cmdMsg = joinBeforeAndCmd beforeMsgs message
                           fullMessage = if null allTickMsgs then cmdMsg else tickText ++ cmdMsg
                       in (loopState { lsCurrent = stateAfterTriggers, lsHistory = history' },
                           joinEv fullMessage triggerMsg ++ sideEvents oldState stateAfterTriggers)

-- | Phase 1.2: additive side events derived from the state transition —
--   they contribute no text ('evTextOf' = ""), so the CLI/TUI rendering is
--   byte-identical; graphical frontends and the protocol (1.4) consume them.
--   Order after all text of the command: room, quest, dialogue, combat,
--   game over, then the presentation queues (audio, animation).
sideEvents :: GameState -> GameState -> [OutputEvent]
sideEvents before after = concat
    [ [ EvRoomChanged (currentRoom (save after))
      | currentRoom (save before) /= currentRoom (save after) ]
    , [ EvQuestUpdate
      | (activeQuests (save before), completedQuests (save before))
        /= (activeQuests (save after), completedQuests (save after)) ]
    , [ EvDialogue
      | activeDialogue (save before) /= activeDialogue (save after)
      , activeDialogue (save after) /= Nothing ]
    , [ EvCombat engagedAfter
      | lookupVarOf before combatEngagedKey /= lookupVarOf after combatEngagedKey ]
      -- the new value as Bool (non-zero = engaged)
    , [ EvGameOver | gameOver (save after) && not (gameOver (save before)) ]
    , map EvSfx (pendingSfx after)
    , case pendingMusic after of
        Just (MusicStart path) -> [EvMusicStart path]
        Just MusicStop         -> [EvMusicStop]
        Nothing                -> []
    , case pendingAnimation after of
        Just (frames, micros) -> [EvAnim micros frames]
        Nothing               -> []
    ]
  where
    lookupVarOf st name = Map.lookup name (variables (save st))
    engagedAfter = case Map.lookup combatEngagedKey (variables (save after)) of
        Just (VVInt n) -> n /= 0
        _              -> False

-- | Compatibility form (Phase 1.2; used by the tests and the loop's aux
--   paths): the same command, its event stream rendered back to the flat CLI
--   text — byte-identical to the pre-1.2 string pipeline.
applyLoopCommand :: Command -> LoopState -> (LoopState, String)
applyLoopCommand cmd loopState =
    let (ls', evs) = applyLoopCommandEv cmd loopState
    in (ls', renderEvents evs)

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
    let (fresh, reqs, lines') = transitionRestart seed loopState
    mapM_ (executeRequest fe (lsCurrent fresh)) reqs
    mapM_ (feEmitLine fe) lines'
    loopGame fe fresh

-- | IO-side seed derivation for restarts (M6: clock stays in the IO path).
newRngSeedIO :: IO Word64
newRngSeedIO = do
    t <- getPOSIXTime
    pure (floor (t * 1000))

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
    | otherwise = Just (renderMsg "save.savezone_only" [])
  where
    policy = worldGamePolicy (world st)

-- | Rogue Phase 1: loading is rejected in ironman mode — restoring a save
--   would let the player outlive a death the checkpoint deletion was supposed
--   to make final.
loadBlockedMessage :: GameState -> Maybe String
loadBlockedMessage st
    | gpIronman (worldGamePolicy (world st)) = Just (renderMsg "load.ironman_blocked" [])
    | otherwise = Nothing

-- | The menu line under the death screen. Permadeath (and ironman, which
--   deletes the checkpoint) offer no undo/load, only restart or quit.
deathMenuText :: GamePolicy -> String
deathMenuText policy
    | gpPermadeath policy || gpIronman policy = renderMsg "menu.restart_quit" []
    | otherwise = renderMsg "menu.death_full" []

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
fireCommandTriggers :: Command -> GameState -> GameState -> (GameState, [OutputEvent])
fireCommandTriggers = fireCommandTriggersSkipping []

-- | Like 'fireCommandTriggers', but ignoring the listed event types (Phase 2.3:
--   the disambiguation answer skips 'OnTurn' — it must not advance the clock).
fireCommandTriggersSkipping :: [EventType] -> Command -> GameState -> GameState -> (GameState, [OutputEvent])
fireCommandTriggersSkipping skipEvents cmd before after =
    let events = filter (`notElem` skipEvents) (commandEvents cmd before after)
        afterWithCmdVars = bindCommandVars cmd after
        (st, msgs) = foldl' (\(s, acc) ev -> let (s', m) = fireTriggers ev s
                                            in (s', joinEv acc m))
                           (afterWithCmdVars, []) events
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
        -- Phase 0.1 (B1): resolve target against reachable entities via resolveTarget
        -- so executeCommand and commandEvents agree on the target ID.
        Interact VTake t ->
            [ OnTake iid | Just iid <- [resolvedItemId VTake t before after]
                         , itemLoc iid before /= Just (CarriedBy ActorPlayer)
                         , itemLoc iid after  == Just (CarriedBy ActorPlayer) ]
        Interact VDrop t ->
            [ OnDrop iid | Just iid <- [resolvedItemId VDrop t before after]
                         , itemLoc iid before == Just (CarriedBy ActorPlayer)
                         , itemLoc iid after  /= Just (CarriedBy ActorPlayer) ]
        -- `use` has no state criterion (its effect is up to the author).
        -- Phase 0.3 (B3): in darkness, use on non-carried targets is refused, so OnUse does not fire.
        Interact VUse t  ->
            [ OnUse iid | Just iid <- [resolvedItemId VUse t before after]
                        , not (isCurrentRoomDark before && itemLoc iid before /= Just (CarriedBy ActorPlayer)) ]
        ActionWithArgs VTake args ->
            let t = unwords args
            in [ OnTake iid | Just iid <- [resolvedItemId VTake t before after]
                            , itemLoc iid before /= Just (CarriedBy ActorPlayer)
                            , itemLoc iid after  == Just (CarriedBy ActorPlayer) ]
        ActionWithArgs VDrop args ->
            let t = unwords args
            in [ OnDrop iid | Just iid <- [resolvedItemId VDrop t before after]
                            , itemLoc iid before == Just (CarriedBy ActorPlayer)
                            , itemLoc iid after  /= Just (CarriedBy ActorPlayer) ]
        ActionWithArgs VUse args ->
            let t = unwords args
            in [ OnUse iid | Just iid <- [resolvedItemId VUse t before after]
                           , not (isCurrentRoomDark before && itemLoc iid before /= Just (CarriedBy ActorPlayer)) ]
        _ -> []
    itemLoc i st = itemLocation <$> Map.lookup i (itemStates (save st))
    lookSearchEvents = case cmd of
        Look            -> [OnLook (currentRoom (save after)) | not (isCurrentRoomDark before), isJust (lookupRoom (currentRoom (save after)) after)]
        SearchCmd _     -> [OnSearch (currentRoom (save after)) | not (isCurrentRoomDark before), isJust (lookupRoom (currentRoom (save after)) after)]
        _               -> []

-- | Resolve an item ID for command triggers using central target resolution (Phase 0.1).
--   Prefers resolving against 'before' (the state when the command was issued),
--   falling back to 'after' (e.g. for synthetic test transitions).
resolvedItemId :: Verb -> String -> GameState -> GameState -> Maybe String
resolvedItemId verb t before after =
    case resolveTarget verb t before of
        TargetItem iid -> Just iid
        _              -> case resolveTarget verb t after of
            TargetItem iid -> Just iid
            _              -> Nothing

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

-- | Dispatch presentation audio and cutscenes, returning the updated state.
dispatchAudioAndCutscene :: Frontend -> LoopState -> IO LoopState
dispatchAudioAndCutscene fe loopState = do
    let cur = lsCurrent loopState
    mapM_ (fePlaySfx fe) (pendingSfx cur)
    let curA = cur { pendingSfx = [] }
    case pendingMusic curA of
        Just (MusicStart path) -> feStartMusic fe path
        Just MusicStop         -> feStopMusic fe
        Nothing                -> pure ()
    let curB = curA { pendingMusic = Nothing }
    curAfterCutscene <- case pendingCutscene curB of
        Just (frames, micros) -> do
            fePlayFrames fe micros frames
            pure (curB { pendingCutscene = Nothing })
        Nothing -> pure curB
    pure (loopState { lsCurrent = curAfterCutscene })

-- | Play narrative lines sequentially, emitting pauses for intermediate lines.
playNarrativeLines :: Frontend -> GameState -> [SessionRequest] -> [String] -> IO ()
playNarrativeLines fe st reqs allLines =
    case reqs of
        [] -> mapM_ (feEmitLine fe) allLines
        _  -> do
            let numPauses = length reqs
                (pausedLines, restLines) = splitAt numPauses allLines
            mapM_ (\l -> feEmitLine fe l >> executeRequest fe st ReqPause) pausedLines
            mapM_ (feEmitLine fe) restLines

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
                        let (loopState', reqs, msgs) = transitionSave name loopState
                        mapM_ (executeRequest fe (lsCurrent loopState')) reqs
                        mapM_ (feEmitLine fe) msgs
                        loopGame fe loopState'
                    Load name -> do
                        let (loopState', reqs, msgs) = transitionLoad name loopState
                        mapM_ (feEmitLine fe) msgs
                        outcome <- firstLoadOutcome <$> mapM (executeRequest fe (lsCurrent loopState')) reqs
                        case outcome of
                            Just (Loaded loadedState diskMeta) -> do
                                let (freshLoop, lookLines) = transitionLoadSuccess diskMeta loadedState
                                mapM_ (feEmitLine fe) lookLines
                                loopGame fe freshLoop
                            Nothing -> loopGame fe loopState'
                    ListSaves -> do
                        let (_, reqs, _) = transitionListSaves loopState
                        mapM_ (executeRequest fe (lsCurrent loopState)) reqs
                        loopGame fe loopState
                    Restart -> runRestart fe loopState
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
                        loopStateAfterAudio <- dispatchAudioAndCutscene fe loopState'
                        case pendingAnimation (lsCurrent loopStateAfterAudio) of
                            Just (frames, micros) -> do
                                fePlayFrames fe micros frames
                                let cleared = (lsCurrent loopStateAfterAudio) { pendingAnimation = Nothing }
                                loopGame fe (loopStateAfterAudio { lsCurrent = cleared })
                            Nothing ->
                                case advanceNarrative (lsCurrent loopStateAfterAudio) of
                                    Nothing ->
                                        loopGame fe loopStateAfterAudio
                                    Just (clearedState, reqs, narrativeLines) -> do
                                        playNarrativeLines fe (lsCurrent loopStateAfterAudio) reqs narrativeLines
                                        loopGame fe (loopStateAfterAudio { lsCurrent = clearedState })
  where
    state = lsCurrent loopState

-- ---------------------------------------------------------------------------
-- Session automaton types and pure transitions (Plan 1.3)
-- ---------------------------------------------------------------------------

-- | IO requests produced by pure session transitions and executed by the
--   game loop interpreter (Plan 1.3).
data SessionRequest
    = ReqSave String           -- ^ Request saving current state to a slot name
    | ReqLoad String           -- ^ Request loading state from a slot name
    | ReqPersistMeta           -- ^ Request persisting meta.* variables to disk
    | ReqPause                 -- ^ Request narrative continuation pause (Enter)
    | ReqDeleteSave String     -- ^ Request deleting an ironman checkpoint slot
    | ReqListSaves             -- ^ Request listing all saved games
    deriving (Show, Eq)

-- | Alias for SessionRequest (Plan 1.3 terminology).
type IoRequest = SessionRequest

-- | Result of executing a session request that produces a value. Only 'ReqLoad'
--   has one: the state read from disk plus the meta variables of that world, so
--   the caller can feed both into 'transitionLoadSuccess' without repeating the
--   file IO.
data RequestOutcome
    = Loaded GameState (Map.Map String VariableValue)
    deriving (Show)

-- | Session automaton states for the pure game loop transitions.
data SessionState
    = SessionPlaying LoopState
    | SessionDeath LoopState
    | SessionDeathPromptLoad LoopState
    | SessionVictory LoopState GameOverReason
    | SessionEnded
    deriving (Show, Eq)

-- | Thin IO interpreter for session requests. Requests that produce a value
--   return it ('ReqLoad'); the rest report 'Nothing' and act through their side
--   effect.
executeRequest :: Frontend -> GameState -> SessionRequest -> IO (Maybe RequestOutcome)
executeRequest _fe st (ReqSave slot)        = saveGame st slot >> pure Nothing
executeRequest _fe st (ReqLoad slot)        = do
    mbLoaded <- loadGame st slot
    case mbLoaded of
        Nothing           -> pure Nothing
        Just loadedState  -> do
            diskMeta <- loadMeta (world loadedState)
            pure (Just (Loaded loadedState diskMeta))
executeRequest _fe st ReqPersistMeta        = persistMeta st >> pure Nothing
executeRequest fe _st ReqPause              = feReadPause fe >> pure Nothing
executeRequest _fe _st (ReqDeleteSave slot)  = deleteSaveSlot slot >> pure Nothing
executeRequest _fe st ReqListSaves          = listSaves (world st) >> pure Nothing

-- | The first value-producing outcome of a request batch (there is at most one
--   today — only 'ReqLoad' reports one).
firstLoadOutcome :: [Maybe RequestOutcome] -> Maybe RequestOutcome
firstLoadOutcome outcomes = case [o | Just o <- outcomes] of
    (o:_) -> Just o
    []    -> Nothing

-- | Pure transition for the 'save' command. Checks the save policy
--   (ironman savezones) and yields an IO request on success.
transitionSave :: String -> LoopState -> (LoopState, [SessionRequest], [String])
transitionSave name loopState =
    let st = lsCurrent loopState
        policy = worldGamePolicy (world st)
        slotName = if gpIronman policy then ironmanCheckpointSlot else name
    in case saveBlockedMessage st of
        Just msg -> (loopState, [], [msg])
        Nothing  -> (loopState { lsSaveSlot = Just slotName }, [ReqSave slotName], [])

-- | Pure transition for the 'load' command. Checks the load policy
--   (ironman disabled) and yields an IO request on success.
transitionLoad :: String -> LoopState -> (LoopState, [SessionRequest], [String])
transitionLoad name loopState =
    let st = lsCurrent loopState
    in case loadBlockedMessage st of
        Just msg -> (loopState, [], [msg])
        Nothing  -> (loopState, [ReqLoad name], [])

-- | Pure state update after a successful game load: merges disk meta.* variables
--   over the loaded state and executes a fresh 'look'.
transitionLoadSuccess :: Map.Map String VariableValue -> GameState -> (LoopState, [String])
transitionLoadSuccess diskMeta loadedState =
    let loadedState' = loadedState
            { save = (save loadedState)
                { variables = mergeMetaVars diskMeta (variables (save loadedState)) } }
        (_, lookMsg) = executeCommand Look loadedState'
    in (initLoopState loadedState', [lookMsg])

-- | Pure transition for the 'list saves' command.
transitionListSaves :: LoopState -> (LoopState, [SessionRequest], [String])
transitionListSaves ls = (ls, [ReqListSaves], [])

-- | Pure transition for restarting the adventure: derives initial state,
--   carries meta variables, reseeds RNG with the given seed, increments
--   meta.runs, and requests disk persistence of the meta state.
transitionRestart :: Word64 -> LoopState -> (LoopState, [SessionRequest], [String])
transitionRestart seed loopState =
    let (restarted, lookMsg) = applyLoopCommand Restart loopState
        freshCurrent = reseedRng seed . bumpMetaRuns $ lsCurrent restarted
        freshLoop = restarted { lsCurrent = freshCurrent }
        lines' = [renderMsg "game.restart_start" [], lookMsg]
    in (freshLoop, [ReqPersistMeta], lines')

-- | Pure formatting of end-game screen lines: blank line, resolved end art
--   (or built-in banner fallback), followed by a blank line.
endScreenLines :: GameState -> GameOverReason -> [String]
endScreenLines st reason =
    let fallback = case reason of
            Death ->
                [ renderMsg "end.rule_line" []
                , renderMsg "death.title" []
                , renderMsg "end.rule_line" []
                ]
            Victory ->
                [ renderMsg "end.rule_line" []
                , renderMsg "victory.title" []
                , renderMsg "end.rule_line" []
                ]
            Custom msg ->
                [ renderMsg "gameover.custom" [("msg", msg)] ]
        artLines = case endArtFor reason st of
            Just art -> [resolveAsciiArt art st]
            Nothing  -> fallback
    in [""] ++ artLines ++ [""]

-- | Pure transition when 'gameOver' is detected: requests meta persistence,
--   checkpoint deletion in ironman mode, produces the end screen and menu lines,
--   and transitions to the appropriate session state (Death, Victory, Ended).
transitionGameOver :: LoopState -> (SessionState, [SessionRequest], [String])
transitionGameOver loopState =
    let st = lsCurrent loopState
        policy = worldGamePolicy (world st)
        mbReason = gameOverReason (save st)
        reqs = case mbReason of
            Just Death ->
                ReqPersistMeta : [ReqDeleteSave slot | gpIronman policy, Just slot <- [lsSaveSlot loopState]]
            Just _ -> [ReqPersistMeta]
            Nothing -> [ReqPersistMeta]
        (nextSession, promptLine) = case mbReason of
            Just Death -> (SessionDeath loopState, [deathMenuText policy])
            Just Victory -> (SessionVictory loopState Victory, [renderMsg "menu.restart_quit" []])
            Just (Custom msg) -> (SessionVictory loopState (Custom msg), [renderMsg "menu.restart_quit" []])
            Nothing -> (SessionEnded, [])
        lines' = case mbReason of
            Just reason -> endScreenLines st reason ++ promptLine
            Nothing     -> []
    in (nextSession, reqs, lines')

-- | Pure transition for the death screen 'undo' option: checks policy
--   and history, restoring the previous state on success.
transitionDeathUndo :: LoopState -> (SessionState, [SessionRequest], [String])
transitionDeathUndo loopState =
    let st = lsCurrent loopState
        policy = worldGamePolicy (world st)
    in if gpPermadeath policy
       then (SessionDeath loopState, [], [renderMsg "undo.permadeath" []])
       else if gpIronman policy
       then (SessionDeath loopState, [], [renderMsg "undo.ironman" []])
       else if not (gpAllowUndo policy)
       then (SessionDeath loopState, [], [renderMsg "undo.disabled" []])
       else case lsHistory loopState of
           [] -> (SessionDeath loopState, [], [renderMsg "undo.nothing" []])
           _  ->
               let (restored, msg) = applyLoopCommand Undo loopState
               in (SessionPlaying restored, [], [msg])

-- | Pure check whether loading from the death screen is permitted.
transitionDeathCanLoad :: LoopState -> (Bool, [String])
transitionDeathCanLoad loopState =
    let policy = worldGamePolicy (world (lsCurrent loopState))
    in if gpPermadeath policy
       then (False, [renderMsg "load.permadeath" []])
       else if gpIronman policy
       then (False, [renderMsg "load.ironman_blocked" []])
       else (True, [renderMsg "load.prompt" []])

-- | Pure determination of slot name and load request for death menu load.
transitionDeathLoadSlot :: Maybe String -> (String, SessionRequest)
transitionDeathLoadSlot nameResult =
    let slot = case nameResult of
            Just n | not (null n) -> n
            _                     -> "savegame"
    in (slot, ReqLoad slot)

-- | Pure transition for death screen choices: 'u' (undo), 'l' (load),
--   'r' (restart), 'q' (quit), or invalid input.
transitionDeathInput :: Word64 -> Maybe String -> LoopState -> (SessionState, [SessionRequest], [String])
transitionDeathInput seed inputResult loopState =
    let policy = worldGamePolicy (world (lsCurrent loopState))
        choice = map toLower (fromMaybe "q" inputResult)
    in case choice of
        "u" -> transitionDeathUndo loopState
        "r" ->
            let (freshLoop, reqs, lines') = transitionRestart seed loopState
            in (SessionPlaying freshLoop, reqs, lines')
        "q" -> (SessionEnded, [], [renderMsg "quit.thanks" []])
        "l" ->
            let (canLoad, msgs) = transitionDeathCanLoad loopState
            in if canLoad
               then (SessionDeathPromptLoad loopState, [], msgs)
               else (SessionDeath loopState, [], msgs)
        _   -> (SessionDeath loopState, [], [deathMenuText policy])

-- | Pure transition for victory screen choices: 'r' (restart), 'q' (quit),
--   or invalid input.
transitionVictoryInput :: Word64 -> Maybe String -> LoopState -> GameOverReason -> (SessionState, [SessionRequest], [String])
transitionVictoryInput seed inputResult loopState reason =
    let choice = map toLower (fromMaybe "q" inputResult)
    in case choice of
        "r" ->
            let (freshLoop, reqs, lines') = transitionRestart seed loopState
            in (SessionPlaying freshLoop, reqs, lines')
        "q" -> (SessionEnded, [], [renderMsg "quit.thanks" []])
        _   -> (SessionVictory loopState reason, [], [renderMsg "menu.restart_quit" []])

-- | Pure narrative step: advances pending narrative lines with pauses (ReqPause)
--   between lines, executes the follow-up outcome, and clears pendingNarrative.
advanceNarrative :: GameState -> Maybe (GameState, [SessionRequest], [String])
advanceNarrative st = case pendingNarrative st of
    Nothing -> Nothing
    Just (nls, followUp) ->
        let (finalState, followMsg) = applyOutcome followUp "" st
            clearedState = finalState { pendingNarrative = Nothing }
            followLines = if null followMsg then [] else [followMsg]
            (reqs, lines') = case nls of
                [] -> ([], followLines)
                [single] -> ([], single : followLines)
                _ -> (replicate (length (init nls)) ReqPause, nls ++ followLines)
        in Just (clearedState, reqs, lines')

-- ---------------------------------------------------------------------------
-- Game over screens (interpreter)
-- ---------------------------------------------------------------------------

-- | Handle game-over screen based on reason
handleGameOver :: Frontend -> LoopState -> IO ()
handleGameOver fe loopState = do
    let (nextSession, reqs, lines') = transitionGameOver loopState
    mapM_ (executeRequest fe (lsCurrent loopState)) reqs
    mapM_ (feEmitLine fe) lines'
    case nextSession of
        SessionDeath ls     -> deathLoop fe ls
        SessionVictory ls _ -> victoryLoop fe ls
        _                   -> return ()

-- | Death screen input loop. Policy gates (Rogue Phase 1):
--   * permadeath: no undo/load at all — only restart or quit;
--   * ironman: the checkpoint was deleted by 'handleGameOver', load is
--     rejected (restoring it would defeat the deletion);
--   * 'gpAllowUndo = false' rejects undo.
--   Everything else behaves as before (undo is the only path that can
--   actually restore, and it needs an unspent history entry).
deathLoop :: Frontend -> LoopState -> IO ()
deathLoop fe loopState = do
    inputResult <- feReadPlain fe (lsCurrent loopState) "> "
    let policy = worldGamePolicy (world (lsCurrent loopState))
    case map toLower (fromMaybe "q" inputResult) of
        "u" -> do
            let (nextSession, reqs, lines') = transitionDeathUndo loopState
            mapM_ (executeRequest fe (lsCurrent loopState)) reqs
            mapM_ (feEmitLine fe) lines'
            case nextSession of
                SessionPlaying ls -> loopGame fe ls
                SessionDeath ls   -> deathLoop fe ls
                _                 -> return ()
        "l" -> do
            let (canPrompt, msgs) = transitionDeathCanLoad loopState
            mapM_ (feEmitLine fe) msgs
            if not canPrompt
                then deathLoop fe loopState
                else do
                    nameResult <- feReadPlain fe (lsCurrent loopState) "> "
                    let (_, req) = transitionDeathLoadSlot nameResult
                    outcome <- executeRequest fe (lsCurrent loopState) req
                    case outcome of
                        Just (Loaded loadedState diskMeta) -> do
                            let (freshLoop, lookLines) = transitionLoadSuccess diskMeta loadedState
                            mapM_ (feEmitLine fe) lookLines
                            loopGame fe freshLoop
                        Nothing -> deathLoop fe loopState
        "r" -> runRestart fe loopState
        "q" -> feEmitLine fe (renderMsg "quit.thanks" [])
        _ -> do
            feEmitLine fe (deathMenuText policy)
            deathLoop fe loopState

-- | Victory/custom game-over input loop
victoryLoop :: Frontend -> LoopState -> IO ()
victoryLoop fe loopState = do
    inputResult <- feReadPlain fe (lsCurrent loopState) "> "
    case map toLower (fromMaybe "q" inputResult) of
        "r" -> runRestart fe loopState
        "q" -> feEmitLine fe (renderMsg "quit.thanks" [])
        _ -> do
            feEmitLine fe (renderMsg "menu.restart_quit" [])
            victoryLoop fe loopState
