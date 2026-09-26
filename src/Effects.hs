-- | Effect interpreter, triggers, room transitions, and condition ticks
module Effects
    ( maxOutcomeDepth
    , applyOutcomeWith
    , applyOutcome
    , applyOutcomes
    , applySetValue
    , modifyValueProp
    , effectValToVarVal
    , effectValueToString
    , modifyNPCHealth
    , moveEntityToRoom
    , runRoomHook
    , findExitDirection
    , transitionToRoom
    , introCutsceneOf
    , killNPC
    , killNPCWithMsg
    , setEntityStateWithEvents
    , fireTriggers
    , fireTriggersWithDepth
    , fireTriggerList
    , tickConditions
    , completeQuestWithMsg
    , vehicleConditionTick
    , joinMessages
    ) where

import Types
import Game
import Quests (canStartQuest, startQuest, advanceQuest, completeQuestWith)
import Vehicles (vehicleConditionTickWith)
import Data.List (intercalate, foldl')
import Data.Bits (shiftR)
import Data.Maybe (listToMaybe, fromMaybe)
import Control.Monad (guard)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- ---------------------------------------------------------------------------
-- Outcome interpreter (single, shared implementation)
-- ---------------------------------------------------------------------------

-- | Maximum nesting depth for outcomes. Prevents runaway recursion from
--   malformed content (e.g. a CheckFlag whose branch loops back).
maxOutcomeDepth :: Int
maxOutcomeDepth = 20

-- | Outcome application with a threaded RNG salt and recursion depth.
--   Returns (state, message, nextSalt, nextDepth).
applyOutcomeWith :: Int -> Int -> Effect -> ItemID -> GameState -> (GameState, String, Int)
applyOutcomeWith depth salt outcome targetId state
    | depth > maxOutcomeDepth =
        ( addDiagnostic
            ("[engine] maximum outcome depth exceeded (depth " ++ show depth
             ++ " > " ++ show maxOutcomeDepth
             ++ "): an effect (or a self-triggering event) is nested too deeply")
            state
        , "", salt )
    | otherwise = case outcome of
    SendMessage msg -> (state, formatWithVars msg state, salt)

    Sequence outcomes ->
        let (st', msg', salt') = foldl' (\(st, acc, s) o ->
                let (st2, m2, s2) = applyOutcomeWith (depth + 1) s o targetId st
                in (st2, joinMessages acc m2, s2))
                (state, "", salt) outcomes
        in (st', msg', salt')

    SetValue vr ev ->
        let (state', msg') = applySetValue vr ev state
        in (state', msg', salt)

    ComputeValue vr expr ->
        let val = evalExpr expr state
            (state', msg') = applySetValue vr (EVInt val) state
        in (state', msg', salt)

    ModifyValue VRPlayerHealth delta ->
        let state' = if delta >= 0
                     then updatePlayerHealth (+ delta) state
                     else let st' = updatePlayerHealth (+ delta) state
                          in if isPlayerDead st' then endGame Death st' else st'
        in (state', "", salt)
    ModifyValue vr delta ->
        let (state', m) = modifyValueProp vr delta state
        in (state', m, salt)

    MoveEntity eid (InRoom room) ->
        let state' = moveEntityToRoom eid room state
        in (state', "", salt)
    MoveEntity eid (CarriedBy _) ->
        let state' = giveItem eid state
        in (state', "", salt)
    MoveEntity eid Removed ->
        let state' = consumeItem eid state
        in (state', "", salt)
    MoveEntity eid (EquippedBy _) ->
        case equipItem eid state of
            Left err    -> (state, err, salt)
            Right st'   -> (st', "", salt)
    MoveEntity _ (InContainer _) ->
        (state, "You can't move an item into a container that way.", salt)

    QuestOp StartQuest qId ->
        if canStartQuest qId state
        then (startQuest qId state, "", salt)
        else (state, "You cannot start that quest right now.", salt)
    QuestOp AdvanceQuest qId ->
        if Map.member qId (activeQuests (save state))
        then (advanceQuest qId state, "", salt)
        else (state, "That quest is not active.", salt)
    QuestOp CompleteQuest qId ->
        if Map.member qId (activeQuests (save state))
        then let (st', rewardMsg) = completeQuestWithMsg qId state
             in (st', rewardMsg, salt)
        else (state, "That quest is not active.", salt)

    Conditional predicate thenOutcome elseOutcome ->
        if evalPredicate predicate state
        then applyOutcomeWith (depth + 1) salt thenOutcome targetId state
        else applyOutcomeWith (depth + 1) salt elseOutcome targetId state

    RandomChoice [] -> (state, "", salt)
    RandomChoice weighted ->
        let totalWeight = max 1 (sum (map fst weighted))
            rng = nextRng (rngState (save state) + fromIntegral salt)
            pick = fromIntegral ((rng `shiftR` 33) `mod` fromIntegral totalWeight)
            st' = state { save = (save state) { rngState = rng } }
            go :: Int -> [(Int, Effect)] -> Effect
            go _ [(_, e)] = e
            go acc ((w, e):rest)
                | pick < acc + w = e
                | otherwise = go (acc + w) rest
            go _ [] = Noop
        in applyOutcomeWith (depth + 1) (salt + 1) (go 0 weighted) targetId st'

    GameEnd reason msg -> (endGame reason state, formatWithVars msg state, salt)

    PlayClip clipId ->
        case Map.lookup clipId (worldClips (world state)) of
            Just clip | not (null (clipFrames clip))
                     , clipFps clip > 0 ->
                (state { pendingCutscene =
                            Just (clipFrames clip, 1000000 `div` clipFps clip) }
                , "", salt)
            _ -> (state, "", salt)

    PlaySfx path ->
        (state { pendingSfx = pendingSfx state ++ [path] }, "", salt)

    PlayMusic path ->
        (state { pendingMusic = Just (MusicStart path) }, "", salt)

    StopMusic ->
        (state { pendingMusic = Just MusicStop }, "", salt)

    ApplyCondition name turns tick end -> (applyCondition name turns tick end state, "", salt)
    ClearCondition name -> (clearCondition name state, "", salt)

    RaiseEvent name ->
        let (st', m) = fireTriggersWithDepth (depth + 1) (OnCustomEvent name) state
        in (st', m, salt)

    ModifySkill skillId delta -> (modifySkill skillId delta state, "", salt)

    SetExit from dir exit ->
        let ss = save state
            withEntity = case exit of
                Locked _ e | Map.notMember e (entityStates ss) ->
                    ss { entityStates = Map.insert e "locked" (entityStates ss) }
                _ -> ss
            in (state { save = withEntity
                { exitOverrides = Map.insert (from, dir) (Just exit) (exitOverrides withEntity) } }
            , "", salt)
    RemoveExit from dir ->
        (state { save = (save state)
            { exitOverrides = Map.insert (from, dir) Nothing (exitOverrides (save state)) } }
        , "", salt)

    Narrative nls followUp ->
        let formatted = map (`formatWithVars` state) nls
        in (state { pendingNarrative = Just (formatted, followUp) }, intercalate "\n" formatted, salt)

    DrawCards n -> (drawCards n state, "", salt)
    DiscardHand -> (discardHand state, "", salt)
    DiscardCard cid -> (discardCard cid state, "", salt)
    ExhaustCard cid -> (exhaustCard cid state, "", salt)
    AddCardToDeck cid dest -> (addCardToDeck cid dest state, "", salt)
    ShuffleDeck -> (shuffleDeck state, "", salt)

    GenerateRoom rawId rawName rawDesc from toDir returnDir ->
        let ss = save state
            newId = formatWithVars rawId state
            rName = formatWithVars rawName state
            rDesc = formatWithVars rawDesc state
            fromId = if from `elem` ["current", "current_room"] then currentRoom ss else from
            mFromFloor = lookupRoom fromId state >>= roomFloor
            newFloor = case (mFromFloor, toDir) of
                (Just fl, Down) -> Just (fl + 1)
                (Just fl, Up)   -> Just (max 1 (fl - 1))
                (Just fl, _)    -> Just fl
                (Nothing, _)    -> Nothing
            newRoom = Room
                { roomId = newId
                , roomName = rName
                , roomDescription = plainText rDesc
                , roomConnections = Map.singleton returnDir (Open fromId)
                , roomTags = Set.empty
                , roomLightFlag = Nothing
                , roomOnEnter = Nothing
                , roomOnLook = Nothing
                , roomOnExit = Nothing
                , roomSearchOutcome = Nothing
                , roomAscii = emptyAscii
                , roomIntro = Nothing
                , roomFloor = newFloor
                }
            dyn' = Map.insert newId newRoom (dynamicRooms ss)
            overrides' = Map.insert (fromId, toDir) (Just (Open newId)) (exitOverrides ss)
            ss' = ss { dynamicRooms = dyn', exitOverrides = overrides' }
        in (state { save = ss' }, "", salt)

    Noop -> (state, "", salt)

-- | Apply SetValue: set a value reference to a new value (handles flags,
--   variables, entity states). Returns the state plus any message produced by
--   the fired side effects (an entity-state change fires its `OnStateChange`).
applySetValue :: ValueRef -> EffectValue -> GameState -> (GameState, String)
applySetValue (VRFlag name) val state =
    (setFlag name (effectValueToString val) state, "")
applySetValue (VRVariable name) val state =
    (setVariableChecked name (effectValToVarVal val) state, "")
applySetValue (VRActorProp (ActorEntity eId) PState) val state =
    setEntityStateWithEvents eId (effectValueToString val) state
applySetValue (VRActorProp ActorPlayer PRoom) val state =
    (fst (transitionToRoom (effectValueToString val) (clearActiveDialogue state)), "")
applySetValue (VRActorProp (ActorRoom rId) PVisited) val state =
    let b = case val of { EVInt n -> n /= 0; EVBool v -> v; _ -> False }
    in (setRoomVisited rId b state, "")
applySetValue (VRActorProp ActorPlayer PHealth) val state =
    let n = case val of { EVInt m -> m; _ -> 0 }
    in if n <= 0 then (endGame Death (setPlayerHP n state), "") else (setPlayerHP n state, "")
applySetValue (VRActorProp (ActorShip vId) PHealth) val state =
    let n = case val of { EVInt m -> m; _ -> 0 }
    in (setVariableChecked ("ship." ++ vId ++ ".hull") (VVInt n) state, "")
applySetValue VRPlayerHealth val state =
    let n = case val of { EVInt m -> m; _ -> 0 }
    in if n <= 0 then (endGame Death (setPlayerHP n state), "") else (setPlayerHP n state, "")
applySetValue _ _ state = (state, "")

-- | Apply ModifyValue to a non-player-health reference.
modifyValueProp :: ValueRef -> Int -> GameState -> (GameState, String)
modifyValueProp (VRFlag name) delta state =
    let cur = case getFlag name state of
            Just "true" -> 1
            _           -> 0
        newVal = if cur + delta > 0 then "true" else "false"
    in (setFlag name newVal state, "")
modifyValueProp (VRVariable name) delta state =
    let cur = case getVariable name state of
            Just (VVInt n)  -> n
            Just (VVText s) -> case reads s of [(n,_)] -> n; _ -> 0
            _               -> 0
    in (setVariableChecked name (VVInt (cur + delta)) state, "")
modifyValueProp (VRItemProp iId prop) delta state =
    (modifyItemProp iId prop delta state, "")
modifyValueProp (VRActorProp (ActorNPC eId) PHealth) delta state
    | resolveActorNpcId eId state `elem` ["all", "all_enemies"] =
        let curRoom = currentRoom (save state)
            enemies = [ npcId def
                      | def <- getNPCsInRoom curRoom state
                      , not (isDeadNPC (npcId def) state)
                      , not (isInParty (npcId def) state)
                      ]
            step (st, msgs) eid =
                let (st', m) = modifyNPCHealth eid delta st
                in (st', if null m then msgs else msgs ++ [m])
            (stFin, msgsFin) = foldl' step (state, []) enemies
        in (stFin, intercalate "\n" msgsFin)
    | otherwise =
        modifyNPCHealth (resolveActorNpcId eId state) delta state
modifyValueProp (VRActorProp (ActorNPC nId) (PCustom prop)) delta state =
    (modifyNPCProp (resolveActorNpcId nId state) prop delta state, "")
modifyValueProp (VRActorProp ActorPlayer PHealth) delta state =
    let cur = playerHealth (player (save state))
        newHP = cur + delta
    in if newHP <= 0
       then (endGame Death (setPlayerHP newHP state), "")
       else (setPlayerHP newHP state, "")
modifyValueProp (VRActorProp (ActorShip vId) PHealth) delta state =
    let cur = case getVariable ("ship." ++ vId ++ ".hull") state of
            Just (VVInt n) -> n
            _              -> 0
    in (setVariableChecked ("ship." ++ vId ++ ".hull") (VVInt (cur + delta)) state, "")
modifyValueProp _ _ state = (state, "")

-- | Convert EffectValue to VariableValue
effectValToVarVal :: EffectValue -> VariableValue
effectValToVarVal (EVInt n)    = VVInt n
effectValToVarVal (EVString s) = VVText s
effectValToVarVal (EVBool b)   = VVInt (if b then 1 else 0)

-- | Convert EffectValue to String (for flag/state values)
effectValueToString :: EffectValue -> String
effectValueToString (EVInt n)    = show n
effectValueToString (EVString s) = s
effectValueToString (EVBool b)   = if b then "true" else "false"

-- | Modify an NPC's health, killing them if <= 0. The kill's state-change
--   event messages are threaded back to the caller.
modifyNPCHealth :: NPCID -> Int -> GameState -> (GameState, String)
modifyNPCHealth nIdRaw delta state =
    let nId = resolveActorNpcId nIdRaw state
    in case Map.lookup nId (npcStates (save state)) of
        Nothing -> (state, "")
        Just n ->
            let oldHealth = fromMaybe 0 (npcHealth n)
                newHealth = oldHealth + delta
                state' = updateNPCState nId (n { npcHealth = Just newHealth }) state
                state'' = clampNPCHealth nId state'
            in if newHealth <= 0 then killNPCWithMsg nId state'' else (state'', "")

-- | Move an entity (player or NPC) to a room
moveEntityToRoom :: EntityID -> RoomID -> GameState -> GameState
moveEntityToRoom "player" room state =
    fst (transitionToRoom room (clearActiveDialogue state))
moveEntityToRoom eId room state =
    moveNPCToRoom eId room state

-- | Public wrapper: apply a single outcome starting at depth 0 / salt 0
applyOutcome :: Effect -> ItemID -> GameState -> CommandResult
applyOutcome outcome targetId state =
    let (st, msg, _) = applyOutcomeWith 0 0 outcome targetId state
    in (st, msg)

-- | Join two message fragments, dropping the empty ones.
joinMessages :: String -> String -> String
joinMessages acc m
    | null m    = acc
    | null acc  = m
    | otherwise = acc ++ "\n" ++ m

-- | Apply zero or more outcomes in sequence
applyOutcomes :: [Effect] -> ItemID -> GameState -> CommandResult
applyOutcomes outcomes targetId state =
    let (st, msg, _) = foldl' (\(s, acc, slt) o ->
            let (s2, m2, slt2) = applyOutcomeWith 0 slt o targetId s
            in (s2, joinMessages acc m2, slt2))
            (state, "", 0) outcomes
    in (st, msg)

-- | Run a room hook (onEnter / onLook / onExit) if one is defined
runRoomHook :: (Room -> Maybe Effect) -> RoomID -> GameState -> (GameState, String)
runRoomHook hook rId state =
    case lookupRoom rId state >>= hook of
        Nothing -> (state, "")
        Just outcome -> applyOutcome outcome "" state

-- | Find direction of exit from one room to another
findExitDirection :: RoomID -> RoomID -> GameState -> Maybe Direction
findExitDirection fromId toId st =
    let conns = effectiveConnections st fromId
        matches = [ dir | (dir, exit) <- Map.toList conns, exitRoomID exit == toId ]
    in listToMaybe matches

-- | Move the player to another room, running exit/enter hooks and marking the
--   destination visited.
transitionToRoom :: RoomID -> GameState -> (GameState, String)
transitionToRoom rawDest state =
    let gw = world state
        dest = canonicalRoomId gw rawDest
        cur = currentRoom (save state)
        mDir = findExitDirection cur dest state
        stateWithRoom = ensureRoomExists dest cur (fromMaybe North mDir) state
        stAfterDialogue = clearActiveDialogue stateWithRoom
        (stAfterExit, exitMsg) = runRoomHook roomOnExit cur stAfterDialogue
        moved = moveToRoom dest stAfterExit
        visited = markCurrentRoomVisited moved
        (finalState, enterMsg) = runRoomHook roomOnEnter dest visited
        followed = followParty dest finalState
        withIntro = case pendingCutscene followed of
            Just _  -> followed
            Nothing -> case introCutsceneOf dest followed of
                Just cs -> followed { pendingCutscene = Just cs }
                Nothing -> followed
        fullMsg = intercalate "\n" (filter (not . null) [exitMsg, enterMsg])
    in (withIntro, fullMsg)

-- | The cutscene a room's `intro` resolves to (Phase H/H4)
introCutsceneOf :: RoomID -> GameState -> Maybe ([String], Int)
introCutsceneOf rId state = do
    r <- lookupRoom rId state
    clip <- Map.lookup (fromMaybe "" (roomIntro r)) (worldClips (world state))
    guard (not (null (clipFrames clip)))
    guard (clipFps clip > 0)
    pure (clipFrames clip, 1000000 `div` clipFps clip)

-- | Mark a quest completed and fire its reward.
completeQuestWithMsg :: QuestID -> GameState -> (GameState, String)
completeQuestWithMsg = completeQuestWith applyOutcome

-- | Vehicle-wide condition tick using applyOutcome.
vehicleConditionTick :: GameState -> (GameState, String)
vehicleConditionTick = vehicleConditionTickWith applyOutcome

-- ---------------------------------------------------------------------------
-- Trigger system (Phase 3f)
-- ---------------------------------------------------------------------------

-- | Mark an NPC dead, without producing trigger messages.
killNPC :: String -> GameState -> GameState
killNPC targetNpcId state = fst (killNPCWithMsg targetNpcId state)

-- | `killNPC`, keeping the messages of the fired `OnStateChange` trigger rules.
killNPCWithMsg :: String -> GameState -> (GameState, String)
killNPCWithMsg targetNpcId state =
    case npcStatus <$> Map.lookup targetNpcId (npcStates (save state)) of
        Just "dead" -> (state, "")
        _ ->
            let state' = state
                    { save = (save state)
                        { npcStates = Map.adjust (\s -> s { npcStatus = "dead" }) targetNpcId (npcStates (save state))
                        , entityStates = Map.insert targetNpcId "unlocked" (entityStates (save state))
                        } }
            in fireTriggers (OnStateChange targetNpcId) state'

-- | Set an entity's state and fire its `OnStateChange` event.
setEntityStateWithEvents :: String -> String -> GameState -> (GameState, String)
setEntityStateWithEvents eId val state
    | getEntityState eId state == Just val = (state, "")
    | otherwise = fireTriggers (OnStateChange eId) (setEntityState eId val state)

-- | Fire triggers matching the given event type (entry point, nesting depth 0).
fireTriggers :: EventType -> GameState -> (GameState, String)
fireTriggers = fireTriggersWithDepth 0

-- | Like `fireTriggers`, but carrying the current event nesting depth.
fireTriggersWithDepth :: Int -> EventType -> GameState -> (GameState, String)
fireTriggersWithDepth depth event state
    | depth > maxOutcomeDepth =
        ( addDiagnostic
            ("[engine] trigger nesting exceeded " ++ show maxOutcomeDepth
             ++ " while handling " ++ show event
             ++ ": a rule probably raises its own event")
            state
        , "" )
    | otherwise =
        let triggers = triggerDefs (world state)
            matching = filter (\t -> trEvent t == event) triggers
        in fireTriggerList depth matching state

-- | Fire a specific list of triggers.
fireTriggerList :: Int -> [TriggerDef] -> GameState -> (GameState, String)
fireTriggerList depth triggers state =
    foldl' fireOne (state, "") triggers
  where
    fireOne (st, acc) tr =
        let tId = trId tr
            tState = Map.lookup tId (triggerStates (save st))
            alreadyFired = maybe False tsFired tState
            cooldownRemaining = maybe 0 tsCooldownRemaining tState
        in if trOnce tr && alreadyFired
           then (st, acc)
           else if cooldownRemaining > 0
           then (decrementCooldown tId st, acc)
           else case trCondition tr of
                Just p  -> if evalPredicate p st
                           then applyTrigEffects tId tr st acc
                           else (st, acc)
                Nothing -> applyTrigEffects tId tr st acc

    decrementCooldown tId st =
        let newTs = Map.adjust (\s -> s { tsCooldownRemaining = max 0 (tsCooldownRemaining s - 1) }) tId (triggerStates (save st))
        in st { save = (save st) { triggerStates = newTs } }

    applyTrigEffects tId tr st acc =
        let (st', msgs) = foldl' (\(s, a) e ->
                let (s', m, _) = applyOutcomeWith (depth + 1) 0 e "" s
                in (s', a ++ m ++ "\n")) (st { save = (save st) { triggerStates = updatedTs } }, acc) (trEffects tr)
            updatedTs = Map.insert tId (TriggerState True (trCooldown tr)) (triggerStates (save st))
        in (st', msgs)

-- ---------------------------------------------------------------------------
-- Conditions tick
-- ---------------------------------------------------------------------------

-- | Advance one turn: tick all conditions, fire tick outcomes, remove expired
--   ones and fire their end outcomes.
tickConditions :: GameState -> (GameState, [String])
tickConditions state = foldl' step (state, []) (Map.toList (conditions (save state)))
  where
    step (st, msgs) (name, cond) =
        let remaining = condRemaining cond - 1
        in if remaining <= 0
           then let (stEnd, mEnd) = maybe (st, "") (\o -> applyOutcome o "" st) (condEndOutcome cond)
                    st' = clearCondition name stEnd
                in (st', if null mEnd then msgs else msgs ++ [mEnd])
           else let st1 = st { save = (save st) { conditions = Map.adjust (\c -> c { condRemaining = remaining }) name (conditions (save st)) } }
                    (st2, mTick) = maybe (st1, "") (\o -> applyOutcome o "" st1) (condTickOutcome cond)
                in (st2, if null mTick then msgs else msgs ++ [mTick])
