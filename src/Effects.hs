-- | Effect interpreter, triggers, room transitions, and condition ticks
module Effects
    ( maxOutcomeDepth
    , applyOutcomeWith
    , applyOutcome
    , applyOutcomeEv
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
    , setNpcStatusWithEvents
    , fireTriggers
    , fireTriggersWithDepth
    , fireTriggerList
    , checkChapterGate
    , currentChapterId
    , chapterVisited
    , tickConditions
    , completeQuestWithMsg
    , vehicleConditionTick
    , joinMessages
    ) where

import Types
import Game
import Messages (evMsg)
import Quests (canStartQuest, startQuest, advanceQuest, completeQuestWith)
import Vehicles (vehicleConditionTickWith)
import Data.List (intercalate, foldl', find, sort, stripPrefix)
import Data.Bits (shiftR, xor)
import Data.Char (ord)
import Data.Word (Word64)
import Numeric (readHex, showHex)
import Data.Maybe (listToMaybe, fromMaybe, isJust)
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
-- | W3 helpers: current chapter id (VVText in `chapter.current`) and the
--   visited marker `chapter.visited.<id>`.
currentChapterId :: GameState -> String
currentChapterId state = case getVariable "chapter.current" state of
    Just (VVText cid) -> cid
    Just (VVInt _)    -> ""   -- never: ids are text
    _                 -> ""

chapterVisited :: String -> GameState -> Bool
chapterVisited cid state =
    getVariable ("chapter.visited." ++ cid) state == Just (VVInt 1)

-- | Enter a chapter: set current, mark visited, show the intro, fire
--   OnChapter. Shared by the auto-gate, next_chapter and goto_chapter.
switchChapter :: ChapterDef -> GameState -> Int -> (GameState, [OutputEvent], Int)
switchChapter cd state salt =
    let st1 = setVariableChecked "chapter.current" (VVText (chId cd)) state
        st2 = setVariableChecked ("chapter.visited." ++ chId cd) (VVInt 1) st1
        intro = case chIntro cd of
            Just m  -> evRaw (formatWithVars m st2)
            Nothing -> []
        (st3, trig) = fireTriggersWithDepth 1 (OnChapter (chId cd)) st2
    in (st3, joinEv intro trig, salt)

-- | W3 auto-gate (W3.2): at most ONE switch per call (called once per turn,
--   after the turn-trigger fold). Candidate = the first chapter in
--   declaration order whose `when:` holds, that is unvisited and not current.
--   Declaration order is a Gameplay-Vertrag (nicht umsortieren).
checkChapterGate :: GameState -> (GameState, [OutputEvent])
checkChapterGate state = case candidate of
    Nothing -> (state, [])
    Just cd -> let (st', evs, _) = switchChapter cd state 0 in (st', evs)
  where
    chapters = chapterDefs (world state)
    cur = currentChapterId state
    eligible cd = isJust (chWhen cd)
                  && not (chapterVisited (chId cd) state)
                  && chId cd /= cur
                  && evalPredicate (fromMaybe PTrue (chWhen cd)) state
    candidate = case filter eligible chapters of
        (cd:_) -> Just cd
        []     -> Nothing

-- | W2: step through reached levels in ascending order, applying level-up effects,
--   messages and firing OnLevelUp events. Monotonically advancing, terminates
--   in at most |progLevels| steps.
stepLevels :: Int -> ProgressionDef -> GameState -> [OutputEvent] -> Int -> (GameState, [OutputEvent], Int)
stepLevels depth prog st acc salt =
    let curXp = getXp st
        curLvl = getLevel st
        mNextLvl = find (\l -> lvlNumber l == curLvl + 1 && lvlXp l <= curXp) (progLevels prog)
    in case mNextLvl of
        Nothing -> (st, acc, salt)
        Just nextLvl ->
            let lvlNum = lvlNumber nextLvl
                st1 = setVariableChecked "level.current" (VVInt lvlNum) st
                lvlEv = case lvlMsg nextLvl of
                    Just m  -> evRaw (formatWithVars m st1)
                    Nothing -> evMsg "levelup.default" [("level", show lvlNum), ("name", lvlName nextLvl)]
                (st2, trigMsgs) = fireTriggersWithDepth (depth + 1) (OnLevelUp lvlNum) st1
                runEff (s, a, slt) eff =
                    let (s', m', slt') = applyOutcomeWith (depth + 1) slt eff "" s
                    in (s', a ++ m', slt')
                (st3, effMsgs, salt') = foldl' runEff (st2, [], salt) (lvlEffects nextLvl)
                acc' = acc ++ lvlEv ++ trigMsgs ++ effMsgs
            in stepLevels depth prog st3 acc' salt'

-- | W1: message decision for a newly learned fact — no message (NPC or
--   silent fact), the catalog default, or an author template.
data LearnMsg = NoMsg | DefaultMsg | AuthorMsg (Maybe String)

-- ---------------------------------------------------------------------------
-- B8: named RNG streams
-- ---------------------------------------------------------------------------

-- | Weighted random pick (B8). The default stream (@""@) is the historical
--   single @rngState@ channel — its maths and draw order are literally
--   unchanged (byte contract). A named stream lives in the VarMap under
--   @rng.<name>@ as a hex text, is initialised from name-hash + the *current*
--   default state on first use (the default state is only read, never
--   advanced) and is fully decoupled from the default stream afterwards.
--
--   K3.2 (user decision 2026-10-03, "Weg 1"): candidates are filtered by
--   'drawEligible' *before* the weights are summed — a gated candidate whose
--   gate does not hold in the current state takes no draw slot. This is the
--   single place for 'RandomChoice' and 'RandomChoiceOn' alike. If every
--   candidate is filtered out, there is *no* draw at all: neither @rngState@
--   nor the named stream is advanced and the salt is returned untouched
--   (exactly like @RandomChoice []@), so a pick that could only ever yield
--   nothing does not shift any later draw. When no candidate is filtered the
--   list is unchanged, so maths and draw order are byte-identical to before.
applyRandomChoice :: String -> [(Int, Effect)] -> Int -> Int -> ItemID
                  -> GameState -> (GameState, [OutputEvent], Int)
applyRandomChoice streamName candidates depth salt targetId state
    | null weighted = (state, [], salt)
    | otherwise =
        let totalWeight = max 1 (sum (map fst weighted))
            (rng, st') = drawStreamRng streamName salt state
            pick = fromIntegral ((rng `shiftR` 33) `mod` fromIntegral totalWeight)
            go :: Int -> [(Int, Effect)] -> Effect
            go _ [(_, e)] = e
            go acc ((w, e):rest)
                | pick < acc + w = e
                | otherwise = go (acc + w) rest
            go _ [] = Noop
        in applyOutcomeWith (depth + 1) (salt + 1) (go 0 weighted) targetId st'
  where
    weighted = filter (drawEligible state . snd) candidates

-- | K3.2: may this candidate take a draw slot in the current state?
--
--   * A /gate/ — @Conditional p then Noop@, the shape of an encounter entry's
--     @when:@ ('compileEncounterTables') and of an @if:@ without @else:@ — is
--     eligible only if @p@ holds now. The predicate is judged by
--     'evalPredicate', the same evaluator 'applyOutcomeWith' uses for the
--     'Conditional' itself, so a condition is never interpreted twice. An
--     empty condition (@PTrue@, @all: []@) holds by 'evalPredicate' and
--     therefore always passes — no special case needed.
--   * Only the outer gate is judged. A nested gate (@Conditional p
--     (Conditional q t Noop) Noop@) passes as soon as @p@ holds; if @q@ fails,
--     the drawn candidate yields 'Noop' exactly as before.
--   * A two-way branch (@Conditional p t e@ with a real @else@) always does
--     something, so it keeps its slot: filtering it would silently make the
--     author's @else:@ unreachable.
--   * Every other effect — including a bare 'Noop' (an authored empty slot,
--     the "nothing happens" atmosphere pattern) — keeps its slot unchanged.
drawEligible :: GameState -> Effect -> Bool
drawEligible st (Conditional p _ Noop) = evalPredicate p st
drawEligible _  _                      = True

-- | K1: Roll a pool of @pool@ dice each with @die@ sides.
--   Draws from the given stream (or default if "") with ascending salt.
--   Keeps the highest @keep@ dice (all if keep >= pool).
--   Writes dice.last_roll, dice.count, dice.highest, dice.sum to the VarMap.
--   Produces no output events (silent effect, like compute_var).
applyRollDice :: Int -> Int -> String -> Int -> Int -> GameState
              -> (GameState, [OutputEvent], Int)
applyRollDice pool die streamName keep salt state
    | pool <= 0 =
        let st' = writeDiceVars [] state
        in (st', [], salt)
    | otherwise =
        let (rawRolls, finalSt, finalSalt) = drawDice pool salt state
            kept = if keep < pool
                   then take (max 0 keep) (reverse (sort rawRolls))
                   else rawRolls
            st' = writeDiceVars kept finalSt
        in (st', [], finalSalt)
  where
    drawDice 0 s st = ([], st, s)
    drawDice n s st =
        let (rng, stNext) = drawStreamRng streamName s st
            dieSides = max 1 die
            roll = 1 + fromIntegral ((rng `shiftR` 33) `mod` fromIntegral dieSides)
            (rest, stFinal, sFinal) = drawDice (n - 1) (s + 1) stNext
        in (roll : rest, stFinal, sFinal)

    writeDiceVars kept st =
        let lastRollStr = intercalate "," (map show kept)
            cnt = length kept
            highest = if null kept then 0 else maximum kept
            totalSum = sum kept
            st1 = setVariableChecked "dice.last_roll" (VVText lastRollStr) st
            st2 = setVariableChecked "dice.count" (VVInt cnt) st1
            st3 = setVariableChecked "dice.highest" (VVInt highest) st2
            st4 = setVariableChecked "dice.sum" (VVInt totalSum) st3
        in st4

-- | Draw one value from an RNG stream and persist its advanced state.
drawStreamRng :: String -> Int -> GameState -> (Word64, GameState)
drawStreamRng "" salt st =
    let rng = nextRng (rngState (save st) + fromIntegral salt)
    in (rng, st { save = (save st) { rngState = rng } })
drawStreamRng streamName salt st =
    let rng = nextRng (streamState streamName st + fromIntegral salt)
    in (rng, setVariable (streamVarKey streamName) (VVText (showHexWord64 rng)) st)

-- | VarMap home of a named stream (engine namespace @rng.*@ — writable only by
--   the engine; the worldbuilder rejects authored writes at compile time).
streamVarKey :: String -> String
streamVarKey name = "rng." ++ name

-- | Current state of a named stream. Read directly from the VarMap (never
--   through 'getVariable' — procedure scopes must not be able to shadow a
--   stream). Uninitialised (or unparsable) values are seeded deterministically
--   from name-hash + the current default state — the default stream itself is
--   not consumed by the initialisation.
streamState :: String -> GameState -> Word64
streamState name st =
    case Map.lookup (streamVarKey name) (variables (save st)) of
        Just (VVText t) -> maybe initSeed id (readHexWord64 t)
        _               -> initSeed
  where
    initSeed = nextRng (rngState (save st) + hashStreamName name)

-- | FNV-1a over the stream name — gives every name an independent seed.
hashStreamName :: String -> Word64
hashStreamName = foldl' step 0xcbf29ce484222325
  where
    step h c = (h `xor` fromIntegral (ord c)) * 0x100000001b3

showHexWord64 :: Word64 -> String
showHexWord64 w = showHex w ""

readHexWord64 :: String -> Maybe Word64
readHexWord64 s = case readHex s of
    [(w, "")] -> Just w
    _         -> Nothing

applyOutcomeWith :: Int -> Int -> Effect -> ItemID -> GameState -> (GameState, [OutputEvent], Int)
applyOutcomeWith depth salt outcome targetId state
    | depth > maxOutcomeDepth =
        ( addDiagnostic
            ("[engine] maximum outcome depth exceeded (depth " ++ show depth
             ++ " > " ++ show maxOutcomeDepth
             ++ "): an effect (or a self-triggering event) is nested too deeply")
            state
        , [], salt )
    | otherwise = case outcome of
    -- Authored YAML text: raw prose (no catalog key).
    SendMessage msg -> (state, evRaw (formatWithVars msg state), salt)

    Block mMsg consumesTurn ->
        let mEv = maybe [] (\m -> evRaw (formatWithVars m state)) mMsg
            state' = state { lastVeto = Just consumesTurn }
        in (state', mEv, salt)

    -- Phase 2.5 (D2): run a named procedure with literal args. Parameters and
    -- locals live in a fresh innermost scope (read via 'getVariable', written
    -- via 'setScopedVariable') that is popped when the call returns — nothing
    -- is persisted. The compiler statically rejects unknown names, arity
    -- mismatches and recursion; the guards below are defensive. Like
    -- 'Sequence', a veto inside the body stops the remaining body effects (and
    -- propagates to the caller through 'lastVeto').
    CallProc name args ->
        case Map.lookup name (procDefs (world state)) of
            Nothing ->
                ( addDiagnostic ("[engine] call to unknown procedure '" ++ name ++ "'") state
                , evMsg "proc.unknown" [], salt )
            Just pd
                | length args /= length (procParams pd) ->
                    ( addDiagnostic
                        ("[engine] procedure '" ++ name ++ "' called with "
                         ++ show (length args) ++ " arguments, expected "
                         ++ show (length (procParams pd))) state
                    , evMsg "proc.arity" [], salt )
                | otherwise ->
                    let scope = Map.fromList (zip (procParams pd) (map effectValToVarVal args))
                        st0 = state { procScopes = scope : procScopes state }
                        step (st, acc, s) o =
                            case lastVeto st of
                                Just _  -> (st, acc, s)
                                Nothing ->
                                    let (st2, m2, s2) = applyOutcomeWith (depth + 1) s o targetId st
                                    in (st2, joinEv acc m2, s2)
                        (st1, msgs, salt') = foldl' step (st0, [], salt) (procEffects pd)
                        stPop = st1 { procScopes = drop 1 (procScopes st1) }
                    in (stPop, msgs, salt')

    Sequence outcomes ->
        let step (st, acc, s) o =
                case lastVeto st of
                    Just _  -> (st, acc, s)
                    Nothing ->
                        let (st2, m2, s2) = applyOutcomeWith (depth + 1) s o targetId st
                        in (st2, joinEv acc m2, s2)
            (st', msg', salt') = foldl' step (state, [], salt) outcomes
        in (st', msg', salt')

    SetValue vr ev ->
        let (state', msg') = applySetValueWithDepth depth vr ev state
        in (state', msg', salt)

    ComputeValue vr expr ->
        let val = evalExpr expr state
            (state', msg') = applySetValueWithDepth depth vr (EVInt val) state
        in (state', msg', salt)

    ModifyValue VRPlayerHealth delta ->
        let state' = if delta >= 0
                     then updatePlayerHealth (+ delta) state
                     else let st' = updatePlayerHealth (+ delta) state
                          in if isPlayerDead st' then endGame Death st' else st'
        in (state', [], salt)
    ModifyValue vr delta ->
        let (state', m) = modifyValuePropWithDepth depth vr delta state
        in (state', m, salt)

    PlaceItem iid loc ->
        case placeItem iid loc state of
            Left err -> (state, evRaw err, salt)
            Right st' -> (st', [], salt)
    MoveEntity _ Dormant ->
        (state, evRaw "Dormant is only an initial item location.", salt)
    MoveEntity eid (InRoom room) ->
        let state' = moveEntityToRoom eid room state
        in (state', [], salt)
    MoveEntity eid (CarriedBy actor) ->
        -- B7: honour the actor. The `give:` string form targets the player
        -- ("give" => CarriedBy ActorPlayer), the object form
        -- (`give: {item: …, to: …}`) can hand items to NPCs.
        let state' = relocateItem eid (CarriedBy actor) state
        in (state', [], salt)
    MoveEntity eid Removed ->
        let actualEid = resolveVarName eid state
        in if isReachableForConsume actualEid state
           then let state' = consumeItem actualEid state
                in (state', [], salt)
           else (state, evMsg "consume.not_reachable" [], salt)
    MoveEntity eid (EquippedBy actor) ->
        case equipItemFor actor eid state of
            Left err    -> (state, evRaw err, salt)
            Right st'   -> (st', [], salt)
    MoveEntity _ (InContainer _) ->
        (state, evMsg "container.move_refused" [], salt)

    QuestOp StartQuest qId ->
        if canStartQuest qId state
        then (startQuest qId state, [], salt)
        else (state, evMsg "quest.cannot_start" [], salt)
    QuestOp AdvanceQuest qId ->
        if Map.member qId (activeQuests (save state))
        then (advanceQuest qId state, [], salt)
        else (state, evMsg "quest.not_active" [], salt)
    QuestOp CompleteQuest qId ->
        if Map.member qId (activeQuests (save state))
        then let (st', rewardMsg) = completeQuestWithMsg qId state
             in (st', rewardMsg, salt)
        else (state, evMsg "quest.not_active" [], salt)

    Conditional predicate thenOutcome elseOutcome ->
        if evalPredicate predicate state
        then applyOutcomeWith (depth + 1) salt thenOutcome targetId state
        else applyOutcomeWith (depth + 1) salt elseOutcome targetId state

    RandomChoice [] -> (state, [], salt)
    RandomChoice weighted -> applyRandomChoice "" weighted depth salt targetId state
    RandomChoiceOn _ [] -> (state, [], salt)
    RandomChoiceOn streamName weighted ->
        applyRandomChoice streamName weighted depth salt targetId state
    RollDice pool die streamName keep ->
        applyRollDice pool die streamName keep salt state

    GameEnd reason msg -> (endGame reason state, evRaw (formatWithVars msg state), salt)

    PlayClip clipId ->
        case Map.lookup clipId (worldClips (world state)) of
            Just clip | not (null (clipFrames clip))
                     , clipFps clip > 0 ->
                (state { pendingCutscene =
                            Just (clipFrames clip, 1000000 `div` clipFps clip) }
                , [], salt)
            _ -> (state, [], salt)

    PlaySfx path ->
        (state { pendingSfx = pendingSfx state ++ [path] }, [], salt)

    PlayMusic path ->
        (state { pendingMusic = Just (MusicStart path) }, [], salt)

    StopMusic ->
        (state { pendingMusic = Just MusicStop }, [], salt)

    ApplyCondition name turns tick end hidden -> (applyConditionWithHidden name turns tick end hidden state, [], salt)
    ClearCondition name -> (clearCondition name state, [], salt)

    RaiseEvent name ->
        let (st', m) = fireTriggersWithDepth (depth + 1) (OnCustomEvent name) state
        in (st', m, salt)

    ModifySkill skillId delta -> (modifySkill skillId delta state, [], salt)

    -- W1: knowledge is the closed VarMap namespace `known.<actor>.<fact>`.
    --   `Learn` is idempotent (Set semantics). Per newly learned fact (in
    --   learning order): first the message (only for the player, when not
    --   silent — the `combine:` cascade passes the deriving entry's message),
--   then the `OnLearn` trigger fires once. The `combine:` closure itself is a
    --   pure, bounded walk over the declared table (queue, every fact at most
    --   once) — a closed operation on the fact set, never trigger recursion.
    Learn actor fact ->
        let (stC, learned) = cascade state [] [(actorId actor, fact, AuthorMsg Nothing)]
            fire (st, acc, s) (_, f, mMsg) =
                let (st', trigMsgs) = fireTriggersWithDepth (depth + 1) (OnLearn f) st
                    entry = case mMsg of
                        AuthorMsg (Just m) -> evRaw (formatWithVars m st)
                        AuthorMsg Nothing  -> evMsg "learn.default" []
                        DefaultMsg         -> evMsg "learn.default" []
                        NoMsg              -> []
                in (st', joinEv acc (joinEv entry trigMsgs), s)
        in foldl' fire (stC, [], salt) learned
      where
        defs = combineDefs (world state)
        facts = factDefs (world state)
        -- The cascade walks the declared `combine:` table as a closed
        -- operation on the fact set (FIFO queue, every fact at most once) —
        -- never trigger recursion. Entries carry their message decision:
        -- 'Nothing' = no message (NPC or silent), 'Just Nothing' = catalog
        -- default, 'Just (Just m)' = author template (combine cdMsg or
        -- fact learn_msg).
        cascade st learned [] = (st, reverse learned)
        cascade st learned ((aId, f, mMsg):q)
            | getVariable ("known." ++ aId ++ "." ++ f) st == Just (VVInt 1)
            = cascade st learned q
            | otherwise =
                let st1 = setVariableChecked ("known." ++ aId ++ "." ++ f) (VVInt 1) st
                    -- K9: Entscheidung 'beim learn':
                    -- Die Aussage-Variablen (statement.<id>.truth, .speaker, .claims) werden
                    -- beim `learn` gesetzt, nicht zur Compile-Zeit:
                    -- 1. Statische Weltvariablen in varDefs sind über getVariable (und damit compare_var/VarIs)
                    --    nicht erreichbar (clampToVarDef prüft nur Schranken, getVariable liest SaveState.variables).
                    -- 2. Spielzustand: Aussagen existieren erst mit ihrer Äußerung im Wissen des Spielers.
                    --    Eine Vorbelegung würde Abfragen erlauben, bevor der Zeuge überhaupt vernommen wurde.
                    -- 3. Nach `forget` wird die Aussage auch aus den Variablen entfernt (sauberer Spielzustand).
                    st2 = case find (\s -> stDefId s == f) (statementDefs (world st1)) of
                        Just sDef ->
                            setVariableChecked ("statement." ++ f ++ ".truth")
                                (VVText (if stDefTruth sDef then "true" else "false"))
                            . setVariableChecked ("statement." ++ f ++ ".speaker")
                                (VVText (stDefSpeaker sDef))
                            . setVariableChecked ("statement." ++ f ++ ".claims")
                                (VVText (stDefClaims sDef))
                            $ st1
                        Nothing -> st1
                    derived =
                        [ (aId, cdYields c, AuthorMsg (cdMsg c))
                        | c <- defs, cdYields c `notElem` map snd3 ((aId, f, mMsg) : q)
                        , all (\p -> getVariable ("known." ++ aId ++ "." ++ p) st2
                                    == Just (VVInt 1)) (cdFacts c) ]
                    learned' = (aId, f, learnMsgFor aId f mMsg) : learned
                in cascade st2 learned' (q ++ derived)
        -- Only the player sees notes; `silent: true` suppresses everything;
        -- author messages (combine cdMsg or fact learn_msg) win over the
        -- catalog default.
        learnMsgFor :: String -> String -> LearnMsg -> LearnMsg
        learnMsgFor aId f mFromCombine
            | aId /= "player"              = NoMsg
            | factSilentFor f == Just True = NoMsg
            | otherwise = case mFromCombine of
                AuthorMsg (Just m) -> AuthorMsg (Just m)
                AuthorMsg Nothing  -> case find (\fd -> factId fd == f) facts of
                    Just fd | Just m <- factLearnMsg fd -> AuthorMsg (Just m)
                    _       -> case find (\sd -> stDefId sd == f) (statementDefs (world state)) of
                        Just _  -> NoMsg
                        Nothing -> DefaultMsg
                DefaultMsg         -> DefaultMsg
                NoMsg              -> NoMsg
        factSilentFor f = case find (\fd -> factId fd == f) facts of
            Just fd -> factSilent fd
            Nothing -> Nothing
        snd3 (_, f, _) = f

    -- K11d: recipe knowledge is the closed VarMap namespace `known_recipe.<id>`
    --   (player-global, no actor layer — recipe knowledge is player knowledge;
    --   an actor variant for NPC teachers would be K11e). `LearnRecipe` is
    --   idempotent (Set semantics): on the FIRST learning the message
    --   `recipes.learn.default` ({recipe} = result item name, or the recipe id
    --   when the recipe has no `result:`), then the `OnLearnRecipe` trigger
    --   fires exactly once, in learning order. Author messages belong in front
    --   of the effect (`msg:` before `learn_recipe:`) — there is no per-recipe
    --   learn_msg in this stage.
    LearnRecipe rId ->
        if getVariable ("known_recipe." ++ rId) state == Just (VVInt 1)
        then (state, [], salt)
        else
            let st1 = setVariableChecked ("known_recipe." ++ rId) (VVInt 1) state
                name = case find (\k -> recipeId k == Just rId)
                                 (Map.keys (itemInteractions (world st1))) of
                    Just k  -> recipeDisplayName k st1
                    Nothing -> rId
                (st2, trigMsgs) = fireTriggersWithDepth (depth + 1) (OnLearnRecipe rId) st1
            in (st2, joinEv (evMsg "recipes.learn.default" [("recipe", name)]) trigMsgs, salt)

    -- W1 (Befehl `notizen`, generierter Trigger bei `journal: notes`): render
    --   the notes book — learned, non-silent player facts in **declaration
    --   order** (ungrouped first, then tags in first-occurrence order). NPC
    --   knowledge is never shown (that is the detective work: questioning).
    ShowNotes ->
        let visible = [ fd | fd <- factDefs (world state)
                        , getVariable ("known.player." ++ factId fd) state == Just (VVInt 1)
                        , factSilent fd /= Just True ]
            header = evMsg "notes.header" []
            lineFor fd =
                let src = case factSource fd of
                        Just s' -> " (" ++ s' ++ ")"
                        Nothing -> ""
                in "- " ++ factText fd ++ src
            tagBlock t = ("[" ++ t ++ "]") : map lineFor [ fd | fd <- visible, factTag fd == Just t ]
            untagged = map lineFor [ fd | fd <- visible, Nothing <- [factTag fd] ]
            tagOrder = nub' [ t | fd <- visible, Just t <- [factTag fd] ]
            nub' = foldr (\x acc -> x : filter (/= x) acc) []
            grouped = untagged ++ concatMap tagBlock tagOrder
            body = if null visible
                then evMsg "notes.empty" []
                else joinEv header (evRaw (unlines grouped))
        in (state, body, salt)

    -- W3: chapters. All three paths route through 'switchChapter' (current +
    -- visited + intro + OnChapter). `goto_chapter` refuses backward jumps (a
    -- visited chapter is never entered again - no retrospection, W3 contract).
    NextChapter ->
        let chapters = chapterDefs (world state)
            cur = currentChapterId state
            rest = drop 1 (dropWhile (\cd -> chId cd /= cur) chapters)
        in case rest of
            [] -> ( addDiagnostic "[engine] next_chapter: no chapter follows the current one" state
                  , evMsg "chapter.no_next" [], salt )
            (cd:_) -> switchChapter cd state salt

    GotoChapter target ->
        case find (\cd -> chId cd == target) (chapterDefs (world state)) of
            Nothing ->
                ( addDiagnostic ("[engine] goto_chapter: unknown chapter '" ++ target ++ "'") state
                , evMsg "chapter.unknown" [], salt )
            Just cd
                | chapterVisited target state ->
                    ( state, evMsg "chapter.refuse_back" [], salt )
                | otherwise -> switchChapter cd state salt

    Mount iId actor -> (mountItem iId actor state, [], salt)
    Unmount iId     -> (unmountItem iId state, [], salt)

    StepToward seeker target opts mMsg   -> pursuitMove True seeker target opts mMsg state salt
    StepAwayFrom seeker target opts mMsg -> pursuitMove False seeker target opts mMsg state salt

    -- B3: closed mass operations over a count set (never a user-supplied
    -- effect list as loop body — that is the G1 line).
    DamageAll cs amount ->
        let (stDmg, evs) = foldl'
                (\(st, acc) n ->
                    let (stN, m) = modifyValueProp (VRActorProp (ActorNPC n) PHealth) (-amount) st
                    in (stN, joinEv acc m))
                (state, []) (countNpcMembers cs state)
        in (stDmg, evs, salt)
    MoveAll cs dest ->
        let loc = whereLocation dest
            st1 = foldl' (\st i -> setItemLoc (itemId i) loc st) state (countItemMembers cs state)
            st2 = foldl' (\st n -> setNpcLoc n loc st) st1 (countNpcMembers cs state)
        in (st2, [], salt)
    RevealAll cs ->
        (foldl' (\st i -> setItemDiscovered (itemId i) st) state
            (countItemMembers cs state), [], salt)
    ConsumeAll cs ->
        (foldl' (\st i -> setItemLoc (itemId i) Removed st) state
            (countItemMembers cs state), [], salt)
    SetStateAll cs newStatus ->
        let st1 = foldl' (\st i -> setItemStatus (itemId i) newStatus st) state
                    (countItemMembers cs state)
            st2 = foldl' (\st n -> setNpcStatus n newStatus st) st1 (countNpcMembers cs state)
        in (st2, [], salt)

    GainXp delta ->
        let curXp = getXp state
            rawXp = curXp + delta
            newXp = max 0 rawXp
            st1 = setVariableChecked "xp.current" (VVInt newXp) state
            clampMsgs = if rawXp < 0 then evMsg "xp.clamped" [] else []
            (st2, lvlMsgs, salt') = case progressionDef (world st1) of
                Nothing   -> (st1, [], salt)
                Just prog -> stepLevels depth prog st1 [] salt
        in (st2, joinEv clampMsgs lvlMsgs, salt')

    Forget actor fact ->
        let key = "known." ++ actorId actor ++ "." ++ fact
        in if getVariable key state == Just (VVInt 1)
           then
               let st1 = state { save = (save state) { variables = Map.delete key (variables (save state)) } }
                   st2 = case find (\s -> stDefId s == fact) (statementDefs (world st1)) of
                       Just _ ->
                           let vars = variables (save st1)
                               vars' = Map.delete ("statement." ++ fact ++ ".truth")
                                     . Map.delete ("statement." ++ fact ++ ".speaker")
                                     . Map.delete ("statement." ++ fact ++ ".claims")
                                     $ vars
                           in st1 { save = (save st1) { variables = vars' } }
                       Nothing -> st1
               in (st2, [], salt)
           else (state, [], salt)

    SetExit from dir exit ->
        let ss = save state
            withEntity = case exit of
                Locked _ e | Map.notMember e (entityStates ss) ->
                    ss { entityStates = Map.insert e "locked" (entityStates ss) }
                _ -> ss
            in (state { save = withEntity
                { exitOverrides = Map.insert (from, dir) (Just exit) (exitOverrides withEntity) } }
            , [], salt)
    RemoveExit from dir ->
        (state { save = (save state)
            { exitOverrides = Map.insert (from, dir) Nothing (exitOverrides (save state)) } }
        , [], salt)

    Narrative nls followUp ->
        let formatted = map (`formatWithVars` state) nls
        in (state { pendingNarrative = Just (formatted, followUp) }, evRaw (intercalate "\n" formatted), salt)

    DrawCards n -> (drawCards n state, [], salt)
    DiscardHand -> (discardHand state, [], salt)
    DiscardCard cid -> (discardCard cid state, [], salt)
    ExhaustCard cid -> (exhaustCard cid state, [], salt)
    AddCardToDeck cid dest -> (addCardToDeck cid dest state, [], salt)
    ShuffleDeck -> (shuffleDeck state, [], salt)

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
                , roomDarkMsg = Nothing
                , roomOnEnter = Nothing
                , roomOnLook = Nothing
                , roomOnExit = Nothing
                , roomSearchOutcome = Nothing
                , roomAscii = emptyAscii
                , roomIntro = Nothing
                , roomFloor = newFloor
                , roomMapPos = Nothing
                }
            dyn' = Map.insert newId newRoom (dynamicRooms ss)
            overrides' = Map.insert (fromId, toDir) (Just (Open newId)) (exitOverrides ss)
            ss' = ss { dynamicRooms = dyn', exitOverrides = overrides' }
        in (state { save = ss' }, [], salt)

    Noop -> (state, [], salt)

-- | Apply SetValue: set a value reference to a new value (handles flags,
--   variables, entity states). Returns the state plus any message produced by
--   the fired side effects (an entity-state change fires its `OnStateChange`).
-- | Set a name in the innermost procedure scope that binds it (Phase 2.5
--   locals); names not bound in any scope go to the adventure VarMap as usual.
--   Locals die with the call: the scope is popped in the 'CallProc' case.
setScopedVariable :: String -> VariableValue -> GameState -> GameState
setScopedVariable name val state = go [] (procScopes state)
  where
    go _ [] = setVariableChecked name val state
    go before (scope:rest)
        | Map.member name scope =
            state { procScopes = reverse before ++ Map.insert name val scope : rest }
        | otherwise = go (scope : before) rest

applySetValue :: ValueRef -> EffectValue -> GameState -> (GameState, [OutputEvent])
applySetValue = applySetValueWithDepth 0

-- | Phase G9c: Check if a faction standing level changed and fire OnStandingChange.
checkStandingTrigger :: Int -> Maybe FactionID -> String -> GameState -> [OutputEvent] -> (GameState, [OutputEvent])
checkStandingTrigger depth mFaction oldLevel st msgs =
    case mFaction of
        Just fid ->
            let newLevel = lookupStandingName fid st
            in if oldLevel /= newLevel
               then let (st', trigMsgs) = fireTriggersWithDepth (depth + 1) (OnStandingChange fid) st
                    in (st', msgs ++ trigMsgs)
               else (st, msgs)
        Nothing -> (st, msgs)

applySetValueWithDepth :: Int -> ValueRef -> EffectValue -> GameState -> (GameState, [OutputEvent])
applySetValueWithDepth _ (VRFlag name) val state =
    (setFlag name (effectValueToString val) state, [])
applySetValueWithDepth depth (VRVariable name) val state =
    let realName = resolveVarName name state
        mVd = Map.lookup realName (varDefs (world state))
        mMax = case vdVarType <$> mVd of
            Just (VTInt _ (Just hi)) -> Just hi
            _                        -> Nothing
        rawInt = case val of
            EVInt n    -> Just n
            EVString s -> case reads s of [(n, "")] -> Just n; _ -> Nothing
            _          -> Nothing
        mFaction = case stripPrefix "faction." realName of
            Just fid | Map.member fid (factions (world state)) -> Just fid
            _                                                  -> Nothing
        oldLevel = maybe "" (`lookupStandingName` state) mFaction
        state' = setScopedVariable realName (effectValToVarVal val) state
        -- K7+K4: on_overflow fires ONLY if the attempted write was strictly greater
        -- than max and clamped to max afterwards. Setting to exactly max does not trigger overflow.
        didOverflow = case (mMax, rawInt) of
            (Just hi, Just n) -> n > hi && getVariable realName state' == Just (VVInt hi)
            _                 -> False
        overflowEffs = maybe [] vdOnOverflow mVd
        (stAfterOf, ofMsgs) =
            if didOverflow && not (null overflowEffs)
            then let (stFin, ms, _) = applyOutcomeWith (depth + 1) 0 (Sequence overflowEffs) "" state'
                 in (stFin, ms)
            else (state', [])
    in checkStandingTrigger depth mFaction oldLevel stAfterOf ofMsgs
applySetValueWithDepth _ (VRActorProp (ActorEntity eIdRaw) PState) val state =
    let eId = resolveVarName eIdRaw state
    in if Map.member eId (npcStates (save state))
       then setNpcStatusWithEvents eId (effectValueToString val) state
       else if isKnownEntity eId state
            then setEntityStateWithEvents eId (effectValueToString val) state
            else ( addDiagnostic ("[engine] set_state: unknown entity '" ++ eId ++ "'") state
                 , evMsg "target.not_seen" [("target", eId)] )
applySetValueWithDepth _ (VRActorProp (ActorNPC nidRaw) PState) val state =
    let nid = resolveVarName nidRaw state
    in setNpcStatusWithEvents nid (effectValueToString val) state
applySetValueWithDepth _ (VRActorProp ActorPlayer PRoom) val state =
    (fst (transitionToRoom (effectValueToString val) (clearActiveDialogue state)), [])
applySetValueWithDepth _ (VRActorProp (ActorRoom rId) PVisited) val state =
    let b = case val of { EVInt n -> n /= 0; EVBool v -> v; _ -> False }
    in (setRoomVisited rId b state, [])
applySetValueWithDepth _ (VRActorProp ActorPlayer PHealth) val state =
    let n = case val of { EVInt m -> m; _ -> 0 }
    in if n <= 0 then (endGame Death (setPlayerHP n state), []) else (setPlayerHP n state, [])
applySetValueWithDepth _ (VRActorProp (ActorShip vId) PHealth) val state =
    let n = case val of { EVInt m -> m; _ -> 0 }
    in (setVariableChecked ("ship." ++ vId ++ ".hull") (VVInt n) state, [])
applySetValueWithDepth _ VRPlayerHealth val state =
    let n = case val of { EVInt m -> m; _ -> 0 }
    in if n <= 0 then (endGame Death (setPlayerHP n state), []) else (setPlayerHP n state, [])
applySetValueWithDepth _ _ _ state = (state, [])

-- | Apply ModifyValue to a non-player-health reference.
modifyValueProp :: ValueRef -> Int -> GameState -> (GameState, [OutputEvent])
modifyValueProp = modifyValuePropWithDepth 0

modifyValuePropWithDepth :: Int -> ValueRef -> Int -> GameState -> (GameState, [OutputEvent])
modifyValuePropWithDepth _ (VRFlag name) delta state =
    let cur = case getFlag name state of
            Just "true" -> 1
            _           -> 0
        newVal = if cur + delta > 0 then "true" else "false"
    in (setFlag name newVal state, [])
modifyValuePropWithDepth depth (VRVariable name) delta state =
    let realName = resolveVarName name state
        cur = case getVariable realName state of
            Just (VVInt n)  -> n
            Just (VVText s) -> case reads s of [(n,_)] -> n; _ -> 0
            _               -> 0
        rawInt = cur + delta
        mVd = Map.lookup realName (varDefs (world state))
        mMax = case vdVarType <$> mVd of
            Just (VTInt _ (Just hi)) -> Just hi
            _                        -> Nothing
        mFaction = case stripPrefix "faction." realName of
            Just fid | Map.member fid (factions (world state)) -> Just fid
            _                                                  -> Nothing
        oldLevel = maybe "" (`lookupStandingName` state) mFaction
        state' = setScopedVariable realName (VVInt rawInt) state
        -- K7+K4: on_overflow fires ONLY if the attempted write was strictly greater
        -- than max and clamped to max afterwards. Setting to exactly max does not trigger overflow.
        didOverflow = case mMax of
            Just hi -> rawInt > hi && getVariable realName state' == Just (VVInt hi)
            Nothing -> False
        overflowEffs = maybe [] vdOnOverflow mVd
        (stAfterOf, ofMsgs) =
            if didOverflow && not (null overflowEffs)
            then let (stFin, ms, _) = applyOutcomeWith (depth + 1) 0 (Sequence overflowEffs) "" state'
                 in (stFin, ms)
            else (state', [])
    in checkStandingTrigger depth mFaction oldLevel stAfterOf ofMsgs
modifyValuePropWithDepth _ (VRItemProp iId prop) delta state =
    (modifyItemProp iId prop delta state, [])
modifyValuePropWithDepth _ (VRActorProp (ActorNPC eId) PHealth) delta state
    | resolveActorNpcId eId state `elem` ["all", "all_enemies"] =
        let curRoom = currentRoom (save state)
            enemies = [ npcId def
                      | def <- getNPCsInRoom curRoom state
                      , not (isDeadNPC (npcId def) state)
                      , not (isInParty (npcId def) state)
                      ]
            step (st, msgs) eid =
                let (st', m) = modifyNPCHealth eid delta st
                in (st', joinEv msgs m)
            (stFin, msgsFin) = foldl' step (state, []) enemies
        in (stFin, msgsFin)
    | otherwise =
        modifyNPCHealth (resolveActorNpcId eId state) delta state
modifyValuePropWithDepth _ (VRActorProp (ActorNPC nId) (PCustom prop)) delta state =
    (modifyNPCProp (resolveActorNpcId nId state) prop delta state, [])
modifyValuePropWithDepth _ (VRActorProp ActorPlayer PHealth) delta state =
    let cur = playerHealth (player (save state))
        newHP = cur + delta
    in if newHP <= 0
       then (endGame Death (setPlayerHP newHP state), [])
       else (setPlayerHP newHP state, [])
modifyValuePropWithDepth _ (VRActorProp (ActorShip vId) PHealth) delta state =
    let cur = case getVariable ("ship." ++ vId ++ ".hull") state of
            Just (VVInt n) -> n
            _              -> 0
    in (setVariableChecked ("ship." ++ vId ++ ".hull") (VVInt (cur + delta)) state, [])
modifyValuePropWithDepth _ _ _ state = (state, [])

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
modifyNPCHealth :: NPCID -> Int -> GameState -> (GameState, [OutputEvent])
modifyNPCHealth nIdRaw delta state =
    let nId = resolveActorNpcId nIdRaw state
    in case Map.lookup nId (npcStates (save state)) of
        Nothing -> (state, [])
        Just n ->
            let oldHealth = fromMaybe 0 (npcHealth n)
                newHealth = oldHealth + delta
                state' = updateNPCState nId (n { npcHealth = Just newHealth }) state
                state'' = clampNPCHealth nId state'
            in if newHealth <= 0 then killNPCWithMsg nId state'' else (state'', [])

-- | Move an entity (player or NPC) to a room
moveEntityToRoom :: EntityID -> RoomID -> GameState -> GameState
moveEntityToRoom "player" room state =
    fst (transitionToRoom room (clearActiveDialogue state))
moveEntityToRoom eId room state =
    moveNPCToRoom eId room state

-- | Event-native form: apply a single outcome starting at depth 0 / salt 0
--   (Phase 1.2 primary path).
applyOutcomeEv :: Effect -> ItemID -> GameState -> (GameState, [OutputEvent])
applyOutcomeEv outcome targetId state =
    let (st, msg, _) = applyOutcomeWith 0 0 outcome targetId state
    in (st, msg)

-- | Compatibility form (tests, aux loop paths): the same outcome, rendered
--   back to the flat CLI text — byte-identical to the pre-1.2 behaviour.
applyOutcome :: Effect -> ItemID -> GameState -> CommandResult
applyOutcome outcome targetId state =
    let (st, evs) = applyOutcomeEv outcome targetId state
    in (st, renderEvents evs)

-- | Join two message fragments, dropping the empty ones (string idiom, kept
--   for the compatibility paths).
joinMessages :: String -> String -> String
joinMessages acc m
    | null m    = acc
    | null acc  = m
    | otherwise = acc ++ "\n" ++ m

-- | Apply zero or more outcomes in sequence
applyOutcomes :: [Effect] -> ItemID -> GameState -> (GameState, [OutputEvent])
applyOutcomes outcomes targetId state =
    let (st, msg, _) = foldl' (\(s, acc, slt) o ->
            let (s2, m2, slt2) = applyOutcomeWith 0 slt o targetId s
            in (s2, joinEv acc m2, slt2))
            (state, [], 0) outcomes
    in (st, msg)

-- | Run a room hook (onEnter / onLook / onExit) if one is defined
runRoomHook :: (Room -> Maybe Effect) -> RoomID -> GameState -> (GameState, [OutputEvent])
runRoomHook hook rId state =
    case lookupRoom rId state >>= hook of
        Nothing -> (state, [])
        Just outcome -> let (st', evs, _) = applyOutcomeWith 0 0 outcome "" state in (st', evs)

-- | Find direction of exit from one room to another
findExitDirection :: RoomID -> RoomID -> GameState -> Maybe Direction
findExitDirection fromId toId st =
    let conns = effectiveConnections st fromId
        matches = [ dir | (dir, exit) <- Map.toList conns, exitRoomID exit == toId ]
    in listToMaybe matches

-- | Move the player to another room, running exit/enter hooks and marking the
--   destination visited.
transitionToRoom :: RoomID -> GameState -> (GameState, [OutputEvent])
transitionToRoom rawDest state =
    let gw = world state
        dest = canonicalRoomId gw rawDest
        cur = currentRoom (save state)
        mDir = findExitDirection cur dest state
        stateWithRoom = ensureRoomExists dest cur (fromMaybe North mDir) state
        stAfterDialogue = clearActiveDialogue stateWithRoom
        (stAfterExit, exitMsg) = runRoomHook roomOnExit cur stAfterDialogue
        moved = setCombatStarted False (moveToRoom dest stAfterExit)
        visited = markCurrentRoomVisited moved
        (finalState, enterMsg) = runRoomHook roomOnEnter dest visited
        followed = followParty dest finalState
        withIntro = case pendingCutscene followed of
            Just _  -> followed
            Nothing -> case introCutsceneOf dest followed of
                Just cs -> followed { pendingCutscene = Just cs }
                Nothing -> followed
        fullMsg = joinAllEv [exitMsg, enterMsg]
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
completeQuestWithMsg :: QuestID -> GameState -> (GameState, [OutputEvent])
completeQuestWithMsg = completeQuestWith applyOutcomeEv

-- | Vehicle-wide condition tick using applyOutcomeEv.
vehicleConditionTick :: GameState -> (GameState, [OutputEvent])
vehicleConditionTick = vehicleConditionTickWith applyOutcomeEv

-- ---------------------------------------------------------------------------
-- Trigger system (Phase 3f)
-- ---------------------------------------------------------------------------

-- | Mark an NPC dead, without producing trigger messages.
killNPC :: String -> GameState -> GameState
killNPC targetNpcId state = fst (killNPCWithMsg targetNpcId state)

-- | B9: what a corpse lets go of. Only with the author flag `drops_on_death:
--   true` — the default keeps everything on the body, which is the byte-frozen
--   contract of every existing adventure. Carried *and* worn items drop into
--   the room the NPC is standing in (an NPC without a room keeps its load).
dropOnDeath :: String -> GameState -> (GameState, [OutputEvent])
dropOnDeath targetNpcId state
    | not (dropsOnDeath targetNpcId state) = (state, [])
    | otherwise = case (npcLocation <$> Map.lookup targetNpcId (npcStates (save state))) of
        Just (InRoom room) ->
            let dropped = [ iId | iId <- droppedItemIds targetNpcId state ]
                state'  = foldl' (\s iId -> relocateItem iId (InRoom room) s) state dropped
            in if null dropped
               then (state, [])
               else ( state'
                    , evMsg "npc.drops_items"
                        ([ ("npc", npcNameOf targetNpcId state)
                         , ("items", intercalate ", " [ nm | iId <- dropped, let nm = itemNameOf iId state ]) ]) )
        _ -> (state, [])

-- | B9: does this NPC let go of its load when it dies? The author flag
--   `drops_on_death:` on the NPC (default False = the corpse keeps everything).
dropsOnDeath :: NPCID -> GameState -> Bool
dropsOnDeath nId state = maybe False npcDropsOnDeath (Map.lookup nId (npcDefs (world state)))

-- | Items on an NPC's person: what it carries in its hands and what it wears.
droppedItemIds :: NPCID -> GameState -> [ItemID]
droppedItemIds nId state =
    [ iId | (iId, is) <- Map.toList (itemStates (save state))
         , itemLocation is `elem` [CarriedBy (ActorNPC nId), EquippedBy (ActorNPC nId)] ]

npcNameOf :: NPCID -> GameState -> String
npcNameOf nId state = maybe nId npcName (Map.lookup nId (npcDefs (world state)))

itemNameOf :: ItemID -> GameState -> String
itemNameOf iId state = maybe iId itemName (Map.lookup iId (itemDefs (world state)))

-- | `killNPC`, keeping the messages of the fired `OnStateChange` trigger rules.
killNPCWithMsg :: String -> GameState -> (GameState, [OutputEvent])
killNPCWithMsg targetNpcId state =
    case npcStatus <$> Map.lookup targetNpcId (npcStates (save state)) of
        Just "dead" -> (state, [])
        _ ->
            let state' = state
                    { save = (save state)
                        { npcStates = Map.adjust (\s -> s { npcStatus = "dead" }) targetNpcId (npcStates (save state))
                        , entityStates = Map.insert targetNpcId "unlocked" (entityStates (save state))
                        } }
                (stateDropped, dropMsgs) = dropOnDeath targetNpcId state'
                (stateDead, triggerMsgs) = fireTriggers (OnStateChange targetNpcId) stateDropped
            in (stateDead, dropMsgs ++ triggerMsgs)

-- | Set an entity's state and fire its `OnStateChange` event.
setEntityStateWithEvents :: String -> String -> GameState -> (GameState, [OutputEvent])
setEntityStateWithEvents eId val state
    | evalPredicate (EntityHasState eId val) state = (setEntityState eId val state, [])
    | otherwise = fireTriggers (OnStateChange eId) (setEntityState eId val state)

-- | Set an NPC's status and fire its `OnStateChange` event (K2).
--
-- Begründung zur Fehlerbehandlung bei unbekanntem NPC:
-- Der Compiler fängt statische Tippfehler bereits hart via UnknownStateTarget (ciError).
-- Zur Laufzeit erzeugen wir dennoch eine defensive Engine-Diagnose (addDiagnostic),
-- falls ein NPC weder statisch noch dynamisch (z.B. aufgelöst via resolveActorNpcId)
-- in 'npcStates' existiert, statt still ins Leere zu schreiben oder abzustürzen.
setNpcStatusWithEvents :: NPCID -> String -> GameState -> (GameState, [OutputEvent])
setNpcStatusWithEvents nIdRaw val state =
    let nId = resolveActorNpcId nIdRaw state
    in case Map.lookup nId (npcStates (save state)) of
        Nothing ->
            ( addDiagnostic ("[engine] set_state: unknown NPC '" ++ nId ++ "'") state
            , [] )
        Just ns
            | npcStatus ns == val -> (state, [])
            | otherwise ->
                let state' = setNpcStatus nId val state
                in fireTriggers (OnStateChange nId) state'

-- | Pursuit (Tür IV): move the seeker exactly one edge toward (or away
--   from) the target. Only NPCs and ships are seekers (decision 2026-09-30);
--   any other actor is refused with a diagnostic. One edge per call, never
--   a whole path — repeated calls keep stepping. The optional author message
--   replaces the catalog line (W1 message pattern).
pursuitMove :: Bool -> ActorRef -> DistanceTarget -> PursuitOptions -> Maybe String
            -> GameState -> Int -> (GameState, [OutputEvent], Int)
pursuitMove toward seeker target opts mMsg state salt =
    case seeker of
        ActorNPC _  -> walk
        ActorShip _ -> walk
        _ -> ( addDiagnostic "[engine] pursuit: only NPCs and ships can be seekers" state
             , [], salt )
  where
    walk = case pursuitStep toward opts state seeker target of
        Just (dir, room) ->
            let st' = setSeekerRoom seeker room state
                evs = case mMsg of
                    Just m  -> evRaw (formatWithVars m st')
                    Nothing | toward -> evMsg "pursuit.step"
                                        [ ("name", actorId seeker), ("room", room)
                                        , ("dir", show dir) ]
                            | otherwise -> evMsg "pursuit.flee"
                                        [ ("name", actorId seeker), ("room", room)
                                        , ("dir", show dir) ]
            in (st', evs, salt)
        Nothing ->
            (state, evMsg "pursuit.no_path" [("name", actorId seeker)], salt)

-- | Set an actor's room position (NPC or ship).
setSeekerRoom :: ActorRef -> RoomID -> GameState -> GameState
setSeekerRoom (ActorNPC n) room state =
    state { save = (save state)
        { npcStates = Map.adjust (\ns -> ns { npcLocation = InRoom room }) n
                        (npcStates (save state)) } }
setSeekerRoom (ActorShip v) room state =
    state { save = (save state)
        { vehicleStates = Map.adjust (\vs -> vs { vsCurrentStop = room }) v
                        (vehicleStates (save state)) } }
setSeekerRoom _ _ state = state

-- | B3: the Location a count where maps to. `in: nowhere` maps to the dormant
--   holding pen (`location: nowhere`, OPEN-04), so `to: {in: nowhere}` takes an
--   item out of the world while keeping it placeable later (unlike `consume:`,
--   whose `Removed` tombstone is permanent).
whereLocation :: CountWhere -> Location
whereLocation (CountInRoom "nowhere") = Dormant
whereLocation (CountInRoom r)    = InRoom r
whereLocation (CountCarriedBy a) = CarriedBy a

-- | B3: set an item's location directly.
setItemLoc :: ItemID -> Location -> GameState -> GameState
setItemLoc i loc state =
    state { save = (save state)
        { itemStates = Map.adjust (\is -> is { itemLocation = loc }) i
                        (itemStates (save state)) } }

-- | B3: reveal a hidden item.
setItemDiscovered :: ItemID -> GameState -> GameState
setItemDiscovered i state =
    state { save = (save state)
        { itemStates = Map.adjust (\is -> is { itemDiscovered = True }) i
                        (itemStates (save state)) } }

-- | B3: set an item's status.
setItemStatus :: ItemID -> String -> GameState -> GameState
setItemStatus = setEntityState

-- | B3: set an NPC's location.
setNpcLoc :: NPCID -> Location -> GameState -> GameState
setNpcLoc n loc state =
    state { save = (save state)
        { npcStates = Map.adjust (\ns -> ns { npcLocation = loc }) n
                        (npcStates (save state)) } }

-- | B3: set an NPC's status.
setNpcStatus :: NPCID -> String -> GameState -> GameState
setNpcStatus n newStatus state =
    state { save = (save state)
        { npcStates = Map.adjust (\ns -> ns { npcStatus = newStatus }) n
                        (npcStates (save state)) } }

-- | Fire triggers matching the given event type (entry point, nesting depth 0).
fireTriggers :: EventType -> GameState -> (GameState, [OutputEvent])
fireTriggers = fireTriggersWithDepth 0

-- | Like `fireTriggers`, but carrying the current event nesting depth.
fireTriggersWithDepth :: Int -> EventType -> GameState -> (GameState, [OutputEvent])
fireTriggersWithDepth depth event state
    | depth > maxOutcomeDepth =
        ( addDiagnostic
            ("[engine] trigger nesting exceeded " ++ show maxOutcomeDepth
             ++ " while handling " ++ show event
             ++ ": a rule probably raises its own event")
            state
        , [] )
    | otherwise =
        let triggers = triggerDefs (world state)
            matching = filter (\t -> matchesEvent (trEvent t) event) triggers
        in fireTriggerList depth matching state
  where
    -- OnTalk supports wildcards: empty npc/topic strings match everything.
    matchesEvent (OnTalk patNpc patTopic) (OnTalk evNpc evTopic) =
        (null patNpc || patNpc == evNpc) && (null patTopic || patTopic == evTopic)
    matchesEvent a b = a == b

-- | Fire a specific list of triggers.
fireTriggerList :: Int -> [TriggerDef] -> GameState -> (GameState, [OutputEvent])
fireTriggerList depth triggers state =
    foldl' fireOne (state, []) triggers
  where
    fireOne (st, acc) tr =
        case lastVeto st of
            Just _  -> (st, acc)
            Nothing ->
                let tId = trId tr
                    tState = Map.lookup tId (triggerStates (save st))
                    alreadyFired = maybe False tsFired tState
                    cooldownRemaining = maybe 0 tsCooldownRemaining tState
                in if trOnce tr && alreadyFired
                   then (st, acc)
                   else if cooldownRemaining > 0
                   then (decrementCooldown tId st, acc)
                   else if not (null (trRequires tr)) && not (all (`hasFlag` st) (trRequires tr))
                   then (st, acc)
                   else case trCondition tr of
                        Just p  -> if evalPredicate p st
                                   then applyTrigEffects tId tr st acc
                                   else (st, acc)
                        Nothing -> applyTrigEffects tId tr st acc

    decrementCooldown tId st =
        let newTs = Map.adjust (\s -> s { tsCooldownRemaining = max 0 (tsCooldownRemaining s - 1) }) tId (triggerStates (save st))
        in st { save = (save st) { triggerStates = newTs } }

    -- Byte-identical to the pre-1.2 string fold: every effect appended its
    -- message plus one "\n" — even when the message was empty.
    applyTrigEffects tId tr st acc =
        let step (s, a) e =
                case lastVeto s of
                    Just _  -> (s, a)
                    Nothing ->
                        let (s', m, _) = applyOutcomeWith (depth + 1) 0 e "" s
                            effNl = case e of
                                Block Nothing _ -> []
                                _               -> nl
                        in (s', a ++ m ++ effNl)
            chainStep (s, a) target =
                case lastVeto s of
                    Just _  -> (s, a)
                    Nothing ->
                        let (s', m) = fireTriggersWithDepth (depth + 1) (OnCustomEvent target) s
                        in (s', a ++ m)
            (st', msgs) = foldl' step (st { save = (save st) { triggerStates = updatedTs } }, acc) (trEffects tr)
            (stFinal, msgsFinal) = foldl' chainStep (st', msgs) (trChainsTo tr)
            updatedTs = Map.insert tId (TriggerState True (trCooldown tr)) (triggerStates (save st))
        in (stFinal, msgsFinal)

-- ---------------------------------------------------------------------------
-- Conditions tick
-- ---------------------------------------------------------------------------

-- | Advance one turn: tick all conditions, fire tick outcomes, remove expired
--   ones and fire their end outcomes.
tickConditions :: GameState -> (GameState, [[OutputEvent]])
tickConditions state = foldl' step (state, []) (Map.toList (conditions (save state)))
  where
    step (st, msgs) (name, cond) =
        let remaining = condRemaining cond - 1
        in if remaining <= 0
           then let (stEnd, mEnd) = maybe (st, []) (\o -> applyOutcomeEv o "" st) (condEndOutcome cond)
                    st' = clearCondition name stEnd
                in (st', if null (renderEvents mEnd) then msgs else msgs ++ [mEnd])
           else let st1 = st { save = (save st) { conditions = Map.adjust (\c -> c { condRemaining = remaining }) name (conditions (save st)) } }
                    (st2, mTick) = maybe (st1, []) (\o -> applyOutcomeEv o "" st1) (condTickOutcome cond)
                in (st2, if null (renderEvents mTick) then msgs else msgs ++ [mTick])
