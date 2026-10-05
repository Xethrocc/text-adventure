{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Compile an Adventure (authoring schema) into engine types, or return structured issues
module Worldbuilder.Compile
    ( CompileResult(..)
    , compileAdventure
    , CompileIssue(..)
    , Severity(..)
    , ciError
    , ciWarning
    , compileAActionOutcome
    , compileAActionOutcomeWith
    , compileOutcomesWith
    , compileNpcAI
    , npcAiStateEvent
    , checkStateTargetRefs
    , authorOwnedVarPrefixes
    , resolveWorldEffects
    , allWorldEffects
    , allAOutcomes
    , deepOutcomes
    , outcomeSurfaces
    , checkUnreachableTriggers
    , checkUnsatisfiableConditions
    , checkDeadExits
    , checkUnreachableRooms
    , checkQuestProgress
    , unreachableRoomIds
    , predicateTruth
    , EntityType(..)
    , knownKeys
    , checkUnknownYamlKeys
    , levenshtein
    , formatUnknownKey
    , checkKeywordCollisions
    , checkUnknownPlaceholders
    , checkReservedVarWrites
    , checkRngVarWrites
    , checkRollDice
    , checkDarkRoomDeadEnds
    , checkDeviceRefs
    ) where

import Worldbuilder.Types
import Worldbuilder.MapLayout (mapFloorOf)
import Worldbuilder.QuestCheck (QuestDiagnostic (..), questDiagnostics)
import Types hiding
    ( itemDefs, itemStates, npcDefs, npcStates, questDefs
    , vehicleDefs, vehicleStates, entityInteractions, itemInteractions
    , varDefs, triggerDefs, rooms, stText )
import qualified Types as E
import qualified Messages as Msg
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Char (toLower, isDigit, isSpace)
import Data.List (nub, stripPrefix, isPrefixOf, isInfixOf, minimumBy, intercalate, sortOn, find)
import Data.Ord (comparing)
import Data.Maybe (mapMaybe, fromMaybe, catMaybes, isNothing, isJust)
import Data.Either (partitionEithers)
import Text.Read (readMaybe)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.Foldable as Foldable
import qualified Data.Text as T
import qualified Verbs

-- | Result of compilation
data CompileResult = CompileResult
    { crWorld    :: E.GameWorld
    , crSave     :: E.SaveState
    , crWarnings :: [CompileIssue]  -- ^ non-fatal diagnostics (Rogue Phase 1: IronmanWithoutSavezones)
    } deriving (Show, Eq)

-- | Severity of a compiler diagnostic
data Severity = SError | SWarning
    deriving (Show, Eq)

-- | Structured compiler diagnostic: points at the exact YAML field.
data CompileIssue = CompileIssue
    { ciPath    :: String     -- ^ e.g. "rooms.loc_17.exits.noth"
    , ciSeverity :: Severity
    , ciCode    :: String     -- ^ stable machine-readable code, e.g. "UnknownDirection"
    , ciMessage :: String     -- ^ human-readable detail
    } deriving (Show, Eq)

-- | Build an error diagnostic
ciError :: String -> String -> String -> CompileIssue
ciError path code msg = CompileIssue path SError code msg

-- | Build a warning diagnostic
ciWarning :: String -> String -> String -> CompileIssue
ciWarning path code msg = CompileIssue path SWarning code msg

-- | 4.3 (D4): validate the `language:` / `messages:` language-pack fields.
--   An unknown language code is a hard error ('UnknownLanguage' — the engine
--   would silently fall back to English); override values must be non-empty
--   ('EmptyMessageOverride' — an empty template would silently change the
--   fragment-algebra behaviour, which drops empty fragments); overrides for
--   keys the engine catalog does not know only warn ('UnknownMsgKey') since
--   they silently do nothing. Returns (errors, warnings).
checkLanguageFields :: Adventure -> ([CompileIssue], [CompileIssue])
checkLanguageFields adv = (langErrs ++ emptyErrs, unknownKeyWarns)
  where
    langErrs =
        [ ciError "language" "UnknownLanguage"
            ("'" ++ l ++ "' is not a known language (expected one of: "
                ++ intercalate ", " Msg.knownLanguages ++ ")")
        | Just l <- [advLanguage adv], l `notElem` Msg.knownLanguages ]
    emptyErrs =
        [ ciError ("messages." ++ k) "EmptyMessageOverride"
            "message override values must not be empty (an empty template would change engine behaviour)"
        | (k, v) <- Map.toList (advMessages adv), null v ]
    unknownKeyWarns =
        [ ciWarning ("messages." ++ k) "UnknownMsgKey"
            ("'" ++ k ++ "' is not an engine message key (this override does nothing)")
        | k <- Map.keys (advMessages adv), k `Map.notMember` Msg.defaultCatalog ]

-- | 4.3.5 (Variante A): grammar metadata checks. Items and NPCs should carry
--   `article:`/`gender:` whenever the effective catalog templates for their
--   message keys reference the grammar placeholders — otherwise those render
--   as empty strings at the output edge ('MissingGrammar'). Conservative: the
--   warning only fires in language-pack worlds (`language:` set) and only when
--   the templates actually demand the args — an article-less `messages:`
--   override silences it. `gender:` is a closed tag set (hard error).
checkGrammarFields :: Adventure -> ([CompileIssue], [CompileIssue])
checkGrammarFields adv = (genderErrs, missingWarns)
  where
    langWorld = isJust (advLanguage adv)
    catalog = Msg.effectiveCatalog (advLanguage adv) (advMessages adv)
    wants keys = any (\k -> maybe False (not . null . Msg.templateGrammarKeys)
                               (Map.lookup k catalog)) keys
    itemWants = wants Msg.itemGrammarMsgKeys
    npcWants  = wants Msg.npcGrammarMsgKeys
    validGenders = ["m", "f", "n"]
    genderErrs =
        [ ciError ("items." ++ aiId i ++ ".gender") "InvalidGender"
            ("'" ++ g ++ "' is not a valid gender tag (expected one of: m, f, n)")
        | i <- advItems adv, Just g <- [E.gGender (aiGrammar i)], g `notElem` validGenders ]
        ++
        [ ciError ("npcs." ++ anId n ++ ".gender") "InvalidGender"
            ("'" ++ g ++ "' is not a valid gender tag (expected one of: m, f, n)")
        | n <- advNPCs adv, Just g <- [E.gGender (anGrammar n)], g `notElem` validGenders ]
    missingWarns =
        [ ciWarning ("items." ++ aiId i ++ ".article") "MissingGrammar"
            "item has no article:/gender: but the language templates use grammar placeholders — they render empty"
        | langWorld, itemWants, i <- advItems adv, E.grammarEmpty (aiGrammar i) ]
        ++
        [ ciWarning ("npcs." ++ anId n ++ ".article") "MissingGrammar"
            "NPC has no article:/gender: but the language templates use grammar placeholders — they render empty"
        | langWorld, npcWants, n <- advNPCs adv, E.grammarEmpty (anGrammar n) ]

-- | Rogue Phase 1: build an engine GamePolicy from the authored `game:` block.
--   Absent block (or absent fields) keeps 'E.defaultGamePolicy' — the
--   Default-Invariante. Validation: savezone rooms must exist (MissingRoom);
--   `ironman` without savezones is legal (never-save hardcore) but warned
--   about ('IronmanWithoutSavezones') because it is usually an oversight.
compileGamePolicy :: Map.Map String E.Room -> Maybe AGamePolicy
                  -> ([CompileIssue], [CompileIssue], E.GamePolicy)
compileGamePolicy _ Nothing = ([], [], E.defaultGamePolicy)
compileGamePolicy rooms (Just ap) =
    let ironman = fromMaybe False (agpIronman ap)
        policy = E.GamePolicy
            { E.gpPermadeath = fromMaybe False (agpPermadeath ap)
            , E.gpAllowUndo  = fromMaybe True (agpAllowUndo ap)
            , E.gpIronman    = ironman
            , E.gpSaveZones  = agpSaveZones ap
            , E.gpMetaSlug   = agpMetaSlug ap
            }
        zoneErrs =
            [ ciError "game.save_zones" "MissingRoom"
                ("save zone references unknown room '" ++ z ++ "'")
            | z <- agpSaveZones ap, not (Map.member z rooms) ]
        ironWarnings =
            [ CompileIssue "game.ironman" SWarning "IronmanWithoutSavezones"
                "ironman is set but save_zones is empty: the game can never be saved"
            | fromMaybe False (agpIronman ap), null (agpSaveZones ap) ]
    in (zoneErrs, ironWarnings, policy)

-- | Rogue Phase 3: validate `set_exit` / `remove_exit` effects — the
--   direction string must parse (UnknownDirection) and `from`/`to` rooms must
--   exist (MissingRoom, same code as the engine's L4 check). Traversed via
--   'allWorldEffects' so rules, rooms, items and NPCs are all covered.
-- | Rogue Phase 3: validate `set_exit` / `remove_exit` over the RAW outcome
--   trees (so the authored direction strings are checkable, unlike after the
--   compile step where an unknown direction maps to `East`). Traversed via
--   'allAOutcomes' — the same surface as 'allWorldEffects', but pre-compile.
--   `from`/`to` rooms must exist (MissingRoom, same code as the engine's L4).
checkSetExitRefs :: Set.Set String -> Adventure -> [CompileIssue]
checkSetExitRefs roomKeys adv =
    concatMap go (allAOutcomes adv)
  where
    sandboxZoneKeys = Set.fromList (map aszId (advSandboxZones adv))
    isSandboxZoneTarget r =
        case E.parseSandboxRoomId r of
            Just (zid, _, _, _) -> zid `Set.member` sandboxZoneKeys
            Nothing             -> r `Set.member` sandboxZoneKeys

    go (AOSetExit from dir to _mLock) =
        setExitIssues from dir (Just to)
            ++ [ ciError "outcomes.set_exit.to" "MissingRoom"
                    ("room '" ++ to ++ "' does not exist")
               | to `Set.notMember` roomKeys, not (isSandboxZoneTarget to) ]
    go (AORemoveExit from dir) = setExitIssues from dir Nothing
    go (AOGenerateRoom _ _ _ from toDir retDir) =
        [ ciError "outcomes.generate_room.direction" "UnknownDirection"
            ("generate_room direction '" ++ toDir ++ "' is unknown")
        | either (const True) (const False) (parseDir toDir) ]
        ++
        [ ciError "outcomes.generate_room.return_direction" "UnknownDirection"
            ("generate_room return_direction '" ++ retDir ++ "' is unknown")
        | not (null retDir) && either (const True) (const False) (parseDir retDir) ]
        ++
        [ ciError "outcomes.generate_room.connect_from" "MissingRoom"
            ("room '" ++ from ++ "' does not exist")
        | not (null from), from `notElem` ["current", "current_room"], from `Set.notMember` roomKeys, not (isSandboxZoneTarget from) ]
    go (AOConditional _ ts es) = go' ts ++ go' es
    go (AONarrative _ follow)  = go' follow
    go (AORandomChoice _ cs)   = concatMap go' (map snd cs)
    go (AOApplyCondition _ _ t e _) = go' (t ++ e)
    go _                         = []
    go' = concatMap go
    setExitIssues from dir mTo =
        [ ciError ("outcomes." ++ tag) "UnknownDirection"
            ("set_exit/remove_exit direction '" ++ dir ++ "' is unknown")
        | either (const True) (const False) (parseDir dir) ]
        ++ [ ciError ("outcomes." ++ tag) "MissingRoom"
                ("room '" ++ r ++ "' does not exist")
           | r <- nub (from : toRooms), r `Set.notMember` roomKeys, not (isSandboxZoneTarget r) ]
      where
        tag = case mTo of
            Just _  -> "set_exit"
            Nothing -> "remove_exit"
        toRooms = maybe [] (:[]) mTo
-- Alle autorenbaren Outcome-Container (Spiegel von 'allWorldEffects',
-- aber auf dem Rohtext-Level — wichtig fuer die Richtungs-Pruefung).
-- | Every authored outcome in the adventure — **recursively** (nested `if:`,
--   `narrative: then:`, condition tick/end and `random:` branches included)
--   and over **every** outcome-bearing surface. Contract: a new outcome field
--   must be added here. This list backs the reference checks
--   ('checkSetExitRefs', 'checkNpcPossessionRefs') and the B6 asset collection
--   ('Worldbuilder.Export.collectAssetRefs') — a missed surface silently skips
--   all three.
-- | Flatten nested outcome branches (`if:`, `narrative: then:`, condition
--   tick/end, `random:` alternatives) so every leaf outcome is listed once,
--   parents before children.
deepOutcomes :: [AActionOutcome] -> [AActionOutcome]
deepOutcomes = concatMap deepOne
  where
    deepOne o = o : concatMap deepOutcomes (nested o)
    nested o = case o of
        AOConditional _ ts es        -> [ts, es]
        AONarrative _ ts             -> [ts]
        AOApplyCondition _ _ ts es _ -> [ts, es]
        AORandomChoice _ branches    -> map snd branches
        _                            -> []

-- | Every outcome-bearing surface with a diagnostic path prefix. Contract:
--   a new outcome field must be added here — this list backs the reference
--   checks ('checkSetExitRefs', 'checkNpcPossessionRefs'), the B6 asset
--   collection ('Worldbuilder.Export.collectAssetRefs') and the B4
--   diagnostics; a missed surface silently skips all of them.
outcomeSurfaces :: Adventure -> [(String, [AActionOutcome])]
outcomeSurfaces a =
    [ ("rooms." ++ arId r, roomOutcomes r) | r <- advRooms a ]
    ++ [ ("rules." ++ atId t, atEffects t) | t <- advTriggers a ]
    ++ [ ("items." ++ aiId i, itemOutcomes i) | i <- advItems a ]
    ++ [ ("npcs." ++ anId n, npcOutcomes n) | n <- advNPCs a ]
    ++ [ ("interactions", interactions) ]
    ++ [ ("quests." ++ aqId q, maybe [] id (aqReward q)) | q <- advQuests a ]
    ++ [ ("vehicles." ++ avId v, vehicleOutcomes v) | v <- advVehicles a ]
    ++ [ ("encounter_tables." ++ ertId t, encounterOutcomes t) | t <- advEncounterTables a ]
    ++ [ ("environment", envOutcomes (advEnvironment a)) ]
    ++ [ ("stealth", stealthOutcomes (advStealth a)) ]
    ++ [ ("patrol", patrolOutcomes (advPatrol a)) ]
    ++ [ ("combat", combatOutcomes (advCombat a)) ]
    ++ [ ("abilities." ++ aabId ab, aabEffects ab) | ab <- advAbilities a ]
    ++ [ ("cards." ++ acdId cd, acdOutcomes cd) | cd <- advCards a ]
    ++ [ ("procedures." ++ apId pr, apEffects pr) | pr <- advProcedures a ]
    ++ [ ("devices." ++ adId dv, deviceOutcomes dv) | dv <- advDevices a ]
    ++ [ ("progression", maybe [] (concatMap alEffects . aplLevels) (advProgression a)) ]
  where
    roomOutcomes r = concat (catMaybes [ arOnEnter r, arOnLook r, arOnExit r, arSearch r ])
    itemOutcomes i = maybe [] id (aiOnTake i) ++ concat (Map.elems (aiVerbMap i))
    dialogOutcomes tr =
        concatMap (concatMap adcOutcomes . adnChoices) (Map.elems (adtNodes tr))
    npcOutcomes n =
        concat (Map.elems (anVerbMap n))
        ++ Map.elems (anTopics n)
        ++ maybe [] pure (anOnTalk n)
        ++ concatMap dialogOutcomes (Map.elems (anDialogue n))
        ++ maybe [] (concatMap (asdEffects . snd) . aiStates) (anAI n)
    vehicleOutcomes v =
        concat (Map.elems (avConditions v))
        ++ concatMap astEffects (avStations v)
    encounterOutcomes tbl = concatMap eneEffects (ertEntries tbl)
    envOutcomes mEnv = case mEnv of
        Nothing -> []
        Just env -> concatMap wtEffects (maybe [] weaTransitions (envWeather env))
                    ++ concatMap drAtZero (envDrains env)
    stealthOutcomes mSt = maybe [] (concatMap obOnHear . stObservers) mSt
    patrolOutcomes mPt = maybe [] (concatMap ahAttack . ptHostiles) mPt
    combatOutcomes mC = maybe [] (\c -> acOnWin c ++ acOnLose c) mC
    interactions = case advInteractions a of
        Just ai -> concatMap aiiEffects (aiItem ai) ++ concatMap aniEffects (aiNpc ai)
        Nothing -> []
    deviceOutcomes d = adOnInsert d ++ adOnRemove d ++ concatMap snd (adOnFlip d)

allAOutcomes :: Adventure -> [AActionOutcome]
allAOutcomes a = concatMap (deepOutcomes . snd) (outcomeSurfaces a)

-- ---------------------------------------------------------------------------
-- B4: Regel-Diagnostik (dead content — nicht-fataler Warnkanal)
-- ---------------------------------------------------------------------------

-- | B4: three-valued truth for condition analysis. 'TruthFalse' carries the
--   reason quoted in the diagnostic. Everything not statically decidable is
--   'TruthUnknown' — the analysis never guesses, so a warning always means
--   the content is definitely dead.
data Truth = TruthTrue | TruthFalse String | TruthUnknown

-- | B4: every flag that can ever become "true" — `initial_flags:` plus every
--   `set_flag` effect anywhere (nested branches included).
setFlagNames :: Adventure -> Set.Set String
setFlagNames a =
    Set.fromList (Map.keys (advInitialFlags a) ++ [f | AOSetFlag f _ <- allAOutcomes a])

-- | B4: every condition with its diagnostic path — `if:` conditions nested in
--   outcomes (per surface path) plus the named gates (rule `when:`, dialogue
--   `visible_when:`, bark/encounter/weather/drain/station/chapter gates).
--   Exit guards are deliberately absent — 'checkDeadExits' reports them.
predicateSites :: Adventure -> [(String, E.Predicate)]
predicateSites a =
    [ (path, p)
    | (path, outs) <- outcomeSurfaces a
    , o <- deepOutcomes outs
    , AOConditional p _ _ <- [o] ]
    ++
    concat
        [ [ ("rules." ++ atId t, p) | t <- advTriggers a, Just p <- [atWhen t] ]
        , [ ("rules." ++ atId t ++ ".requires", E.HasFlag f) | t <- advTriggers a, f <- atRequires t ]
        , [ ("npcs." ++ anId n ++ ".barks." ++ show k, p)
          | n <- advNPCs a, (k, b) <- zip [1 :: Int ..] (anBarks n), Just p <- [abWhen b] ]
        , [ ("npcs." ++ anId n ++ ".dialogue", p)
          | n <- advNPCs a
          , tr <- Map.elems (anDialogue n)
          , node <- Map.elems (adtNodes tr)
          , ch <- adnChoices node
          , Just p <- [adcVisible ch] ]
        , [ ("vehicles." ++ avId v ++ ".stations", p)
          | v <- advVehicles a, st <- avStations v, Just p <- [astWhen st] ]
        , [ ("encounter_tables." ++ ertId t, p) | t <- advEncounterTables a, Just p <- [ertWhen t] ]
        , [ ("encounter_tables." ++ ertId t ++ ".entries", p)
          | t <- advEncounterTables a, e <- ertEntries t, Just p <- [eneWhen e] ]
        , [ ("environment.weather", p)
          | Just env <- [advEnvironment a]
          , wt <- maybe [] weaTransitions (envWeather env), Just p <- [wtWhen wt] ]
        , [ ("environment.drains", p)
          | Just env <- [advEnvironment a]
          , d <- envDrains env, Just p <- [drWhen d] ]
        , [ ("chapters." ++ achId c, p) | c <- advChapters a, Just p <- [achWhen c] ]
        ]

-- | B4: evaluate a condition to 'TruthTrue' / 'TruthFalse reason' /
--   'TruthUnknown'. Flags that are never set are statically false; `all:`
--   and `any:` propagate; direct contradictions inside `all:` (a condition
--   and its negation, disjoint numeric bounds on one variable, two different
--   text values for one variable) are caught syntactically.
-- | Can this predicate hold at all? @Just False@ means provably never,
--   @Nothing@ means we cannot tell (a runtime value we do not model).
predicateTruth :: Adventure -> E.Predicate -> Maybe Bool
predicateTruth a p = case evalTruth (setFlagNames a) p of
    TruthTrue    -> Just True
    TruthFalse _ -> Just False
    TruthUnknown -> Nothing

evalTruth :: Set.Set String -> E.Predicate -> Truth
evalTruth flagSet = go
  where
    go p = case p of
        E.PTrue  -> TruthTrue
        E.PNot q -> case go q of
            TruthTrue    -> TruthFalse "it negates a condition that always holds"
            TruthFalse _ -> TruthTrue
            TruthUnknown -> TruthUnknown
        E.PAll qs ->
            let flat = flattenAll qs
                rs = map go flat
            in case [ f | f@(TruthFalse _) <- rs ] of
                (f : _) -> f
                [] -> case contradictionIn flat of
                    Just reason -> TruthFalse reason
                    Nothing | all isTrue rs -> TruthTrue
                            | otherwise     -> TruthUnknown
        E.PAny qs ->
            let rs = map go qs
            in if any isTrue rs then TruthTrue
               else if all isFalse rs then TruthFalse "every alternative is impossible"
               else TruthUnknown
        E.HasFlag f | not (Set.member f flagSet) ->
            TruthFalse ("flag '" ++ f ++ "' is never set")
        _ -> TruthUnknown
    isTrue TruthTrue = True
    isTrue _         = False
    isFalse (TruthFalse _) = True
    isFalse _              = False
    flattenAll = concatMap (\q -> case q of E.PAll inner -> flattenAll inner; _ -> [q])
    contradictionIn ps =
        case [ () | q <- ps, q `elem` [ neg | E.PNot neg <- ps ] ] of
            (_ : _) -> Just "both a condition and its negation must hold"
            [] -> case [ v | E.CompareVar v _ _ <- ps, boundsClash (boundsFor v ps) ] of
                (v : _) -> Just ("numeric bounds on '" ++ v ++ "' cannot hold at once")
                [] -> case [ v | E.VarIs v s1 <- ps, E.VarIs v' s2 <- ps
                               , v == v', s1 /= s2 ] of
                    (v : _) -> Just ("text variable '" ++ v ++ "' must be two different values at once")
                    []      -> Nothing
    boundsFor v ps = [ (op, n) | E.CompareVar v' op n <- ps, v' == v ]
    boundsClash bounds =
        let lowers = [ n + 1 | (E.CGt, n) <- bounds ] ++ [ n | (E.CGte, n) <- bounds ]
                     ++ [ n | (E.CEq, n) <- bounds ]
            uppers = [ n - 1 | (E.CLt, n) <- bounds ] ++ [ n | (E.CLte, n) <- bounds ]
                     ++ [ n | (E.CEq, n) <- bounds ]
            neqs   = [ n | (E.CNeq, n) <- bounds ]
        in case (lowers, uppers) of
            ((_ : _), (_ : _)) ->
                let lo = maximum lowers
                    hi = minimum uppers
                in lo > hi || (lo == hi && lo `elem` neqs)
            _ -> False

-- | B4: rules whose event can never fire — unknown room/item references in
--   `on:`, `on: custom <name>` without any `raise: <name>`, `on: chapter <id>`
--   without that chapter, `on: levelup` without a `progression:` section.
--   (`on: command/before <verb>` is already a hard error elsewhere.)
checkUnreachableTriggers :: Adventure -> [CompileIssue]
checkUnreachableTriggers a =
    [ ciWarning ("rules." ++ atId t) "UnreachableTrigger"
        ("rule listens on '" ++ atOn t ++ "', which can never fire: " ++ reason)
    | t <- advTriggers a, Just reason <- [unreachableReason t] ]
  where
    outs = allAOutcomes a
    rooms = Set.fromList (map arId (advRooms a) ++ [r | AOGenerateRoom r _ _ _ _ _ <- outs])
    items = Set.fromList (map aiId (advItems a))
    chapters = Set.fromList (map achId (advChapters a))
    raised = Set.fromList ([n | AORaiseEvent n <- outs] ++ [map toLower target | t <- advTriggers a, target <- atChainsTo t])
    roomReason r
        | Set.member r rooms = Nothing
        | otherwise          = Just ("no room '" ++ r ++ "' is declared")
    itemReason i
        | Set.member i items = Nothing
        | otherwise          = Just ("no item '" ++ i ++ "' is declared")
    unreachableReason t = case compileAtOn (atOn t) of
        Left _              -> Nothing  -- BadTriggerEvent already fails the compile
        Right (E.OnEnter r)  -> roomReason r
        Right (E.OnLeave r)  -> roomReason r
        Right (E.OnLook r)   -> roomReason r
        Right (E.OnSearch r) -> roomReason r
        Right (E.OnTake i)   -> itemReason i
        Right (E.OnDrop i)   -> itemReason i
        Right (E.OnUse i)    -> itemReason i
        Right (E.OnCustomEvent n)
            | not (Set.member n raised) ->
                Just ("no effect ever raises '" ++ n ++ "'")
        Right (E.OnChapter c)
            | not (Set.member c chapters) ->
                Just ("no chapter '" ++ c ++ "' is declared")
        Right (E.OnLevelUp _) | isNothing (advProgression a) ->
            Just "the adventure declares no 'progression:' section, so nobody ever levels up"
        Right _ -> Nothing

-- | B4: conditions that can never hold — rule `when:`, dialogue gates, bark,
--   encounter, weather, drain, station and chapter gates, and `if:`
--   conditions nested in outcomes. Reasons: contradictions (see 'evalTruth')
--   or flags that no effect ever sets.
checkUnsatisfiableConditions :: Adventure -> [CompileIssue]
checkUnsatisfiableConditions a =
    [ ciWarning path "UnsatisfiableCondition" ("condition can never hold: " ++ reason)
    | (path, p) <- predicateSites a, TruthFalse reason <- [evalTruth flagSet p] ]
  where
    flagSet = setFlagNames a

-- | B4: exits that can never be taken — guarded by a condition that can never
--   hold, or locked by an entity nothing can ever unlock. Unlock paths
--   modelled: NPCs unlock by dying, containers by the `lock`/`unlock` verbs,
--   interactions (`use` with `state: unlocked`) and `set_state` effects.
checkDeadExits :: Adventure -> [CompileIssue]
checkDeadExits a = concatMap deadFor (advRooms a)
  where
    flagSet = setFlagNames a
    outs = allAOutcomes a
    unlockable = Set.fromList $
        map anId (advNPCs a)
        ++ map acnId (advContainers a)
        ++ [ aiId i | i <- advItems a, isJust (aiCapacity i) ]
        ++ [ e | AOSetEntityState e "unlocked" <- outs ]
        ++ [ aeiTarget x | Just blk <- [advInteractions a], x <- aiEntity blk
                         , aeiState x == "unlocked" ]
    deadFor r =
        [ ciWarning ("rooms." ++ arId r ++ ".exits." ++ dir) "DeadExit" msg
        | (dir, ex) <- Map.toList (arExits r)
        , msg <- exitReason ex ]
    exitReason ex = case aeWhen ex of
        Just g | TruthFalse reason <- evalTruth flagSet g ->
            ["its guard can never hold: " ++ reason]
        _ -> case aeLocked ex of
            Just e | not (Set.member e unlockable) ->
                ["it is locked by entity '" ++ e ++ "', which can never be unlocked"]
            _ -> []

-- | B4: dead quest content (4.6, S3) — a quest nothing starts, or one that can
--   be started but never advances. See "Worldbuilder.QuestCheck" for why the
--   fixpoint and the missing reachability modelling are deliberate.
checkQuestProgress :: Adventure -> [CompileIssue]
checkQuestProgress a =
    [ ciWarning (qdPath d) (qdCode d) (qdReason d)
    | d <- questDiagnostics (allAOutcomes a) a ]

-- | B4: rooms the player can never reach from `start_room`. Edges are the
--   declared exits plus dynamic ones (`set_exit`, `generate_room`); explicit
--   arrivals (`move:`, vehicle stops) count as reachable. Every exit of an
--   unreachable room is never passable — reported once per room.
checkUnreachableRooms :: Adventure -> [CompileIssue]
checkUnreachableRooms a =
    [ ciWarning ("rooms." ++ r) "UnreachableRoom"
        ("room '" ++ r ++ "' is not reachable from start_room '" ++ advStartRoom a
         ++ "' — its exits can never be taken")
    | r <- unreachableRoomIds a ]

-- | Rooms the player can never reach, in **declaration order**.
--
--   Extracted from 'checkUnreachableRooms' so the project view can show reach
--   without a second copy of the rules (dynamic exits, generated rooms,
--   explicit arrivals) — a second copy would be a second truth.
unreachableRoomIds :: Adventure -> [String]
unreachableRoomIds a = [ r | r <- map arId (advRooms a), not (Set.member r reachable) ]
  where
    outs = allAOutcomes a
    roomIds = Set.fromList (map arId (advRooms a))
    edges = Map.fromListWith (++)
        ( [ (arId r, [aeTarget ex | ex <- Map.elems (arExits r)]) | r <- advRooms a ]
          ++ [ (from, [to]) | AOSetExit from _ to _ <- outs ]
          ++ [ (from, [newId]) | AOGenerateRoom newId _ _ from _ _ <- outs ] )
    seeds = Set.toList (Set.fromList
        (filter (`Set.member` roomIds) (advStartRoom a : explicitArrivals)))
    explicitArrivals =
        [ r | AORoomTransition r <- outs ]
        ++ [ r | v <- advVehicles a
               , r <- avEntryRoom v : maybe [] pure (avCockpit v)
                         ++ map asRoom (Map.elems (avStops v)) ]
    reachable = go Set.empty seeds
    go seen [] = seen
    go seen (x : rest)
        | Set.member x seen = go seen rest
        | otherwise         = go (Set.insert x seen) (Map.findWithDefault [] x edges ++ rest)
collisions :: Ord a => [(String, a)] -> [(a, [String])]
collisions pairs =
    [ (k, keys)
    | (k, keys) <- Map.toList
        (Map.fromListWith (++) [(t, [src]) | (src, t) <- pairs])
    , length keys > 1 ]

-- | Phase 4.2: group assigned phases by target; only targets assigned in more
--   than one distinct phase are clashes.
clashes :: Ord a => [(a, String)] -> [(a, [String])]
clashes pairs =
    [ (k, ps)
    | (k, ps) <- Map.toList
        (Map.fromListWith (++) [(t, [p]) | (t, p) <- pairs])
    , length (nub ps) > 1 ]

-- | Compile an Adventure into engine types, or return structured issues
compileAdventure :: Adventure -> Either [CompileIssue] CompileResult
compileAdventure adv =
    let -- Verb registry first: items/NPCs may reference custom verbs
        (verbErrs, verbRegistry) = compileVerbs (advVerbs adv)
        -- Compile each section, collecting issues
        (roomErrs, compiledRooms, vehicleExtraRooms) = compileRooms (advRooms adv) (advVehicles adv)
        (itemErrs, itemDefs, itemStates) = compileItems verbRegistry (advItems adv)
        (npcErrs, npcDefs, npcStates) = compileNPCs verbRegistry (advNPCs adv)
        
        questDefs = compileQuests (advQuests adv)
        (vehicleErrs, vehicleDefs, vehicleStates) = compileVehicles (advVehicles adv)
        (entityInteractions, itemInteractions, npcIx) = compileInteractions (advInteractions adv)
        
        (varErrs, varDefs, varInitials) = compileVariables (advVariables adv)
        (trigErrs, triggerDefs) = compileTriggers (advTriggers adv) (advNPCs adv) (advVariables adv)
        (encErrs, encounterDefs) = compileEncounterTables (advEncounterTables adv)
        (facErrs, factionDefs, factionInitials) = compileFactions (advFactions adv)
        (facConflictErrs, facVarDefs, facVarInitials) =
            mergeFactionVars varDefs varInitials factionDefs factionInitials
        (envErrs, envTriggerDefs, envVarDefs, envVarInitials) =
            compileEnvironment facVarDefs (advEnvironment adv)
        (envConflictErrs, envAllVarDefs, envAllVarInitials) =
            mergeEnvironmentVars facVarDefs facVarInitials envVarDefs envVarInitials

        allRooms = Map.union compiledRooms vehicleExtraRooms
        roomKeys = Set.fromList (Map.keys allRooms)

        (stealthErrs, stealthTriggerDefs, stealthVarDefs, stealthVarInitials) =
            compileStealth (Map.keys allRooms) (map anId (advNPCs adv)) (advStealth adv)
        (stealthConflictErrs, stealthAllVarDefs, stealthAllVarInitials) =
            mergeStealthVars envAllVarDefs envAllVarInitials stealthVarDefs stealthVarInitials

        -- Modul 7i: `patrol:` laesst NPCs umherziehen und zuschlagen.
        (patrolErrs, patrolTriggerDefs, patrolVarDefs, patrolVarInitials) =
            compilePatrol (Map.keys allRooms) (map anId (advNPCs adv)) (advPatrol adv)
        (patrolConflictErrs, patrolAllVarDefs, patrolAllVarInitials) =
            mergePatrolVars stealthAllVarDefs stealthAllVarInitials patrolVarDefs patrolVarInitials

        -- Phase 7g: `party:` blocks declare the follow variable party.<npcId>
        -- and one order verb that toggles membership.
        (partyErrs, partyVerbEntries, partyVarDefs, partyVarInitials) =
            compileParty verbRegistry (advNPCs adv)
        (partyConflictErrs, partyAllVarDefs, partyAllVarInitials) =
            mergePartyVars patrolAllVarDefs patrolAllVarInitials partyVarDefs partyVarInitials
        npcDefsWithParty = Map.mapWithKey (addPartyVerbEntry partyVerbEntries) npcDefs

        -- Phase 7h: ship systems (VarMap) + station verbs per interior room
        (shipErrs, shipTriggerDefs, shipVarDefs, shipVarInitials) =
            compileShipSystems verbRegistry (advVehicles adv)
        (shipConflictErrs, shipAllVarDefs, shipAllVarInitials) =
            mergeShipVars partyAllVarDefs partyAllVarInitials shipVarDefs shipVarInitials
        (progErrs, compiledProgression, progVarDefs, progVarInitials) =
            compileProgression (advProgression adv)
        (progConflictErrs, allVarDefs, allVarInitials) =
            mergeProgressionVars shipAllVarDefs shipAllVarInitials progVarDefs progVarInitials
        progVarErrs = checkProgressionVarReserved varDefs
        gainXpWarns = checkGainXpWithoutProgression adv

        allTriggerDefs = triggerDefs ++ encounterDefs ++ envTriggerDefs ++ stealthTriggerDefs
                        ++ pursuitTriggers
            ++ patrolTriggerDefs ++ shipTriggerDefs
            ++ knowledgeTriggers ++ journalTriggers ++ deviceTriggers
        -- W1/W4: inject the generated combine/notes/device verbs into the registry so
        -- `parseCommandWith` understands them like any custom verb.
        verbRegistryFull = Map.unions [verbRegistry, knowledgeVerb, journalVerb, deviceVerbs]
        knowledgeClashErrs = knowledgeVerbCollisions ++ journalVerbCollisions ++ deviceVerbCollisions

        -- Rogue Phase 1: the authored `game:` policy. Savezone rooms must
        -- exist (MissingRoom); ironman without savezones is a warning (legal
        -- hardcore setting, but usually an oversight). Warnings do not block
        -- compilation — they travel through 'crWarnings' to the author.
        (gameErrs, gameWarns, compiledPolicy) = compileGamePolicy allRooms (advGame adv)
        (combatErrs, combatProfileCompiled) = compileCombat (advCombat adv)
        (initVarErrs, initialVars) =
            compileInitialVariables allVarDefs allVarInitials (advInitialVariables adv)
        -- 4.4: `player: {inventory_limit: N}` seeds the VarMap (explicit
        -- initial_variables win over it).
        inventoryLimitVars = maybe Map.empty
            (\n -> Map.singleton "inventory.limit" (E.VVInt n))
            (advPlayer adv >>= apInventoryLimit)
        (initStateErrs, initialFlags, initialQuests) =
            compileInitialState (advActiveQuests adv) (advInitialFlags adv) questDefs

        compileAbility ab = E.PlayerAbility
            { E.paId = aabId ab
            , E.paName = fromMaybe (aabId ab) (aabName ab)
            , E.paCostVar = fromMaybe "" (aabCostVar ab)
            , E.paCost = fromMaybe 0 (aabCost ab)
            , E.paCooldown = fromMaybe 0 (aabCooldown ab)
            , E.paEffects = map compileAActionOutcome (aabEffects ab)
            }
        compiledAbilities = Map.fromList [ (E.paId pa, pa) | a <- advAbilities adv, let pa = compileAbility a ]

        (cardErrs, compiledCards) = compileCards (advCards adv)
        (szErrs, compiledSandboxZones) = compileSandboxZones (advSandboxZones adv)
        (procCompileErrs, compiledProcs) = compileProcedures (advProcedures adv)
        compiledChapters = compileChapters (advChapters adv)
        (chapterErrs, chapterWarns) = checkChapterRefs (advChapters adv) adv
        (pursuitErrs, pursuitTriggers) =
            compilePursuit (advPursuit adv) (Map.keys npcDefsWithParty)
        compiledFacts = compileFacts (advFacts adv)
        compiledStatements = compileStatements (advStatements adv)
        compiledCombines = compileCombines (advCombines adv)
        factRefErrs = checkFactRefs (advFacts adv) (advStatements adv) gw adv
        knownVarErrs = checkKnownVarReserved varDefs
        -- W1.4/W1.5: `kombiniere`-Trigger aus combine: (nur wenn Eintraege
        -- existieren) und der `notizen`-Befehl nur bei `journal: notes`.
        combineVerb = fromMaybe "kombiniere" (advCombineVerb adv)
        factsById = Map.fromList [ (afdId f, f) | f <- advFacts adv ]
        (knowledgeTriggers, knowledgeVerb) =
            if null (advCombines adv)
                then ([], Map.empty)
                else compileKnowledgeTriggers combineVerb (advCombines adv) factsById
        notesMode = advJournal adv == Just "notes"
        journalErrs =
            [ ciError "journal" "BadJournalMode"
                ("journal: '" ++ m ++ "' is not a mode (expected 'notes' or 'messages')")
            | Just m <- [advJournal adv], m `notElem` ["notes", "messages"] ]
        journalVerb = if notesMode
            then Map.singleton "notizen" (E.VerbDef "notizen" ["notes"])
            else Map.empty
        journalTriggers = if notesMode then compileNotesTrigger "notizen" else []
        -- Registry-Kollisionen: ein Autor-Verb darf nicht mit den
        -- generierten Worten kollidieren (combine_verb ist autorenwaehlbar
        -- und damit selbst die Referenz).
        knowledgeVerbCollisions =
            [ ciError ("verbs." ++ v) "CombineVerbClash"
                ("'" ++ v ++ "' collides with the generated combine verb")
            | v <- Map.keys verbRegistry, v == combineVerb, not (null (advCombines adv)) ]
        journalVerbCollisions =
            [ ciError ("verbs." ++ v) "NotesVerbClash"
                ("'" ++ v ++ "' collides with the generated notes command (journal: notes)")
            | v <- Map.keys verbRegistry, v `elem` ["notizen", "notes"], notesMode ]
        compiledDevices = compileDevices (advDevices adv)
        (containerErrs, compiledContainers, containerInitials) =
            compileContainers allRooms (advContainers adv)
        (deviceErrs, deviceWarns) = checkDeviceRefs (advDevices adv) adv
        (deviceTriggers, deviceVerbs) = compileDeviceTriggers (advDevices adv) (advItems adv)
        hasHolders = any (\d -> not (null (adFits d)) || isJust (adFitsTag d) || not (null (adOnInsert d)) || not (null (adOnRemove d))) (advDevices adv)
        deviceVerbCollisions =
            [ ciError ("verbs." ++ v) "DeviceVerbClash"
                ("'" ++ v ++ "' collides with generated device verb")
            | v <- Map.keys verbRegistry
            , hasHolders && v `elem` ["stecke", "ziehe", "insert", "remove"] ]
        mStartingDeck = case advDeck adv of
            Just d  -> Just d
            Nothing -> advPlayer adv >>= apDeck
        mHandLimit = case advHandLimit adv of
            Just hl -> Just hl
            Nothing -> advPlayer adv >>= apHandLimit
        deckErrs = case mStartingDeck of
            Nothing -> []
            Just deckCards ->
                [ ciError ("deck." ++ cid) "UnknownCardInDeck"
                    ("deck references card '" ++ cid ++ "', which is not declared under 'cards:'")
                | cid <- deckCards
                , cid `Map.notMember` compiledCards ]

        gw = E.GameWorld
                { E.rooms = allRooms
                , E.itemDefs = itemDefs
                , E.npcDefs = npcDefsWithParty
                , E.entityInteractions = entityInteractions
                , E.itemInteractions = itemInteractions
                , E.npcInteractions = npcIx
                , E.questDefs = questDefs
                , E.vehicleDefs = vehicleDefs
                , E.verbDefs = verbRegistryFull
                , E.varDefs = allVarDefs
                , E.triggerDefs = allTriggerDefs
                , E.combatProfile = combatProfileCompiled
                , E.worldName = fromMaybe "" (advName adv)
                , E.abilities = compiledAbilities
                , E.worldEndArt = Map.map compileAscii (advEndArt adv)
                , E.worldTitleArt = compileAscii (advTitleArt adv)
                , E.worldClips = compileClips (advClips adv)
                , E.worldGamePolicy = compiledPolicy
                , E.cardDefs = compiledCards
                , E.sandboxZones = compiledSandboxZones
                , E.procDefs = compiledProcs
                , E.chapterDefs = compiledChapters
                , E.factDefs = compiledFacts
                , E.statementDefs = compiledStatements
                , E.combineDefs = compiledCombines
                , E.deviceDefs = compiledDevices
                , E.containerDefs = compiledContainers
                , E.progressionDef = compiledProgression
                , E.worldLanguage = advLanguage adv
                , E.worldMessages = advMessages adv
                }
        npcIds = Set.fromList (map anId (advNPCs adv))
        gwResolved = resolveWorldEffects npcIds gw
        facRefErrs = checkStandingRefs (advFactions adv) gwResolved
        encRefErrs = checkEncounterRefs (advEncounterTables adv) gwResolved
        npcRefErrs = checkDamageNpcRefs gwResolved
        trigIdErrs = checkTriggerIds (advTriggers adv)
        cmdVerbErrs = checkCommandVerbRefs verbRegistry (advTriggers adv)
        chainTargetErrs = checkChainTargets (advTriggers adv) (advNPCs adv)
        stateTargetErrs = checkStateTargetRefs adv allRooms
        stopCostErrs = checkStopCostItems (advVehicles adv) gwResolved
        cooldownCondErrs = checkCooldownConditionReserved gwResolved
        hotspotErrs = checkHotspotRefs gwResolved
        setExitErrs = checkSetExitRefs roomKeys adv
        ambientErrs = checkAmbientRates gwResolved
        clipErrs = checkClips (advClips adv) gwResolved
        procCallErrs = checkProcRefs (advProcedures adv) adv
        possessionErrs = checkNpcPossessionRefs adv
        npcIxErrs = checkNpcInteractionRefs adv
        dynamicItemErrs = checkDynamicItemRefs adv
        questRefErrs = checkQuestRefs adv
        mapOverlapErrs = checkMapPositions adv
        reservedVarErrs = checkReservedVarWrites adv
        diceErrs = checkRollDice adv
        (langErrs, langWarns) = checkLanguageFields adv
        (gramErrs, gramWarns) = checkGrammarFields adv


        allErrors = verbErrs ++ roomErrs ++ itemErrs ++ npcErrs ++ vehicleErrs
                    ++ varErrs ++ facErrs ++ facConflictErrs ++ trigErrs ++ encErrs
                    ++ envErrs ++ envConflictErrs
                    ++ stealthErrs ++ stealthConflictErrs
                    ++ patrolErrs ++ patrolConflictErrs
                    ++ partyErrs ++ partyConflictErrs
                    ++ shipErrs ++ shipConflictErrs
                    ++ combatErrs
                    ++ initVarErrs ++ initStateErrs ++ facRefErrs ++ encRefErrs ++ npcRefErrs
                    ++ trigIdErrs
                    ++ cmdVerbErrs
                    ++ chainTargetErrs
                    ++ stateTargetErrs
                    ++ stopCostErrs
                    ++ cooldownCondErrs
                    ++ hotspotErrs
                    ++ ambientErrs
                    ++ clipErrs
                    ++ setExitErrs
                    ++ gameErrs
                    ++ cardErrs
                    ++ deckErrs
                    ++ szErrs
                    ++ procCompileErrs
                    ++ procCallErrs
                    ++ factRefErrs
                    ++ knownVarErrs
                    ++ chapterErrs
                    ++ pursuitErrs
                    ++ containerErrs
                    ++ knowledgeClashErrs
                    ++ journalErrs
                    ++ deviceErrs
                    ++ progErrs
                    ++ progConflictErrs
                    ++ progVarErrs
                    ++ possessionErrs
                    ++ npcIxErrs
                    ++ dynamicItemErrs
                    ++ questRefErrs
                    ++ mapOverlapErrs
                    ++ reservedVarErrs
                    ++ diceErrs
                    ++ langErrs
                    ++ gramErrs
    in case allErrors of
        (_:_) -> Left allErrors
        [] ->
            let startRoomId = advStartRoom adv
                aiInitialVars = Map.fromList
                    [ ("state." ++ anId n, E.VVText (fst (head states)))
                    | n <- advNPCs adv
                    , Just (ANpcAI states@(_:_)) <- [anAI n]
                    ]
                startSave = E.SaveState
                        { E.player = compilePlayer (advPlayer adv)
                        , E.currentRoom = startRoomId
                        , E.inventory = []
                        , E.itemStates = itemStates
                        , E.npcStates = npcStates
                        , E.entityStates = initialEntityStates allRooms containerInitials
                        , E.flags = initialFlags
                        , E.turnCount = 0
                        , E.gameOver = False
                        , E.gameOverReason = Nothing
                        , E.visitedRooms = Set.empty
                        , E.equipment = Map.empty
                        , E.conditions = Map.empty
                        , E.activeQuests = initialQuests
                        , E.completedQuests = Set.empty
                        , E.vehicleStates = vehicleStates
                        , E.currentVehicle = Nothing
                        , E.activeDialogue = Nothing
                        , E.rngState = E.initialRngState
                        , E.variables = Map.unions [initialVars, inventoryLimitVars, aiInitialVars]
                        , E.triggerStates = Map.empty
                        , E.exitOverrides = Map.empty
                        , E.deckState = case mStartingDeck of
                              Just deckCards -> Just (E.defaultDeckState
                                  { E.drawPile = deckCards
                                  , E.maxHandSize = fromMaybe 0 mHandLimit
                                  })
                              Nothing        -> Nothing
                        , E.dynamicRooms = Map.empty
                        }
                yamlKeyWarns = case advRawValue adv of
                    Just v  -> checkUnknownYamlKeys v
                    Nothing -> []
                keywordWarns = checkKeywordCollisions adv
                placeholderWarns = checkUnknownPlaceholders adv allVarDefs
                darkRoomWarns = checkDarkRoomDeadEnds adv
                deadContentWarns = checkUnreachableTriggers adv ++ checkUnsatisfiableConditions adv
                                ++ checkDeadExits adv ++ checkUnreachableRooms adv
                                ++ checkQuestProgress adv
                allWarns = gameWarns ++ yamlKeyWarns ++ keywordWarns ++ placeholderWarns ++ darkRoomWarns
                          ++ chapterWarns
                          ++ deviceWarns
                          ++ gainXpWarns
                          ++ deadContentWarns
                          ++ langWarns
                          ++ gramWarns
            in Right (CompileResult gwResolved startSave allWarns)
  where
    -- Every locked exit starts locked in entityStates
    initialEntityStates rooms containerInits =
        Map.fromList containerInits `Map.union` Map.fromList
            [ (e, "locked")
            | room <- Map.elems rooms
            , E.Locked _ e <- Map.elems (E.roomConnections room)
            ]

-- ---------------------------------------------------------------------------
-- YAML Key Validation (Compiler-Härtung)
-- ---------------------------------------------------------------------------

-- | Standard Levenshtein distance between two strings.
levenshtein :: String -> String -> Int
levenshtein s1 s2 = last (foldl transform [0 .. length s1] s2)
  where
    transform (d:ds) c = scanl (step c) (d + 1) (zip3 s1 (d:ds) ds)
    transform [] _     = []
    step c above (ch, diag, left) =
        minimum [above + 1, left + 1, diag + if ch == c then 0 else 1]

-- | Format an unknown YAML key warning message, providing a suggestion if
-- any known key has Levenshtein distance <= 2.
formatUnknownKey :: String -> Set.Set String -> String
formatUnknownKey unk known =
    let candidates = [ (k, levenshtein unk k) | k <- Set.toList known ]
        close = filter (\(_, d) -> d <= 2) candidates
    in case close of
        [] -> "'" ++ unk ++ "' is not a known key"
        _  ->
            let best = fst (minimumBy (comparing snd <> comparing fst) close)
            in "'" ++ unk ++ "' is not a known key - did you mean '" ++ best ++ "'?"

-- | Check parsed JSON/YAML value for unknown keys across all entity types.
checkUnknownYamlKeys :: Aeson.Value -> [CompileIssue]
checkUnknownYamlKeys (Aeson.Object topObj) =
    let topWarns = checkKeys "" EntAdventure (KM.keys topObj)
        sectionWarns =
            checkListOrMap "rooms" EntRoom (KM.lookup "rooms" topObj) checkRoomNested
            ++ checkListOrMap "items" EntItem (KM.lookup "items" topObj) noNested
            ++ checkListOrMap "npcs" EntNPC (KM.lookup "npcs" topObj) checkNpcNested
            ++ checkListOrMap "quests" EntQuest (KM.lookup "quests" topObj) checkQuestNested
            ++ checkListOrMap "rules" EntRule (KM.lookup "rules" topObj) noNested
            ++ checkListOrMap "cards" EntCard (KM.lookup "cards" topObj) noNested
            ++ checkListOrMap "sandbox_zones" EntSandboxZone (KM.lookup "sandbox_zones" topObj) checkZoneNested
            ++ checkListOrMap "vehicles" EntVehicle (KM.lookup "vehicles" topObj) noNested
            ++ checkListOrMap "variables" EntVariable (KM.lookup "variables" topObj) noNested
            ++ checkListOrMap "verbs" EntVerb (KM.lookup "verbs" topObj) noNested
            ++ checkListOrMap "factions" EntFaction (KM.lookup "factions" topObj) noNested
            ++ checkListOrMap "encounter_tables" EntEncounterTable (KM.lookup "encounter_tables" topObj) noNested
            ++ checkListOrMap "abilities" EntAbility (KM.lookup "abilities" topObj) noNested
            ++ checkListOrMap "clips" EntClip (KM.lookup "clips" topObj) noNested
            ++ checkSingleton "player" EntPlayer (KM.lookup "player" topObj)
            ++ checkSingleton "game" EntGame (KM.lookup "game" topObj)
            ++ checkSingleton "environment" EntEnvironment (KM.lookup "environment" topObj)
            ++ checkSingleton "stealth" EntStealth (KM.lookup "stealth" topObj)
            ++ checkSingleton "patrol" EntPatrol (KM.lookup "patrol" topObj)
            ++ checkCombat (KM.lookup "combat" topObj)
            ++ checkSingleton "interactions" EntInteractions (KM.lookup "interactions" topObj)
            ++ checkProgressionSection (KM.lookup "progression" topObj)
            ++ checkListOrMap "statements" EntStatement (KM.lookup "statements" topObj) noNested
    in topWarns ++ sectionWarns
checkUnknownYamlKeys _ = []

checkProgressionSection :: Maybe Aeson.Value -> [CompileIssue]
checkProgressionSection Nothing = []
checkProgressionSection (Just (Aeson.Object progObj)) =
    checkKeys "progression" EntProgression (KM.keys progObj)
    ++ checkListOrMap "progression.levels" EntLevel (KM.lookup "levels" progObj) noNested
checkProgressionSection (Just _) = []

checkKeys :: String -> EntityType -> [Aeson.Key] -> [CompileIssue]
checkKeys prefix entType actualKeys =
    let allowed = knownKeys entType
    in [ CompileIssue
            { ciPath = if null prefix then kStr else prefix
            , ciSeverity = SWarning
            , ciCode = "UnknownYamlKey"
            , ciMessage = formatUnknownKey kStr allowed
            }
       | k <- actualKeys
       , let kStr = K.toString k
       , kStr `Set.notMember` allowed
       ]

noNested :: String -> Aeson.Object -> [CompileIssue]
noNested _ _ = []

checkListOrMap :: String
               -> EntityType
               -> Maybe Aeson.Value
               -> (String -> Aeson.Object -> [CompileIssue])
               -> [CompileIssue]
checkListOrMap section entType mVal nestedFn = case mVal of
    Nothing -> []
    Just (Aeson.Array arr) ->
        concatMap (checkOneItem section entType nestedFn) (Foldable.toList arr)
    Just (Aeson.Object obj) ->
        concatMap (\(k, v) -> checkOneNamedItem (section ++ "." ++ K.toString k) entType v nestedFn) (KM.toList obj)
    Just _ -> []

checkOneItem :: String -> EntityType -> (String -> Aeson.Object -> [CompileIssue]) -> Aeson.Value -> [CompileIssue]
checkOneItem section entType nestedFn (Aeson.Object o) =
    let eid = extractEntityId o
        path = section ++ "." ++ eid
    in checkKeys path entType (KM.keys o) ++ nestedFn path o
checkOneItem _ _ _ _ = []

checkOneNamedItem :: String -> EntityType -> Aeson.Value -> (String -> Aeson.Object -> [CompileIssue]) -> [CompileIssue]
checkOneNamedItem path entType (Aeson.Object o) nestedFn =
    checkKeys path entType (KM.keys o) ++ nestedFn path o
checkOneNamedItem _ _ _ _ = []

extractEntityId :: Aeson.Object -> String
extractEntityId o =
    case KM.lookup "id" o of
        Just (Aeson.String s) -> T.unpack s
        _ -> case KM.lookup "name" o of
            Just (Aeson.String s) -> T.unpack s
            _ -> "?"

checkRoomNested :: String -> Aeson.Object -> [CompileIssue]
checkRoomNested roomPath o =
    case KM.lookup "exits" o of
        Just (Aeson.Object exitsObj) ->
            concatMap (\(dirKey, val) ->
                case val of
                    Aeson.Object exitObj ->
                        checkKeys (roomPath ++ ".exits." ++ K.toString dirKey) EntExitRef (KM.keys exitObj)
                    _ -> []
                ) (KM.toList exitsObj)
        _ -> []

checkQuestNested :: String -> Aeson.Object -> [CompileIssue]
checkQuestNested questPath o =
    case KM.lookup "stages" o of
        Just (Aeson.Array arr) ->
            concatMap (\case
                Aeson.Object stObj ->
                    let sid = extractEntityId stObj
                        stPath = questPath ++ ".stages." ++ sid
                    in checkKeys stPath EntQuestStage (KM.keys stObj)
                _ -> []
                ) (Foldable.toList arr)
        _ -> []

checkZoneNested :: String -> Aeson.Object -> [CompileIssue]
checkZoneNested zonePath o =
    case KM.lookup "biomes" o of
        Just (Aeson.Array arr) ->
            concatMap (\case
                Aeson.Object bObj ->
                    let bid = extractEntityId bObj
                        bPath = zonePath ++ ".biomes." ++ bid
                    in checkKeys bPath EntBiomeTemplate (KM.keys bObj)
                _ -> []
                ) (Foldable.toList arr)
        Just (Aeson.Object obj) ->
            concatMap (\(bKey, bVal) ->
                case bVal of
                    Aeson.Object bObj ->
                        let bPath = zonePath ++ ".biomes." ++ K.toString bKey
                        in checkKeys bPath EntBiomeTemplate (KM.keys bObj)
                    _ -> []
                ) (KM.toList obj)
        _ -> []

checkNpcNested :: String -> Aeson.Object -> [CompileIssue]
checkNpcNested npcPath o =
    case KM.lookup "ai" o of
        Just (Aeson.Object aiObj) ->
            checkKeys (npcPath ++ ".ai") EntNPC (KM.keys aiObj)
            ++ case KM.lookup "states" aiObj of
                Just (Aeson.Object statesObj) ->
                    concatMap (\(stKey, stVal) ->
                        case stVal of
                            Aeson.Object stObj ->
                                checkKeys (npcPath ++ ".ai.states." ++ K.toString stKey) EntNPC (KM.keys stObj)
                            _ -> []
                        ) (KM.toList statesObj)
                _ -> []
        _ -> []

checkSingleton :: String -> EntityType -> Maybe Aeson.Value -> [CompileIssue]
checkSingleton path entType (Just (Aeson.Object o)) =
    checkKeys path entType (KM.keys o)
checkSingleton _ _ _ = []

checkCombat :: Maybe Aeson.Value -> [CompileIssue]
checkCombat (Just (Aeson.Object o)) =
    checkKeys "combat" EntCombat (KM.keys o)
    ++ case KM.lookup "screen" o of
        Just (Aeson.Object screenObj) ->
            checkKeys "combat.screen" EntCombatScreen (KM.keys screenObj)
        _ -> []
checkCombat _ = []

-- ---------------------------------------------------------------------------
-- Direction parsing (strict — unknown = compile error)
-- ---------------------------------------------------------------------------

-- | Rogue Phase 3: total direction parse for the compile step. Invalid
--   direction strings are reported by 'checkSetExitRefs' (UnknownDirection);
--   the mapping itself uses East as a harmless placeholder so the effect tree
--   always type-checks before the check runs.
dirOf :: String -> E.Direction
dirOf = either (const E.East) id . parseDir

parseDir :: String -> Either String E.Direction
parseDir s = case map toLower s of
    "north"      -> Right E.North
    "south"      -> Right E.South
    "east"       -> Right E.East
    "west"       -> Right E.West
    "up"         -> Right E.Up
    "down"       -> Right E.Down
    "northeast"  -> Right E.Northeast
    "northwest"  -> Right E.Northwest
    "southeast"  -> Right E.Southeast
    "southwest"  -> Right E.Southwest
    "ne"         -> Right E.Northeast
    "nw"         -> Right E.Northwest
    "se"         -> Right E.Southeast
    "sw"         -> Right E.Southwest
    other       -> Left $ "Unknown direction '" ++ other ++ "'"

-- ---------------------------------------------------------------------------
-- Rooms
-- ---------------------------------------------------------------------------

compileRooms :: [ARoom] -> [AVehicle] -> ([CompileIssue], Map.Map String E.Room, Map.Map String E.Room)
compileRooms rooms vehicles =
    let (errors, goodRooms) = foldr compileRoomAcc ([], []) rooms
        roomMap = Map.fromList [(arId r, room) | (r, room) <- goodRooms]
        vehicleInterior = concatMap (\(_, rs) -> mapMaybe compileVehicleRoom rs)
                                      [(avId v, avInterior v) | v <- vehicles]
        interiorMap = Map.fromList [(E.roomId r, r) | r <- vehicleInterior]
    in (errors, roomMap, interiorMap)
  where
    compileVehicleRoom :: ARoom -> Maybe E.Room
    compileVehicleRoom r = case compileRoom (tagVehicleAr r) of
        Right rm -> Just rm
        Left _   -> Nothing  -- interior rooms should have no exits, skip errors

    compileRoomAcc :: ARoom -> ([CompileIssue], [(ARoom, E.Room)]) -> ([CompileIssue], [(ARoom, E.Room)])
    compileRoomAcc r (es, acc) = case compileRoom r of
        Left errs -> (errs ++ es, acc)
        Right rm  -> (es, (r, rm) : acc)

compileRoom :: ARoom -> Either [CompileIssue] E.Room
compileRoom r =
    let roomPath = "rooms." ++ arId r
        exitList = Map.toList (arExits r)
        -- Parse each exit: (dirStr, parseResult)
        parsed = [(d, parseDir d) | (d, _) <- exitList]
        -- Direction syntax errors
        dirErrors =
            [ ciError (roomPath ++ ".exits." ++ d) "UnknownDirection" msg
            | (d, Left msg) <- parsed ]
        -- Good pairs: (Direction, Exit)
        goodPairs = [(dir, compileExitInner ref)
                    | (d, Right dir) <- parsed
                    , let ref = arExits r Map.! d ]
        -- Collision: two source keys (e.g. "se" and "southeast") → same Direction
        collErrs =
            [ ciError (roomPath ++ ".exits") "DuplicateDirection"
                ("exit keys " ++ show keys ++ " all resolve to " ++ show dir)
            | (dir, keys) <- collisions [(d, dir) | (d, Right dir) <- parsed] ]
        allErrs = dirErrors ++ collErrs
    in if null allErrs
       then Right $ E.Room
            { E.roomId = arId r
            , E.roomName = arName r
            , E.roomDescription = compileCondText (arTexts r)
            , E.roomConnections = Map.fromList goodPairs
            , E.roomTags = Set.fromList (arTags r)
            , E.roomLightFlag = arLightFlag r
            , E.roomDarkMsg = arDarkMsg r
            , E.roomOnEnter = compileMaybeOutcomes (arOnEnter r)
            , E.roomOnLook = compileMaybeOutcomes (arOnLook r)
            , E.roomOnExit = compileMaybeOutcomes (arOnExit r)
            , E.roomSearchOutcome = compileMaybeOutcomes (arSearch r)
            , E.roomAscii = compileAscii (arAscii r)
            , E.roomIntro = arIntro r
            , E.roomFloor = arFloor r
            , E.roomMapPos = arMapPos r
            }
       else Left allErrs
  where
    compileExitInner :: AExitRef -> E.Exit
    compileExitInner ref = case aeWhen ref of
        Just p  -> E.Guarded (aeTarget ref) p (aeMsg ref)
        Nothing -> case aeLocked ref of
            Nothing     -> E.Open (aeTarget ref)
            Just entity -> E.Locked (aeTarget ref) entity

tagVehicleAr :: ARoom -> ARoom
tagVehicleAr r = r { arTags = "vehicle" : arTags r }

-- ---------------------------------------------------------------------------
-- Verbs (Phase 3a): adventure-declared custom verbs
-- ---------------------------------------------------------------------------

-- | Reserved words the runtime interprets as core verbs; these cannot be
--   redeclared as custom verbs.
reservedVerbWords :: [String]
reservedVerbWords =
    [ "take", "pick", "grab", "get", "drop", "put"
    , "examine", "inspect", "look", "read", "watch"
    , "use", "activate"
    , "talk", "speak", "chat"
    , "attack", "hit", "kill", "search"
    , "go", "move", "walk", "north", "south", "east", "west", "up", "down"
    , "enter", "board", "exit", "drive", "wait", "refuel", "repair"
    , "equip", "wear", "wield", "unequip", "remove"
    , "look", "inventory", "inv", "i", "stats", "journal", "quests"
    ]

-- ---------------------------------------------------------------------------
-- Variables (Phase 3b)
-- ---------------------------------------------------------------------------

-- | Compile declared variables into the engine schema + initial save values.
compileVariables :: [AVariable] -> ([CompileIssue], Map.Map String E.VarDef, Map.Map String E.VariableValue)
compileVariables vars =
    let results = map compileVar vars
        errors = concat [e | Left e <- results]
        defs = Map.fromList [(avbVarName av, d) | Right (av, d, _) <- results]
        initials = Map.fromList [(avbVarName av, v) | Right (av, _, v) <- results]
    in (errors, defs, initials)

compileVar :: AVariable -> Either [CompileIssue] (AVariable, E.VarDef, E.VariableValue)
compileVar av =
    let bp = "variables." ++ avbVarName av
        vtype = parseVarType (avbVarType av) (avbMin av) (avbMax av)
        resetErr = case avbResetOn av of
            Just rEv
                | rEv `notElem` ["turn", "combat_start"] ->
                    [ciError (bp ++ ".reset_on") "InvalidResetOn"
                        ("unknown reset_on event '" ++ rEv ++ "' (expected: turn, combat_start)")]
                | isNothing (avbMax av) ->
                    [ciError (bp ++ ".reset_on") "ResetWithoutMax"
                        ("variable '" ++ avbVarName av ++ "' has reset_on but no max declared")]
                | otherwise -> []
            Nothing -> []
        overflowErr = case (avbOnOverflow av, avbMax av) of
            (effs, Nothing) | not (null effs) ->
                [ciError (bp ++ ".on_overflow") "OverflowWithoutMax"
                    ("variable '" ++ avbVarName av ++ "' has on_overflow but no max declared")]
            _ -> []
        refillErr = if avbRefillPerTurn av < 0
            then [ciError (bp ++ ".refill_per_turn") "NegativeRefill"
                    ("variable '" ++ avbVarName av ++ "' has negative refill_per_turn")]
            else []
        valErrs = resetErr ++ overflowErr ++ refillErr
        overflowEffs = map compileAActionOutcome (avbOnOverflow av)
    in if not (null valErrs)
       then Left valErrs
       else case vtype of
           Left msg -> Left [ciError bp "InvalidVariableType" msg]
           Right vt -> case compileVarInitial av vt of
               Left msg -> Left [ciError (bp ++ ".initial") "InvalidVariableInitial" msg]
               Right vv -> Right (av, E.VarDef (avbVarName av) vt vv overflowEffs, vv)

parseVarType :: String -> Maybe Int -> Maybe Int -> Either String E.VariableType
parseVarType "bool" _ _     = Right E.VTBool
parseVarType "int" min_ max_ = Right (E.VTInt min_ max_)
parseVarType "text" _ _     = Right E.VTText
parseVarType "enum" _ _     = Right (E.VTEnum [])
parseVarType other _ _      = Left $ "Unknown variable type '" ++ other ++ "' (expected: bool, int, text, enum)"

-- | Parse the initial value of a variable from the YAML JSON value.
compileVarInitial :: AVariable -> E.VariableType -> Either String E.VariableValue
compileVarInitial av vt = case (vt, avbInitial av) of
    (E.VTBool, Just (Aeson.Bool b))            -> Right (E.VVBool b)
    (E.VTBool, Just (Aeson.Number n))          -> Right (E.VVBool (n > 0))
    (E.VTBool, Nothing)                        -> Right (E.VVBool False)
    (E.VTBool, _)                              -> Left "expected bool or 0/1 for variable type bool"

    (E.VTInt _ _, Just (Aeson.Number n))       -> Right (E.VVInt (truncate n))
    (E.VTInt _ _, Nothing)                     -> Right (E.VVInt 0)
    (E.VTInt _ _, _)                           -> Left "expected number for variable type int"

    (E.VTText, Just (Aeson.String s))          -> Right (E.VVText (T.unpack s))
    (E.VTText, Nothing)                        -> Right (E.VVText "")
    (E.VTText, _)                              -> Left "expected string for variable type text"

    (E.VTEnum _, Just (Aeson.String s))        -> Right (E.VVText (T.unpack s))
    (E.VTEnum _, Nothing)                      -> Right (E.VVText "")
    (E.VTEnum _, _)                            -> Left "expected string for variable type enum"

-- ---------------------------------------------------------------------------
-- Factions (Phase 7a)
-- ---------------------------------------------------------------------------

-- | Compile `factions:` into VarDefs + initial VarMap values. Each faction
--   becomes the variable `faction.<id>` (int, initial standing).
compileFactions :: [AFaction] -> ([CompileIssue], Map.Map String E.VarDef, Map.Map String E.VariableValue)
compileFactions factions =
    let dupErrs =
            [ ciError ("factions." ++ fid) "DuplicateFaction"
                ("faction '" ++ fid ++ "' is declared more than once")
            | (fid, others) <- collisions [(afId f, afId f) | f <- factions]
            , not (null others) ]
        defs = Map.fromList
            [ ("faction." ++ afId f, E.VarDef ("faction." ++ afId f) (E.VTInt Nothing Nothing) (E.VVInt (afInitial f)) [])
            | f <- factions ]
        initials = Map.fromList
            [ ("faction." ++ afId f, E.VVInt (afInitial f))
            | f <- factions ]
        -- P2-18: `levels:` were pure authoring decoration — nothing validated or
        -- displayed them. Check that a threshold is named at most once and that
        -- every name is non-empty. Ascending order is deliberately NOT required:
        -- the fixtures order levels by relation quality (best first) and lookup
        -- takes the highest matching threshold, so list order carries no meaning.
        levelErrs =
            [ ciError ("factions." ++ afId f ++ ".levels") "DuplicateFactionLevel"
                ("threshold " ++ show at ++ " is named more than once")
            | f <- factions
            , at <- duplicates (map aflAt (afLevels f)) ]
            ++
            [ ciError ("factions." ++ afId f ++ ".levels") "BadFactionLevel"
                ("level at " ++ show (aflAt l) ++ " needs a non-empty name")
            | f <- factions, l <- afLevels f, null (aflName l) ]
        duplicates xs =
            [ x | (x, n) <- Map.toList (Map.fromListWith (+) [(x, 1 :: Int) | x <- xs]), n > 1 ]
    in (dupErrs ++ levelErrs, defs, initials)

-- | Merge faction vars into the declared variables, rejecting name clashes
--   (an author must not declare `faction.X` as a plain variable).
mergeFactionVars :: Map.Map String E.VarDef -> Map.Map String E.VariableValue
                 -> Map.Map String E.VarDef -> Map.Map String E.VariableValue
                 -> ([CompileIssue], Map.Map String E.VarDef, Map.Map String E.VariableValue)
mergeFactionVars varDefs varInitials facDefs facInitials =
    let clashErrs =
            [ ciError ("variables." ++ name) "FactionVariableClash"
                ("'" ++ name ++ "' is a faction variable; declare it under 'factions:' instead")
            | name <- Map.keys varDefs
            , name `Map.member` facDefs ]
    in (clashErrs, Map.union facDefs varDefs, Map.union facInitials varInitials)

-- ---------------------------------------------------------------------------
-- Environment (Phase 7d): weather + drains
-- ---------------------------------------------------------------------------

-- | Compile the `environment:` segment. Weather becomes the `env.weather`
--   variable (index into `states`, initial state index) plus one OnTurn
--   trigger per transition; each drain becomes an OnTurn trigger that moves
--   the variable and, once it hits ≤ 0, fires the at_zero outcomes.
--   `facVarDefs` are the declared variables (author + faction) used to
--   reject drains on undeclared variables.
compileEnvironment :: Map.Map String E.VarDef -> Maybe AEnvironment
                   -> ([CompileIssue], [E.TriggerDef], Map.Map String E.VarDef, Map.Map String E.VariableValue)
compileEnvironment _ Nothing = ([], [], Map.empty, Map.empty)
compileEnvironment varDefs (Just env) =
    let (wErrs, wTriggers, wVarDefs, wInitials) = compileWeather (envWeather env)
        (dErrs, dTriggers) = compileDrains varDefs (envDrains env)
    in (wErrs ++ dErrs, wTriggers ++ dTriggers, wVarDefs, wInitials)

-- | Weather machine: validate states/transitions, seed `env.weather`, and
--   turn each transition into an OnTurn trigger that sets the new state.
compileWeather :: Maybe AWeatherDef
               -> ([CompileIssue], [E.TriggerDef], Map.Map String E.VarDef, Map.Map String E.VariableValue)
compileWeather Nothing = ([], [], Map.empty, Map.empty)
compileWeather (Just wd) =
    let states = weaStates wd
        stateIndex s = length (takeWhile (/= s) states)
        badInitial = if weaInitial wd `elem` states
                        then [] else [ ciError "environment.weather.initial" "UnknownWeatherState"
                            ("weather state '" ++ weaInitial wd ++ "' is not in 'states'") ]
        badTransitions =
            [ ciError ("environment.weather.transitions." ++ show i) "UnknownWeatherState"
                ("weather state '" ++ wtTo t ++ "' is not in 'states'")
            | (i, t) <- zip [0 :: Int ..] (weaTransitions wd)
            , wtTo t `notElem` states ]
        initIdx = stateIndex (weaInitial wd)
        varDefs = Map.singleton "env.weather"
            (E.VarDef "env.weather" (E.VTInt Nothing Nothing) (E.VVInt initIdx) [])
        initials = Map.singleton "env.weather" (E.VVInt initIdx)
        transitions =
            [ E.TriggerDef ("environment.weather." ++ show i) E.OnTurn (wtWhen t)
                (E.SetValue (E.VRVariable "env.weather") (E.EVInt (stateIndex (wtTo t)))
                    : map compileAActionOutcome (wtEffects t)) False 0 1 [] []
            | (i, t) <- zip [0 :: Int ..] (weaTransitions wd)
            , stateIndex (wtTo t) >= 0 ]
    in (badInitial ++ badTransitions, transitions, varDefs, initials)

-- | Each drain: OnTurn trigger whose condition is the drain's `when`, whose
--   effects move the variable then check `var ≤ 0` to fire at_zero outcomes.
compileDrains :: Map.Map String E.VarDef -> [ADrainDef] -> ([CompileIssue], [E.TriggerDef])
compileDrains varDefs drains =
    let unknown =
            [ ciError ("environment.drains." ++ drVar d) "UnknownDrainVariable"
                ("drain variable '" ++ drVar d ++ "' is not declared under 'variables:'")
            | d <- drains
            , drVar d `Map.notMember` varDefs ]
        triggers =
            [ E.TriggerDef ("environment.drain." ++ drVar d) E.OnTurn (drWhen d)
                [ E.ModifyValue (E.VRVariable (drVar d)) (drPerTurn d)
                , E.Conditional (E.CompareVar (drVar d) E.CLte 0)
                    (compileOutcomes (drAtZero d)) E.Noop ]
                False 0 1 [] []
            | d <- drains ]
    in (unknown, triggers)

-- | Merge the `env.weather` variable into the declared variables, rejecting
--   an author-declared clash (the `env.*` namespace is reserved).
mergeEnvironmentVars :: Map.Map String E.VarDef -> Map.Map String E.VariableValue
                      -> Map.Map String E.VarDef -> Map.Map String E.VariableValue
                      -> ([CompileIssue], Map.Map String E.VarDef, Map.Map String E.VariableValue)
mergeEnvironmentVars varDefs varInitials envDefs envInitials =
    let clashErrs =
            [ ciError ("variables." ++ name) "EnvironmentVariableClash"
                ("'" ++ name ++ "' is an environment variable; the 'env.*' namespace is reserved")
            | name <- Map.keys varDefs
            , name `Map.member` envDefs ]
    in (clashErrs, Map.union envDefs varDefs, Map.union envInitials varInitials)

-- ---------------------------------------------------------------------------
-- Stealth (Phase 7e): noise + observers
-- ---------------------------------------------------------------------------

-- | Compile the `stealth:` segment. Noise becomes a variable; the compiler
--   emits one `on: enter` trigger per room (gain, clamped to max), one
--   `on: turn` observer trigger per NPC (fires while noise >= hears_at and the
--   observer is not dead, re-arms after `cooldown` turns), and one final
--   `on: turn` decay trigger.
--   Order matters: observers run BEFORE decay, so a guard hears the full
--   noise of the same turn before it fades. No core change.
compileStealth :: [String] -> [String] -> Maybe AStealth
               -> ([CompileIssue], [E.TriggerDef], Map.Map String E.VarDef, Map.Map String E.VariableValue)
compileStealth _ _ Nothing = ([], [], Map.empty, Map.empty)
compileStealth roomIds npcIds (Just st) =
    let spec = stNoise st
        var = nsVar spec
        onMove = nsOnMove spec
        decay = nsDecay spec
        maxN = nsMax spec
        varDefs = Map.singleton var (E.VarDef var (E.VTInt Nothing (Just maxN)) (E.VVInt 0) [])
        initials = Map.singleton var (E.VVInt 0)
        clampToMax = E.Conditional (E.CompareVar var E.CGte maxN)
                         (E.SetValue (E.VRVariable var) (E.EVInt maxN)) E.Noop
        clampToZero = E.Conditional (E.CompareVar var E.CLte 0)
                          (E.SetValue (E.VRVariable var) (E.EVInt 0)) E.Noop
        moveTriggers =
            [ E.TriggerDef ("stealth.nmove." ++ rId) (E.OnEnter rId) Nothing
                [ E.ModifyValue (E.VRVariable var) onMove, clampToMax ] False 0 1 [] []
            | rId <- roomIds ]
        observerTriggers =
            [ E.TriggerDef ("stealth.observe." ++ obNPC o) E.OnTurn
                (Just (E.PAll
                    [ E.CompareVar var E.CGte (obHearsAt o)
                    -- A body hears nothing. This is the condition-side twin of
                    -- the engine's `isDeadNPC` (status == "dead"), written with
                    -- existing predicates — no core change. Distance keeps being
                    -- modelled by `hears_at`, so there is deliberately no room
                    -- check here.
                    , E.PNot (E.EntityHasState (obNPC o) "dead") ]))
                [ compileOutcomes (obOnHear o) ] False (obCooldown o) 1 [] []
            | o <- stObservers st ]
        decayTrigger =
            [ E.TriggerDef "stealth.decay" E.OnTurn Nothing
                [ E.ModifyValue (E.VRVariable var) decay, clampToZero ] False 0 1 [] [] ]
        unknownNpc =
            [ ciError ("stealth.observers." ++ obNPC o) "UnknownObserverNPC"
                ("observer npc '" ++ obNPC o ++ "' is not declared under 'npcs:'")
            | o <- stObservers st
            , obNPC o `notElem` npcIds ]
    in (unknownNpc, moveTriggers ++ observerTriggers ++ decayTrigger, varDefs, initials)

-- | Merge the noise variable into the declared variables, rejecting a clash
--   (the author must not declare it, e.g. also under `variables:`).
mergeStealthVars :: Map.Map String E.VarDef -> Map.Map String E.VariableValue
                  -> Map.Map String E.VarDef -> Map.Map String E.VariableValue
                  -> ([CompileIssue], Map.Map String E.VarDef, Map.Map String E.VariableValue)
mergeStealthVars varDefs varInitials stealthDefs stealthInitials =
    let clashErrs =
            [ ciError ("variables." ++ name) "StealthVariableClash"
                ("'" ++ name ++ "' is the stealth noise variable; declare it under 'stealth:' instead")
            | name <- Map.keys varDefs
            , name `Map.member` stealthDefs ]
    in (clashErrs, Map.union stealthDefs varDefs, Map.union stealthInitials varInitials)

-- ---------------------------------------------------------------------------
-- Patrol (Modul 7i)
-- ---------------------------------------------------------------------------

-- | Compile the `patrol:` segment: per hostile one gate variable, one
--   `on: turn` step block (a guardian gets none) and one `on: turn` attack
--   trigger.
--
--   Two mechanics matter:
--
--   * The path index lives in a variable (`path !! index` is the current room),
--     so the next room is `path !! ((i+1) mod n)`. Trigger conditions are
--     evaluated *live* inside one linear fold, so a flat chain of
--     \"index == k\" triggers would cascade: step k sets k+1, and step k+1
--     already matches in the same turn — the NPC would teleport along the whole
--     path. Every hostile therefore gets a `moved` gate: a clearing trigger runs
--     first, each step trigger is gated on `moved == 0` and sets it, so exactly
--     one step fires per turn.
--   * The attack is a single trigger whose effects are one conditional per path
--     room. Only one can match (an NPC stands in exactly one room), so these
--     conditionals cannot cascade.
--
--   No core change: step blocks are plain MoveEntity/SetValue triggers.
compilePatrol :: [String] -> [String] -> Maybe APatrol
              -> ([CompileIssue], [E.TriggerDef], Map.Map String E.VarDef, Map.Map String E.VariableValue)
compilePatrol _ _ Nothing = ([], [], Map.empty, Map.empty)
compilePatrol roomIds npcIds (Just p) =
    let hostiles = ptHostiles p
        -- Nur ziehende Feinde brauchen Rundkurs-Zustand: ein Wächter hat
        -- weder Schritt- noch Gate-Trigger, also auch keine Variablen.
        walkers = [ h | h <- hostiles, not (ahGuardian h) ]
        movedVar h = "patrol." ++ ahNPC h ++ ".moved"
        indexVar h = "patrol." ++ ahNPC h ++ ".index"
        varDefs = Map.union
            (Map.fromList [ (movedVar h, E.VarDef (movedVar h) (E.VTInt Nothing Nothing) (E.VVInt 0) [])
                          | h <- walkers ])
            (Map.fromList [ (indexVar h, E.VarDef (indexVar h) (E.VTInt Nothing Nothing) (E.VVInt (ahStartIndex h)) [])
                          | h <- walkers ])
        initials = Map.union
            (Map.fromList [ (movedVar h, E.VVInt 0) | h <- walkers ])
            (Map.fromList [ (indexVar h, E.VVInt (ahStartIndex h)) | h <- walkers ])

        clearTrig h =
            E.TriggerDef ("patrol.clear." ++ ahNPC h) E.OnTurn Nothing
                [ E.SetValue (E.VRVariable (movedVar h)) (E.EVInt 0) ] False 0 1 [] []

        stepTrigs h = [ stepTrig h i | i <- [0 .. length (ahPath h) - 1] ]
        stepTrig h i =
            E.TriggerDef ("patrol.step." ++ ahNPC h ++ "." ++ show i) E.OnTurn
                (Just (E.PAll
                    [ E.PNot (E.EntityHasState (ahNPC h) "dead")
                    , E.CompareVar (indexVar h) E.CEq i
                    , E.CompareVar (movedVar h) E.CEq 0 ]))
                [ E.MoveEntity (ahNPC h) (E.InRoom (ahPath h !! next))
                , E.SetValue (E.VRVariable (indexVar h)) (E.EVInt next)
                , E.SetValue (E.VRVariable (movedVar h)) (E.EVInt 1)
                ] False 0 1 [] []
          where next = (i + 1) `mod` length (ahPath h)

        warnTrigs h
            | Just txt <- ahWarn h = [ sharedTrig "warn" (E.SendMessage txt) h ]
            | otherwise            = []

        attackTrigs h
            | null (ahAttack h) = []
            | otherwise         = [ sharedTrig "attack" (compileOutcomes (ahAttack h)) h ]

        -- Warnung und Angriff teilen sich die Struktur: ein Trigger, dessen
        -- Effekte je Pfadraum prüfen, ob Spieler und Feind zusammenstehen.
        -- Nur einer kann zutreffen (ein NPC steht in genau einem Raum), also
        -- kaskadiert hier nichts.
        sharedTrig kind payload h =
            E.TriggerDef ("patrol." ++ kind ++ "." ++ ahNPC h) E.OnTurn
                (Just (E.PNot (E.EntityHasState (ahNPC h) "dead")))
                [ E.Conditional
                    (E.PAll [ E.Location E.ActorPlayer r, E.Location (E.ActorNPC (ahNPC h)) r ])
                    payload E.Noop
                | r <- ahPath h ] False 0 1 [] []

        trigs h | ahGuardian h = warnTrigs h ++ attackTrigs h
                | otherwise    = clearTrig h : (stepTrigs h ++ warnTrigs h ++ attackTrigs h)

        unknownNpc =
            [ ciError ("patrol.hostiles." ++ ahNPC h) "UnknownPatrolNPC"
                ("patrol npc '" ++ ahNPC h ++ "' is not declared under 'npcs:'")
            | h <- hostiles, ahNPC h `notElem` npcIds ]
        emptyPath =
            [ ciError ("patrol.hostiles." ++ ahNPC h) "EmptyPatrolPath"
                ("patrol npc '" ++ ahNPC h ++ "' needs a non-empty 'path:'")
            | h <- hostiles, null (ahPath h) ]
        unknownRooms =
            [ ciError ("patrol.hostiles." ++ ahNPC h ++ ".path") "UnknownPatrolRoom"
                ("patrol room '" ++ r ++ "' is not a declared room")
            | h <- hostiles, r <- ahPath h, r `notElem` roomIds ]
    in ( unknownNpc ++ emptyPath ++ unknownRooms
       , concatMap trigs hostiles
       , varDefs, initials )

-- | Merge the patrol variables into the declared ones, rejecting a clash
--   (the author must not declare `patrol.<npc>.*` under `variables:`).
mergePatrolVars :: Map.Map String E.VarDef -> Map.Map String E.VariableValue
                -> Map.Map String E.VarDef -> Map.Map String E.VariableValue
                -> ([CompileIssue], Map.Map String E.VarDef, Map.Map String E.VariableValue)
mergePatrolVars varDefs varInitials patrolDefs patrolInitials =
    let clashErrs =
            [ ciError ("variables." ++ name) "PatrolVariableClash"
                ("'" ++ name ++ "' is a patrol variable; declare it under 'patrol:' instead")
            | name <- Map.keys varDefs
            , name `Map.member` patrolDefs ]
    in (clashErrs, Map.union patrolDefs varDefs, Map.union patrolInitials varInitials)

-- ---------------------------------------------------------------------------
-- Party (Phase 7g)
-- ---------------------------------------------------------------------------

-- | Phase 7g: compile the `party:` blocks on NPCs. Each recruitable NPC gets
--   the follow variable `party.<npcId>` (a plain VarMap entry — no new
--   SaveState field, no NPCState field) and one order-verb entry in its
--   compiled verb map that toggles membership. The returned per-NPC entries
--   are merged by `addPartyVerbEntry`.
compileParty :: Map.Map String E.VerbDef -> [ANPC]
             -> ( [CompileIssue]
                , Map.Map E.NPCID ((E.VerbPhase, E.Verb, String), E.Effect)
                , Map.Map String E.VarDef
                , Map.Map String E.VariableValue )
compileParty registry npcs =
    let parties = [(n, p) | n <- npcs, Just p <- [anParty n], aptCanJoin p]
        varName n = "party." ++ anId n
        varDefs = Map.fromList
            [ (varName n, E.VarDef (varName n) (E.VTInt (Just 0) (Just 1)) (E.VVInt 0) [])
            | (n, _) <- parties ]
        initials = Map.fromList [(varName n, E.VVInt 0) | (n, _) <- parties]
        toggle n p = E.Conditional (E.CompareVar (varName n) E.CGte 1)
            (E.Sequence [ E.SendMessage (fromMaybe (anName n ++ " stays behind.") (aptStayMsg p))
                        , E.SetValue (E.VRVariable (varName n)) (E.EVInt 0) ])
            (E.Sequence [ E.SendMessage (fromMaybe (anName n ++ " falls in behind you.") (aptFollowMsg p))
                        , E.SetValue (E.VRVariable (varName n)) (E.EVInt 1) ])
        entries = Map.fromList
            [ (anId n, ((E.PhaseAfter, E.VCustom verb, anState n), toggle n p))
            | (n, p) <- parties
            , Just verb <- [resolveOrderVerb registry (aptOrderVerb p)] ]
        errors = concat
            [ (if resolveOrderVerb registry (aptOrderVerb p) == Nothing
               then [ ciError ("npcs." ++ anId n ++ ".party.order_verb") "PartyOrderVerbUnknown"
                        ("party order verb '" ++ aptOrderVerb p ++ "' is not a declared adventure verb") ]
               else [])
              ++ (if aptHpTracked p && anMaxHealth n == Nothing
                  then [ ciError ("npcs." ++ anId n ++ ".party") "PartyHealthMissing"
                            ("party npc '" ++ anId n ++ "' tracks hp but declares no max_hp") ]
                  else [])
            | (n, p) <- parties ]
    in (errors, entries, varDefs, initials)

-- | Canonical custom-verb name for an order verb. It must resolve to a
--   *declared* adventure verb: a core verb is rejected, because it would
--   shadow the built-in behaviour for that NPC.
resolveOrderVerb :: Map.Map String E.VerbDef -> String -> Maybe String
resolveOrderVerb registry w = case Verbs.resolveVerb registry w of
    Just (E.VCustom name) -> Just name
    _                     -> Nothing

-- | Merge an NPC's compiled party order verb into its def. An authored entry
--   with the same `(verb, state)` key runs first; the membership toggle runs
--   after it.
addPartyVerbEntry :: Map.Map E.NPCID ((E.VerbPhase, E.Verb, String), E.Effect) -> E.NPCID -> E.NPCDef -> E.NPCDef
addPartyVerbEntry entries nId def = case Map.lookup nId entries of
    Nothing -> def
    Just (key, eff) -> def
        { E.npcVerbMap = Map.insertWith (\new old -> E.Sequence [old, new]) key eff (E.npcVerbMap def) }

-- | Merge the party follow variables into the declared variables, rejecting a
--   clash (the author must not declare them themselves, e.g. under
--   `variables:`).
mergePartyVars :: Map.Map String E.VarDef -> Map.Map String E.VariableValue
               -> Map.Map String E.VarDef -> Map.Map String E.VariableValue
               -> ([CompileIssue], Map.Map String E.VarDef, Map.Map String E.VariableValue)
mergePartyVars varDefs varInitials partyDefs partyInitials =
    let clashErrs =
            [ ciError ("variables." ++ name) "PartyVariableClash"
                ("'" ++ name ++ "' is a party follow variable; it comes from the 'party:' block of that NPC")
            | name <- Map.keys varDefs
            , name `Map.member` partyDefs ]
    in (clashErrs, Map.union partyDefs varDefs, Map.union partyInitials varInitials)

-- ---------------------------------------------------------------------------
-- Ship systems and stations (Phase 7h)
-- ---------------------------------------------------------------------------

-- | Phase 7h: compile `systems:` and `stations:` on vehicles.
--   Systems become VarMap entries `ship.<vehicleId>.<name>` (no new state
--   field, no new save file); every station becomes a trigger on its verb,
--   gated to the interior room the player has to stand in
--   (`{ at: player, room: <room> }`).
compileShipSystems :: Map.Map String E.VerbDef -> [AVehicle]
                   -> ([CompileIssue], [E.TriggerDef], Map.Map String E.VarDef, Map.Map String E.VariableValue)
compileShipSystems registry vehicles =
    let sysVar v name = "ship." ++ avId v ++ "." ++ name
        entries = [ (v, name, spec)
                  | v <- vehicles, (name, spec) <- Map.toList (avSystems v) ]
        varDefs = Map.fromList
            [ (sysVar v name, E.VarDef (sysVar v name) (E.VTInt (Just 0) (bound spec)) (E.VVInt (asInitial spec)) [])
            | (v, name, spec) <- entries ]
        initials = Map.fromList
            [ (sysVar v name, E.VVInt (asInitial spec))
            | (v, name, spec) <- entries ]
        bound spec = if asMax spec > 0 then Just (asMax spec) else Nothing
        roomIds v = map arId (avInterior v)
        stationDefs =
            [ E.TriggerDef
                ( "ship." ++ avId v ++ ".station." ++ astRoom st )
                (E.OnCommand verb)
                (Just (E.PAll ([E.Location E.ActorPlayer (astRoom st)] ++ maybe [] (:[]) (astWhen st))))
                [ compileOutcomes (astEffects st) ]
                False
                0
                1
                []
                []
            | v <- vehicles
            , st <- avStations v
            , Just verb <- [resolveStationVerb registry (astVerb st)] ]
        errors = concat
            [ [ ciError ("vehicles." ++ avId v ++ ".stations") "UnknownStationRoom"
                  ("station room '" ++ astRoom st ++ "' is not an interior room of '" ++ avId v ++ "'")
              | st <- avStations v, astRoom st `notElem` roomIds v ]
              ++ [ ciError ("vehicles." ++ avId v ++ ".stations") "UnknownStationVerb"
                     ("station verb '" ++ astVerb st ++ "' is not a declared adventure verb")
                 | st <- avStations v, resolveStationVerb registry (astVerb st) == Nothing ]
            | v <- vehicles ]
    in (errors, stationDefs, varDefs, initials)

-- | Canonical custom-verb name of a station verb. Core verbs are rejected:
--   a station must not shadow built-in behaviour for its room.
resolveStationVerb :: Map.Map String E.VerbDef -> String -> Maybe String
resolveStationVerb registry w = case Verbs.resolveVerb registry w of
    Just (E.VCustom name) -> Just name
    _                     -> Nothing

-- | Merge the ship systems into the declared variables, rejecting a clash
--   (the author must not declare them themselves, e.g. under `variables:`).
mergeShipVars :: Map.Map String E.VarDef -> Map.Map String E.VariableValue
              -> Map.Map String E.VarDef -> Map.Map String E.VariableValue
              -> ([CompileIssue], Map.Map String E.VarDef, Map.Map String E.VariableValue)
mergeShipVars varDefs varInitials shipDefs shipInitials =
    let clashErrs =
            [ ciError ("variables." ++ name) "ShipVariableClash"
                ("'" ++ name ++ "' is a ship system; it comes from the 'systems:' block of that vehicle")
            | name <- Map.keys varDefs
            , name `Map.member` shipDefs ]
    in (clashErrs, Map.union shipDefs varDefs, Map.union shipInitials varInitials)

-- ---------------------------------------------------------------------------
-- Combat (Phase 7f)
-- ---------------------------------------------------------------------------

-- | Compile the `combat:` segment into the engine's CombatProfile.
--   Nothing -> CombatClassic (the default; without a block everything stays
--   bit-identical to pre-7f behaviour). Rejects unknown profiles and the
--   not-yet-implemented `tactical` profile.
compileCombat :: Maybe ACombat -> ([CompileIssue], E.CombatProfile)
compileCombat Nothing = ([], E.CombatClassic Nothing)
compileCombat (Just ac) = case acProfile ac of
    "off"       -> ([], E.CombatOff (acAttackRefused ac))
    "classic"   -> (screenErrs, E.CombatClassic (compileCombatScreen (acScreen ac)))
    "narrative" -> ([], E.CombatNarrative (E.NarrativeCombat
                        (acDifficulty ac)
                        (compileOutcomes (acOnWin ac))
                        (compileOutcomes (acOnLose ac))))
    "tactical"  ->
        let (initErrs, initRule) = case acInitiative ac of
                Nothing -> ([], E.PlayerFirst)
                Just s  -> case map toLower s of
                    "player_first" -> ([], E.PlayerFirst)
                    "player-first" -> ([], E.PlayerFirst)
                    "playerfirst"  -> ([], E.PlayerFirst)
                    "enemy_first"  -> ([], E.EnemyFirst)
                    "enemy-first"  -> ([], E.EnemyFirst)
                    "enemyfirst"   -> ([], E.EnemyFirst)
                    "npc_first"    -> ([], E.EnemyFirst)
                    "npc-first"    -> ([], E.EnemyFirst)
                    "npcfirst"     -> ([], E.EnemyFirst)
                    "by_speed"     -> ([], E.BySpeed)
                    "by-speed"     -> ([], E.BySpeed)
                    "byspeed"      -> ([], E.BySpeed)
                    other          -> ([ ciError "combat.initiative" "UnknownInitiativeRule"
                                           ("unknown initiative rule '" ++ other ++ "' (expected player_first | enemy_first | by_speed)") ]
                                      , E.PlayerFirst)
            flee = fromMaybe True (acFleeAllowed ac)
            rounds = fromMaybe 100 (acMaxRounds ac)
            speedAttr = fromMaybe "speed" (acSpeedAttribute ac)
        in (initErrs, E.CombatTactical (E.TacticalCombat initRule flee rounds speedAttr))
    other       -> ([ ciError "combat.profile" "UnknownCombatProfile"
                        ("unknown combat profile '" ++ other ++ "' (expected off | narrative | classic | tactical)") ]
                    , E.CombatClassic Nothing)
  where
    -- The screen belongs to the `classic` profile; a width below 1 cell would
    -- draw a bar that shows nothing.
    screenErrs =
        [ ciError "combat.screen.bar_width" "BadScreenBarWidth"
            ("bar_width must be at least 1 (got " ++ show (asBarWidth s) ++ ")")
        | Just s <- [acScreen ac], asBarWidth s < 1 ]

-- | Compile the authored `combat.screen` block. `Nothing` keeps the classic
--   output bit-identical (no screen at all).
compileCombatScreen :: Maybe ACombatScreen -> Maybe E.CombatScreen
compileCombatScreen Nothing  = Nothing
compileCombatScreen (Just s) = Just E.CombatScreen
    { E.csArt      = compileAscii (asArt s)
    , E.csBarWidth = asBarWidth s
    , E.csScene    = asScene s
    , E.csFooter   = asFooter s
    }

-- | `combat.` is the engine's namespace for the combat round state (Phase 7f-3,
--   step A1): the engine owns `combat.round` and `combat.engaged`, so an
--   author-declared variable in that namespace would silently collide with them.
--
--   Checked against the **author-declared** variables (`variables:`) rather than
--   the merged set, so the rule keeps holding once 7f-3 emits the engine's own
--   entries — a "clash table" against the merged set would then flag the
--   engine's own definitions.
-- ---------------------------------------------------------------------------
-- | W3: compile `chapters:` — order preserved (auto-gate tie-break and
--   `next_chapter` direction; Gameplay-Vertrag: nicht umsortieren).
compileChapters :: [AChapterDef] -> [E.ChapterDef]
compileChapters cs =
    [ E.ChapterDef (achId c) (achIntro c) (achWhen c) | c <- cs ]

-- | Pursuit (Tür IV): `pursuit:` entries become one `on: turn` trigger each —
--   emitted in npc-id order (the trigger list order is semantically live, so
--   a second compile run must be byte-identical). Checks: unknown pursuer
--   (MissingNPC), unknown `ignores:` value (UnknownPursuitIgnore).
compilePursuit :: [APursuitEntry] -> [String] -> ([CompileIssue], [E.TriggerDef])
compilePursuit entries knownNpcs = (errs, map mkTrigger (sortOn apeNpc entries))
  where
    errs =
        [ ciError ("pursuit." ++ apeNpc e) "MissingNPC"
            ("pursuit references npc '" ++ apeNpc e
                ++ "', which is not declared under 'npcs:'")
        | e <- entries, apeNpc e `notElem` knownNpcs ]
        ++
        [ ciError ("pursuit." ++ apeNpc e) "UnknownPursuitIgnore"
            ("ignores: '" ++ v ++ "' is not a pursuit ignore (use 'locked' or 'guarded')")
        | e <- entries, v <- apeIgnores e, v `notElem` ["locked", "guarded"] ]
    mkTrigger e = E.TriggerDef
        { E.trId = "pursuit_" ++ apeNpc e
        , E.trEvent = E.OnTurn
        , E.trCondition = Nothing
        , E.trEffects =
            [ E.StepToward (E.ActorNPC (apeNpc e)) (apeTarget e) opts (apeMsg e) ]
        , E.trOnce = False
        , E.trCooldown = 0
        , E.trWeight = 1
        , E.trRequires = []
        , E.trChainsTo = []
        }
      where
        opts = E.PursuitOptions
            ("locked" `elem` apeIgnores e)
            ("guarded" `elem` apeIgnores e)

-- | W3 checks: duplicate ids, unknown `goto_chapter` targets, statically
--   recognizable backward jumps (a `goto_chapter: X` inside the `on: chapter
--   Y` rule of a later chapter X>Y in declaration order). Unreachable
--   chapters (no `when:`, never a goto target, not the first) are a
--   **warning**, returned separately (they must not block compilation).
checkChapterRefs :: [AChapterDef] -> Adventure -> ([CompileIssue], [CompileIssue])
checkChapterRefs cs adv = (dupErrs ++ targetErrs ++ backwardErrs, unreachableWarns)
  where
    ids = map achId cs
    indexMap = Map.fromList (zip ids [0 :: Int ..])
    dupErrs =
        [ ciError ("chapters." ++ cid) "DuplicateChapter"
            ("chapter '" ++ cid ++ "' is declared more than once")
        | (cid, n) <- Map.toList (Map.fromListWith (+) [ (i, 1 :: Int) | i <- ids ])
        , n > 1 ]
    targetErrs =
        [ ciError "outcomes.goto_chapter" "UnknownChapter"
            ("goto_chapter target '" ++ t ++ "' is not declared under 'chapters:'")
        | t <- nub (concatMap gotoTargets (allAOutcomes adv))
        , t `notElem` ids ]
    gotoTargets ao = case ao of
        AOGotoChapter t  -> [t]
        AOConditional _ ts es -> concatMap gotoTargets ts ++ concatMap gotoTargets es
        AONarrative _ follow  -> concatMap gotoTargets follow
        AORandomChoice _ cs'  -> concatMap (concatMap gotoTargets . snd) cs'
        AOApplyCondition _ _ t e _ -> concatMap gotoTargets t ++ concatMap gotoTargets e
        _ -> []
    -- A `goto_chapter: X` fired from chapter Y's own on: chapter rule set is
    -- a backward jump when X is declared before Y (no retrospection, W3).
    backwardErrs =
        [ ciError ("chapters." ++ src) "ChapterBackwardsJump"
            ("goto_chapter '" ++ tgt ++ "' from chapter '" ++ src
             ++ "' is a backward jump — retrospection is forbidden (W3)")
        | t <- advTriggers adv
        , let on = atOn t
        , let effs = atEffects t
        , ("chapter", src) <- [breakOn on :: (String, String)]
        , tgt <- concatMap gotoTargets effs
        , Just iSrc <- [Map.lookup src indexMap], Just iTgt <- [Map.lookup tgt indexMap]
        , iTgt <= iSrc ]
      where
        breakOn onStr = case words onStr of
            ["chapter", cid] -> ("chapter", cid)
            _                -> ("", "")
    unreachableWarns =
        [ ciWarning ("chapters." ++ achId c) "UnreachableChapter"
            ("chapter '" ++ achId c
             ++ "' has no auto-gate, is never a goto target, and is not the first chapter")
        | c <- drop 1 cs
        , Nothing <- [achWhen c]
        , achId c `notElem` concatMap gotoTargets (allAOutcomes adv) ]

-- | W1: author-facing actor reference - "player", "ship:<id>" or an NPC id
--   (same convention as the engine's parseActorString).
compileActorRef :: String -> E.ActorRef
compileActorRef = E.parseActorString

-- | W1 (Befehl `kombiniere`, W1.4): generate one trigger per combine entry
--   and argument shape — `kombiniere a b` and `kombiniere a mit b`, both
--   orders — gated on the player actually knowing the premises (detective
--   fairness). Emitted in declaration order (deterministic emission, W1
--   rule 5); the unmatched-input case falls through to the engine's
--   unknown-command answer. The verb word is injected into the registry.
compileKnowledgeTriggers
    :: String                      -- ^ combine verb word
    -> [ACombineDef]               -- ^ combine entries (declaration order)
    -> Map.Map String AFactDef     -- ^ declared facts by id (keys for matching)
    -> ([E.TriggerDef], Map.Map String E.VerbDef)
compileKnowledgeTriggers verb cs factsById =
    ( successTriggers ++ [fallback]
    , Map.singleton verb (E.VerbDef verb aliases) )
  where
    -- The fallback fires only when no success arm matches (exclusive by
    -- construction) and answers with a generic message - otherwise a wrong
    -- `kombiniere` command would be answered with silence.
    fallback = E.TriggerDef
        { E.trId = "combine.fallback"
        , E.trEvent = E.OnCommand verb
        , E.trCondition = Just (PNot (PAny
            ( [ matchPred (head (acdFacts c)) (acdFacts c !! 1) | c <- pairs ]
              ++ [ singlePred (head (acdFacts c)) | c <- singles ] )))
        , E.trEffects = [ E.SendMessage "You cannot combine these like that." ]
        , E.trOnce = False
        , E.trCooldown = 0
        , E.trWeight = 1
        , E.trRequires = []
        , E.trChainsTo = []
        }
    pairs = [ c | c <- cs, length (acdFacts c) == 2 ]
    singles = [ c | c <- cs, length (acdFacts c) == 1 ]
    aliases = if verb == "kombiniere" then ["combine"] else []
    keysFor f = case Map.lookup f factsById of
        Just fd -> nub (f : afdKeys fd)
        Nothing -> [f]
    matchVar var f = PAny [ VarIs var k | k <- keysFor f ]
    arm v1 v2 a b = PAll
        [ matchVar v1 a, matchVar v2 b
        , Knows ActorPlayer a, Knows ActorPlayer b ]
    matchPred a b = PAny
        [ arm "cmd.arg1" "cmd.arg2" a b, arm "cmd.arg1" "cmd.arg3" a b
        , arm "cmd.arg1" "cmd.arg2" b a, arm "cmd.arg1" "cmd.arg3" b a ]
    singlePred a = PAll [ matchVar "cmd.arg1" a, Knows ActorPlayer a ]
    effects c = case acdMsg c of
        Just m  -> [ E.SendMessage m, E.Learn E.ActorPlayer (acdYields c) ]
        Nothing -> [ E.Learn E.ActorPlayer (acdYields c) ]
    triggerOf c = E.TriggerDef
        { E.trId = "combine." ++ acdYields c
        , E.trEvent = E.OnCommand verb
        , E.trCondition = Just (matchPred (head (acdFacts c))
                                          (acdFacts c !! 1))
        , E.trEffects = effects c
        , E.trOnce = False
        , E.trCooldown = 0
        , E.trWeight = 1
        , E.trRequires = []
        , E.trChainsTo = []
        }
    pairTriggers = [ triggerOf c | c <- pairs ]
    singleTriggers =
        [ E.TriggerDef
            { E.trId = "combine." ++ acdYields c
            , E.trEvent = E.OnCommand verb
            , E.trCondition = Just (singlePred (head (acdFacts c)))
            , E.trEffects = effects c
            , E.trOnce = False
            , E.trCooldown = 0
            , E.trWeight = 1
            , E.trRequires = []
            , E.trChainsTo = []
            }
       | c <- singles ]
    successTriggers = pairTriggers ++ singleTriggers

-- | W1 (Befehl `notizen`, W1.5): generate the notes command trigger when the
--   adventure opts in (`journal: notes`); verb injected into the registry.
compileNotesTrigger :: String -> [E.TriggerDef]
compileNotesTrigger verb =
    [ E.TriggerDef
        { E.trId = "notes"
        , E.trEvent = E.OnCommand verb
        , E.trCondition = Nothing
        , E.trEffects = [E.ShowNotes]
        , E.trOnce = False
        , E.trCooldown = 0
        , E.trWeight = 1
        , E.trRequires = []
        , E.trChainsTo = []
        } ]

-- W1: knowledge model (facts: / combine:)
-- ---------------------------------------------------------------------------

-- | Compile `facts:` entries — order is preserved ('[E.FactDef]', not a Map):
--   the notes book shows facts in declaration order (Gameplay-Vertrag).
compileFacts :: [AFactDef] -> [E.FactDef]
compileFacts fdefs =
    [ E.FactDef (afdId f) (afdKeys f) (afdText f)
                (afdSource f) (afdTag f) (afdLearnMsg f) (afdSilent f)
    | f <- fdefs ]

-- | Compile `combine:` entries (order preserved — the cascade walks the
--   table in declaration order).
compileCombines :: [ACombineDef] -> [E.CombineDef]
compileCombines cs =
    [ E.CombineDef (acdFacts c) (acdYields c) (acdMsg c) | c <- cs ]

-- | K9: statements with speaker and truth value
compileStatements :: [AStatement] -> [E.StatementDef]
compileStatements sdefs =
    [ E.StatementDef (stId s) (stSpeaker s) (stClaims s)
                     (stTruth s) (stText s) (stWhen s) (stTag s)
    | s <- sdefs ]

-- | Validate the knowledge model: unknown fact or statement references (premises, yields,
--   'knows:' predicates, learn:/forget: outcomes, combine premises), a yields
--   without premises, duplicate fact ids, duplicate statement ids, and clashing fact/statement ids.
--   Walked over the raw outcomes (via 'allAOutcomes') so rules, procedures and rooms are all covered.
checkFactRefs :: [AFactDef] -> [AStatement] -> E.GameWorld -> Adventure -> [CompileIssue]
checkFactRefs fdefs sdefs gw adv =
    dupErrs ++ dupStmtErrs ++ clashErrs ++ yieldsErrs ++ refErrs ++ predErrs
  where
    factIds = Set.fromList (map afdId fdefs)
    statementIds = Set.fromList (map stId sdefs)
    allKnowledgeIds = Set.union factIds statementIds
    dupErrs =
        [ ciError ("facts." ++ fid) "DuplicateFact"
            ("fact '" ++ fid ++ "' is declared more than once")
        | fid <- Map.keys dupMap ]
      where
        dupMap = Map.filter (> (1 :: Int))
            (Map.fromListWith (+) [ (afdId f, 1 :: Int) | f <- fdefs ])
    dupStmtErrs =
        [ ciError ("statements." ++ sid) "DuplicateStatement"
            ("statement '" ++ sid ++ "' is declared more than once")
        | sid <- Map.keys dupStmtMap ]
      where
        dupStmtMap = Map.filter (> (1 :: Int))
            (Map.fromListWith (+) [ (stId s, 1 :: Int) | s <- sdefs ])
    clashErrs =
        [ ciError ("statements." ++ sid) "StatementFactClash"
            ("'" ++ sid ++ "' is declared as both a fact and a statement")
        | sid <- Set.toList (Set.intersection factIds statementIds) ]
    yieldsErrs = concat
        [ [ ciError ("combine." ++ cdYields c) "YieldsWithoutPremises"
                ("combine entry for '" ++ cdYields c
                 ++ "' has no premises - it would learn unconditionally")
          | null (cdFacts c) ]
          ++ [ ciError ("combine." ++ cdYields c) "UnknownFact"
                 ("combine yields fact '" ++ cdYields c
                  ++ "' is not declared under 'facts:'")
             | cdYields c `Set.notMember` factIds ]
        | c <- E.combineDefs gw ]
    refErrs =
        [ ciError ("outcomes." ++ kind) "UnknownFact"
            ("'" ++ kind ++ "' references undeclared fact '" ++ f ++ "'")
        | (kind, f) <- nub (concatMap factRefsIn (allAOutcomes adv))
        , f `Set.notMember` allKnowledgeIds ]
    predErrs =
        [ ciError "predicates.knows" "UnknownFact"
            ("'knows' references undeclared fact '" ++ f ++ "'")
        | f <- nub (concatMap knowsInPredicate (allWorldPredicates gw))
        , f `Set.notMember` allKnowledgeIds ]
    knowsInPredicate p = case p of
        E.Knows _ f -> [f]
        E.PNot q    -> knowsInPredicate q
        E.PAll qs   -> concatMap knowsInPredicate qs
        E.PAny qs  -> concatMap knowsInPredicate qs
        _           -> []
    factRefsIn ao = case ao of
        AOLearn f _  -> [("learn", f)]
        AOForget f _ -> [("forget", f)]
        AOConditional _ ts es -> concatMap factRefsIn ts ++ concatMap factRefsIn es
        AONarrative _ follow  -> concatMap factRefsIn follow
        AORandomChoice _ cs   -> concatMap (concatMap factRefsIn . snd) cs
        AOApplyCondition _ _ t e _ -> concatMap factRefsIn t ++ concatMap factRefsIn e
        _ -> []

-- | W1: `known.` belongs to the knowledge model — author-declared variables
--   in this namespace would collide with learned facts.
--   K9: `statement.` belongs to statement metadata variables.
checkKnownVarReserved :: Map.Map String E.VarDef -> [CompileIssue]
checkKnownVarReserved varDefs =
    [ ciError ("variables." ++ name) "KnownVariableClash"
        ("'" ++ name ++ "' is in the reserved 'known.' namespace; "
         ++ "the engine owns the learned-fact state (W1)")
    | name <- Map.keys varDefs, "known." `isPrefixOf` name ]
    ++
    [ ciError ("variables." ++ name) "StatementVariableClash"
        ("'" ++ name ++ "' is in the reserved 'statement.' namespace; "
         ++ "the engine owns the statement metadata variables (K9)")
    | name <- Map.keys varDefs, "statement." `isPrefixOf` name ]

-- ---------------------------------------------------------------------------
-- W4: Interactive Devices / Fixtures (Hebel / Halterung)
-- ---------------------------------------------------------------------------

compileDeviceActorRef :: String -> E.ActorRef
compileDeviceActorRef "player" = E.ActorPlayer
compileDeviceActorRef s        = E.ActorEntity s

-- | 4.4: stationary containers. The live state (open/locked) lives in
--   entityStates ("open"/"closed"/"locked") — no new save field. Returns the
--   defs, the initial states and the errors (duplicate id, missing room).
compileContainers :: Map.Map String E.Room -> [AContainerDef]
                   -> ([CompileIssue], Map.Map String E.ContainerDef, [(String, String)])
compileContainers allRooms cons = (errs, Map.fromList [ (acnId c, toDef c) | c <- cons ], initials)
  where
    toDef c = E.ContainerDef (acnId c)
                (if null (acnName c) then acnId c else acnName c)
                (acnLocation c)
                (E.ContainerState (acnOpen c) (acnLocked c) (acnCapacity c))
    initials =
        [ (acnId c, if acnLocked c then "locked" else if acnOpen c then "open" else "closed")
        | c <- cons ]
    ids = map acnId cons
    errs =
        [ ciError ("containers." ++ acnId c) "DuplicateContainer"
            ("container id '" ++ acnId c ++ "' is declared more than once")
        | c <- cons, length (filter (== acnId c) ids) > 1 ]
        ++
        [ ciError ("containers." ++ acnId c) "MissingRoom"
            ("container '" ++ acnId c ++ "' sits in unknown room '"
                ++ acnLocation c ++ "'")
        | c <- cons, acnLocation c `notElem` map roomId (Map.elems allRooms) ]

compileDevices :: [ADeviceDef] -> Map.Map String E.DeviceDef
compileDevices devs = Map.fromList
    [ (adId d, E.DeviceDef
        { E.devId          = adId d
        , E.devName        = fromMaybe (adId d) (adName d)
        , E.devKeys        = if null (adKeys d) then [adId d] else adKeys d
        , E.devLocation    = adLocation d
        , E.devDescription = adDescription d
        , E.devFitsTag     = adFitsTag d
        , E.devFits        = adFits d
        , E.devInsertMsg   = adInsertMsg d
        , E.devRemoveMsg   = adRemoveMsg d
        , E.devOnInsert    = map compileAActionOutcome (adOnInsert d)
        , E.devOnRemove    = map compileAActionOutcome (adOnRemove d)
        , E.devFlipVerb    = adFlipVerb d
        , E.devFlipStates  = adFlipStates d
        , E.devOnFlip      = Map.fromList [ (st, map compileAActionOutcome effs) | (st, effs) <- adOnFlip d ]
        })
    | d <- devs
    ]

checkDeviceRefs :: [ADeviceDef] -> Adventure -> ([CompileIssue], [CompileIssue])
checkDeviceRefs devs adv =
    let roomIds = Set.fromList (map arId (advRooms adv))
        itemIds = Set.fromList (map aiId (advItems adv))
        allTags = Set.fromList (concatMap aiTags (advItems adv))
        devIds = map adId devs
        dupIssues =
            [ ciError ("devices." ++ did) "DuplicateDevice"
                ("device '" ++ did ++ "' is declared more than once")
            | (did, n) <- Map.toList (Map.fromListWith (+) [ (i, 1 :: Int) | i <- devIds ])
            , n > 1 ]
        locIssues =
            [ ciError ("devices." ++ adId d ++ ".location") "UnknownDeviceLocation"
                ("device '" ++ adId d ++ "' references unknown location '" ++ adLocation d ++ "'")
            | d <- devs
            , adLocation d `Set.notMember` roomIds ]
        itemIssues =
            [ ciError ("devices." ++ adId d ++ ".fits") "UnknownDeviceItem"
                ("device '" ++ adId d ++ "' references unknown item '" ++ it ++ "' in fits")
            | d <- devs
            , it <- adFits d
            , it `Set.notMember` itemIds ]
        flipIssues =
            [ ciError ("devices." ++ adId d ++ ".flip_states") "DeviceFlipStateCount"
                ("device '" ++ adId d ++ "' specifies flip_verb '" ++ fv ++ "' but has fewer than 2 flip_states")
            | d <- devs
            , Just fv <- [adFlipVerb d]
            , length (adFlipStates d) < 2 ]
        tagWarns =
            [ ciWarning ("devices." ++ adId d ++ ".fits_tag") "UnknownDeviceTag"
                ("device '" ++ adId d ++ "' specifies fits_tag '" ++ tag ++ "', which matches no declared items")
            | d <- devs
            , Just tag <- [adFitsTag d]
            , tag `Set.notMember` allTags ]
        noEffWarns =
            [ ciWarning ("devices." ++ adId d) "DeviceWithoutEffects"
                ("device '" ++ adId d ++ "' has no insert, remove, or flip effects")
            | d <- devs
            , null (adOnInsert d)
            , null (adOnRemove d)
            , null (adOnFlip d)
            , isNothing (adFlipVerb d) ]
        hardErrors = dupIssues ++ locIssues ++ itemIssues ++ flipIssues
        warnings = tagWarns ++ noEffWarns
    in (hardErrors, warnings)

compileDeviceTriggers
    :: [ADeviceDef]
    -> [AItem]
    -> ([E.TriggerDef], Map.Map String E.VerbDef)
compileDeviceTriggers devs items
    | null devs = ([], Map.empty)
    | otherwise =
        let hasHolders = any (\d -> not (null (adFits d)) || isJust (adFitsTag d) || not (null (adOnInsert d)) || not (null (adOnRemove d))) devs
            distinctFlipVerbs = nub [ fv | d <- devs, Just fv <- [adFlipVerb d], length (adFlipStates d) >= 2 ]

            insertVerbDef = ("stecke", E.VerbDef "stecke" ["insert"])
            removeVerbDef = ("ziehe", E.VerbDef "ziehe" ["remove"])
            flipVerbDefs = [ (fv, E.VerbDef fv (flipAliases fv)) | fv <- distinctFlipVerbs ]
            flipAliases "umlegen" = ["flip"]
            flipAliases "flip"    = ["umlegen"]
            flipAliases _         = []

            verbsToInject = Map.fromList $
                (if hasHolders then [insertVerbDef, removeVerbDef] else [])
                ++ flipVerbDefs

            (allInsertTrigs, allRemoveTrigs) = if hasHolders
                then (concatMap compileInsertTriggers devs ++ [insertFallback], concatMap compileRemoveTriggers devs ++ [removeFallback])
                else ([], [])

            allFlipTrigs = concatMap (compileFlipTriggers devs) distinctFlipVerbs

            insertFallback =
                let handled = [ c | t <- concatMap compileInsertTriggers devs, Just c <- [E.trCondition t] ]
                in E.TriggerDef
                    { E.trId = "device.insert.fallback"
                    , E.trEvent = E.OnBefore "stecke"
                    , E.trCondition = Just (E.PNot (E.PAny handled))
                    , E.trEffects = [ E.Block (Just "You cannot insert that.") False ]
                    , E.trOnce = False
                    , E.trCooldown = 0
                    , E.trWeight = 1
                    , E.trRequires = []
                    , E.trChainsTo = []
                    }

            removeFallback =
                let handled = [ c | t <- concatMap compileRemoveTriggers devs, Just c <- [E.trCondition t] ]
                in E.TriggerDef
                    { E.trId = "device.remove.fallback"
                    , E.trEvent = E.OnBefore "ziehe"
                    , E.trCondition = Just (E.PNot (E.PAny handled))
                    , E.trEffects = [ E.Block (Just "You cannot remove that.") False ]
                    , E.trOnce = False
                    , E.trCooldown = 0
                    , E.trWeight = 1
                    , E.trRequires = []
                    , E.trChainsTo = []
                    }
        in (allInsertTrigs ++ allRemoveTrigs ++ allFlipTrigs, verbsToInject)
  where
    compileInsertTriggers d =
        let dId = adId d
            dLoc = adLocation d
            dName = fromMaybe dId (adName d)
            dKeys = if null (adKeys d) then [dId] else nub (dId : adKeys d)
            matchDevVar v = E.PAny [ E.VarIs v k | k <- dKeys ]
            argMatchDev = E.PAny [ matchDevVar "cmd.arg2", matchDevVar "cmd.arg3" ]
            allItemIds = [ aiId i | i <- items ]
            deviceOccupiedPred = E.PAny [ E.ActorHas (E.ActorEntity dId) i | i <- allItemIds ]

            itemFits it = (aiId it `elem` adFits d) || maybe False (\t -> t `elem` aiTags it) (adFitsTag d)
            fitting = filter itemFits items
            nonFitting = filter (not . itemFits) items

            occupiedTrig = E.TriggerDef
                { E.trId = "device." ++ dId ++ ".occupied"
                , E.trEvent = E.OnBefore "stecke"
                , E.trCondition = Just (E.PAll [ E.Location E.ActorPlayer dLoc, argMatchDev, deviceOccupiedPred ])
                , E.trEffects = [ E.Block (Just ("There is already something in the " ++ dName ++ ".")) False ]
                , E.trOnce = False
                , E.trCooldown = 0
                , E.trWeight = 1
                , E.trRequires = []
                , E.trChainsTo = []
                }

            rejectTrig it =
                let itKeys = nub (aiId it : aiKeywords it)
                    argMatchItem = E.PAny [ E.VarIs "cmd.arg1" k | k <- itKeys ]
                    itName = if null (aiName it) then aiId it else aiName it
                in E.TriggerDef
                    { E.trId = "device." ++ dId ++ ".reject." ++ aiId it
                    , E.trEvent = E.OnBefore "stecke"
                    , E.trCondition = Just (E.PAll
                        [ E.Location E.ActorPlayer dLoc
                        , argMatchDev
                        , argMatchItem
                        , E.PNot deviceOccupiedPred
                        ])
                    , E.trEffects = [ E.Block (Just ("The " ++ itName ++ " does not fit into the " ++ dName ++ ".")) False ]
                    , E.trOnce = False
                    , E.trCooldown = 0
                    , E.trWeight = 1
                    , E.trRequires = []
                    , E.trChainsTo = []
                    }

            notCarriedTrig it =
                let itKeys = nub (aiId it : aiKeywords it)
                    argMatchItem = E.PAny [ E.VarIs "cmd.arg1" k | k <- itKeys ]
                    itName = if null (aiName it) then aiId it else aiName it
                in E.TriggerDef
                    { E.trId = "device." ++ dId ++ ".not_carried." ++ aiId it
                    , E.trEvent = E.OnBefore "stecke"
                    , E.trCondition = Just (E.PAll
                        [ E.Location E.ActorPlayer dLoc
                        , argMatchDev
                        , argMatchItem
                        , E.PNot deviceOccupiedPred
                        , E.PNot (E.ActorHas E.ActorPlayer (aiId it))
                        ])
                    , E.trEffects = [ E.Block (Just ("You are not carrying " ++ itName ++ ".")) False ]
                    , E.trOnce = False
                    , E.trCooldown = 0
                    , E.trWeight = 1
                    , E.trRequires = []
                    , E.trChainsTo = []
                    }

            insertTrig it =
                let itKeys = nub (aiId it : aiKeywords it)
                    argMatchItem = E.PAny [ E.VarIs "cmd.arg1" k | k <- itKeys ]
                    itName = if null (aiName it) then aiId it else aiName it
                    insMsg = fromMaybe ("You insert the " ++ itName ++ " into the " ++ dName ++ ".") (adInsertMsg d)
                in E.TriggerDef
                    { E.trId = "device." ++ dId ++ ".insert." ++ aiId it
                    , E.trEvent = E.OnCommand "stecke"
                    , E.trCondition = Just (E.PAll
                        [ E.Location E.ActorPlayer dLoc
                        , argMatchDev
                        , argMatchItem
                        , E.PNot deviceOccupiedPred
                        , E.ActorHas E.ActorPlayer (aiId it)
                        ])
                    , E.trEffects = [ E.SendMessage insMsg, E.Mount (aiId it) (E.ActorEntity dId) ]
                                    ++ map compileAActionOutcome (adOnInsert d)
                    , E.trOnce = False
                    , E.trCooldown = 0
                    , E.trWeight = 1
                    , E.trRequires = []
                    , E.trChainsTo = []
                    }
        in [occupiedTrig]
           ++ map rejectTrig nonFitting
           ++ map notCarriedTrig fitting
           ++ map insertTrig fitting

    compileRemoveTriggers d =
        let dId = adId d
            dLoc = adLocation d
            dName = fromMaybe dId (adName d)
            dKeys = if null (adKeys d) then [dId] else nub (dId : adKeys d)
            matchDevVar v = E.PAny [ E.VarIs v k | k <- dKeys ]
            argMatchDev = E.PAny [ matchDevVar "cmd.arg2", matchDevVar "cmd.arg3" ]

            itemFits it = (aiId it `elem` adFits d) || maybe False (\t -> t `elem` aiTags it) (adFitsTag d)
            fitting = filter itemFits items

            removeTrig it =
                let itKeys = nub (aiId it : aiKeywords it)
                    argMatchItem = E.PAny [ E.VarIs "cmd.arg1" k | k <- itKeys ]
                    itName = if null (aiName it) then aiId it else aiName it
                    isMounted = E.ActorHas (E.ActorEntity dId) (aiId it)
                    removeMatch = E.PAny
                        [ E.PAll [ argMatchItem, argMatchDev ]
                        , E.PAll [ argMatchItem, E.PAny [ E.CompareVar "cmd.count" E.CEq 1, E.VarIs "cmd.arg2" "raus", E.VarIs "cmd.arg2" "out" ] ]
                        , E.PAll [ matchDevVar "cmd.arg1", E.CompareVar "cmd.count" E.CEq 1 ]
                        ]
                    remMsg = fromMaybe ("You remove the " ++ itName ++ " from the " ++ dName ++ ".") (adRemoveMsg d)
                in E.TriggerDef
                    { E.trId = "device." ++ dId ++ ".remove." ++ aiId it
                    , E.trEvent = E.OnCommand "ziehe"
                    , E.trCondition = Just (E.PAll
                        [ E.Location E.ActorPlayer dLoc
                        , removeMatch
                        , isMounted
                        ])
                    , E.trEffects = [ E.SendMessage remMsg, E.Unmount (aiId it) ]
                                    ++ map compileAActionOutcome (adOnRemove d)
                    , E.trOnce = False
                    , E.trCooldown = 0
                    , E.trWeight = 1
                    , E.trRequires = []
                    , E.trChainsTo = []
                    }

            notInDevTrig it =
                let itKeys = nub (aiId it : aiKeywords it)
                    argMatchItem = E.PAny [ E.VarIs "cmd.arg1" k | k <- itKeys ]
                    itName = if null (aiName it) then aiId it else aiName it
                    isMounted = E.ActorHas (E.ActorEntity dId) (aiId it)
                in E.TriggerDef
                    { E.trId = "device." ++ dId ++ ".not_in_dev." ++ aiId it
                    , E.trEvent = E.OnBefore "ziehe"
                    , E.trCondition = Just (E.PAll
                        [ E.Location E.ActorPlayer dLoc
                        , E.PAll [ argMatchItem, argMatchDev ]
                        , E.PNot isMounted
                        ])
                    , E.trEffects = [ E.Block (Just ("There is no " ++ itName ++ " in the " ++ dName ++ ".")) False ]
                    , E.trOnce = False
                    , E.trCooldown = 0
                    , E.trWeight = 1
                    , E.trRequires = []
                    , E.trChainsTo = []
                    }

            emptyTrig = E.TriggerDef
                { E.trId = "device." ++ dId ++ ".empty"
                , E.trEvent = E.OnBefore "ziehe"
                , E.trCondition = Just (E.PAll
                    [ E.Location E.ActorPlayer dLoc
                    , matchDevVar "cmd.arg1"
                    , E.CompareVar "cmd.count" E.CEq 1
                    , E.PNot (E.PAny [ E.ActorHas (E.ActorEntity dId) (aiId fit) | fit <- fitting ])
                    ])
                , E.trEffects = [ E.Block (Just ("There is nothing in the " ++ dName ++ ".")) False ]
                , E.trOnce = False
                , E.trCooldown = 0
                , E.trWeight = 1
                , E.trRequires = []
                , E.trChainsTo = []
                }
        in map removeTrig fitting
           ++ map notInDevTrig fitting
           ++ [emptyTrig]

    compileFlipTriggers devsForFv fv =
        let targetDevs = [ d | d <- devsForFv, adFlipVerb d == Just fv, length (adFlipStates d) >= 2 ]
            trigsForDev d =
                let dId = adId d
                    dLoc = adLocation d
                    dName = fromMaybe dId (adName d)
                    dKeys = if null (adKeys d) then [dId] else nub (dId : adKeys d)
                    matchDevVar v = E.PAny [ E.VarIs v k | k <- dKeys ]
                    flipMatch = E.PAny [ matchDevVar "cmd.arg1", E.CompareVar "cmd.count" E.CEq 0 ]
                    s1 = head (adFlipStates d)
                    s2 = adFlipStates d !! 1
                    authorS2 = case lookup s2 (adOnFlip d) of
                        Just effs -> map compileAActionOutcome effs
                        Nothing   -> []
                    authorS1 = case lookup s1 (adOnFlip d) of
                        Just effs -> map compileAActionOutcome effs
                        Nothing   -> []
                    flipMsgS2 = "You flip the " ++ dName ++ " to " ++ s2 ++ "."
                    flipMsgS1 = "You flip the " ++ dName ++ " to " ++ s1 ++ "."
                    toS2Effs = E.Sequence $
                        [ E.SetValue (E.VRActorProp (E.ActorEntity dId) E.PState) (E.EVString s2)
                        , E.SendMessage flipMsgS2
                        ] ++ authorS2
                    toS1Effs = E.Sequence $
                        [ E.SetValue (E.VRActorProp (E.ActorEntity dId) E.PState) (E.EVString s1)
                        , E.SendMessage flipMsgS1
                        ] ++ authorS1
                    toggleTrig = E.TriggerDef
                        { E.trId = "device." ++ dId ++ ".flip"
                        , E.trEvent = E.OnCommand fv
                        , E.trCondition = Just (E.PAll
                            [ E.Location E.ActorPlayer dLoc
                            , flipMatch
                            ])
                        , E.trEffects = [ E.Conditional (E.EntityHasState dId s2) toS1Effs toS2Effs ]
                        , E.trOnce = False
                        , E.trCooldown = 0
                        , E.trWeight = 1
                        , E.trRequires = []
                        , E.trChainsTo = []
                        }
                in [toggleTrig]
            devTrigs = concatMap trigsForDev targetDevs
            handled = [ c | t <- devTrigs, Just c <- [E.trCondition t] ]
            fallbackTrig = E.TriggerDef
                { E.trId = "device.flip." ++ fv ++ ".fallback"
                , E.trEvent = E.OnBefore fv
                , E.trCondition = Just (E.PNot (E.PAny handled))
                , E.trEffects = [ E.Block (Just "You cannot flip that.") False ]
                , E.trOnce = False
                , E.trCooldown = 0
                , E.trWeight = 1
                , E.trRequires = []
                , E.trChainsTo = []
                }
        in devTrigs ++ [fallbackTrig]

-- ---------------------------------------------------------------------------
-- W2: Player Progression (XP / Level)
-- ---------------------------------------------------------------------------

compileProgression :: Maybe AProgressionDef -> ([CompileIssue], Maybe E.ProgressionDef, Map.Map String E.VarDef, Map.Map String E.VariableValue)
compileProgression Nothing = ([], Nothing, Map.empty, Map.empty)
compileProgression (Just prog) =
    let levels = aplLevels prog
        emptyErrs = if null levels
                    then [ciError "progression.levels" "EmptyLevels" "progression: must define at least one level in 'levels:'"]
                    else []
        firstXpErrs = case levels of
            (l:_) | alXp l /= 0 -> [ciError "progression.levels.0" "BadLevelXp" "the first level must have xp: 0"]
            _                   -> []
        monoErrs = [ ciError ("progression.levels." ++ show idx) "NonMonotonicXp"
                        ("level " ++ show idx ++ " xp (" ++ show (alXp l2)
                         ++ ") must be strictly greater than level " ++ show (idx - 1)
                         ++ " xp (" ++ show (alXp l1) ++ ")")
                   | (idx, (l1, l2)) <- zip [2 :: Int ..] (zip levels (drop 1 levels))
                   , alXp l2 <= alXp l1
                   ]
        errs = emptyErrs ++ firstXpErrs ++ monoErrs
        compiledLevels = zipWith compileLevel [1 :: Int ..] levels
        compiledProg = if null emptyErrs then Just (E.ProgressionDef compiledLevels) else Nothing
        progDefs = Map.fromList
            [ ("xp.current",    E.VarDef "xp.current" (E.VTInt (Just 0) Nothing) (E.VVInt 0) [])
            , ("level.current", E.VarDef "level.current" (E.VTInt (Just 1) Nothing) (E.VVInt 1) [])
            , ("bonus.attack",  E.VarDef "bonus.attack" (E.VTInt Nothing Nothing) (E.VVInt 0) [])
            , ("bonus.defense", E.VarDef "bonus.defense" (E.VTInt Nothing Nothing) (E.VVInt 0) [])
            , ("bonus.hp",       E.VarDef "bonus.hp" (E.VTInt Nothing Nothing) (E.VVInt 0) [])
            ]
        progInitials = Map.fromList
            [ ("xp.current",    E.VVInt 0)
            , ("level.current", E.VVInt 1)
            , ("bonus.attack",  E.VVInt 0)
            , ("bonus.defense", E.VVInt 0)
            , ("bonus.hp",       E.VVInt 0)
            ]
    in (errs, compiledProg, progDefs, progInitials)
  where
    compileLevel idx l =
        E.LevelDef
            { E.lvlNumber  = fromMaybe idx (alLevel l)
            , E.lvlXp      = alXp l
            , E.lvlName    = alName l
            , E.lvlMsg     = alMsg l
            , E.lvlEffects = map compileAActionOutcome (alEffects l)
            }

mergeProgressionVars :: Map.Map String E.VarDef -> Map.Map String E.VariableValue
                     -> Map.Map String E.VarDef -> Map.Map String E.VariableValue
                     -> ([CompileIssue], Map.Map String E.VarDef, Map.Map String E.VariableValue)
mergeProgressionVars varDefs varInitials progDefs progInitials =
    let clashErrs =
            [ ciError ("variables." ++ name) "ProgressionVariableClash"
                ("'" ++ name ++ "' is owned by the progression system; it comes from the 'progression:' block (W2)")
            | name <- Map.keys varDefs
            , name `Map.member` progDefs ]
    in (clashErrs, Map.union progDefs varDefs, Map.union progInitials varInitials)

checkProgressionVarReserved :: Map.Map String E.VarDef -> [CompileIssue]
checkProgressionVarReserved varDefs =
    [ ciError ("variables." ++ name) "ProgressionVariableClash"
        ("'" ++ name ++ "' is in the reserved '" ++ prefix ++ "' namespace; "
         ++ "the engine owns progression and combat bonus state (W2)")
    | name <- Map.keys varDefs
    , prefix <- ["xp.", "level.", "bonus."]
    , prefix `isPrefixOf` name ]

hasGainXpOutcome :: AActionOutcome -> Bool
hasGainXpOutcome ao = case ao of
    AOGainXp _                   -> True
    AOConditional _ t e          -> any hasGainXpOutcome t || any hasGainXpOutcome e
    AONarrative _ f              -> any hasGainXpOutcome f
    AORandomChoice _ cs          -> any (any hasGainXpOutcome . snd) cs
    AOApplyCondition _ _ tk ed _ -> any hasGainXpOutcome tk || any hasGainXpOutcome ed
    _                            -> False

checkGainXpWithoutProgression :: Adventure -> [CompileIssue]
checkGainXpWithoutProgression adv =
    case advProgression adv of
        Just _  -> []
        Nothing ->
            if any hasGainXpOutcome (allAOutcomes adv)
            then [ ciWarning "outcomes.gain_xp" "GainXpWithoutProgression"
                     "adventure uses gain_xp but defines no progression: section" ]
            else []

-- ---------------------------------------------------------------------------
-- Phase 2.5 (D2): procedures
-- ---------------------------------------------------------------------------

-- | Variable namespaces the engine owns (mirrors the per-prefix clash checks:
--   `cmd.`, `combat.`, `env.`, `faction.`, `party.`, `patrol.`, `ship.`,
--   `stealth.`). Procedure parameters must not shadow them.
reservedVarPrefixes :: [String]
reservedVarPrefixes =
    ["bonus.", "chapter.", "cmd.", "combat.", "env.", "faction.", "known.", "level.", "party.", "patrol.", "ship.", "stealth.", "xp."]

-- | Compile `procedures:` entries. Duplicate ids and parameter names in
--   engine-owned variable namespaces are rejected here.
compileProcedures :: [AProcDef] -> ([CompileIssue], Map.Map String E.ProcDef)
compileProcedures procs = (dupErrs ++ paramErrs, compiled)
  where
    compiled = Map.fromList
        [ (apId p, E.ProcDef (apId p) (apParams p) (map compileAActionOutcome (apEffects p)))
        | p <- procs ]
    dupErrs =
        [ ciError ("procedures." ++ pid) "DuplicateProc"
            ("procedure '" ++ pid ++ "' is declared more than once")
        | pid <- Map.keys dupMap ]
      where
        dupMap = Map.filter (> (1 :: Int))
            (Map.fromListWith (+) [ (apId p, 1 :: Int) | p <- procs ])
    paramErrs =
        [ ciError ("procedures." ++ apId p ++ ".params." ++ pname) "ProcParamReserved"
            ("'" ++ pname ++ "' is in a reserved variable namespace (the engine owns it)")
        | p <- procs, pname <- apParams p
        , any (`isPrefixOf` pname) reservedVarPrefixes ]

-- | Validate `call:` sites against the declared procedures: unknown names and
--   arity mismatches are errors — and so is **recursion**: D2 forbids it
--   statically (a cycle in the call graph is a compile error; the runtime
--   `maxOutcomeDepth` guard is deliberately not relied upon for that).
checkProcRefs :: [AProcDef] -> Adventure -> [CompileIssue]
checkProcRefs procs adv = concatMap siteIssues callSites ++ recursionErrs
  where
    paramMap = Map.fromList [ (apId p, length (apParams p)) | p <- procs ]
    siteIssues (name, nArgs) = case Map.lookup name paramMap of
        Nothing ->
            [ ciError "outcomes.call" "UnknownProc"
                ("call to undeclared procedure '" ++ name ++ "'") ]
        Just nParams
            | nArgs == nParams -> []
            | otherwise ->
                [ ciError "outcomes.call" "ProcArity"
                    ("procedure '" ++ name ++ "' takes " ++ show nParams
                     ++ " arguments, but the call passes " ++ show nArgs) ]
    callSites = [ (name, length args) | AOCallProc name args <- allAOutcomes adv ]
    -- Call graph over the procedure bodies; 'callsIn' sees nested calls.
    graph = Map.fromList [ (apId p, callsIn (apEffects p)) | p <- procs ]
    callsIn = concatMap go
      where
        go (AOCallProc name _)          = [name]
        go (AOConditional _ ts es)      = callsIn ts ++ callsIn es
        go (AONarrative _ follow)       = callsIn follow
        go (AORandomChoice _ cs)        = concatMap (callsIn . snd) cs
        go (AOApplyCondition _ _ t e _) = callsIn t ++ callsIn e
        go _                            = []
    recursionErrs =
        [ ciError ("procedures." ++ p) "ProcRecursion"
            ("procedure '" ++ p ++ "' takes part in a call cycle — recursion is "
             ++ "statically forbidden (D2)")
        | p <- Map.keys graph, p `Set.member` reachableFrom p ]
    reachableFrom start = go Set.empty (Map.findWithDefault [] start graph)
      where
        go seen [] = seen
        go seen (x:xs)
            | x `Set.member` seen = go seen xs
            | otherwise = go (Set.insert x seen) (Map.findWithDefault [] x graph ++ xs)

-- | Phase 7f-3 (A3): the engine marks an ability's cooldown as a **condition**
--   named `cooldown_<abilityId>`. That prefix belongs to the engine — an author
--   condition of the same name would silently share the marker with a cooldown,
--   and the effect DSL cannot tell the two apart. Condition-side twin of
--   variable write protection ('checkReservedVarWrites').
--   Hinweis: Bleibt bewusst getrennt von der Variablen-Schreibschutz-Prüfung,
--   da Bedingungen (Conditions) eine andere semantische Ebene als Variablen sind.
checkCooldownConditionReserved :: E.GameWorld -> [CompileIssue]
checkCooldownConditionReserved gw =
    [ ciError ("conditions." ++ name) "CooldownConditionClash"
        ("'" ++ name ++ "' is in the reserved 'cooldown_' namespace; "
         ++ "the engine marks ability cooldowns there (7f-3)")
    | name <- nub (concatMap conditionNamesInEffect (allWorldEffects gw))
    , "cooldown_" `isPrefixOf` name ]

-- | Every condition name an effect tree mentions, including nested effects
--   (`ApplyCondition` carries tick/end effects, `Sequence`/`Conditional`/
--   `RandomChoice`/`Narrative` nest further effects).
conditionNamesInEffect :: E.Effect -> [String]
conditionNamesInEffect eff = case eff of
    E.ApplyCondition n _ mt me _ -> n : concatMap conditionNamesInEffect (catMaybes [mt, me])
    E.ClearCondition n         -> [n]
    E.Sequence es              -> concatMap conditionNamesInEffect es
    E.Conditional _ a b        -> conditionNamesInEffect a ++ conditionNamesInEffect b
    E.RandomChoice cs          -> concatMap (conditionNamesInEffect . snd) cs
    E.Narrative _ e            -> conditionNamesInEffect e
    _                          -> []

-- | Phase E: validate hotspot bindings on every art in the world. Each marker
--   must be a non-reserved glyph that actually occurs in the art, be unique
--   within its art, and point at a declared item or NPC.
checkHotspotRefs :: E.GameWorld -> [CompileIssue]
checkHotspotRefs gw =
    concat
      [ checkArt ("rooms." ++ rId) (E.roomAscii r) | (rId, r) <- Map.toList (E.rooms gw) ]
      ++ concat [ checkArt ("items." ++ iId) (E.itemAscii i) | (iId, i) <- Map.toList (E.itemDefs gw) ]
      ++ concat [ checkArt ("npcs." ++ nId) (E.npcAscii n) | (nId, n) <- Map.toList (E.npcDefs gw) ]
  where
    known = Set.fromList (Map.keys (E.itemDefs gw) ++ Map.keys (E.npcDefs gw))
    checkArt path art =
        let spots = E.aaHotspots art
            glyphs = map E.hsGlyph spots
            texts = artTexts art
            dupErrs =
                [ ciError (path ++ ".ascii.hotspots") "DuplicateHotspotGlyph"
                    ("marker '" ++ [g] ++ "' is used by more than one hotspot")
                | (g, n) <- Map.toList (Map.fromListWith (+) [(g, 1 :: Int) | g <- glyphs]), n > 1 ]
            reservedErrs =
                [ ciError (path ++ ".ascii.hotspots") "ReservedHotspotGlyph"
                    ("marker '" ++ [g] ++ "' is reserved (digits and whitespace cannot be markers)")
                | g <- glyphs, isDigit g || isSpace g ]
            missingErrs =
                [ ciError (path ++ ".ascii.hotspots") "HotspotGlyphMissing"
                    ("marker '" ++ [g] ++ "' does not occur in the art")
                | g <- glyphs, not (any (elem g) texts) ]
            unknownErrs =
                [ ciError (path ++ ".ascii.hotspots") "UnknownHotspotTarget"
                    ("hotspot target '" ++ t ++ "' is not a declared item or NPC")
                | t <- map E.hsTarget spots, t `Set.notMember` known ]
        in dupErrs ++ reservedErrs ++ missingErrs ++ unknownErrs
    artTexts art = concat
        [ E.ctDefault (E.aaStatic art) : map E.tvText (E.ctVariants (E.aaStatic art))
        , concatMap (\f -> E.ctDefault f : map E.tvText (E.ctVariants f)) (E.aaFrames art)
        ]

-- | Phase H (H1): validate the rate of every ambient loop. An ambient block
--   must carry at least one frame and a positive rate — otherwise the loop
--   would either show nothing or freeze the player's terminal. The D15
--   loop-seam check belongs to the video converter (F) and is deliberately
--   not attempted here.
checkAmbientRates :: E.GameWorld -> [CompileIssue]
checkAmbientRates gw =
    concat
      [ checkArt ("rooms." ++ rId) (E.aaAmbient (E.roomAscii r)) | (rId, r) <- Map.toList (E.rooms gw) ]
      ++ concat [ checkArt ("items." ++ iId) (E.aaAmbient (E.itemAscii i)) | (iId, i) <- Map.toList (E.itemDefs gw) ]
      ++ concat [ checkArt ("npcs." ++ nId) (E.aaAmbient (E.npcAscii n)) | (nId, n) <- Map.toList (E.npcDefs gw) ]
  where
    checkArt path = \case
        Nothing -> []
        Just amb -> concat
            [ [ ciError (path ++ ".fps") "AmbientFpsInvalid"
                    ("ambient fps must be positive, but is " ++ show (E.ambFps amb))
              | E.ambFps amb <= 0 ]
            , [ ciError (path ++ ".frames") "AmbientFramesEmpty"
                    "an ambient block declares no frames"
              | null (E.ambFrames amb) ] ]

-- | Verify every `faction.<id>` reference in the compiled world resolves to a
--   declared faction. Only runs when the `factions:` segment is present
--   (default-invariant: without it, `faction.*` strings are plain variables).
checkStandingRefs :: [AFaction] -> E.GameWorld -> [CompileIssue]
checkStandingRefs factions gw
    | null factions = []
    | otherwise =
        let declared = Set.fromList (map afId factions)
            refs = nub (collectFactionRefs gw)
        in [ ciError "factions" "UnknownFaction"
                ("faction '" ++ fid ++ "' is referenced but not declared in 'factions:'")
           | fid <- refs, fid `Set.notMember` declared ]

-- | Collect every `faction.<id>` identifier referenced anywhere in the world
--   (effects, predicates, dialogue gates, conditional texts).
collectFactionRefs :: E.GameWorld -> [String]
collectFactionRefs gw = nub (concatMap refsInEffect (allWorldEffects gw)
                             ++ concatMap refsInPredicate (allWorldPredicates gw))

-- | Every effect tree reachable from the compiled world: room hooks, item and
--   NPC verb maps, dialogue choices, trigger effects, quest rewards, vehicle
--   conditions and item interactions. Shared by the module reference checks so
--   a new module does not have to walk the world again.
allWorldEffects :: E.GameWorld -> [E.Effect]
allWorldEffects gw = concat
    [ concatMap roomHooks (Map.elems (E.rooms gw))
    , concatMap (Map.elems . itemVerbMap) (Map.elems (E.itemDefs gw))
    , concatMap (Map.elems . npcVerbMap) (Map.elems (E.npcDefs gw))
    , map dcOutcome (worldDialogueChoices gw)
    , concatMap trEffects (E.triggerDefs gw)
    , [ e | Just e <- map questReward (Map.elems (E.questDefs gw)) ]
    , concatMap (Map.elems . vehicleConditionEffects) (Map.elems (E.vehicleDefs gw))
    , Map.elems (E.itemInteractions gw)
    , Map.elems (E.npcInteractions gw)
    , concatMap E.paEffects (Map.elems (E.abilities gw))
    , concatMap E.cardEffects (Map.elems (E.cardDefs gw))
    , concatMap E.procEffects (Map.elems (E.procDefs gw))
    , maybe [] (concatMap E.lvlEffects . E.progLevels) (E.progressionDef gw)
    ]
  where
    roomHooks r = catMaybes [roomOnEnter r, roomOnLook r, roomOnExit r, roomSearchOutcome r]

-- | Every predicate tree reachable from the compiled world (dialogue gates,
--   trigger conditions, conditional texts).
allWorldPredicates :: E.GameWorld -> [E.Predicate]
allWorldPredicates gw = concat
    [ [ p | Just p <- map dcVisible (worldDialogueChoices gw) ]
    , [ p | Just p <- map trCondition (E.triggerDefs gw) ]
    , [ p | r <- Map.elems (E.rooms gw), E.Guarded _ p _ <- Map.elems (E.roomConnections r) ]
    , concatMap condTextPreds (map roomDescription (Map.elems (E.rooms gw)))
    , concatMap condTextPreds (map itemDescription (Map.elems (E.itemDefs gw)))
    , concatMap condTextPreds (map npcDescription (Map.elems (E.npcDefs gw)))
    , [ p | Just p <- map E.stDefWhen (E.statementDefs gw) ]
    ]
  where
    condTextPreds ct = map tvWhen (ctVariants ct)

-- | Every dialogue choice in every NPC dialogue tree.
worldDialogueChoices :: E.GameWorld -> [E.DialogueChoice]
worldDialogueChoices gw =
    [ c | npc <- Map.elems (E.npcDefs gw)
        , tree <- Map.elems (npcDialogueTrees npc)
        , node <- Map.elems (dtNodes tree)
        , c <- dnChoices node ]

-- | Faction references inside an Effect tree.
refsInEffect :: E.Effect -> [String]
refsInEffect e = case e of
    E.ModifyValue (E.VRVariable n) _ -> factionFromVar n
    E.SetValue (E.VRVariable n) _     -> factionFromVar n
    E.Sequence es                     -> concatMap refsInEffect es
    E.RandomChoice cs                 -> concatMap (refsInEffect . snd) cs
    E.Conditional p t el              -> refsInPredicate p ++ refsInEffect t ++ refsInEffect el
    E.Narrative _ follow              -> refsInEffect follow
    E.ApplyCondition _ _ (Just t) (Just el) _ -> refsInEffect t ++ refsInEffect el
    E.ApplyCondition _ _ (Just t) Nothing _   -> refsInEffect t
    E.ApplyCondition _ _ Nothing (Just el) _  -> refsInEffect el
    _                                 -> []

-- | Faction references inside a Predicate tree.
refsInPredicate :: E.Predicate -> [String]
refsInPredicate p = case p of
    E.CompareVar name _ _          -> factionFromVar name
    E.Compare (E.VRVariable n) _ _ -> factionFromVar n
    E.Compare _ _ (E.VRVariable n) -> factionFromVar n
    E.PNot q                       -> refsInPredicate q
    E.PAll qs                      -> concatMap refsInPredicate qs
    E.PAny qs                      -> concatMap refsInPredicate qs
    _                              -> []

-- | `"faction.x"` -> `Just "x"`; anything else -> Nothing.
factionFromVar :: String -> [String]
factionFromVar n = case stripPrefix "faction." n of
    Just rest | not (null rest) -> [rest]
    _                           -> []

-- ---------------------------------------------------------------------------
-- Party references (Phase 7g)
-- ---------------------------------------------------------------------------

-- | Phase 7g: every `damage_npc` target must be a declared NPC. The reference
--   is collected from the compiled world, so effects in rules, room hooks,
--   dialogue choices and verb maps all count.
checkDamageNpcRefs :: E.GameWorld -> [CompileIssue]
checkDamageNpcRefs gw =
    let declared = Set.fromList (Map.keys (E.npcDefs gw))
        dynamicTargets = Set.fromList ["chosen", "target", "current_target", "all", "all_enemies"]
        refs = nub (concatMap hpTargetsInEffect (allWorldEffects gw))
    in [ ciError "damage_npc" "UnknownDamageNPC"
            ("damage_npc targets '" ++ nid ++ "', which is not declared under 'npcs:'")
       | nid <- refs, nid `Set.notMember` declared, nid `Set.notMember` dynamicTargets ]

-- | `VRActorProp (ActorNPC <entity>) PHealth` references inside an Effect tree — what
--   `damage_npc` compiles to.
hpTargetsInEffect :: E.Effect -> [String]
hpTargetsInEffect e = case e of
    E.ModifyValue (E.VRActorProp (E.ActorNPC eid) E.PHealth) _ -> [eid]
    E.SetValue (E.VRActorProp (E.ActorNPC eid) E.PHealth) _    -> [eid]
    E.Sequence es                           -> concatMap hpTargetsInEffect es
    E.RandomChoice cs                       -> concatMap (hpTargetsInEffect . snd) cs
    E.Conditional _ t el                    -> hpTargetsInEffect t ++ hpTargetsInEffect el
    E.Narrative _ follow                    -> hpTargetsInEffect follow
    E.ApplyCondition _ _ (Just t) (Just el) _ -> hpTargetsInEffect t ++ hpTargetsInEffect el
    E.ApplyCondition _ _ (Just t) Nothing _   -> hpTargetsInEffect t
    E.ApplyCondition _ _ Nothing (Just el) _  -> hpTargetsInEffect el
    _                                       -> []

-- ---------------------------------------------------------------------------
-- Verbs (Phase 3a): adventure-declared custom verbs
-- ---------------------------------------------------------------------------

-- | Compile declared verbs into the registry; validate names/aliases.
compileVerbs :: [AVerb] -> ([CompileIssue], Map.Map String E.VerbDef)
compileVerbs verbs =
    let defs = Map.fromList [(avbName v, E.VerbDef (avbName v) (avbAliases v)) | v <- verbs]
        emptyErrs =
            [ ciError "verbs" "EmptyVerbName" "verb name must not be empty"
            | v <- verbs, null (avbName v) ]
        reservedErrs =
            [ ciError ("verbs." ++ avbName v) "ReservedVerbName"
                ("'" ++ avbName v ++ "' is a built-in verb and cannot be redeclared")
            | v <- verbs
            , map toLower (avbName v) `elem` reservedVerbWords ]
        -- alias collisions: only across different verbs, not within one verb
        aliasErrs =
            [ ciError "verbs" "DuplicateVerbAlias"
                ("the word '" ++ w ++ "' is shared by verbs " ++ show (filter (/= verb) others))
            | (w, verb : others) <- Map.toList $
                Map.fromListWith (++) [
                    (map toLower w, [vdName d])
                    | d <- Map.elems defs
                    , w <- vdName d : vdAliases d
                ]
            , not (null others) ]
    in (emptyErrs ++ reservedErrs ++ aliasErrs, defs)

-- ---------------------------------------------------------------------------
-- Items
-- ---------------------------------------------------------------------------

compileItems :: Map.Map String E.VerbDef -> [AItem] -> ([CompileIssue], Map.Map String E.ItemDef, Map.Map String E.ItemState)
compileItems registry items =
    let (defErrs, defs) = compileItemDefs registry items
        (stateErrs, states) = compileItemStates items
    in (defErrs ++ stateErrs, defs, states)

compileItemDefs :: Map.Map String E.VerbDef -> [AItem] -> ([CompileIssue], Map.Map String E.ItemDef)
compileItemDefs registry items =
    let results = map (compileItemDefSafe registry) items
        errors = concat [e | Left e <- results]
        defs = Map.fromList [(aiId i, d) | Right (i, d) <- results]
    in (errors, defs)

compileItemDefSafe :: Map.Map String E.VerbDef -> AItem -> Either [CompileIssue] (AItem, E.ItemDef)
compileItemDefSafe registry i =
    let bp = "items." ++ aiId i  -- base path for this item
        slotWrap = case compileSlot (aiEquipSlot i) of
            Left msg -> Left [ciError (bp ++ ".slot") "UnknownSlot" msg]
            Right v  -> Right v
        effectsWrap = case compileEffects (aiEquipEffects i) of
            Left msg -> Left [ciError (bp ++ ".effects") "InvalidEffect" msg]
            Right v  -> Right v
        verbMapWrap = case compileVerbMapSafe registry (bp ++ ".verb_map") (aiVerbMap i) of
            Left errs -> Left errs
            Right v   -> Right v
    in case (slotWrap, effectsWrap, verbMapWrap) of
        (Left es, _, _) -> Left es
        (_, Left es, _) -> Left es
        (_, _, Left es) -> Left es
        (Right slot, Right effects, Right verbMap) ->
            -- Merge on_take into verb_map (Phase 4.2: `on_take:` is the
            -- historical PhaseAfter take entry; an `instead:take,<state>`
            -- entry replaces it and it does not run).
            let verbMap' = case aiOnTake i of
                    Nothing -> verbMap
                    Just outcomes ->
                        if Map.member (E.PhaseAfter, E.VTake, aiState i) verbMap
                        then Map.insert (E.PhaseAfter, E.VTake, aiState i)
                             (E.Sequence [compileOutcomes outcomes, Map.findWithDefault (E.Noop) (E.PhaseAfter, E.VTake, aiState i) verbMap])
                             verbMap
                        else Map.insert (E.PhaseAfter, E.VTake, aiState i) (compileOutcomes outcomes) verbMap
            in Right (i, E.ItemDef
                { E.itemId = aiId i
                , E.itemName = aiName i
                , E.itemDescription = compileCondText (aiTexts i)
                , E.itemAscii = compileAscii (aiAscii i)
                , E.itemKeywords = aiKeywords i
                , E.itemTags = Set.fromList (aiTags i)
                , E.itemEquipSlot = slot
                , E.itemEquipEffects = effects
                , E.itemHidden = aiHidden i
                , E.itemDiscoverText = aiDiscover i
                , E.itemPortable = fromMaybe True (aiPortable i)
                , E.itemTakeFailure = aiTakeFailure i
                , E.itemVerbMap = verbMap'
                , E.itemCapacity = aiCapacity i
                , E.itemGrammar = aiGrammar i
                })

compileItemStates :: [AItem] -> ([CompileIssue], Map.Map String E.ItemState)
compileItemStates items =
    let results = map compileItemStateSafe items
        errors = concat [e | Left e <- results]
        states = Map.fromList [s | Right s <- results]
    in (errors, states)

compileItemStateSafe :: AItem -> Either [CompileIssue] (String, E.ItemState)
compileItemStateSafe i
    | isJust (aiInContainer i), isJust (aiCarriedBy i) =
        Left [ ciError ("items." ++ aiId i) "CarriedByConflict"
                ("item '" ++ aiId i ++ "' sets both 'in_container' and 'carried_by' — it can only start in one place") ]
    | otherwise = Right (aiId i, E.ItemState
        { E.itemLocation = case aiInContainer i of
            Just cid -> E.InContainer cid
            Nothing  -> case aiCarriedBy i of
                Just a  -> E.CarriedBy (compileActorRef a)
                Nothing -> E.InRoom (aiLocation i)
        , E.itemStatus = aiState i
        , E.itemProps = aiProps i
        , E.itemDiscovered = not (aiHidden i)
        })

compileSlot :: Maybe String -> Either String (Maybe E.EquipSlot)
compileSlot Nothing = Right Nothing
compileSlot (Just s) = case s of
    "head"      -> Right (Just E.Head)
    "body"      -> Right (Just E.Body)
    "hands"     -> Right (Just E.Hands)
    "feet"      -> Right (Just E.Feet)
    "weapon"    -> Right (Just E.Weapon)
    "offhand"   -> Right (Just E.Offhand)
    "accessory" -> Right (Just E.Accessory)
    other       -> Left $ "Unknown equipment slot '" ++ other ++ "'"

compileEffects :: [String] -> Either String [E.EquipEffect]
compileEffects = mapM compileEffect
  where
    compileEffect s = case s of
        'a':'t':'t':'a':'c':'k':'+':n -> case readMaybe n of
            Just v  -> Right (E.AttackBonus v)
            Nothing -> Left $ "Invalid attack bonus: '" ++ s ++ "'"
        'd':'e':'f':'e':'n':'s':'e':'+':n -> case readMaybe n of
            Just v  -> Right (E.DefenseBonus v)
            Nothing -> Left $ "Invalid defense bonus: '" ++ s ++ "'"
        'm':'a':'x':'h':'p':'+':n -> case readMaybe n of
            Just v  -> Right (E.MaxHealthBonus v)
            Nothing -> Left $ "Invalid maxhp bonus: '" ++ s ++ "'"
        other -> Left $ "Unknown equip effect: '" ++ other ++ "'"

-- ---------------------------------------------------------------------------
-- NPCs
-- ---------------------------------------------------------------------------

compileNPCs :: Map.Map String E.VerbDef -> [ANPC] -> ([CompileIssue], Map.Map String E.NPCDef, Map.Map String E.NPCState)
compileNPCs registry npcs =
    let defResults = map (compileNPCDefSafe registry) npcs
        defErrors = concat [e | Left e <- defResults]
        defs = Map.fromList [(anId n, d) | Right (n, d) <- defResults]
        states = Map.fromList [(anId n, compileNPCState n) | n <- npcs]
    in (defErrors, defs, states)

compileNPCDefSafe :: Map.Map String E.VerbDef -> ANPC -> Either [CompileIssue] (ANPC, E.NPCDef)
compileNPCDefSafe registry n =
    case compileVerbMapSafe registry ("npcs." ++ anId n ++ ".verb_map") (anVerbMap n) of
        Left errs -> Left errs
        Right verbMap -> Right (n, E.NPCDef
            { E.npcId = anId n
            , E.npcName = anName n
            , E.npcDescription = compileCondText (anTexts n)
            , E.npcAscii = compileAscii (anAscii n)
            , E.npcTopics = Map.map compileAActionOutcome (anTopics n)
            , E.npcDialogueTrees = compileDialogueTrees (anDialogue n)
            , E.npcKeywords = anKeywords n
            , E.npcMaxHealth = anMaxHealth n
            , E.npcAttackBase = anAttack n
            , E.npcDefenseBase = anDefense n
            , E.npcVerbMap = verbMap
            -- B9: written only when set (byte contract of the NPCDef encoder)
            , E.npcDropsOnDeath = anDropsOnDeath n
            , E.npcGrammar = anGrammar n
            })

compileDialogueTrees :: Map.Map String ADialogueTree -> Map.Map String E.DialogueTree
compileDialogueTrees = Map.map compileDialogueTree

compileDialogueTree :: ADialogueTree -> E.DialogueTree
compileDialogueTree t = E.DialogueTree
    { E.dtEntry = adtEntry t
    , E.dtNodes = Map.mapWithKey (\k n -> (compileDialogueNode n) { E.dnId = k }) (adtNodes t)
    }

compileDialogueNode :: ADialogueNode -> E.DialogueNode
compileDialogueNode n = E.DialogueNode
    { E.dnId = ""  -- placeholder, set by mapWithKey
    , E.dnText = adnText n
    , E.dnChoices = map compileDialogueChoice (adnChoices n)
    }

compileDialogueChoice :: ADialogueChoice -> E.DialogueChoice
compileDialogueChoice c = E.DialogueChoice
    { E.dcText = adcText c
    , E.dcNextNode = adcNext c
    , E.dcVisible = adcVisible c
    , E.dcOutcome = compileOutcomes (adcOutcomes c)
    }

compileNPCState :: ANPC -> E.NPCState
compileNPCState n = E.NPCState
    { E.npcLocation = E.InRoom (anLocation n)
    , E.npcStatus = anState n
    , E.npcHealth = anMaxHealth n
    , E.npcProps = Map.empty
    , E.npcDialogueNode = Nothing
    }

-- ---------------------------------------------------------------------------
-- Quests
-- ---------------------------------------------------------------------------

compileQuests :: [AQuest] -> Map.Map String E.Quest
compileQuests = Map.fromList . map compileQuest

compileQuest :: AQuest -> (String, E.Quest)
compileQuest q =
    ( aqId q
    , E.Quest
        { E.questId = aqId q
        , E.questName = aqName q
        , E.questDescription = aqDesc q
        , E.questPrereqs = Map.fromList [(flag, "true") | flag <- aqPrereqs q]
        , E.questStages = [E.QuestStage (aqsId s) (aqsDesc s) (aqsHint s) | s <- aqStages q]
        , E.questReward = compileMaybeOutcomes (aqReward q)
        , E.questOnComplete = aqOnComplete q
        }
    )

-- ---------------------------------------------------------------------------
-- Vehicles
-- ---------------------------------------------------------------------------

compileVehicles :: [AVehicle] -> ([CompileIssue], Map.Map String E.VehicleDef, Map.Map String E.VehicleState)
compileVehicles vehicles =
    let defResults = map compileVehicleDefSafe vehicles
        errors = concat [e | Left e <- defResults]
        defs = Map.fromList [(avId v, d) | Right (v, d) <- defResults]
        states = Map.fromList [(avId v, compileVehicleState v) | v <- vehicles]
    in (errors, defs, states)

compileVehicleDefSafe :: AVehicle -> Either [CompileIssue] (AVehicle, E.VehicleDef)
compileVehicleDefSafe v =
    case compileVehicleType (avType v) of
        Left msg -> Left [ciError ("vehicles." ++ avId v ++ ".type") "UnknownVehicleType" msg]
        Right vtype ->
            let def = E.VehicleDef
                    { E.vehicleId = avId v
                    , E.vehicleName = avName v
                    , E.vehicleDescription = avDesc v
                    , E.vehicleType = vtype
                    , E.vehicleRooms = map arId (avInterior v)
                    , E.vehicleEntryRoom = avEntryRoom v
                    , E.vehicleCockpitRoom = avCockpit v
                    , E.vehicleStops = Map.fromList
                        [ (asRoom stop, E.VehicleStop (asRoom stop) label (asCost stop))
                        | (label, stop) <- Map.toList (avStops v) ]
                    , E.vehicleRoute = []  -- authored stop order; empty = key order
                    , E.vehicleKeywords = avKeywords v
                    , E.vehicleFuelProp = fmap (\f -> E.FuelSpec (afItem f) (afMax f)) (avFuel v)
                    , E.vehicleConditionEffects = Map.map (\os -> combineOutcomes (map compileAActionOutcome os)) (avConditions v)
                    }
            in Right (v, def)

compileVehicleType :: String -> Either String E.VehicleType
compileVehicleType "auto"   = Right E.AutomaticRoute
compileVehicleType "paid"   = Right E.PaidVehicle
compileVehicleType "player" = Right E.PlayerControlled
compileVehicleType other    = Left $ "Unknown vehicle type '" ++ other ++ "' (expected: player, auto, paid)"

combineOutcomes :: [E.Effect] -> E.Effect
combineOutcomes [] = E.Noop
combineOutcomes [o] = o
combineOutcomes os = E.Sequence os

compileVehicleState :: AVehicle -> E.VehicleState
compileVehicleState v = E.VehicleState
    { E.vsCurrentStop = case avStartStop v >>= (`Map.lookup` avStops v) of
          Just stop -> asRoom stop
          Nothing   -> case Map.lookup (headSafe (Map.keys (avStops v))) (avStops v) of
              Just stop -> asRoom stop
              Nothing   -> avEntryRoom v
    -- P2-21: a fuelled vehicle starts with a full tank (the old `Nothing` made
    -- every vehicle report "0/<max>" until the player refuelled).
    , E.vsFuel = afMax <$> avFuel v
    , E.vsActiveConditions = Set.empty
    , E.vsRoomOverrides = Map.empty
    }
  where
    headSafe [] = ""
    headSafe xs = head xs

-- ---------------------------------------------------------------------------
-- Interactions
-- ---------------------------------------------------------------------------

compileInteractions :: Maybe AInteractions
                    -> ( Map.Map (String, String) (String, String)
                       , Map.Map (String, String) E.Effect
                       , Map.Map (String, String) E.Effect
                       )
compileInteractions Nothing = (Map.empty, Map.empty, Map.empty)
compileInteractions (Just ix) = (entityMap, itemMap, npcMap)
  where
    entityMap = Map.fromList
        [ ((aeiItem e, aeiTarget e), (aeiState e, fromMaybe "" (aeiMsg e)))
        | e <- aiEntity ix ]
    itemMap = Map.fromList
        [ ((aiiItem1 i, aiiItem2 i), compileOutcomes (aiiEffects i))
        | i <- aiItem ix ]
    npcMap = Map.fromList
        [ ((aniItem n, aniTarget n), compileOutcomes (aniEffects n))
        | n <- aiNpc ix ]

-- ---------------------------------------------------------------------------
-- Verb maps (strict — unknown verb = compile error, custom verbs resolved)
-- ---------------------------------------------------------------------------

-- | Compile a verb map with structured diagnostics.  The path prefix points at
--   the owning field (e.g. "items.crystal.verb_map").  The registry resolves
--   custom verbs; unknown verbs are errors.
-- | Compile a verb_map. Keys are `[before:|instead:]verb[,state]` (Phase 4.2):
--   a `before:`/`instead:` prefix selects the phase, keys without one keep the
--   historical 'E.PhaseAfter' behaviour. A `(verb, state)` pair may be
--   assigned in only one phase ('VerbPhaseClash').
compileVerbMapSafe :: Map.Map String E.VerbDef -> String -> Map.Map String [AActionOutcome]
                   -> Either [CompileIssue] (Map.Map (E.VerbPhase, E.Verb, String) E.Effect)
compileVerbMapSafe registry pathPrefix vm =
    let entries = Map.toList vm
        parsed = [ (key, phase, parseVerbStrict registry verbStr, state)
                 | (key, _) <- entries
                 , let (phase, keyRest) = parsePhasePrefix key
                 , let (verbStr, rest) = break (== ',') keyRest
                 , let state = case rest of
                         ',':s -> s
                         _     -> "intact" ]
        verbErrs =
            [ ciError (pathPrefix ++ "." ++ key) "UnknownVerb" msg
            | (key, _, Left msg, _) <- parsed ]
        -- (Verb, State) collisions within one phase, e.g. "use,intact" + "activate,intact"
        collErrs =
            [ ciError pathPrefix "DuplicateVerbKey"
                ("verb keys " ++ show keys ++ " all resolve to " ++ show verb ++ ":" ++ state)
            | ((verb, state), keys) <- collisions
                [(k, (v, st)) | (k, _, Right v, st) <- parsed] ]
        -- Phase 4.2: one (verb, state) pair, one phase — otherwise the lookup
        -- order would silently shadow an entry.
        clashErrs =
            [ ciError pathPrefix "VerbPhaseClash"
                ("verb key " ++ show verb ++ ":" ++ state ++ " is assigned in phases "
                 ++ show phases ++ "; use exactly one")
            | ((verb, state), phases) <- clashes
                [((v, st), phaseTag ph) | (_, ph, Right v, st) <- parsed] ]
    in if null (verbErrs ++ collErrs ++ clashErrs)
       then Right $ Map.fromList
            [ ((ph, verb, state), compileOutcomes outcomes)
            | (key, ph, Right verb, state) <- parsed
            , Just outcomes <- [Map.lookup key vm] ]
       else Left (verbErrs ++ collErrs ++ clashErrs)
  where
    parsePhasePrefix key = case key of
        ('b':'e':'f':'o':'r':'e':':':rest)     -> (E.PhaseBefore, rest)
        ('i':'n':'s':'t':'e':'a':'d':':':rest) -> (E.PhaseInstead, rest)
        _                                     -> (E.PhaseAfter, key)
    phaseTag E.PhaseAfter   = "after" :: String
    phaseTag E.PhaseBefore  = "before"
    phaseTag E.PhaseInstead = "instead"

-- | Parse verb strings: core first, then custom verb registry.
--   Replaces the previous hardcoded list with Verbs.resolveVerb.
parseVerbStrict :: Map.Map String E.VerbDef -> String -> Either String E.Verb
parseVerbStrict registry s = case Verbs.resolveVerb registry s of
    Just verb -> Right verb
    Nothing -> Left $ "Unknown verb '" ++ s ++ "' (expected a core verb or a declared adventure verb)"

-- ---------------------------------------------------------------------------
-- Action outcome compilation
-- ---------------------------------------------------------------------------

compileOutcomes :: [AActionOutcome] -> E.Effect
compileOutcomes [] = E.Noop
compileOutcomes [o] = compileAActionOutcome o
compileOutcomes os = E.Sequence (map compileAActionOutcome os)

-- | K2: Compile outcomes while resolving NPC state targets to ActorNPC.
compileOutcomesWith :: Set.Set String -> [AActionOutcome] -> E.Effect
compileOutcomesWith _ [] = E.Noop
compileOutcomesWith npcIds [o] = compileAActionOutcomeWith npcIds o
compileOutcomesWith npcIds os = E.Sequence (map (compileAActionOutcomeWith npcIds) os)

-- | K2: Resolve VRActorProp (ActorEntity e) PState to ActorNPC if e is in npcIds.
resolveNpcStateEffect :: Set.Set String -> E.Effect -> E.Effect
resolveNpcStateEffect npcIds eff = case eff of
    E.SetValue (E.VRActorProp (E.ActorEntity e) E.PState) v
        | e `Set.member` npcIds -> E.SetValue (E.VRActorProp (E.ActorNPC e) E.PState) v
    E.Sequence es -> E.Sequence (map (resolveNpcStateEffect npcIds) es)
    E.Conditional p t el -> E.Conditional p (resolveNpcStateEffect npcIds t) (resolveNpcStateEffect npcIds el)
    E.RandomChoice cs -> E.RandomChoice [ (w, resolveNpcStateEffect npcIds e) | (w, e) <- cs ]
    E.RandomChoiceOn s cs -> E.RandomChoiceOn s [ (w, resolveNpcStateEffect npcIds e) | (w, e) <- cs ]
    E.Narrative ls f -> E.Narrative ls (resolveNpcStateEffect npcIds f)
    E.ApplyCondition n t tick end h ->
        E.ApplyCondition n t (fmap (resolveNpcStateEffect npcIds) tick)
                             (fmap (resolveNpcStateEffect npcIds) end) h
    other -> other

-- | K2: Compile an action outcome with known NPC IDs (resolves NPC state targets).
compileAActionOutcomeWith :: Set.Set String -> AActionOutcome -> E.Effect
compileAActionOutcomeWith npcIds ao = case ao of
    AOSetEntityState e s ->
        let actor = if e `Set.member` npcIds
                    then E.ActorNPC e
                    else E.ActorEntity e
        in E.SetValue (E.VRActorProp actor E.PState) (E.EVString s)
    other -> resolveNpcStateEffect npcIds (compileAActionOutcome other)

-- | K2: Resolve VRActorProp (ActorEntity e) PState to ActorNPC for any NPC in gw.
resolveWorldEffects :: Set.Set String -> E.GameWorld -> E.GameWorld
resolveWorldEffects npcIds gw
    | Set.null npcIds = gw
    | otherwise = gw
        { E.rooms = Map.map mapRoom (E.rooms gw)
        , E.itemDefs = Map.map mapItem (E.itemDefs gw)
        , E.npcDefs = Map.map mapNpc (E.npcDefs gw)
        , E.itemInteractions = Map.map mapEff (E.itemInteractions gw)
        , E.npcInteractions = Map.map mapEff (E.npcInteractions gw)
        , E.questDefs = Map.map mapQuest (E.questDefs gw)
        , E.vehicleDefs = Map.map mapVehicle (E.vehicleDefs gw)
        , E.triggerDefs = map mapTrig (E.triggerDefs gw)
        , E.abilities = Map.map mapAbility (E.abilities gw)
        , E.cardDefs = Map.map mapCard (E.cardDefs gw)
        , E.procDefs = Map.map mapProc (E.procDefs gw)
        , E.deviceDefs = Map.map mapDevice (E.deviceDefs gw)
        , E.progressionDef = fmap mapProg (E.progressionDef gw)
        }
  where
    mapEff = resolveNpcStateEffect npcIds
    mapRoom r = r
        { E.roomOnEnter = fmap mapEff (E.roomOnEnter r)
        , E.roomOnLook = fmap mapEff (E.roomOnLook r)
        , E.roomOnExit = fmap mapEff (E.roomOnExit r)
        , E.roomSearchOutcome = fmap mapEff (E.roomSearchOutcome r)
        }
    mapItem i = i
        { E.itemVerbMap = Map.map mapEff (E.itemVerbMap i)
        }
    mapNpc n = n
        { E.npcTopics = Map.map mapEff (E.npcTopics n)
        , E.npcDialogueTrees = Map.map mapTree (E.npcDialogueTrees n)
        , E.npcVerbMap = Map.map mapEff (E.npcVerbMap n)
        }
    mapTree dt = dt { E.dtNodes = Map.map mapNode (E.dtNodes dt) }
    mapNode dn = dn { E.dnChoices = map mapChoice (E.dnChoices dn) }
    mapChoice dc = dc { E.dcOutcome = mapEff (E.dcOutcome dc) }
    mapQuest q = q { E.questReward = fmap mapEff (E.questReward q) }
    mapVehicle v = v
        { E.vehicleConditionEffects = Map.map mapEff (E.vehicleConditionEffects v)
        }
    mapTrig td = td { E.trEffects = map mapEff (E.trEffects td) }
    mapAbility ab = ab { E.paEffects = map mapEff (E.paEffects ab) }
    mapCard cd = cd { E.cardEffects = map mapEff (E.cardEffects cd) }
    mapProc pd = pd { E.procEffects = map mapEff (E.procEffects pd) }
    mapDevice dv = dv
        { E.devOnInsert = map mapEff (E.devOnInsert dv)
        , E.devOnRemove = map mapEff (E.devOnRemove dv)
        , E.devOnFlip = Map.map (map mapEff) (E.devOnFlip dv)
        }
    mapProg pr = pr
        { E.progLevels = map mapLvl (E.progLevels pr) }
    mapLvl lvl = lvl { E.lvlEffects = map mapEff (E.lvlEffects lvl) }

compileMaybeOutcomes :: Maybe [AActionOutcome] -> Maybe E.Effect
compileMaybeOutcomes Nothing = Nothing
compileMaybeOutcomes (Just os) = Just (compileOutcomes os)

compileAActionOutcome :: AActionOutcome -> E.Effect
compileAActionOutcome ao = case ao of
    AOMessage s -> E.SendMessage s
    AOHealPlayer n -> E.ModifyValue E.VRPlayerHealth n
    AODamagePlayer n -> E.ModifyValue E.VRPlayerHealth (-n)
    AOGiveItem i -> E.MoveEntity i (E.CarriedBy E.ActorPlayer)
    AOGiveTo i tgt -> E.MoveEntity i (E.CarriedBy (compileActorRef tgt))
    -- B9: NPC equipment. The engine keeps the slot in the item's own
    -- definition (`itemEquipSlot`, YAML `slot:`), so no new SaveState field is needed; the
    -- actor comes from the same `to` field as the B7 give-sugar.
    AOGiveEquipTo i tgt -> E.MoveEntity i (E.EquippedBy (compileActorRef tgt))
    AOConsumeItem i -> E.MoveEntity i E.Removed
    AOSetFlag f v -> E.SetValue (E.VRFlag f) (E.EVString v)
    AOStartQuest q -> E.QuestOp E.StartQuest q
    AOAdvanceQuest q -> E.QuestOp E.AdvanceQuest q
    AOCompleteQuest q -> E.QuestOp E.CompleteQuest q
    AOEquipItem i -> E.MoveEntity i (E.EquippedBy E.ActorPlayer)
    AORoomTransition r -> E.SetValue (E.VRActorProp E.ActorPlayer E.PRoom) (E.EVString r)
    AOMoveNPC n r -> E.MoveEntity n (E.InRoom r)
    AODamageNPC n amount -> E.ModifyValue (E.VRActorProp (E.ActorNPC n) E.PHealth) (-amount)
    AOGameEnd r m -> E.GameEnd (parseGameOverReason r) (fromMaybe "" m)
    AOConditional p ts es ->
        E.Conditional p (compileOutcomes ts) (compileOutcomes es)
    AOSetVar name v -> E.SetValue (E.VRVariable name) (E.EVInt v)
    AOSetTextVar name s -> E.SetValue (E.VRVariable name) (E.EVString s)
    AOAddVar name d -> E.ModifyValue (E.VRVariable name) d
    AOComputeVar name expr -> E.ComputeValue (E.VRVariable name) expr
    AOCallProc name args -> E.CallProc name args
    AOLearn f a -> E.Learn (compileActorRef a) f
    AONextChapter -> E.NextChapter
    AOGotoChapter t -> E.GotoChapter t
    AOStepToward seeker target mMsg ->
        E.StepToward (compileActorRef seeker) target E.defaultPursuitOptions mMsg
    AOStepAwayFrom seeker target mMsg ->
        E.StepAwayFrom (compileActorRef seeker) target E.defaultPursuitOptions mMsg
    AODamageAll cs amount -> E.DamageAll cs amount
    AOMoveAll cs dest -> E.MoveAll cs dest
    AORevealAll cs -> E.RevealAll cs
    AOConsumeAll cs -> E.ConsumeAll cs
    AOSetStateAll cs newStatus -> E.SetStateAll cs newStatus
    AOSetInventoryLimit n -> E.SetValue (E.VRVariable "inventory.limit") (E.EVInt n)
    AOSayNode node -> E.SetValue (E.VRVariable "dialog_node") (E.EVString node)
    AODialogEnd -> E.SetValue (E.VRVariable "dialog_node") (E.EVString "")
    AOForget f a -> E.Forget (compileActorRef a) f
    AONarrative ls follow -> E.Narrative ls (compileOutcomes follow)
    AOStandingAdd fid n -> E.ModifyValue (E.VRVariable ("faction." ++ fid)) n
    AOStandingSet fid n -> E.SetValue (E.VRVariable ("faction." ++ fid)) (E.EVInt n)
    AOSetEntityState e s -> E.SetValue (E.VRActorProp (E.ActorEntity e) E.PState) (E.EVString s)
    AOMount i tgt -> E.Mount i (compileDeviceActorRef tgt)
    AOUnmount i   -> E.Unmount i
    -- P1-17: previously unreachable engine effects, now authorable.
    AOApplyCondition name turns tick end hidden ->
        E.ApplyCondition name turns (outcomesMaybe tick) (outcomesMaybe end) hidden
    AOClearCondition name -> E.ClearCondition name
    -- Rogue Phase 3: dynamic exits. Direction strings are validated in
    -- 'checkSetExitRefs' (UnknownDirection) — they must not be silently
    -- mapped; missing `from`/`to` rooms surface as MissingRoom in the
    -- engine validate pass (idsFromOutcomeRoom).
    AOSetExit from dir to mLock ->
        E.SetExit from (dirOf dir) (case mLock of
            Nothing -> E.Open to
            Just e  -> E.Locked to e)
    AORemoveExit from dir ->
        E.RemoveExit from (dirOf dir)
    AOModifySkill skillId delta -> E.ModifySkill skillId delta
    AORandomChoice streamName weighted ->
        (if null streamName then E.RandomChoice else E.RandomChoiceOn streamName)
            [ (w, compileOutcomes os) | (w, os) <- weighted ]
    AORollDice pool die stream keep ->
        E.RollDice pool die stream keep
    AORaiseEvent name -> E.RaiseEvent name
    AOPlayClip clipId -> E.PlayClip clipId
    AOPlaySfx path -> E.PlaySfx path
    AOPlayMusic path -> E.PlayMusic path
    AOStopMusic -> E.StopMusic
    AODrawCards n -> E.DrawCards n
    AODiscardHand -> E.DiscardHand
    AODiscardCard cid -> E.DiscardCard cid
    AOExhaustCard cid -> E.ExhaustCard cid
    AOAddCardToDeck cid destStr ->
        let dest = case map toLower destStr of
                "discard" -> E.DestDiscard
                "hand"    -> E.DestHand
                _         -> E.DestDraw
        in E.AddCardToDeck cid dest
    AOShuffleDeck -> E.ShuffleDeck
    AOBlock mMsg turn -> E.Block mMsg turn
    AOGenerateRoom rId rName rDesc fromR toDirStr retDirStr ->
        let toDir = dirOf toDirStr
            retDir = if null retDirStr
                     then E.oppositeDirection toDir
                     else dirOf retDirStr
        in E.GenerateRoom rId rName rDesc fromR toDir retDir
    AOGainXp n -> E.GainXp n

-- | `Just` the compiled effect for a non-empty outcome list, else `Nothing`
--   (engine `ApplyCondition` takes optional tick/end effects).
outcomesMaybe :: [AActionOutcome] -> Maybe E.Effect
outcomesMaybe [] = Nothing
outcomesMaybe os = Just (compileOutcomes os)

-- | Parse a game-end reason string ("victory", "death", or a custom label).
parseGameOverReason :: String -> E.GameOverReason
parseGameOverReason r = case map toLower r of
    "victory" -> E.Victory
    "death"   -> E.Death
    other     -> E.Custom other

-- ---------------------------------------------------------------------------
-- Conditional text (Phase 3g)
-- ---------------------------------------------------------------------------

-- | Compile YAML ACondText into engine CondText.
compileCondText :: ACondText -> E.CondText
compileCondText act = E.CondText
    { E.ctDefault = actDefault act
    , E.ctVariants = [ E.TextVariant (atvWhen tv) (atvText tv) | tv <- actVariants act ]
    }

-- | Compile declared clips into the engine's clip map (Phase H/H4). Frames
--   from a companion file were already embedded during parsing (D14), so this
--   step is pure.
compileClips :: [AClip] -> Map.Map String E.Clip
compileClips clips = Map.fromList
    [ (acId c, E.Clip (acFrames c) (acFps c)) | c <- clips, not (null (acFrames c)) ]

-- | Phase H (H4): validate clip declarations and every reference to them —
--   `intro:` on a room and `play_clip:` effects. Duplicate ids, unknown ids
--   and unusable clips (empty frames, non-positive fps) are compile errors.
--   References are collected from the compiled world (allWorldEffects), so
--   play_clip in rules, room hooks, dialogue choices and verb maps all count.
checkClips :: [AClip] -> E.GameWorld -> [CompileIssue]
checkClips clips gw =
    dupErrs ++ unusableErrs ++ unknownErrs
  where
    declaredIds = map acId clips
    dupErrs =
        [ ciError ("clips." ++ cid) "DuplicateClip"
            ("clip id '" ++ cid ++ "' is declared more than once")
        | (cid, n) <- Map.toList (Map.fromListWith (+) [(cid, 1 :: Int) | cid <- declaredIds])
        , n > 1 ]
    unusableErrs = concat
        [ [ ciError ("clips." ++ acId c ++ ".fps") "ClipFpsInvalid"
                ("clip fps must be positive, but is " ++ show (acFps c))
          | acFps c <= 0 ]
          ++ [ ciError ("clips." ++ acId c ++ ".frames") "ClipFramesEmpty"
                "a clip declares no frames (inline `frames:` or `file:`)"
          | null (acFrames c) ]
        | c <- clips ]
    known = Set.fromList declaredIds
    effectRefs = [ cid | E.PlayClip cid <- allWorldEffects gw ]
    roomRefs =
        [ (path, cid)
        | (rId, r) <- Map.toList (E.rooms gw)
        , let path = "rooms." ++ rId
        , Just cid <- [E.roomIntro r] ]
    unknownErrs =
        [ ciError (path ++ suffix) "UnknownClip"
            ("clip '" ++ cid ++ "' is not declared in the clips: segment")
        | (path, cid) <- roomRefs ++ [ ("play_clip", cid) | cid <- effectRefs ]
        , let suffix = if path == "play_clip" then "" else ".intro"
        , cid `Set.notMember` known ]

-- | Compile authored ASCII art (static CondText plus optional animation
--   frames) into the engine's `AsciiArt` (Phase B/D/H).
compileAscii :: AAscii -> E.AsciiArt
compileAscii a = E.AsciiArt
    { E.aaStatic = compileCondText (asaStatic a)
    , E.aaFrames = map compileCondText (asaFrames a)
    , E.aaEvery  = asaEvery a
    , E.aaAmbient = asaAmbient a
    , E.aaHotspots = asaHotspots a
    }

-- ---------------------------------------------------------------------------
-- Trigger rules (Phase 3f)
-- ---------------------------------------------------------------------------

-- | Compile authored trigger rules into engine TriggerDefs.
compileTriggers :: [ATrigger] -> [ANPC] -> [AVariable] -> ([CompileIssue], [E.TriggerDef])
compileTriggers triggers npcs vars =
    let results = map compileOne triggers
        errors = concat [e | Left e <- results]
        defs = [d | Right d <- results]
        (aiErrors, aiDefs) = compileNpcAI npcs
        (varResetDefs, varRefillDefs) = compileVarTriggers vars
    in (errors ++ aiErrors, defs ++ barkDefs ++ talkDefs ++ aiDefs ++ varResetDefs ++ varRefillDefs)
  where
    -- 4.5 sugar: `barks:` on an NPC becomes `on: turn` triggers with a
    -- cooldown (one mechanism, the compiler owns the ids).
    barkDefs = concat
        [ [ E.TriggerDef
            { E.trId = "bark." ++ anId n ++ "." ++ show k
            , E.trEvent = E.OnTurn
            , E.trCondition = abWhen b
            , E.trEffects = [E.SendMessage (abText b)]
            , E.trOnce = False
            , E.trCooldown = fromMaybe 20 (abCooldown b)
            , E.trWeight = 1
            , E.trRequires = []
            , E.trChainsTo = []
            }
          | (k, b) <- zip [(1 :: Int) ..] (anBarks n) ]
        | n <- npcs ]
    -- 4.5 sugar: `on_talk:` becomes an `on: talk` trigger filtered on the NPC.
    talkDefs =
        [ E.TriggerDef
            { E.trId = "talk." ++ anId n
            , E.trEvent = E.OnTalk (anId n) ""
            , E.trCondition = Nothing
            , E.trEffects = [compileAActionOutcome eff]
            , E.trOnce = False
            , E.trCooldown = 0
            , E.trWeight = 1
            , E.trRequires = []
            , E.trChainsTo = []
            }
        | n <- npcs, Just eff <- [anOnTalk n] ]
    compileOne t = case compileAtOn (atOn t) of
        Left err -> Left [ciError ("rules." ++ atId t) "BadTriggerEvent" err]
        Right ev -> Right E.TriggerDef
            { E.trId = atId t
            , E.trEvent = ev
            , E.trCondition = atWhen t
            , E.trEffects = map compileAActionOutcome (atEffects t)
            , E.trOnce = atOnce t
            , E.trCooldown = atCooldown t
            , E.trWeight = atWeight t
            , E.trRequires = atRequires t
            , E.trChainsTo = atChainsTo t
            }

-- | K7+K4: Compiler-sugar for variable cycles (refill_per_turn, reset_on).
-- Variables with cycle fields compile to engine TriggerDefs with id schema
-- `var.<name>.reset`, `var.<name>.combatreset`, and `var.<name>.refill`.
--
-- REIHENFOLGE:
-- 1. Autoren-Regeln ('defs') stehen GANZ VORNE in der TriggerDef-Liste:
--    `defs ++ barkDefs ++ talkDefs ++ aiDefs ++ varResetDefs ++ varRefillDefs`.
--    Eine Autorenregel auf 'on: turn' sieht daher den Zustand VOR dem Reset.
-- 2. Zwischen den Compiler-Zucker-Listen werden 'varResetDefs' VOR 'varRefillDefs'
--    eingehängt. Da 'fireTriggers' / 'fireTriggerList' die Trigger per 'foldl''
--    strikt in Definitionsreihenfolge (der Listenreihenfolge, NICHT alphabetisch)
--    ausführt, wird bei 'OnTurn' immer zuerst der Reset auf 'max' durchgeführt
--    und danach der Refill addiert. Ein Reset überschreibt somit niemals den
--    im selben Zug regenerierten Refill.
compileVarTriggers :: [AVariable] -> ([E.TriggerDef], [E.TriggerDef])
compileVarTriggers vars = (concatMap makeResetTriggers vars, concatMap makeRefillTriggers vars)
  where
    makeResetTriggers av = case (avbResetOn av, avbMax av) of
        (Just "turn", Just maxVal) ->
            [ E.TriggerDef
                { E.trId         = "var." ++ avbVarName av ++ ".reset"
                , E.trEvent      = E.OnTurn
                , E.trCondition  = Nothing
                , E.trEffects    = [E.SetValue (E.VRVariable (avbVarName av)) (E.EVInt maxVal)]
                , E.trOnce       = False
                , E.trCooldown   = 0
                , E.trWeight     = 1
                , E.trRequires   = []
                , E.trChainsTo   = []
                }
            ]
        (Just "combat_start", Just maxVal) ->
            [ E.TriggerDef
                { E.trId         = "var." ++ avbVarName av ++ ".combatreset"
                , E.trEvent      = E.OnCombatStart
                , E.trCondition  = Nothing
                , E.trEffects    = [E.SetValue (E.VRVariable (avbVarName av)) (E.EVInt maxVal)]
                , E.trOnce       = False
                , E.trCooldown   = 0
                , E.trWeight     = 1
                , E.trRequires   = []
                , E.trChainsTo   = []
                }
            ]
        _ -> []

    makeRefillTriggers av
        | avbRefillPerTurn av > 0 =
            [ E.TriggerDef
                { E.trId         = "var." ++ avbVarName av ++ ".refill"
                , E.trEvent      = E.OnTurn
                , E.trCondition  = Nothing
                , E.trEffects    = [E.ModifyValue (E.VRVariable (avbVarName av)) (avbRefillPerTurn av)]
                , E.trOnce       = False
                , E.trCooldown   = 0
                , E.trWeight     = 1
                , E.trRequires   = []
                , E.trChainsTo   = []
                }
            ]
        | otherwise = []

-- | K2: Canonical custom event name for an NPC AI state.
-- Schema: "npc_ai_<npcId>_<stateName>"
-- This prefixed namespace guarantees that AI state transition events
-- never collide with author-defined custom events (e.g. "alarm", "hebel_umgelegt").
npcAiStateEvent :: String -> String -> String
npcAiStateEvent nId stName = "npc_ai_" ++ nId ++ "_" ++ stName

-- | K2: Compile NPC AI behavior states into engine TriggerDefs.
-- Each state in `npcs.<id>.ai.states.<state>` compiles to exactly one TriggerDef:
--
-- 1. Identifier: "ai.<npcId>.<stateName>" (guarded by reservedTriggerPrefixes).
-- 2. Event:
--    - If `on:` is omitted: OnCustomEvent "npc_ai_<npcId>_<stateName>"
--    - If `on: {custom: xyz}` or `on: custom xyz`: OnCustomEvent xyz
--    - If `on: turn`: OnTurn
--    - Otherwise: parsed via `compileAtOn`
-- 3. Condition:
--    - If event is OnTurn or another non-custom event: gates on `state.<npcId> == <stateName>`
--      (combined with `when:` if specified).
--    - If event is OnCustomEvent: uses `when:` if specified.
-- 4. Effects:
--    - If event is OnCustomEvent: starts with setting `state.<npcId>` to `<stateName>`
--      so transitions via `chains_to` update the behavior state.
--    - Compiled authored effects (`asdEffects`).
--    - If `go_to: <target>` is specified:
--      appends effects that set `state.<npcId>` to `<target>` and raise
--      `npc_ai_<npcId>_<target>` to trigger the target state.
-- 5. Requires: `asdRequires` (K3 flag gate).
-- 6. ChainsTo: `asdChainsTo`, where target state names of the same NPC
--    resolve to `npc_ai_<npcId>_<target>` (K3 event chaining).
-- 7. Once, Cooldown, Weight: from `ANpcStateDef`.
compileNpcAI :: [ANPC] -> ([CompileIssue], [E.TriggerDef])
compileNpcAI npcs =
    let results = concatMap compileOneNpc npcs
        errs = concat [e | Left e <- results]
        trigs = [d | Right d <- results]
    in (errs, trigs)
  where
    compileOneNpc n = case anAI n of
        Nothing -> []
        Just ai ->
            let states = aiStates ai
                stateNames = map fst states
                nid = anId n
            in map (compileState nid stateNames) states

    compileState nid stateNames (stName, stDef) =
        case resolveEvent of
            Left err -> Left [ciError ("npcs." ++ nid ++ ".ai.states." ++ stName) "BadTriggerEvent" err]
            Right (ev, isCustom) ->
                case validateGoTo of
                    Just err -> Left [err]
                    Nothing -> Right E.TriggerDef
                        { E.trId = "ai." ++ nid ++ "." ++ stName
                        , E.trEvent = ev
                        , E.trCondition = cond isCustom
                        , E.trEffects = effs isCustom
                        , E.trOnce = asdOnce stDef
                        , E.trCooldown = asdCooldown stDef
                        , E.trWeight = asdWeight stDef
                        , E.trRequires = asdRequires stDef
                        , E.trChainsTo = chains
                        }
      where
        customEvName = npcAiStateEvent nid stName

        resolveEvent
            | null (asdOn stDef) = Right (E.OnCustomEvent customEvName, True)
            | asdOn stDef == "turn" = Right (E.OnTurn, False)
            | "custom " `isPrefixOf` asdOn stDef =
                Right (E.OnCustomEvent (drop 7 (asdOn stDef)), True)
            | otherwise = case compileAtOn (asdOn stDef) of
                Left err -> Left err
                Right (E.OnCustomEvent c) -> Right (E.OnCustomEvent c, True)
                Right other -> Right (other, False)

        cond isCustom =
            let stateCheck = E.VarIs ("state." ++ nid) stName
            in if isCustom
               then asdWhen stDef
               else case asdWhen stDef of
                   Nothing -> Just stateCheck
                   Just p  -> Just (E.PAll [stateCheck, p])

        validateGoTo = case asdGoTo stDef of
            Nothing -> Nothing
            Just tgt
                | tgt `elem` stateNames -> Nothing
                | otherwise -> Just (ciError ("npcs." ++ nid ++ ".ai.states." ++ stName ++ ".go_to")
                                     "UnknownState"
                                     ("go_to targets unknown state '" ++ tgt ++ "'"))

        effs isCustom =
            let enterEff = if isCustom
                           then [E.SetValue (E.VRVariable ("state." ++ nid)) (E.EVString stName)]
                           else []
                authoredEffs = map (compileAActionOutcomeWith (Set.singleton nid)) (asdEffects stDef)
                transitionEffs = case asdGoTo stDef of
                    Nothing -> []
                    Just tgt ->
                        [ E.SetValue (E.VRVariable ("state." ++ nid)) (E.EVString tgt)
                        , E.RaiseEvent (npcAiStateEvent nid tgt)
                        ]
            in enterEff ++ authoredEffs ++ transitionEffs

        chains = map (\target ->
            if target `elem` stateNames
            then npcAiStateEvent nid target
            else target) (asdChainsTo stDef)

-- | Compiler-owned trigger-id prefixes. The compiler generates triggers with
--   these ids (encounter.<id>, environment.*, stealth.*, ship.*, party.*) and
--   they share the runtime `triggerStates` namespace with author `rules:` ids.
reservedTriggerPrefixes :: [String]
reservedTriggerPrefixes = ["encounter.", "environment.", "stealth.", "ship.", "party.", "bark.", "talk.", "ai.", "var."]

-- | Validate authored trigger rules: ids must be unique and must not use a
--   compiler-owned prefix (which would silently hijack a module trigger).
checkTriggerIds :: [ATrigger] -> [CompileIssue]
checkTriggerIds ts =
    [ ciError ("rules." ++ tid) "DuplicateTriggerId"
        ("rule id '" ++ tid ++ "' is declared more than once")
    | (tid, others) <- collisions [(atId t, atId t) | t <- ts]
    , not (null others) ]
    ++
    [ ciError ("rules." ++ atId t) "ReservedTriggerId"
        ("rule id '" ++ atId t ++ "' uses a compiler-owned prefix")
    | t <- ts, p <- reservedTriggerPrefixes, p `isPrefixOf` atId t ]

-- | Every `on: command <verb>` rule must name a verb the runtime can actually
--   emit (`GameLoop.commandVerbName`): a core command name or a declared
--   custom verb. Otherwise the rule validates clean and silently never fires.
checkCommandVerbRefs :: Map.Map String E.VerbDef -> [ATrigger] -> [CompileIssue]
checkCommandVerbRefs registry triggers =
    [ ciError ("rules." ++ atId t) "UnknownCommandVerb"
        ("rule '" ++ atId t ++ "' listens on '" ++ prefix ++ " " ++ v
         ++ "', which is neither a core command nor a declared custom verb")
    | t <- triggers
    , Just (prefix, v) <- [commandVerbOf (atOn t)]
    , not (Set.member (map toLower v) known) ]
  where
    known = Set.fromList (map (map toLower) (Verbs.coreCommandVerbs ++ Map.keys registry))
    commandVerbOf s = case words s of
        ["command", v] -> Just ("command", v)
        ["before", v]  -> Just ("before", v)
        _              -> Nothing

-- | Every `chains_to:` target must name a custom event that some rule listens
--   on via `on: custom <name>`. Dead targets indicate author typos and fail compilation.
checkChainTargets :: [ATrigger] -> [ANPC] -> [CompileIssue]
checkChainTargets triggers npcs =
    triggerIssues ++ aiIssues
  where
    knownCustomEvents = Set.fromList
        ( [ map toLower n
          | t <- triggers
          , Right (E.OnCustomEvent n) <- [compileAtOn (atOn t)]
          ]
          ++
          [ map toLower (npcAiStateEvent (anId n) stName)
          | n <- npcs
          , Just ai <- [anAI n]
          , (stName, stDef) <- aiStates ai
          , null (asdOn stDef)
          ]
          ++
          [ map toLower c
          | n <- npcs
          , Just ai <- [anAI n]
          , (_, stDef) <- aiStates ai
          , not (null (asdOn stDef))
          , Right (E.OnCustomEvent c) <- [compileAiEvent (asdOn stDef)]
          ]
        )

    compileAiEvent onStr
        | onStr == "turn" = Right E.OnTurn
        | "custom " `isPrefixOf` onStr = Right (E.OnCustomEvent (drop 7 onStr))
        | otherwise = compileAtOn onStr

    triggerIssues =
        [ ciError ("rules." ++ atId t ++ ".chains_to") "UnknownChainTarget"
            ("rule '" ++ atId t ++ "' chains to unknown event '" ++ target
             ++ "' (no rule listens on 'on: custom " ++ target ++ "')")
        | t <- triggers
        , target <- atChainsTo t
        , map toLower target `Set.notMember` knownCustomEvents
        ]

    aiIssues =
        [ ciError ("npcs." ++ anId n ++ ".ai.states." ++ stName ++ ".chains_to") "UnknownChainTarget"
            ("state '" ++ stName ++ "' chains to unknown event '" ++ target
             ++ "' (no rule listens on 'on: custom " ++ target ++ "')")
        | n <- npcs
        , Just ai <- [anAI n]
        , let stateNames = map fst (aiStates ai)
        , (stName, stDef) <- aiStates ai
        , target <- asdChainsTo stDef
        , target `notElem` stateNames
        , map toLower target `Set.notMember` knownCustomEvents
        ]

-- | K2: Every `set_state` target must resolve to a known NPC, item, device,
--   container, vehicle, or exit lock (static or dynamic).
checkStateTargetRefs :: Adventure -> Map.Map String E.Room -> [CompileIssue]
checkStateTargetRefs adv rooms =
    [ ciError (path ++ ".set_state") "UnknownStateTarget"
        ("set_state targets unknown entity or NPC '" ++ target ++ "'")
    | (path, outs) <- outcomeSurfaces adv
    , AOSetEntityState target _ <- deepOutcomes outs
    , target `Set.notMember` validTargets
    ]
  where
    validTargets = Set.unions
        [ Set.fromList (map anId (advNPCs adv))
        , Set.fromList (map aiId (advItems adv))
        , Set.fromList (map adId (advDevices adv))
        , Set.fromList (map acnId (advContainers adv))
        , Set.fromList (map avId (advVehicles adv))
        , staticExitLocks
        , dynamicExitLocks
        ]
    staticExitLocks = Set.fromList
        [ lockKey
        | room <- Map.elems rooms
        , E.Locked _ lockKey <- Map.elems (E.roomConnections room)
        ]
    dynamicExitLocks = Set.fromList
        [ lockKey
        | AOSetExit _ _ _ (Just lockKey) <- allAOutcomes adv
        ]

-- | A stop `cost.item` must be a declared item id (P1-19) — otherwise the fare
--   can never be paid and the vehicle silently behaves like `auto`.
checkStopCostItems :: [AVehicle] -> E.GameWorld -> [CompileIssue]
checkStopCostItems vehicles gw =
    [ ciError ("vehicles." ++ avId v ++ ".stops." ++ label) "UnknownStopCostItem"
        ("stop '" ++ label ++ "' costs item '" ++ iid ++ "', which is not declared")
    | v <- vehicles
    , (label, stop) <- Map.toList (avStops v)
    , Just (iid, _) <- [asCost stop]
    , not (Map.member iid (E.itemDefs gw)) ]

-- | Parse the `on` string into an EventType.
--   Supported: "enter <room>", "leave <room>", "look <room>", "search <room>",
--   "take <item>", "drop <item>", "use <item>", "state <entity>", "custom <name>",
--   "command <verb>", "before <verb>", "turn".
compileAtOn :: String -> Either String E.EventType
compileAtOn s =
    case words (map toLower s) of
        ["turn"]                     -> Right E.OnTurn
        ["combat_start"]             -> Right E.OnCombatStart
        ["combat", "start"]          -> Right E.OnCombatStart
        ["talk"]                     -> Right (E.OnTalk "" "")
        ["enter", r]                 -> Right (E.OnEnter r)
        ["leave", r]                 -> Right (E.OnLeave r)
        ["look", r]                  -> Right (E.OnLook r)
        ["search", r]                -> Right (E.OnSearch r)
        ["take", i]                  -> Right (E.OnTake i)
        ["drop", i]                  -> Right (E.OnDrop i)
        ["use", i]                   -> Right (E.OnUse i)
        ["state", e]                 -> Right (E.OnStateChange e)
        ["custom", n]                -> Right (E.OnCustomEvent n)
        ["command", v]               -> Right (E.OnCommand v)
        ["chapter", cid]             -> Right (E.OnChapter cid)
        ["before", v]                -> Right (E.OnBefore v)
        ["levelup", lvlStr]          -> case readMaybe lvlStr of
            Just lvl -> Right (E.OnLevelUp lvl)
            Nothing  -> Left ("Invalid level number in 'levelup " ++ lvlStr ++ "'")
        ["level_up", lvlStr]         -> case readMaybe lvlStr of
            Just lvl -> Right (E.OnLevelUp lvl)
            Nothing  -> Left ("Invalid level number in 'level_up " ++ lvlStr ++ "'")
        _                            -> Left ("Unsupported trigger event '" ++ s ++ "'")

-- ---------------------------------------------------------------------------
-- Encounter tables (Phase 7c)
-- ---------------------------------------------------------------------------

-- | Compile encounter tables into one trigger each: a `RandomChoice` over the
--   weighted entries, wrapped so per-entry `when` gates become a Conditional.
compileEncounterTables :: [AEncounterTable] -> ([CompileIssue], [E.TriggerDef])
compileEncounterTables tables =
    let results = map compileTable tables
        errors = concat [e | Left e <- results]
        defs = [d | Right d <- results]
    in (errors, defs)
  where
    compileTable t = case compileAtOn (ertOn t) of
        Left err -> Left [ciError ("encounter_tables." ++ ertId t) "BadTriggerEvent" err]
        Right ev -> Right E.TriggerDef
            { E.trId = "encounter." ++ ertId t
            , E.trEvent = ev
            , E.trCondition = ertWhen t
            , E.trEffects = [E.RandomChoice [(eneWeight e, compileEntry e) | e <- ertEntries t]]
            , E.trOnce = False
            , E.trCooldown = ertCooldown t
            , E.trWeight = 1
            , E.trRequires = []
            , E.trChainsTo = []
            }
    compileEntry e = case eneWhen e of
        Nothing -> compileOutcomes (eneEffects e)
        Just p  -> E.Conditional p (compileOutcomes (eneEffects e)) E.Noop

-- | Validate encounter tables: unique ids, non-empty entries, positive weights.
checkEncounterRefs :: [AEncounterTable] -> E.GameWorld -> [CompileIssue]
checkEncounterRefs tables _ =
    [ ciError ("encounter_tables." ++ ertId t) "DuplicateEncounterTable"
        ("table '" ++ ertId t ++ "' is declared more than once")
    | t <- tables
    , length [t' | t' <- tables, ertId t == ertId t'] > 1 ]
    ++ [ ciError ("encounter_tables." ++ ertId t) "EmptyEncounterTable"
            "encounter table has no entries"
       | t <- tables, null (ertEntries t) ]
    ++ [ ciError ("encounter_tables." ++ ertId t ++ ".entries") "BadEncounterWeight"
            "entry weight must be a positive integer"
       | t <- tables, e <- ertEntries t, eneWeight e < 1 ]

-- ---------------------------------------------------------------------------
-- Player & initial state (Phase 4d)
-- ---------------------------------------------------------------------------

-- | Compile optional player stats block; defaults 100/100/10/5/no skills.
compilePlayer :: Maybe AAdventurePlayer -> E.Player
compilePlayer Nothing = E.Player 100 100 10 5 Map.empty
compilePlayer (Just ap) = E.Player
    { E.playerHealth    = fromMaybe 100 (apMaxHealth ap)
    , E.playerMaxHealth = fromMaybe 100 (apMaxHealth ap)
    , E.playerAttack    = fromMaybe 10 (apAttack ap)
    , E.playerDefense   = fromMaybe 5 (apDefense ap)
    , E.playerSkills    = apSkills ap
    }

-- | Overlay initial_variables YAML over the declared var initial values.
--   Validates type consistency against declared varDefs.
compileInitialVariables :: Map.Map String E.VarDef -> Map.Map String E.VariableValue
                         -> Map.Map String Aeson.Value
                         -> ([CompileIssue], Map.Map String E.VariableValue)
compileInitialVariables varDefs base overrides =
    let (errs, over) = partitionEithers
            [ toVar name v | (name, v) <- Map.toList overrides ]
    in (errs, Map.union (Map.fromList over) base)
  where
    toVar name v =
        let shapeVal = case v of
                Aeson.Number n -> Right (E.VVInt (truncate n :: Int))
                Aeson.String s  -> Right (E.VVText (T.unpack s))
                Aeson.Bool b    -> Right (E.VVBool b)
                _               -> Left "must be a number, string, or boolean"
        in case shapeVal of
            Left err -> Left (ciError ("initial_variables." ++ name) "BadInitialVariable" err)
            Right val -> case Map.lookup name varDefs of
                Nothing -> Right (name, val)
                Just vd -> if compatible (E.vdVarType vd) val
                           then Right (name, val)
                           else Left (ciError ("initial_variables." ++ name) "VariableTypeMismatch"
                                        ("expected " ++ show (E.vdVarType vd)))

    compatible :: E.VariableType -> E.VariableValue -> Bool
    compatible E.VTBool      E.VVBool{}   = True
    compatible (E.VTInt _ _) E.VVInt{}    = True
    compatible E.VTText      E.VVText{}   = True
    compatible (E.VTEnum xs) (E.VVText s) = s `elem` xs
    compatible _             _            = False

-- | Compile initial flags and active quests from YAML.
--   Unknown quest IDs are compile errors.
compileInitialState :: [String] -> Map.Map String String -> Map.Map String E.Quest
                    -> ([CompileIssue], Map.Map String String, Map.Map String Int)
compileInitialState questIds flagMap questDefs =
    let missing = [ q | q <- questIds, not (Map.member q questDefs) ]
        errs = [ ciError ("active_quests." ++ q) "UnknownQuest" "active quest is not defined in questDefs"
               | q <- missing ]
    in (errs, flagMap, Map.fromList [(q, 0) | q <- questIds, Map.member q questDefs])

-- ---------------------------------------------------------------------------
-- Cards & Deck (Schritt 2 / Phase 2D)
-- ---------------------------------------------------------------------------

-- | Compile authored cards into engine Cards and validate card types and targets.
compileCards :: [ACard] -> ([CompileIssue], Map.Map String E.Card)
compileCards cards =
    let results = map compileOneCard cards
        errors = concat [e | Left e <- results]
        dupErrs =
            [ ciError ("cards." ++ cid) "DuplicateCardId"
                ("card id '" ++ cid ++ "' is declared more than once")
            | (cid, others) <- collisions [(acdId c, acdId c) | c <- cards]
            , not (null others) ]
        cardMap = Map.fromList [ (acdId c, card) | Right (c, card) <- results ]
    in (errors ++ dupErrs, cardMap)
  where
    compileOneCard c =
        let cPath = "cards." ++ acdId c
            mType = parseCardType (acdType c)
            mTarget = parseCardTarget (acdTarget c)
            typeErr = case mType of
                Nothing -> [ciError (cPath ++ ".type") "UnknownCardType"
                             ("Unknown card type '" ++ acdType c ++ "' (expected: attack, skill, power, curse, status)")]
                Just _  -> []
            targetErr = case mTarget of
                Nothing -> [ciError (cPath ++ ".target") "UnknownCardTarget"
                             ("Unknown card target '" ++ acdTarget c ++ "' (expected: self, single_enemy, all_enemies, none)")]
                Just _  -> []
            allErrs = typeErr ++ targetErr
        in if null allErrs
           then Right (c, E.Card
                { E.cardId = acdId c
                , E.cardName = if null (acdName c) then acdId c else acdName c
                , E.cardCost = acdCost c
                , E.cardType = fromMaybe E.CardSkill mType
                , E.cardDescription = acdDescription c
                , E.cardTarget = fromMaybe E.TargetNone mTarget
                , E.cardExhaust = acdExhaust c
                , E.cardEffects = map compileAActionOutcome (acdOutcomes c)
                })
           else Left allErrs

    parseCardType s = case map toLower s of
        "attack" -> Just E.CardAttack
        "skill"  -> Just E.CardSkill
        "power"  -> Just E.CardPower
        "curse"  -> Just E.CardCurse
        "status" -> Just E.CardStatus
        _        -> Nothing

    parseCardTarget s = case map toLower s of
        "self"         -> Just E.TargetSelf
        "single_enemy" -> Just E.TargetSingleEnemy
        "single"       -> Just E.TargetSingleEnemy
        "enemy"        -> Just E.TargetSingleEnemy
        "all_enemies"  -> Just E.TargetAllEnemies
        "all"          -> Just E.TargetAllEnemies
        "none"         -> Just E.TargetNone
        _              -> Nothing

-- ---------------------------------------------------------------------------
-- Procedural Sandbox Zones (Schritt 3 / Phase 3E)
-- ---------------------------------------------------------------------------

-- | Compile authored sandbox zones into engine SandboxZones and validate biomes and directions.
compileSandboxZones :: [ASandboxZone] -> ([CompileIssue], Map.Map String E.SandboxZone)
compileSandboxZones zones =
    let results = map compileOneZone zones
        errors = concat [e | Left e <- results]
        dupErrs =
            [ ciError ("sandbox_zones." ++ zid) "DuplicateSandboxZone"
                ("sandbox zone id '" ++ zid ++ "' is declared more than once")
            | (zid, others) <- collisions [(aszId z, aszId z) | z <- zones]
            , not (null others) ]
        zoneMap = Map.fromList [ (aszId z, sz) | Right (z, sz) <- results ]
    in (errors ++ dupErrs, zoneMap)
  where
    compileOneZone z =
        let zPath = "sandbox_zones." ++ aszId z
            idErr = if null (aszId z)
                    then [ciError "sandbox_zones" "EmptySandboxZoneId" "sandbox zone id must not be empty"]
                    else []
            emptyBiomesErr = if null (aszBiomes z)
                             then [ciError zPath "EmptySandboxZone" "sandbox zone has no biomes declared"]
                             else []
            (biomeErrs, compiledBiomes) = compileBiomes zPath (aszBiomes z)
            allErrs = idErr ++ emptyBiomesErr ++ biomeErrs
        in if null allErrs
           then Right (z, E.SandboxZone
                { E.szId = aszId z
                , E.szOrigin = aszOrigin z
                , E.szBiomes = compiledBiomes
                , E.szDefaultFloor = aszFloor z
                })
           else Left allErrs

    compileBiomes zPath biomes =
        let results = map (compileOneBiome zPath) biomes
            errors = concat [e | Left e <- results]
            dupErrs =
                [ ciError (zPath ++ ".biomes." ++ bid) "DuplicateBiomeId"
                    ("biome id '" ++ bid ++ "' is declared more than once in zone")
                | (bid, others) <- collisions [(abtId b, abtId b) | b <- biomes]
                , not (null others) ]
            biomeList = [ b | Right b <- results ]
        in (errors ++ dupErrs, biomeList)

    compileOneBiome zPath b =
        let bPath = zPath ++ ".biomes." ++ abtId b
            idErr = if null (abtId b)
                    then [ciError (zPath ++ ".biomes") "EmptyBiomeId" "biome id must not be empty"]
                    else []
            weight = fromMaybe 1 (abtWeight b)
            weightErr = if weight <= 0
                        then [ciError (bPath ++ ".weight") "BadBiomeWeight" "biome weight must be a positive integer"]
                        else []
            parsedDirs = [(d, parseDir d) | d <- abtPassableDirs b]
            dirErrs = [ ciError (bPath ++ ".passable_dirs." ++ d) "UnknownDirection" msg
                      | (d, Left msg) <- parsedDirs ]
            goodDirs = if null (abtPassableDirs b)
                       then [E.North, E.South, E.East, E.West]
                       else [dir | (_, Right dir) <- parsedDirs]
            allErrs = idErr ++ weightErr ++ dirErrs
        in if null allErrs
           then Right E.BiomeTemplate
                { E.btId = abtId b
                , E.btWeight = weight
                , E.btNamePattern = fromMaybe "Wildnis ({x}, {y})" (abtNamePattern b)
                , E.btDescription = compileCondText (abtDescription b)
                , E.btTags = abtTags b
                , E.btAsciiArt = case abtAsciiArt b of
                    Just aa -> compileAscii aa
                    Nothing -> E.emptyAscii
                , E.btPassableDirs = goodDirs
                }
           else Left allErrs

-- ---------------------------------------------------------------------------
-- Validation warnings (Phase 0.4)
-- ---------------------------------------------------------------------------

-- | Phase 0.4: warn when two entities (items, NPCs) placed in the same room share a keyword.
-- | B7: NPC possession references must resolve — `carried_by:` on items and
--   `give: {item: …, to: …}` outcomes may only name "player" or an existing
--   NPC id (the engine's actor-string convention).
checkNpcPossessionRefs :: Adventure -> [CompileIssue]
checkNpcPossessionRefs adv =
    concatMap itemGo (advItems adv) ++ concatMap outcomeGo (allAOutcomes adv)
  where
    npcIds = Set.fromList (map anId (advNPCs adv))
    bad a = a /= "player" && a `Set.notMember` npcIds
    itemGo i =
        [ ciError ("items." ++ aiId i ++ ".carried_by") "UnknownNpc"
            ("item '" ++ aiId i ++ "' starts on unknown npc '" ++ a ++ "'")
        | Just a <- [aiCarriedBy i], bad a ]
    outcomeGo (AOGiveTo _ tgt) =
        [ ciError "outcomes.give.to" "UnknownNpc"
            ("give target '" ++ tgt ++ "' is not 'player' or an existing npc id")
        | bad tgt ]
    outcomeGo (AOGiveEquipTo _ tgt) =
        [ ciError "outcomes.give.to" "UnknownNpc"
            ("give target '" ++ tgt ++ "' is not 'player' or an existing npc id")
        | bad tgt ]
    outcomeGo _ = []

-- | B9: both halves of an `interactions: npc:` entry must resolve. A typo in
--   the item id would silently fall through to the attack fallback, so both
--   references are hard errors (same contract as `devices.*.fits`).
checkNpcInteractionRefs :: Adventure -> [CompileIssue]
checkNpcInteractionRefs adv = concatMap entryGo (maybe [] aiNpc (advInteractions adv))
  where
    npcIds = Set.fromList (map anId (advNPCs adv))
    itemIds = Set.fromList (map aiId (advItems adv))
    entryGo n = concat
        [ [ ciError ("interactions.npc[" ++ aniItem n ++ "]") "UnknownNpcInteractionItem"
            ("npc interaction references unknown item '" ++ aniItem n ++ "'")
        | aniItem n `Set.notMember` itemIds ]
        , [ ciError ("interactions.npc[" ++ aniItem n ++ "]") "UnknownNpc"
            ("npc interaction references unknown npc '" ++ aniTarget n ++ "'")
        | aniTarget n `Set.notMember` npcIds ]
        ]

-- | K11a: dynamic item references in consume: must be bound.
--   Inside 'interactions: item:', {item1} and {item2} are bound.
--   Everywhere else, dynamic {item*} references are unbound and rejected.
checkDynamicItemRefs :: Adventure -> [CompileIssue]
checkDynamicItemRefs adv = itemIxIssues ++ otherIssues
  where
    itemIxIssues = concatMap checkItemIx (maybe [] aiItem (advInteractions adv))
    checkItemIx ix =
        let path = "interactions.item[" ++ aiiItem1 ix ++ "," ++ aiiItem2 ix ++ "]"
            allowed = Set.fromList ["{item1}", "{item2}", "{var:item1}", "{var:item2}"]
        in [ ciError (path ++ ".consume") "UnknownItemRef"
                ("consume: " ++ formatUnknownKey ref (Set.fromList ["{item1}", "{item2}"]))
           | AOConsumeItem ref <- deepOutcomes (aiiEffects ix)
           , '{' `elem` ref
           , ref `Set.notMember` allowed ]

    otherSurfaces =
        [ (path, outs)
        | (path, outs) <- outcomeSurfaces adv
        , path /= "interactions" ]
        ++ case advInteractions adv of
            Just ai -> [ ("interactions.npc[" ++ aniItem n ++ "]", aniEffects n) | n <- aiNpc ai ]
            Nothing -> []

    otherIssues =
        [ ciError (path ++ ".consume") "UnknownItemRef"
            ("consume references unbound dynamic item '" ++ ref ++ "'")
        | (path, outs) <- otherSurfaces
        , AOConsumeItem ref <- deepOutcomes outs
        , '{' `elem` ref ]

-- | 4.6: two authored map positions on the same cell of the same floor. A hard
--   error like every other duplicate in the schema (cf. `DuplicateDirection`):
--   the map cannot say which room owns the cell, and the editor would draw one
--   on top of the other. Only *authored* positions can collide — the auto-layout
--   never places a room on an occupied cell — so this cannot fire on computed
--   positions.
checkMapPositions :: Adventure -> [CompileIssue]
checkMapPositions adv = concatMap floorCells floors
  where
    floors = Set.toAscList (Set.fromList (map mapFloorOf (advRooms adv)))
    floorCells f =
        [ ciError ("rooms." ++ rid) "MapOverlap"
            ("map position (" ++ show x ++ "," ++ show y ++ ") on floor "
             ++ show f ++ " is already taken by room '" ++ other ++ "'")
        | (x, y, rid, other) <- hits ]
      where
        placed = [ (arId r, p) | r <- advRooms adv, mapFloorOf r == f, Just p <- [arMapPos r] ]
        hits = [ (E.mapPosX p, E.mapPosY p, a, b)
               | (a, p) <- placed, (b, q) <- placed
               , a < b
               , E.mapPosX p == E.mapPosX q
               , E.mapPosY p == E.mapPosY q ]

-- | 4.6: every quest reference must resolve, and resolve *here* — with a
--   diagnostic path, so @Locate@ can point at the authoring line. The world
--   validator reports the same class as 'MissingQuest', but only after the
--   world exists, without a Fundstelle, and @--force@ writes anyway (measured:
--   a typo'd @start_quest:@ exits 1 today, so this is a sharpening of the
--   diagnostic, not a new gap). Two surfaces:
--
--   * quest-effect targets (@start_quest:@/@advance_quest:@/@complete_quest:@)
--     in every outcome surface, nested branches included ('deepOutcomes');
--   * @on_complete:@ on a quest, which used to be parsed by nobody at all.
checkQuestRefs :: Adventure -> [CompileIssue]
checkQuestRefs adv =
    concatMap surfaceGo (outcomeSurfaces adv) ++ concatMap questGo (advQuests adv)
  where
    questIds = Set.fromList (map aqId (advQuests adv))
    surfaceGo (path, outs) =
        [ err
        | o <- deepOutcomes outs
        , (kind, qId) <- questRef o
        , Just err <- [badEffect path kind qId] ]
    questRef o = case o of
        AOStartQuest qId    -> [("start_quest", qId)]
        AOAdvanceQuest qId  -> [("advance_quest", qId)]
        AOCompleteQuest qId -> [("complete_quest", qId)]
        _ -> []
    badEffect path kind qId
        | qId `Set.member` questIds = Nothing
        | otherwise = Just $ ciError path "UnknownQuestEffect"
            (kind ++ " references unknown quest '" ++ qId ++ "'")
    questGo q = case aqOnComplete q of
        Just next | next `Set.notMember` questIds ->
            [ ciError ("quests." ++ aqId q ++ ".on_complete") "UnknownOnComplete"
                ("on_complete references unknown quest '" ++ next ++ "'") ]
        _ -> []

checkKeywordCollisions :: Adventure -> [CompileIssue]
checkKeywordCollisions adv =
    let roomItems =
            [ (aiLocation i, "item '" ++ aiId i ++ "'", kw)
            | i <- advItems adv
            , aiLocation i /= "inventory"
            , isNothing (aiInContainer i)
            , isNothing (aiCarriedBy i)
            , kw <- nub (map (map toLower . trimSpaces) (aiKeywords i))
            , not (null kw)
            ]
        roomNPCs =
            [ (anLocation n, "npc '" ++ anId n ++ "'", kw)
            | n <- advNPCs adv
            , kw <- nub (map (map toLower . trimSpaces) (anKeywords n))
            , not (null kw)
            ]
        allEntries = roomItems ++ roomNPCs
        byRoom = Map.fromListWith (++) [ (r, [(ent, kw)]) | (r, ent, kw) <- allEntries ]
        issuesForRoom (r, ents) =
            let byKw = Map.fromListWith (++) [ (kw, [ent]) | (ent, kw) <- ents ]
            in [ ciWarning ("rooms." ++ r) "KeywordCollision"
                    ("keyword '" ++ kw ++ "' in room '" ++ r ++ "' is shared by " ++ formatEntities (nub (reverse es)))
               | (kw, es) <- Map.toList byKw
               , length (nub es) > 1
               ]
        formatEntities [e1, e2] = e1 ++ " and " ++ e2
        formatEntities es = intercalate ", " es
        trimSpaces = dropWhile isSpace . reverse . dropWhile isSpace . reverse
    in concatMap issuesForRoom (Map.toList byRoom)

-- | K1: Validate roll_dice outcomes.
--   Hard errors: pool < 1, die < 2, keep < 0 or keep > pool.
checkRollDice :: Adventure -> [CompileIssue]
checkRollDice adv =
    concatMap surfaceIssues (outcomeSurfaces adv)
  where
    surfaceIssues (path, os) = concatMap (diceIssues path) (deepOutcomes os)
    diceIssues path (AORollDice pool die _stream keep) =
        [ ciError (path ++ ".roll_dice.pool") "InvalidDicePool"
            ("dice pool must be at least 1 (got " ++ show pool ++ ")")
        | pool < 1 ]
        ++
        [ ciError (path ++ ".roll_dice.die") "InvalidDiceSides"
            ("die sides must be at least 2 (got " ++ show die ++ ")")
        | die < 2 ]
        ++
        [ ciError (path ++ ".roll_dice.keep") "InvalidDiceKeep"
            ("keep count must be between 0 and pool (" ++ show pool ++ "), got " ++ show keep)
        | keep < 0 || keep > pool ]
    diceIssues _ _ = []

-- | Specification of an engine-reserved variable prefix and its write-protection rules.
data ReservedVarWriteRule = ReservedVarWriteRule
    { rvrPrefix        :: String
    , rvrCode          :: String
    , rvrMessage       :: String -> String
    , rvrCheckOutcomes  :: Bool
    , rvrCheckInitials :: Bool
    , rvrCheckProcs    :: Bool
    }

reservedVarWriteRules :: [ReservedVarWriteRule]
reservedVarWriteRules =
    [ ReservedVarWriteRule
        { rvrPrefix        = "rng."
        , rvrCode          = "RngVarWrite"
        , rvrMessage       = \n -> "variable '" ++ n
            ++ "' is reserved for named RNG streams (B8) and cannot be written by content"
        , rvrCheckOutcomes  = True
        , rvrCheckInitials = True
        , rvrCheckProcs    = True
        }
    , ReservedVarWriteRule
        { rvrPrefix        = "dice."
        , rvrCode          = "RngVarWrite"
        , rvrMessage       = \n -> "variable '" ++ n
            ++ "' is reserved and cannot be written by content"
        , rvrCheckOutcomes  = True
        , rvrCheckInitials = True
        , rvrCheckProcs    = True
        }
    , ReservedVarWriteRule
        { rvrPrefix        = "chapter."
        , rvrCode          = "ChapterVariableClash"
        , rvrMessage       = \n -> "'" ++ n
            ++ "' is in the reserved 'chapter.' namespace; the engine owns the chapter state (W3)"
        , rvrCheckOutcomes  = False
        , rvrCheckInitials = False
        , rvrCheckProcs    = False
        }
    , ReservedVarWriteRule
        { rvrPrefix        = "combat."
        , rvrCode          = "CombatVariableClash"
        , rvrMessage       = \n -> "'" ++ n
            ++ "' is in the reserved 'combat.' namespace; the engine owns the combat round state (7f-3)"
        , rvrCheckOutcomes  = False
        , rvrCheckInitials = False
        , rvrCheckProcs    = False
        }
    , ReservedVarWriteRule
        { rvrPrefix        = "statement."
        , rvrCode          = "StatementVariableClash"
        , rvrMessage       = \n -> "'" ++ n
            ++ "' is in the reserved 'statement.' namespace; the engine owns the statement metadata variables (K9)"
        , rvrCheckOutcomes  = True
        , rvrCheckInitials = True
        , rvrCheckProcs    = True
        }
    ]

-- | Author-owned variable prefixes that content is expected and encouraged to write.
--   These must NEVER appear in 'reservedVarWriteRules'.
authorOwnedVarPrefixes :: [String]
authorOwnedVarPrefixes = ["state."]

-- | Unified write protection for engine-reserved variable namespaces:
--   - @rng.*@ (B8) and @dice.*@ (K1): outcomes (@set_var@/@set_text_var@/@add_var@/@compute_var@),
--     @variables:@, @initial_variables:@, and @procedures:@ parameters. Code: 'RngVarWrite'.
--   - @chapter.*@ (W3): @variables:@ declarations. Code: 'ChapterVariableClash'.
--   - @combat.*@ (7f-3): @variables:@ declarations. Code: 'CombatVariableClash'.
--   Walks 'outcomeSurfaces' + 'deepOutcomes', the one surface contract.
--
--   Note: 'checkCooldownConditionReserved' guards the @cooldown_*@ condition namespace
--   and deliberately remains separate, as conditions are a different semantic level than variables.
checkReservedVarWrites :: Adventure -> [CompileIssue]
checkReservedVarWrites adv =
    concatMap surfaceIssues (outcomeSurfaces adv)
    ++ [ ciError ("variables." ++ n) (rvrCode p) (rvrMessage p n)
       | n <- map avbVarName (advVariables adv)
       , Just p <- [findPrefix n] ]
    ++ [ ciError ("initial_variables." ++ n) (rvrCode p) (rvrMessage p n)
       | n <- Map.keys (advInitialVariables adv)
       , Just p <- [findPrefix n]
       , rvrCheckInitials p ]
    ++ [ ciError ("procedures." ++ apId pr ++ ".params." ++ n) (rvrCode p) (rvrMessage p n)
       | pr <- advProcedures adv
       , n <- apParams pr
       , Just p <- [findPrefix n]
       , rvrCheckProcs p ]
  where
    findPrefix n = find (\p -> rvrPrefix p `isPrefixOf` n) reservedVarWriteRules
    surfaceIssues (path, os) = concatMap (writeIssues path) (deepOutcomes os)
    writeIssues path ao =
        [ ciError (path ++ "." ++ key) (rvrCode p) (rvrMessage p n)
        | (key, n) <- writes ao
        , Just p <- [findPrefix n]
        , rvrCheckOutcomes p ]
    writes ao = case ao of
        AOSetVar name _     -> [("set_var", name)]
        AOSetTextVar name _ -> [("set_text_var", name)]
        AOAddVar name _     -> [("add_var", name)]
        AOComputeVar name _ -> [("compute_var", name)]
        _                   -> []

-- | Backward compatibility alias for 'checkReservedVarWrites'.
checkRngVarWrites :: Adventure -> [CompileIssue]
checkRngVarWrites = checkReservedVarWrites

-- | Phase 0.4: warn when texts reference an unknown variable placeholder '{name}'.
checkUnknownPlaceholders :: Adventure -> Map.Map String E.VarDef -> [CompileIssue]
checkUnknownPlaceholders adv varDefs =
    let writtenVars = concatMap outcomeWrittenVars (allAOutcomes adv)
        aiKnown = Set.fromList
            [ "state." ++ anId n
            | n <- advNPCs adv
            , isJust (anAI n)
            ]
        allKnown = Set.unions
            [ Map.keysSet varDefs
            , Set.fromList writtenVars
            , Set.fromList (map avbVarName (advVariables adv))
            , Map.keysSet (advInitialVariables adv)
            , aiKnown
            ]
        texts = allAdventureTexts adv
        checkText (path, str) =
            let placeholders = extractPlaceholders str
                unknowns = filter (not . isKnownPlaceholder allKnown) placeholders
            in [ ciWarning path "UnknownPlaceholder"
                    ("text references unknown variable placeholder '{" ++ p ++ "}'")
               | p <- nub unknowns ]
    in concatMap checkText texts
  where
    isKnownPlaceholder declared name
        | name `Set.member` declared = True
        | name `Set.member` systemVars = True
        | name `Set.member` commandVars = True
        | name `Set.member` engineVars = True
        | "cmd." `isPrefixOf` name = True
        | "combat." `isPrefixOf` name = True
        | "item." `isPrefixOf` name = True
        | "npc." `isPrefixOf` name = True
        | "flag." `isPrefixOf` name = True
        | "flag:" `isPrefixOf` name = True
        | "condition_turns." `isPrefixOf` name = True
        | "known." `isPrefixOf` name = True
        | "statement." `isPrefixOf` name = True
        | name `elem` ["x", "y", "z", "item1", "item2"] = True
        | hasDynamicCmd name = True
        | otherwise = False

    hasDynamicCmd s = "{cmd." `isInfixOf` s

    -- K1 (Variante A): engine-provided variables written by effects,
    -- known to the validator without author declaration in variables:.
    engineVars = Set.fromList
        [ "dice.last_roll"
        , "dice.count"
        , "dice.highest"
        , "dice.sum"
        ]

    systemVars = Set.fromList
        [ "player.hp", "player.health", "player.max_hp", "player.max_health"
        , "turn.count", "turns"
        , "hand.count", "cards_in_hand"
        , "deck.count", "draw_pile.count"
        , "discard.count", "discard_pile.count"
        , "exhaust.count", "exhaust_pile.count"
        , "room.name", "current_room.name"
        , "room.id", "current_room.id", "room"
        ]
    commandVars = Set.fromList
        [ "cmd.verb", "cmd.count", "cmd.raw_args", "cmd.target", "cmd.target_kind" ]

    extractPlaceholders [] = []
    extractPlaceholders ('\\':'{':cs) = extractPlaceholders cs
    extractPlaceholders ('\\':'}':cs) = extractPlaceholders cs
    extractPlaceholders ('{':'{':cs) = extractPlaceholders cs
    extractPlaceholders ('}':'}':cs) = extractPlaceholders cs
    extractPlaceholders ('{':cs) =
        case matchBrace cs of
            Just (inside, rest) ->
                let clean = if "var:" `isPrefixOf` inside
                            then drop 4 inside
                            else inside
                    varName = case break (== ':') clean of
                        (n, _) -> n
                    trimmed = dropWhile isSpace (reverse (dropWhile isSpace (reverse varName)))
                in if not (null trimmed)
                      && not (any isSpace trimmed)
                      && not ("if" `isPrefixOf` trimmed)
                      && not ("=" `isPrefixOf` trimmed)
                   then trimmed : extractPlaceholders rest
                   else extractPlaceholders rest
            Nothing -> extractPlaceholders cs
    extractPlaceholders (_:cs) = extractPlaceholders cs

    matchBrace str = go (1 :: Int) [] str
      where
        go 0 acc rest = Just (reverse acc, rest)
        go _ _   []   = Nothing
        go d acc ('\\':'{':rest) = go d ('{':'\\':acc) rest
        go d acc ('\\':'}':rest) = go d ('}':'\\':acc) rest
        go d acc ('{':rest)      = go (d + 1) ('{':acc) rest
        go d acc ('}':rest)
            | d == 1             = Just (reverse acc, rest)
            | otherwise          = go (d - 1) ('}':acc) rest
        go d acc (c:rest)        = go d (c:acc) rest

    outcomeWrittenVars ao = case ao of
        AOSetVar name _           -> [name]
        AOSetTextVar name _       -> [name]
        AOAddVar name _           -> [name]
        AOComputeVar name _       -> [name]
        AOConditional _ ts es     -> concatMap outcomeWrittenVars (ts ++ es)
        AONarrative _ follow      -> concatMap outcomeWrittenVars follow
        AORandomChoice _ cs       -> concatMap (concatMap outcomeWrittenVars . snd) cs
        AOApplyCondition _ _ ts es _ -> concatMap outcomeWrittenVars (ts ++ es)
        _                         -> []

    condTextStrings ct = actDefault ct : map atvText (actVariants ct)
    asciiStrings aa = concatMap condTextStrings (asaStatic aa : asaFrames aa)

    allAdventureTexts a = concat
        [ -- Rooms
          concat [ [ ("rooms." ++ arId r ++ ".desc", s) | s <- condTextStrings (arTexts r) ]
                 ++ [ ("rooms." ++ arId r ++ ".ascii", s) | s <- asciiStrings (arAscii r) ]
                 ++ maybe [] (\m -> [("rooms." ++ arId r ++ ".dark_msg", m)]) (arDarkMsg r)
                 ++ [ ("rooms." ++ arId r ++ ".exits." ++ dir ++ ".msg", m)
                    | (dir, ref) <- Map.toList (arExits r), Just m <- [aeMsg ref] ]
                 ++ concatMap (outcomeTexts ("rooms." ++ arId r))
                              (concat (catMaybes [arOnEnter r, arOnLook r, arOnExit r, arSearch r]))
                 | r <- advRooms a ]
          -- Items
        , concat [ [ ("items." ++ aiId i ++ ".desc", s) | s <- condTextStrings (aiTexts i) ]
                 ++ [ ("items." ++ aiId i ++ ".ascii", s) | s <- asciiStrings (aiAscii i) ]
                 ++ maybe [] (\m -> [("items." ++ aiId i ++ ".discover", m)]) (aiDiscover i)
                 ++ maybe [] (\m -> [("items." ++ aiId i ++ ".take_failure", m)]) (aiTakeFailure i)
                 ++ concatMap (outcomeTexts ("items." ++ aiId i ++ ".on_take")) (fromMaybe [] (aiOnTake i))
                 ++ concatMap (outcomeTexts ("items." ++ aiId i ++ ".verb_map")) (concat (Map.elems (aiVerbMap i)))
                 | i <- advItems a ]
          -- NPCs
        , concat [ [ ("npcs." ++ anId n ++ ".desc", s) | s <- condTextStrings (anTexts n) ]
                 ++ [ ("npcs." ++ anId n ++ ".ascii", s) | s <- asciiStrings (anAscii n) ]
                 ++ [ ("npcs." ++ anId n ++ ".dialogue", adnText node)
                    | tree <- Map.elems (anDialogue n)
                    , node <- Map.elems (adtNodes tree) ]
                 ++ [ ("npcs." ++ anId n ++ ".dialogue", adcText c)
                    | tree <- Map.elems (anDialogue n)
                    , node <- Map.elems (adtNodes tree)
                    , c <- adnChoices node ]
                 ++ concatMap (outcomeTexts ("npcs." ++ anId n ++ ".verb_map")) (concat (Map.elems (anVerbMap n)))
                 | n <- advNPCs a ]
          -- Rules
        , concat [ concatMap (outcomeTexts ("rules." ++ atId t)) (atEffects t)
                 | t <- advTriggers a ]
          -- Quests
        , concat [ [ ("quests." ++ aqId q ++ ".desc", aqDesc q) ]
                 ++ concatMap (outcomeTexts ("quests." ++ aqId q ++ ".reward")) (maybe [] id (aqReward q))
                 ++ concat [ [ ("quests." ++ aqId q ++ ".stages." ++ aqsId st ++ ".desc", aqsDesc st) ]
                             ++ maybe [] (\h -> [ ("quests." ++ aqId q ++ ".stages." ++ aqsId st ++ ".hint", h) ]) (aqsHint st)
                           | st <- aqStages q ]
                 | q <- advQuests a ]
          -- Cards
        , concat [ [ ("cards." ++ acdId c ++ ".desc", acdDescription c) ]
                 ++ concatMap (outcomeTexts ("cards." ++ acdId c)) (acdOutcomes c)
                 | c <- advCards a ]
          -- Sandbox zones
        , concat [ [ ("sandbox_zones." ++ aszId z ++ ".biomes." ++ abtId b ++ ".desc", s)
                   | s <- condTextStrings (abtDescription b) ]
                 ++ [ ("sandbox_zones." ++ aszId z ++ ".biomes." ++ abtId b ++ ".ascii", s)
                    | s <- maybe [] asciiStrings (abtAsciiArt b) ]
                 | z <- advSandboxZones a, b <- aszBiomes z ]
          -- Statements
        , [ ("statements." ++ stId s ++ ".text", stText s)
          | s <- advStatements a ]
        ]

    outcomeTexts path ao = case ao of
        AOMessage s                -> [(path, s)]
        AOBlock (Just s) _         -> [(path, s)]
        AOGameEnd _ (Just s)       -> [(path, s)]
        AOConditional _ ts es      -> concatMap (outcomeTexts path) (ts ++ es)
        AONarrative ls follow      -> [ (path, l) | l <- ls ] ++ concatMap (outcomeTexts path) follow
        AORandomChoice _ cs        -> concatMap (concatMap (outcomeTexts path) . snd) cs
        AOApplyCondition _ _ ts es _ -> concatMap (outcomeTexts path) (ts ++ es)
        _                          -> []

-- | Phase 0.4: warn when a dark room contains items but has no light_flag,
--   no feelable items, and no reachable lightsource (potential author dead-end).
checkDarkRoomDeadEnds :: Adventure -> [CompileIssue]
checkDarkRoomDeadEnds adv =
    let rooms = advRooms adv
        roomMap = Map.fromList [ (arId r, r) | r <- rooms ]
        items = advItems adv
        startRoomId = advStartRoom adv
        reachable = Set.fromList (reachableRoomIds startRoomId rooms)

        carriedLightsource =
            any (\i -> aiLocation i == "inventory" && "lightsource" `elem` aiTags i) items

        canPickUpLightsource item =
            case Map.lookup (aiLocation item) roomMap of
                Nothing -> False
                Just rm -> "dark" `notElem` arTags rm
                           || "feelable" `elem` aiTags item
                           || maybe False (not . null) (arLightFlag rm)

        hasReachableLightsource =
            carriedLightsource ||
            any (\i -> "lightsource" `elem` aiTags i
                       && aiLocation i /= "inventory"
                       && isNothing (aiInContainer i)
                       && isNothing (aiCarriedBy i)
                       && aiLocation i `Set.member` reachable
                       && canPickUpLightsource i) items

        checkRoom r
            | "dark" `elem` arTags r =
                let rId = arId r
                    rItems = [ i | i <- items, aiLocation i == rId, isNothing (aiInContainer i), isNothing (aiCarriedBy i) ]
                    hasItems = not (null rItems)
                    hasFeelable = any (\i -> "feelable" `elem` aiTags i) rItems
                    hasLightFlag = maybe False (not . null) (arLightFlag r)
                in if hasItems && not hasFeelable && not hasLightFlag && not hasReachableLightsource
                   then [ ciWarning ("rooms." ++ rId) "DarkRoomDeadEnd"
                            ("dark room '" ++ rId ++ "' contains items but has no light_flag, no feelable items, and no reachable lightsource (potential author dead-end)") ]
                   else []
            | otherwise = []
    in concatMap checkRoom rooms
  where
    reachableRoomIds start rms =
        let rMap = Map.fromList [ (arId r, r) | r <- rms ]
            go visited [] = Set.toList visited
            go visited (curr:rest)
                | curr `Set.member` visited = go visited rest
                | otherwise =
                    let targets = case Map.lookup curr rMap of
                            Just rm -> map aeTarget (Map.elems (arExits rm))
                            Nothing -> []
                        validTargets = filter (`Map.member` rMap) targets
                        newQ = rest ++ filter (`Set.notMember` visited) validTargets
                    in go (Set.insert curr visited) newQ
        in go Set.empty [start]