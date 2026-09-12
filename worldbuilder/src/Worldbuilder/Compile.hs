-- | Compile an Adventure (authoring schema) into engine types, or return structured issues
module Worldbuilder.Compile
    ( CompileResult(..)
    , compileAdventure
    , CompileIssue(..)
    , Severity(..)
    ) where

import Worldbuilder.Types
import Types as E
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Char (toLower)
import Data.List (nub, stripPrefix, takeWhile, zip, notElem)
import Data.Maybe (mapMaybe, fromMaybe, catMaybes)
import Data.Either (partitionEithers)
import Text.Read (readMaybe)
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Verbs

-- | Result of compilation
data CompileResult = CompileResult
    { crWorld :: E.GameWorld
    , crSave  :: E.SaveState
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

-- | Group source keys that normalize to the same target key (collision detection).
--   Returns [(target, [sourceKeys])] for targets with more than one source.
collisions :: Ord a => [(String, a)] -> [(a, [String])]
collisions pairs =
    [ (k, keys)
    | (k, keys) <- Map.toList
        (Map.fromListWith (++) [(t, [src]) | (src, t) <- pairs])
    , length keys > 1 ]

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
        (entityInteractions, itemInteractions) = compileInteractions (advInteractions adv)
        
        (varErrs, varDefs, varInitials) = compileVariables (advVariables adv)
        (trigErrs, triggerDefs) = compileTriggers (advTriggers adv)
        (encErrs, encounterDefs) = compileEncounterTables (advEncounterTables adv)
        (facErrs, factionDefs, factionInitials) = compileFactions (advFactions adv)
        (facConflictErrs, facVarDefs, facVarInitials) =
            mergeFactionVars varDefs varInitials factionDefs factionInitials
        (envErrs, envTriggerDefs, envVarDefs, envVarInitials) =
            compileEnvironment facVarDefs (advEnvironment adv)
        (envConflictErrs, allVarDefs, allVarInitials) =
            mergeEnvironmentVars facVarDefs facVarInitials envVarDefs envVarInitials
        allTriggerDefs = triggerDefs ++ encounterDefs ++ envTriggerDefs
        (initVarErrs, initialVars) =
            compileInitialVariables allVarDefs allVarInitials (advInitialVariables adv)
        (initStateErrs, initialFlags, initialQuests) =
            compileInitialState (advActiveQuests adv) (advInitialFlags adv) questDefs

        allRooms = Map.union compiledRooms vehicleExtraRooms
        gw = E.GameWorld
                { E.rooms = allRooms
                , E.itemDefs = itemDefs
                , E.npcDefs = npcDefs
                , E.entityInteractions = entityInteractions
                , E.itemInteractions = itemInteractions
                , E.questDefs = questDefs
                , E.vehicleDefs = vehicleDefs
                , E.verbDefs = verbRegistry
                , E.varDefs = allVarDefs
                , E.triggerDefs = allTriggerDefs
                }
        facRefErrs = checkStandingRefs (advFactions adv) gw
        encRefErrs = checkEncounterRefs (advEncounterTables adv) gw

        allErrors = verbErrs ++ roomErrs ++ itemErrs ++ npcErrs ++ vehicleErrs
                    ++ varErrs ++ facErrs ++ facConflictErrs ++ trigErrs ++ encErrs
                    ++ envErrs ++ envConflictErrs
                    ++ initVarErrs ++ initStateErrs ++ facRefErrs ++ encRefErrs
    in case allErrors of
        (_:_) -> Left allErrors
        [] ->
            let startRoomId = advStartRoom adv
                startSave = E.SaveState
                        { E.player = compilePlayer (advPlayer adv)
                        , E.currentRoom = startRoomId
                        , E.inventory = []
                        , E.itemStates = itemStates
                        , E.npcStates = npcStates
                        , E.entityStates = initialEntityStates allRooms
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
                        , E.variables = initialVars
                        , E.containers = Map.empty
                        , E.triggerStates = Map.empty
                        }
            in Right (CompileResult gw startSave)
  where
    -- Every locked exit starts locked in entityStates
    initialEntityStates rooms =
        Map.fromList
            [ (e, "locked")
            | room <- Map.elems rooms
            , E.Locked _ e <- Map.elems (E.roomConnections room)
            ]

-- ---------------------------------------------------------------------------
-- Direction parsing (strict — unknown = compile error)
-- ---------------------------------------------------------------------------

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
            , E.roomOnEnter = compileMaybeOutcomes (arOnEnter r)
            , E.roomOnLook = compileMaybeOutcomes (arOnLook r)
            , E.roomOnExit = compileMaybeOutcomes (arOnExit r)
            , E.roomSearchOutcome = compileMaybeOutcomes (arSearch r)
            , E.roomAscii = arAscii r
            }
       else Left allErrs
  where
    compileExitInner :: AExitRef -> E.Exit
    compileExitInner ref = case aeLocked ref of
        Nothing -> E.Open (aeTarget ref)
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
    , "examine", "inspect", "look", "read"
    , "use", "activate"
    , "talk", "speak", "chat"
    , "attack", "hit", "kill", "search"
    , "go", "move", "walk", "north", "south", "east", "west", "up", "down"
    , "enter", "board", "exit", "drive", "wait", "refuel", "repair"
    , "equip", "wear", "wield", "unequip", "remove"
    , "swim", "crawl", "dig", "game"
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
    in case vtype of
        Left msg -> Left [ciError bp "InvalidVariableType" msg]
        Right vt -> case compileVarInitial av vt of
            Left msg -> Left [ciError (bp ++ ".initial") "InvalidVariableInitial" msg]
            Right vv -> Right (av, E.VarDef (avbVarName av) vt vv, vv)

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
            [ ("faction." ++ afId f, E.VarDef ("faction." ++ afId f) (E.VTInt Nothing Nothing) (E.VVInt (afInitial f)))
            | f <- factions ]
        initials = Map.fromList
            [ ("faction." ++ afId f, E.VVInt (afInitial f))
            | f <- factions ]
    in (dupErrs, defs, initials)

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
            | (i, t) <- zip [0 ..] (weaTransitions wd)
            , wtTo t `notElem` states ]
        initIdx = stateIndex (weaInitial wd)
        varDefs = Map.singleton "env.weather"
            (E.VarDef "env.weather" (E.VTInt Nothing Nothing) (E.VVInt initIdx))
        initials = Map.singleton "env.weather" (E.VVInt initIdx)
        transitions =
            [ E.TriggerDef ("environment.weather." ++ show i) E.OnTurn (wtWhen t)
                (E.SetValue (E.VRVariable "env.weather") (E.EVInt (stateIndex (wtTo t)))
                    : map compileAActionOutcome (wtEffects t)) False 0
            | (i, t) <- zip [0 ..] (weaTransitions wd)
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
                False 0
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
collectFactionRefs gw = nub $ concat
    [ concatMap refsInEffect roomEffs
    | r <- Map.elems (rooms gw)
    , let roomEffs = catMaybes [roomOnEnter r, roomOnLook r, roomOnExit r, roomSearchOutcome r]
    ]
    ++ concatMap (refsInEffect . snd) (concatMap (Map.toList . itemVerbMap) (Map.elems (itemDefs gw)))
    ++ concatMap (refsInEffect . snd) (concatMap (Map.toList . npcVerbMap) (Map.elems (npcDefs gw)))
    ++ concatMap refsInChoice
        [ c | npc <- Map.elems (npcDefs gw)
            , tree <- Map.elems (npcDialogueTrees npc)
            , node <- Map.elems (dtNodes tree)
            , c <- dnChoices node ]
    ++ concatMap refsInCondText (map roomDescription (Map.elems (rooms gw)))
    ++ concatMap refsInCondText (map itemDescription (Map.elems (itemDefs gw)))
    ++ concatMap refsInCondText (map npcDescription (Map.elems (npcDefs gw)))
    ++ concatMap (maybe [] refsInPredicate . trCondition) (triggerDefs gw)
    ++ concatMap refsInEffect (concatMap trEffects (triggerDefs gw))
    ++ concatMap (maybe [] refsInEffect) (map questReward (Map.elems (questDefs gw)))
    ++ concatMap (refsInEffect . snd) (concatMap (Map.toList . vehicleConditionEffects) (Map.elems (vehicleDefs gw)))
    ++ concatMap (refsInEffect . snd) (Map.toList (itemInteractions gw))
  where
    refsInChoice c = refsInEffect (dcOutcome c) ++ maybe [] refsInPredicate (dcVisible c)
    refsInCondText ct = concatMap (refsInPredicate . tvWhen) (ctVariants ct)

-- | Faction references inside an Effect tree.
refsInEffect :: E.Effect -> [String]
refsInEffect e = case e of
    E.ModifyValue (E.VRVariable n) _ -> factionFromVar n
    E.SetValue (E.VRVariable n) _     -> factionFromVar n
    E.Sequence es                     -> concatMap refsInEffect es
    E.RandomChoice cs                 -> concatMap (refsInEffect . snd) cs
    E.Conditional p t el              -> refsInPredicate p ++ refsInEffect t ++ refsInEffect el
    E.Narrative _ follow              -> refsInEffect follow
    E.ApplyCondition _ _ (Just t) (Just el) -> refsInEffect t ++ refsInEffect el
    E.ApplyCondition _ _ (Just t) Nothing   -> refsInEffect t
    E.ApplyCondition _ _ Nothing (Just el)  -> refsInEffect el
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
            -- Merge on_take into verb_map
            let verbMap' = case aiOnTake i of
                    Nothing -> verbMap
                    Just outcomes ->
                        if Map.member (E.VTake, aiState i) verbMap
                        then Map.insert (E.VTake, aiState i)
                             (E.Sequence [compileOutcomes outcomes, Map.findWithDefault (E.Noop) (E.VTake, aiState i) verbMap])
                             verbMap
                        else Map.insert (E.VTake, aiState i) (compileOutcomes outcomes) verbMap
            in Right (i, E.ItemDef
                { E.itemId = aiId i
                , E.itemName = aiName i
                , E.itemDescription = compileCondText (aiTexts i)
                , E.itemKeywords = aiKeywords i
                , E.itemTags = Set.fromList (aiTags i)
                , E.itemEquipSlot = slot
                , E.itemEquipEffects = effects
                , E.itemHidden = aiHidden i
                , E.itemDiscoverText = aiDiscover i
                , E.itemPortable = fromMaybe True (aiPortable i)
                , E.itemTakeFailure = aiTakeFailure i
                , E.itemVerbMap = verbMap'
                })

compileItemStates :: [AItem] -> ([CompileIssue], Map.Map String E.ItemState)
compileItemStates items =
    let results = map compileItemStateSafe items
        errors = concat [e | Left e <- results]
        states = Map.fromList [s | Right s <- results]
    in (errors, states)

compileItemStateSafe :: AItem -> Either [CompileIssue] (String, E.ItemState)
compileItemStateSafe i = Right (aiId i, E.ItemState
    { E.itemLocation = case aiInContainer i of
        Just cid -> E.InContainer cid
        Nothing  -> E.InRoom (aiLocation i)
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
            , E.npcDialogue = Map.empty  -- legacy, we use DialogueTrees
            , E.npcDialogueTrees = compileDialogueTrees (anDialogue n)
            , E.npcKeywords = anKeywords n
            , E.npcMaxHealth = anMaxHealth n
            , E.npcAttackBase = anAttack n
            , E.npcDefenseBase = anDefense n
            , E.npcVerbMap = verbMap
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
                    , E.vehicleStops = Map.fromList [(rId, E.VehicleStop rId label Nothing) | (label, rId) <- Map.toList (avStops v)]
                    , E.vehicleKeywords = avKeywords v
                    , E.vehicleFuelProp = avFuel v
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
          Just room -> room
          Nothing   -> case Map.lookup (headSafe (Map.keys (avStops v))) (avStops v) of
              Just room -> room
              Nothing   -> avEntryRoom v
    , E.vsFuel = Nothing
    , E.vsActiveConditions = Set.empty
    , E.vsRoomOverrides = Map.empty
    }
  where
    headSafe [] = ""
    headSafe xs = head xs

-- ---------------------------------------------------------------------------
-- Interactions
-- ---------------------------------------------------------------------------

compileInteractions :: Maybe AInteractions -> (Map.Map (String, String) (String, String), Map.Map (String, String) E.Effect)
compileInteractions Nothing = (Map.empty, Map.empty)
compileInteractions (Just ix) = (entityMap, itemMap)
  where
    entityMap = Map.fromList
        [ ((aeiItem e, aeiTarget e), (aeiState e, fromMaybe "" (aeiMsg e)))
        | e <- aiEntity ix ]
    itemMap = Map.fromList
        [ ((aiiItem1 i, aiiItem2 i), compileOutcomes (aiiEffects i))
        | i <- aiItem ix ]

-- ---------------------------------------------------------------------------
-- Verb maps (strict — unknown verb = compile error, custom verbs resolved)
-- ---------------------------------------------------------------------------

-- | Build alias→canonical lookup table from the verb registry.
verbAliasLookup :: Map.Map String E.VerbDef -> Map.Map String String
verbAliasLookup = Verbs.verbAliasMap

-- | Compile a verb map with structured diagnostics.  The path prefix points at
--   the owning field (e.g. "items.crystal.verb_map").  The registry resolves
--   custom verbs; unknown verbs are errors.
compileVerbMapSafe :: Map.Map String E.VerbDef -> String -> Map.Map String [AActionOutcome]
                   -> Either [CompileIssue] (Map.Map (E.Verb, String) E.Effect)
compileVerbMapSafe registry pathPrefix vm =
    let entries = Map.toList vm
        parsed = [ (key, parseVerbStrict registry verbStr, state)
                 | (key, _) <- entries
                 , let (verbStr, rest) = break (== ',') key
                 , let state = case rest of
                         ',':s -> s
                         _     -> "intact" ]
        verbErrs =
            [ ciError (pathPrefix ++ "." ++ key) "UnknownVerb" msg
            | (key, Left msg, _) <- parsed ]
        -- (Verb, State) collisions, e.g. "use,intact" + "activate,intact"
        collErrs =
            [ ciError pathPrefix "DuplicateVerbKey"
                ("verb keys " ++ show keys ++ " all resolve to " ++ show verb ++ ":" ++ state)
            | ((verb, state), keys) <- collisions [(k, (v, st)) | (k, Right v, st) <- parsed] ]
    in if null (verbErrs ++ collErrs)
       then Right $ Map.fromList
            [ ((verb, state), compileOutcomes outcomes)
            | (key, Right verb, state) <- parsed
            , Just outcomes <- [Map.lookup key vm] ]
       else Left (verbErrs ++ collErrs)

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

compileMaybeOutcomes :: Maybe [AActionOutcome] -> Maybe E.Effect
compileMaybeOutcomes Nothing = Nothing
compileMaybeOutcomes (Just os) = Just (compileOutcomes os)

compileAActionOutcome :: AActionOutcome -> E.Effect
compileAActionOutcome ao = case ao of
    AOMessage s -> E.SendMessage s
    AOHealPlayer n -> E.ModifyValue E.VRPlayerHealth n
    AODamagePlayer n -> E.ModifyValue E.VRPlayerHealth (-n)
    AOGiveItem i -> E.MoveEntity i (E.CarriedBy "player")
    AOConsumeItem i -> E.MoveEntity i E.Removed
    AOSetFlag f v -> E.SetValue (E.VRFlag f) (E.EVString v)
    AOCheckFlag f _ t e ->
        E.Conditional (E.HasFlag f) (compileAActionOutcome t) (compileAActionOutcome e)
    AOStartQuest q -> E.QuestOp E.StartQuest q
    AOAdvanceQuest q -> E.QuestOp E.AdvanceQuest q
    AOCompleteQuest q -> E.QuestOp E.CompleteQuest q
    AOEquipItem i -> E.MoveEntity i (E.EquippedBy "player" "weapon")
    AORoomTransition r -> E.SetValue (E.VRProperty "player" "room") (E.EVString r)
    AOMoveNPC n r -> E.MoveEntity n (E.InRoom r)
    AOGameEnd r m -> E.GameEnd (parseGameOverReason r) (fromMaybe "" m)
    AOConditional p ts es ->
        E.Conditional p (compileOutcomes ts) (compileOutcomes es)
    AOSetVar name v -> E.SetValue (E.VRVariable name) (E.EVInt v)
    AOAddVar name d -> E.ModifyValue (E.VRVariable name) d
    AONarrative ls -> E.Sequence (map E.SendMessage ls ++ [E.Noop])
    AOStandingAdd fid n -> E.ModifyValue (E.VRVariable ("faction." ++ fid)) n
    AOStandingSet fid n -> E.SetValue (E.VRVariable ("faction." ++ fid)) (E.EVInt n)
    AOSetEntityState e s -> E.SetValue (E.VRProperty e "state") (E.EVString s)

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

-- ---------------------------------------------------------------------------
-- Trigger rules (Phase 3f)
-- ---------------------------------------------------------------------------

-- | Compile authored trigger rules into engine TriggerDefs.
compileTriggers :: [ATrigger] -> ([CompileIssue], [E.TriggerDef])
compileTriggers triggers =
    let results = map compileOne triggers
        errors = concat [e | Left e <- results]
        defs = [d | Right d <- results]
    in (errors, defs)
  where
    compileOne t = case compileAtOn (atOn t) of
        Left err -> Left [ciError ("rules." ++ atId t) "BadTriggerEvent" err]
        Right ev -> Right E.TriggerDef
            { E.trId = atId t
            , E.trEvent = ev
            , E.trCondition = atWhen t
            , E.trEffects = map compileAActionOutcome (atEffects t)
            , E.trOnce = atOnce t
            , E.trCooldown = atCooldown t
            }

-- | Parse the `on` string into an EventType.
--   Supported: "enter <room>", "leave <room>", "look <room>", "search <room>",
--   "take <item>", "drop <item>", "use <item>", "state <entity>", "custom <name>",
--   "command <verb>", "turn".
compileAtOn :: String -> Either String E.EventType
compileAtOn s =
    case words (map toLower s) of
        ["turn"]                     -> Right E.OnTurn
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