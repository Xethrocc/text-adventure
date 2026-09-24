-- | Test suite for the worldbuilder: strict compilation, directions,
--   verbs, on_take merging, string-scalar handling, and YAML round-trips.
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad (forM, when)
import Data.List (isInfixOf, nub)
import qualified Data.Aeson as Aeson
import Data.Maybe (listToMaybe)
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Exit (exitFailure)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath ((</>))
import System.IO (hSetEncoding, stdout, utf8)
import Control.Exception (try, SomeException)
import Worldbuilder.Types
import Worldbuilder.Locate (lineForPath)
import Worldbuilder.Compile (CompileResult (..), compileAdventure, CompileIssue(..), Severity(..), compileAActionOutcome, allWorldEffects)
import Worldbuilder.ParseFile (parseAdventureFile)
import Worldbuilder.Rng
import Worldbuilder.Generate
import Worldbuilder.Run (RunConfig (..), RunResult (..), defaultRunConfig, prepareRun, pruneOldRuns)
import qualified SaveLoad
import Data.List (sort, sortOn, stripPrefix)
import Data.YAML.Aeson (decode1)
import Data.YAML (posLine)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Types as E
import Game (emptyGameState, evalPredicate)
import Validate (validateWorld, validateGameState, ValidationError (..))

-- | Helper: a minimal room set for validateGameState tests
minWorld :: E.GameWorld
minWorld = E.GameWorld
    { rooms = Map.fromList
        [ ("room_0", E.Room "room_0" "Room 0" (E.CondText "test" []) Map.empty Set.empty Nothing Nothing Nothing Nothing Nothing (E.AsciiArt (E.CondText "" []) [] 0 [] Nothing) Nothing Nothing)
        ]
    , itemDefs = Map.empty
    , npcDefs = Map.empty
    , entityInteractions = Map.empty
    , itemInteractions = Map.empty
    , questDefs = Map.empty
    , vehicleDefs = Map.empty
    , verbDefs = Map.empty
    , varDefs = Map.empty
    , triggerDefs = []
    , combatProfile = E.CombatClassic Nothing
    , worldGamePolicy = E.defaultGamePolicy
    , worldName = ""
    , abilities = Map.empty
    , worldEndArt = Map.empty
    , worldTitleArt = E.AsciiArt (E.CondText "" []) [] 1 [] Nothing
    , worldClips = Map.empty
    }

-- | Helper: a minimal valid SaveState referencing room_0
minSave :: E.SaveState
minSave = E.SaveState
    { player = E.Player 100 100 10 5 Map.empty
    , currentRoom = "room_0"
    , inventory = []
    , itemStates = Map.empty
    , npcStates = Map.empty
    , entityStates = Map.empty
    , flags = Map.empty
    , turnCount = 0
    , gameOver = False
    , gameOverReason = Nothing
    , visitedRooms = Set.empty
    , equipment = Map.empty
    , conditions = Map.empty
    , activeQuests = Map.empty
    , completedQuests = Set.empty
    , vehicleStates = Map.empty
    , currentVehicle = Nothing
    , activeDialogue = Nothing
    , rngState = 0
    , variables = Map.empty
    , triggerStates = Map.empty
    , exitOverrides = Map.empty
    }

runTest :: String -> IO Bool -> IO Bool
runTest name testAction = do
    passed <- testAction
    putStrLn $ (if passed then "[PASS] " else "[FAIL] ") ++ name
    pure passed

expectEqual :: (Eq a, Show a) => a -> a -> IO Bool
expectEqual expected actual
    | expected == actual = pure True
    | otherwise = do
        putStrLn $ "  expected: " ++ show expected
        putStrLn $ "    actual: " ++ show actual
        pure False

expectRight :: Either [CompileIssue] a -> IO Bool
expectRight (Right _) = pure True
expectRight (Left errs) = do
    putStrLn $ "  unexpected compile errors: " ++ show errs
    pure False

expectLeft :: Either [CompileIssue] a -> IO Bool
expectLeft (Left _) = pure True
expectLeft (Right _) = do
    putStrLn "  expected Left (compile error), got Right"
    pure False

-- | Concatenate compile issue codes and messages into a flat string
issuesText :: [CompileIssue] -> String
issuesText = unwords . concatMap (\i -> [ciCode i ++ ": " ++ ciMessage i])

expectContains :: String -> String -> IO Bool
expectContains needle haystack
    | needle `isInfixOf` haystack = pure True
    | otherwise = do
        putStrLn $ "  expected to contain: " ++ show needle
        putStrLn $ "             actual: " ++ show haystack
        pure False

expectTrue :: String -> Bool -> IO Bool
expectTrue _      True  = pure True
expectTrue label False = do
    putStrLn $ "  expected True: " ++ label
    pure False

-- ---------------------------------------------------------------------------
-- Helper: minimal adventure builders
-- ---------------------------------------------------------------------------

minRoom :: String -> ARoom
minRoom rid = ARoom
    { arId = rid
    , arName = rid
    , arTexts = ACondText "test room" []
    , arExits = Map.empty
    , arTags = []
    , arLightFlag = Nothing
    , arOnEnter = Nothing
    , arOnLook = Nothing
    , arOnExit = Nothing
    , arSearch = Nothing
    , arAscii = AAscii (ACondText "" []) [] 0 [] Nothing
    , arIntro = Nothing
    , arFloor = Nothing
    }

-- | Build a minimal adventure with one room
minAdventure :: ARoom -> Adventure
minAdventure room = Adventure
    { advName = Nothing
    , advStartRoom = arId room
    , advRooms = [room]
    , advItems = []
    , advNPCs = []
    , advQuests = []
    , advVehicles = []
    , advInteractions = Nothing
    , advVerbs = []
    , advVariables = []
    , advTriggers = []
    , advPlayer = Nothing
    , advInitialVariables = Map.empty
    , advInitialFlags = Map.empty
    , advActiveQuests = []
    , advFactions = []
    , advEncounterTables = []
    , advEnvironment = Nothing
    , advStealth = Nothing
    , advPatrol = Nothing
    , advCombat = Nothing
    , advAbilities = []
    , advEndArt = Map.empty
    , advTitleArt = AAscii (ACondText "" []) [] 1 [] Nothing
    , advClips = []
    , advGame = Nothing
    }

-- ===== Rogue Phase 1: authored game policy =====

-- | Rogue Phase 1: the authored `game:` block compiles to the engine
--   GamePolicy. Absent block keeps the default; savezone rooms are validated
--   (MissingRoom); ironman without savezones is a non-fatal warning.
testGamePolicyCompiles :: IO Bool
testGamePolicyCompiles = do
    -- default: no game block -> defaultGamePolicy
    r0 <- case compileAdventure (minAdventure (minRoom "loc_0")) of
            Left _  -> expectTrue "default compiles" False
            Right cr -> expectEqual E.defaultGamePolicy (E.worldGamePolicy (crWorld cr))
    -- full block incl. explicit meta_slug (Rogue Phase 2, M8): the engine
    -- applies slugify to the override (idempotent, so "katakombe_vhal"
    -- stays itself and a raw "Katakomben von Vhal" gets cleaned up).
    let advWithGame = (minAdventure (minRoom "loc_0"))
            { advGame = Just (AGamePolicy (Just True) (Just False) (Just True)
                                ["loc_0"] (Just "katakombe_vhal")) }
    r1 <- case compileAdventure advWithGame of
            Left errs -> expectTrue ("policy compiles, got: " ++ show errs) False
            Right cr -> do
                rA <- expectEqual (E.GamePolicy True False True ["loc_0"] (Just "katakombe_vhal"))
                                  (E.worldGamePolicy (crWorld cr))
                rB <- expectTrue "no warnings for a complete ironman setup"
                          (null (crWarnings cr))
                pure (rA && rB)
    -- ironman without savezones compiles, warning attached
    let advHardcore = (minAdventure (minRoom "loc_0"))
            { advGame = Just (AGamePolicy (Just True) Nothing (Just True) [] Nothing) }
    r2 <- case compileAdventure advHardcore of
            Left _   -> expectTrue "ironman without savezones compiles" False
            Right cr -> do
                rA <- expectTrue "ironman flag set"
                          (E.gpIronman (E.worldGamePolicy (crWorld cr)))
                rB <- expectEqual ["IronmanWithoutSavezones"]
                          [ciCode i | i <- crWarnings cr]
                pure (rA && rB)
    -- unknown savezone room: MissingRoom, hard error
    let advBad = (minAdventure (minRoom "loc_0"))
            { advGame = Just (AGamePolicy Nothing Nothing (Just True) ["nope"] Nothing) }
    r3 <- case compileAdventure advBad of
            Left errs -> expectTrue "unknown savezone is MissingRoom"
                              (any (\i -> ciCode i == "MissingRoom") errs)
            Right _   -> expectTrue "unknown savezone must fail" False
    pure (r0 && r1 && r2 && r3)


-- | Rogue Phase 3 helper: does an Effect tree contain a dynamic-exit effect?
carriesExitEffect :: E.Effect -> Bool
carriesExitEffect e = case e of
    E.SetExit {}   -> True
    E.RemoveExit {} -> True
    E.Sequence os  -> any carriesExitEffect os
    E.RandomChoice cs -> any (carriesExitEffect . snd) cs
    E.Conditional _ t el -> carriesExitEffect t || carriesExitEffect el
    E.Narrative _ f -> carriesExitEffect f
    E.ApplyCondition _ _ (Just t) (Just el) -> carriesExitEffect t || carriesExitEffect el
    E.ApplyCondition _ _ (Just t) Nothing   -> carriesExitEffect t
    E.ApplyCondition _ _ Nothing (Just el)  -> carriesExitEffect el
    _                -> False

-- | Rogue Phase 3: `set_exit` / `remove_exit` compile to the engine effects;
--   unknown directions and missing rooms are rejected.
testSetExitCompiles :: IO Bool
testSetExitCompiles = do
    -- happy path: open + locked rewire
    let advOk = (minAdventure (minRoom "loc_0"))
            { advRooms =
                [ (minRoom "loc_0") { arOnEnter = Just
                    [ AOSetExit "loc_0" "east" "loc_b" Nothing
                    , AOSetExit "loc_0" "west" "loc_b" (Just "seal")
                    , AORemoveExit "loc_0" "north" ] }
                , (minRoom "loc_b") { arExits = Map.fromList [("west", AExitRef "loc_0" Nothing)] }
                ] }
    r1 <- case compileAdventure advOk of
            Left errs -> expectTrue ("set_exit compiles, got: " ++ show errs) False
            Right cr -> do
                -- die on_enter-Outcome-Liste kompiliert 1:1 (Sequence ueber dem Hook)
                rA <- expectTrue "open set_exit compiles to the engine effect"
                          (compileAActionOutcome (AOSetExit "loc_0" "east" "loc_b" Nothing)
                              == E.SetExit "loc_0" E.East (E.Open "loc_b"))
                rB <- expectTrue "locked set_exit compiles to the engine effect"
                          (compileAActionOutcome (AOSetExit "loc_0" "west" "loc_b" (Just "seal"))
                              == E.SetExit "loc_0" E.West (E.Locked "loc_b" "seal"))
                rC <- expectTrue "remove_exit compiles to the engine effect"
                          (compileAActionOutcome (AORemoveExit "loc_0" "north")
                              == E.RemoveExit "loc_0" E.North)
                rD <- expectTrue "the compiled world carries the effects"
                          (any carriesExitEffect (allWorldEffects (crWorld cr)))
                pure (rA && rB && rC && rD)
    -- unknown direction
    let advBadDir = (minAdventure (minRoom "loc_0"))
            { advRooms = [ (minRoom "loc_0") { arOnEnter = Just [AORemoveExit "loc_0" "sideways"] } ] }
    r2 <- case compileAdventure advBadDir of
            Left errs -> expectTrue "unknown direction rejected"
                          (any (\i -> ciCode i == "UnknownDirection") errs)
            Right _   -> expectTrue "unknown direction must fail" False
    -- missing from-room
    let advBadRoom = (minAdventure (minRoom "loc_0"))
            { advRooms = [ (minRoom "loc_0") { arOnEnter = Just [AOSetExit "ghost" "north" "loc_0" Nothing] } ] }
    r3 <- case compileAdventure advBadRoom of
            Left errs -> expectTrue "missing from-room rejected"
                          (any (\i -> ciCode i == "MissingRoom") errs)
            Right _   -> expectTrue "missing room must fail" False
    -- missing to-room
    let advBadTo = (minAdventure (minRoom "loc_0"))
            { advRooms = [ (minRoom "loc_0") { arOnEnter = Just [AOSetExit "loc_0" "north" "ghost" Nothing] } ] }
    r4 <- case compileAdventure advBadTo of
            Left errs -> expectTrue "missing to-room rejected"
                          (any (\i -> ciCode i == "MissingRoom") errs)
            Right _   -> expectTrue "missing to-room must fail" False
    pure (r1 && r2 && r3 && r4)

-- ---------------------------------------------------------------------------
-- Direction tests
-- ---------------------------------------------------------------------------

testAllDirectionsCompile :: IO Bool
testAllDirectionsCompile = do
    let exits = Map.fromList
            [ ("north", AExitRef "loc_a" Nothing)
            , ("south", AExitRef "loc_a" Nothing)
            , ("east", AExitRef "loc_a" Nothing)
            , ("west", AExitRef "loc_a" Nothing)
            , ("up", AExitRef "loc_a" Nothing)
            , ("down", AExitRef "loc_a" Nothing)
            , ("northeast", AExitRef "loc_a" Nothing)
            , ("northwest", AExitRef "loc_a" Nothing)
            , ("southeast", AExitRef "loc_a" Nothing)
            , ("southwest", AExitRef "loc_a" Nothing)
            ]
        room = (minRoom "loc_0") { arExits = exits }
        adv = minAdventure room
    case compileAdventure adv of
        Left errs -> do
            putStrLn $ "  compile errors: " ++ show errs
            pure False
        Right cr -> do
            let conns = E.roomConnections (Map.findWithDefault (error "missing room") "loc_0" (E.rooms (crWorld cr)))
                expected = [E.North, E.South, E.East, E.West, E.Up, E.Down
                           , E.Northeast, E.Northwest, E.Southeast, E.Southwest]
            expectEqual expected (Map.keys conns)

testDirectionAliases :: IO Bool
testDirectionAliases = do
    let exits = Map.fromList
            [ ("ne", AExitRef "loc_a" Nothing)
            , ("nw", AExitRef "loc_a" Nothing)
            , ("se", AExitRef "loc_a" Nothing)
            , ("sw", AExitRef "loc_a" Nothing)
            ]
        room = (minRoom "loc_0") { arExits = exits }
    case compileAdventure (minAdventure room) of
        Left errs -> do
            putStrLn $ "  compile errors: " ++ show errs
            pure False
        Right cr -> do
            let conns = E.roomConnections (Map.findWithDefault (error "missing") "loc_0" (E.rooms (crWorld cr)))
            expectEqual [E.Northeast, E.Northwest, E.Southeast, E.Southwest] (Map.keys conns)

testUnknownDirectionFails :: IO Bool
testUnknownDirectionFails = do
    let exits = Map.fromList [("noth", AExitRef "loc_a" Nothing)]
        room = (minRoom "loc_0") { arExits = exits }
    case compileAdventure (minAdventure room) of
        Left errs -> do
            let combined = issuesText errs
            expectContains "Unknown direction" combined
        Right _ -> do
            putStrLn "  expected compile failure for direction 'noth'"
            pure False
-- ---------------------------------------------------------------------------
-- Verb tests
-- ---------------------------------------------------------------------------

minItem :: String -> AItem
minItem iid = AItem
    { aiId = iid
    , aiName = iid
    , aiTexts = ACondText "test item" []
    , aiAscii = AAscii (ACondText "" []) [] 0 [] Nothing
    , aiKeywords = []
    , aiTags = []
    , aiLocation = "loc_0"
    , aiState = "intact"
    , aiEquipSlot = Nothing
    , aiEquipEffects = []
    , aiHidden = False
    , aiDiscover = Nothing
    , aiProps = Map.empty
    , aiOnTake = Nothing
    , aiVerbMap = Map.empty
    , aiPortable = Nothing
    , aiTakeFailure = Nothing
    , aiInContainer = Nothing
    }

advWithItem :: AItem -> Adventure
advWithItem item =
    let adv = minAdventure (minRoom "loc_0")
    in adv { advItems = [item] }

testActivateVerbMapsToUse :: IO Bool
testActivateVerbMapsToUse = do
    let vm = Map.fromList [("activate,intact", [AOMessage "activated"])]
        item = (minItem "shrine") { aiVerbMap = vm }
    case compileAdventure (advWithItem item) of
        Left errs -> do
            putStrLn $ "  compile errors: " ++ show errs
            pure False
        Right cr -> do
            let def = Map.findWithDefault (error "missing") "shrine" (E.itemDefs (crWorld cr))
            expectEqual (Just (E.VUse, "intact")) (Map.lookupMin (E.itemVerbMap def) >>= (\(k, _) -> Just k))

testUnknownVerbFails :: IO Bool
testUnknownVerbFails = do
    let vm = Map.fromList [("frobnicate,intact", [AOMessage "x"])]
        item = (minItem "thing") { aiVerbMap = vm }
    case compileAdventure (advWithItem item) of
        Left errs -> do
            let combined = issuesText errs
            expectContains "Unknown verb" combined
        Right _ -> do
            putStrLn "  expected compile failure for unknown verb 'frobnicate'"
            pure False

testRepeatedVerbAliases :: IO Bool
testRepeatedVerbAliases = do
    -- "examine" and "look" are aliases — both should compile to VLookAt
    let vm = Map.fromList [("examine,intact", [AOMessage "examined"])]
        item = (minItem "thing") { aiVerbMap = vm }
    case compileAdventure (advWithItem item) of
        Left errs -> do
            putStrLn $ "  compile errors: " ++ show errs
            pure False
        Right cr -> do
            let def = Map.findWithDefault (error "missing") "thing" (E.itemDefs (crWorld cr))
                firstVerb = fst . fst <$> Map.lookupMin (E.itemVerbMap def)
            expectEqual (Just E.VLookAt) firstVerb

-- ---------------------------------------------------------------------------
-- on_take tests
-- ---------------------------------------------------------------------------

testOnTakeMergedIntoVerbMap :: IO Bool
testOnTakeMergedIntoVerbMap = do
    let item = (minItem "crystal") { aiOnTake = Just [AOSetFlag "took_crystal" "true"] }
    case compileAdventure (advWithItem item) of
        Left errs -> do
            putStrLn $ "  compile errors: " ++ show errs
            pure False
        Right cr -> do
            let def = Map.findWithDefault (error "missing") "crystal" (E.itemDefs (crWorld cr))
            case Map.lookup (E.VTake, "intact") (E.itemVerbMap def) of
                Just (E.SetValue (E.VRFlag f) (E.EVString v)) -> expectEqual ("took_crystal", "true") (f, v)
                Just other -> do
                    putStrLn $ "  wrong outcome: " ++ show other
                    pure False
                Nothing -> do
                    putStrLn "  no VTake entry in verb map"
                    pure False

testOnTakeConflictMerges :: IO Bool
testOnTakeConflictMerges = do
    let vm = Map.fromList [("take,intact", [AOMessage "already handled"])]
        item = (minItem "crystal") { aiOnTake = Just [AOSetFlag "took" "true"], aiVerbMap = vm }
    case compileAdventure (advWithItem item) of
        Left errs -> do
            putStrLn $ "  compile errors: " ++ show errs
            pure False
        Right cr -> do
            let def = Map.findWithDefault (error "missing") "crystal" (E.itemDefs (crWorld cr))
            case Map.lookup (E.VTake, "intact") (E.itemVerbMap def) of
                Just (E.Sequence _) -> pure True
                Just other -> do
                    putStrLn $ "  expected MultipleOutcomes, got: " ++ show other
                    pure False
                Nothing -> do
                    putStrLn "  no VTake entry in verb map"
                    pure False

-- ---------------------------------------------------------------------------
-- Slot / effect tests
-- ---------------------------------------------------------------------------

testInvalidSlotFails :: IO Bool
testInvalidSlotFails = do
    let item = (minItem "thing") { aiEquipSlot = Just "backpack" }
    case compileAdventure (advWithItem item) of
        Left errs -> do
            let combined = issuesText errs
            expectContains "equipment slot" combined
        Right _ -> do
            putStrLn "  expected compile failure for unknown slot"
            pure False

testInvalidEffectFails :: IO Bool
testInvalidEffectFails = do
    let item = (minItem "thing") { aiEquipEffects = ["attack+abc"] }
    case compileAdventure (advWithItem item) of
        Left errs -> do
            let combined = issuesText errs
            expectContains "attack bonus" combined
        Right _ -> do
            putStrLn "  expected compile failure for invalid effect"
            pure False

-- ---------------------------------------------------------------------------
-- String-scalar tests
-- ---------------------------------------------------------------------------

testExitRefStringNoQuotes :: IO Bool
testExitRefStringNoQuotes = do
    case Aeson.eitherDecode (BLC.pack "\"hallway\"") of
        Left err -> do
            putStrLn $ "  failed to parse: " ++ err
            pure False
        Right (ref :: AExitRef) ->
            expectEqual "hallway" (aeTarget ref) >>= \ok ->
                if ok then pure True
                else do
                    putStrLn "  (quotes leaked into target room id)"
                    pure False

testExitRefObject :: IO Bool
testExitRefObject = do
    case Aeson.eitherDecode (BLC.pack "{\"to\": \"hallway\", \"locked_by\": \"door\"}") of
        Left err -> do
            putStrLn $ "  failed to parse: " ++ err
            pure False
        Right (ref :: AExitRef) ->
            expectEqual ("hallway", Just "door") (aeTarget ref, aeLocked ref)

testActionOutcomeStringNoQuotes :: IO Bool
testActionOutcomeStringNoQuotes = do
    case Aeson.eitherDecode (BLC.pack "\"You see a hallway.\"") of
        Left err -> do
            putStrLn $ "  failed to parse: " ++ err
            pure False
        Right (outcome :: AActionOutcome) ->
            expectEqual (AOMessage "You see a hallway.") outcome

testDialogueChoiceStringNoQuotes :: IO Bool
testDialogueChoiceStringNoQuotes = do
    case Aeson.eitherDecode (BLC.pack "\"Where is the shrine?\"") of
        Left err -> do
            putStrLn $ "  failed to parse: " ++ err
            pure False
        Right (choice :: ADialogueChoice) ->
            expectEqual "Where is the shrine?" (adcText choice)

-- ---------------------------------------------------------------------------
-- Phase 2: Duplicate direction / verb-key collision detection
-- ---------------------------------------------------------------------------

-- | Zwei Exit-Keys, die auf dieselbe Direction mappen ("se" + "southeast"),
--   müssen als DuplicateDirection-Fehler gemeldet werden.
testDuplicateDirectionFails :: IO Bool
testDuplicateDirectionFails = do
    let exits = Map.fromList
            [ ("se", AExitRef "loc_a" Nothing)
            , ("southeast", AExitRef "loc_b" Nothing)
            ]
        room = (minRoom "loc_0") { arExits = exits }
    case compileAdventure (minAdventure room) of
        Left errs -> do
            let codes = [ciCode e | e <- errs]
            expectTrue "DuplicateDirection detected" ("DuplicateDirection" `elem` codes)
        Right _ -> do
            putStrLn "  expected compile failure for duplicate directions (se + southeast)"
            pure False

-- | Zwei verb_map-Keys, die auf dasselbe (Verb, State) mappen
--   ("use,intact" + "activate,intact"), müssen als DuplicateVerbKey gemeldet werden.
testDuplicateVerbKeyFails :: IO Bool
testDuplicateVerbKeyFails = do
    let vm = Map.fromList
            [ ("use,intact", [AOMessage "used"])
            , ("activate,intact", [AOMessage "activated"])
            ]
        item = (minItem "shrine") { aiVerbMap = vm }
    case compileAdventure (advWithItem item) of
        Left errs -> do
            let codes = [ciCode e | e <- errs]
            expectTrue "DuplicateVerbKey detected" ("DuplicateVerbKey" `elem` codes)
        Right _ -> do
            putStrLn "  expected compile failure for duplicate verb keys (use + activate)"
            pure False

-- | Der Compile-Fehler trägt den exakten YAML-Pfad (rooms.<id>.exits.<dir>)
testIssuePathPointsAtField :: IO Bool
testIssuePathPointsAtField = do
    let exits = Map.fromList [("noth", AExitRef "loc_a" Nothing)]
        room = (minRoom "loc_0") { arExits = exits }
    case compileAdventure (minAdventure room) of
        Left [issue] -> do
            let expectedPath = "rooms.loc_0.exits.noth"
            r1 <- expectEqual expectedPath (ciPath issue)
            r2 <- expectEqual SError (ciSeverity issue)
            pure (r1 && r2)
        Left errs -> do
            putStrLn $ "  expected exactly one issue, got: " ++ show errs
            pure False
        Right _ -> do
            putStrLn "  expected compile failure"
            pure False

-- ---------------------------------------------------------------------------
-- Phase 2: validateGameState (SaveState reference validation)
-- ---------------------------------------------------------------------------

-- | start_room, das nicht existiert, wird erkannt
testInvalidStartRoomDetected :: IO Bool
testInvalidStartRoomDetected = do
    let badSave = minSave { currentRoom = "nonexistent_room" }
    expectTrue "InvalidStartRoom detected"
        (InvalidStartRoom "nonexistent_room" `elem` validateGameState minWorld badSave)

-- | Item-Location, die auf keinen Raum zeigt, wird erkannt
testInvalidItemLocationDetected :: IO Bool
testInvalidItemLocationDetected = do
    let badSave = minSave
            { itemStates = Map.singleton "torch" (E.ItemState (E.InRoom "nowhere") "intact" Map.empty False) }
    expectTrue "InvalidItemLocation detected"
        (InvalidItemLocation "torch" "nowhere" `elem` validateGameState minWorld badSave)

-- | NPC-Location, die auf keinen Raum zeigt, wird erkannt
testInvalidNPCLocationDetected :: IO Bool
testInvalidNPCLocationDetected = do
    let badSave = minSave
            { npcStates = Map.singleton "oldman" (E.NPCState (E.InRoom "void_room") "alive" Nothing Map.empty Nothing) }
    expectTrue "InvalidNPCLocation detected"
        (InvalidNPCLocation "oldman" "void_room" `elem` validateGameState minWorld badSave)

-- | Quest ohne Stages wird erkannt (benötigt Welt mit einer Quest)
testEmptyQuestStagesDetected :: IO Bool
testEmptyQuestStagesDetected = do
    let badQuest = E.Quest "q1" "Empty Quest" "no stages" Map.empty [] Nothing
        badWorld = minWorld { questDefs = Map.singleton "q1" badQuest }
    expectTrue "EmptyQuestStages detected"
        (EmptyQuestStages "q1" `elem` validateGameState badWorld minSave)

-- | Quest-Prereq-Flag, das nie gesetzt wird, wird erkannt
testUnknownQuestPrereqDetected :: IO Bool
testUnknownQuestPrereqDetected = do
    let badQuest = E.Quest "q2" "Bad prereq" "flag never set"
                    (Map.singleton "never_set_flag" "true")
                    [E.QuestStage "s1" "step one" Nothing] Nothing
        badWorld = minWorld { questDefs = Map.singleton "q2" badQuest }
    expectTrue "UnknownQuestPrereq detected"
        (UnknownQuestPrereq "q2" "never_set_flag" `elem` validateGameState badWorld minSave)

-- | gültiger minimale SaveState erzeugt keine Fehler
testValidSaveStateHasNoNewErrors :: IO Bool
testValidSaveStateHasNoNewErrors = do
    expectEqual [] (validateGameState minWorld minSave)

-- ---------------------------------------------------------------------------
-- YAML file round-trips
-- ---------------------------------------------------------------------------

-- | Try several candidate paths for the examples directory
--   (tests may run from the project root or from worldbuilder/).
findExample :: String -> IO (Maybe FilePath)
findExample fname = firstExisting
    [ "examples" </> fname
    , "../examples" </> fname
    , "../../examples" </> fname
    , "examples/fixtures" </> fname
    , "../examples/fixtures" </> fname
    , "../../examples/fixtures" </> fname
    , "examples/genres" </> fname
    , "../examples/genres" </> fname
    , "../../examples/genres" </> fname
    , "examples/templates" </> fname
    , "../examples/templates" </> fname
    , "../../examples/templates" </> fname
    ]
  where
    firstExisting [] = pure Nothing
    firstExisting (p : rest) = do
        ok <- doesFileExist p
        if ok then pure (Just p) else firstExisting rest

testDemoYamlCompiles :: IO Bool
testDemoYamlCompiles = do
    mbPath <- findExample "demo.yaml"
    case mbPath of
        Nothing -> do
            putStrLn "  demo.yaml not found in examples/ or ../examples/"
            pure False
        Just path -> do
            advResult <- parseAdventureFile path
            case advResult of
                Left err -> do
                    putStrLn $ "  failed to parse demo.yaml: " ++ err
                    pure False
                Right adv -> compileCheck adv
  where
    compileCheck :: Adventure -> IO Bool
    compileCheck adv = case compileAdventure adv of
        Left errs -> do
            putStrLn $ "  compile errors: " ++ show errs
            pure False
        Right cr -> do
            let errors = validateWorld (crWorld cr)
            if null errors then pure True
            else do
                putStrLn $ "  validation issues: " ++ show errors
                pure False

testTheFogYamlCompiles :: IO Bool
testTheFogYamlCompiles = do
    mbPath <- findExample "thefog.yaml"
    case mbPath of
        Nothing -> do
            putStrLn "  thefog.yaml not found in examples/ or ../examples/"
            pure False
        Just path -> do
            advResult <- parseAdventureFile path
            case advResult of
                Left err -> do
                    putStrLn $ "  failed to parse thefog.yaml: " ++ err
                    pure False
                Right adv -> case compileAdventure adv of
                    Left errs -> do
                        putStrLn $ "  compile errors: " ++ show errs
                        pure False
                    Right _ -> pure True

-- | Mini-Fixture: fantasy-magic with custom verb 'cast' (Phase 3a)
testFantasyMagicFixtureCompiles :: IO Bool
testFantasyMagicFixtureCompiles = do
    mbPath <- findExample "fantasy-magic.yaml"
    case mbPath of
        Nothing -> do
            putStrLn "  fantasy-magic.yaml not found"
            pure False
        Just path -> do
            advResult <- parseAdventureFile path
            case advResult of
                Left err -> do
                    putStrLn $ "  failed to parse fantasy-magic.yaml: " ++ err
                    pure False
                Right adv -> case compileAdventure adv of
                    Left errs -> do
                        putStrLn $ "  compile errors: " ++ show errs
                        pure False
                    Right cr -> do
                        let errors = validateWorld (crWorld cr)
                            stateErrs = validateGameState (crWorld cr) (crSave cr)
                        -- No masking filter: the reachability check now runs in
                        -- validateGameState, where the real start_room
                        -- (crSave.currentRoom = room_start) is known. Previously
                        -- UnreachableRoom was filtered out because the validator
                        -- guessed the start room as the alphabetically-first one.
                        if not (null errors) || not (null stateErrs)
                        then do
                            putStrLn $ "  validation issues: " ++ show (errors ++ stateErrs)
                            pure False
                        else do
                            -- Verify the verb registry contains 'cast'
                            let registry = E.verbDefs (crWorld cr)
                            case Map.lookup "cast" registry of
                                Nothing -> do
                                    putStrLn "  verb 'cast' not found in compiled verbDefs"
                                    pure False
                                Just vd -> do
                                    r1 <- expectEqual "cast" (E.vdName vd)
                                    r2 <- expectTrue "magic alias present" ("magic" `elem` E.vdAliases vd)
                                    pure (r1 && r2)

-- | Mini-Fixture: space-oxygen with declared variables (Phase 3b)
testSpaceOxygenFixtureCompiles :: IO Bool
testSpaceOxygenFixtureCompiles = do
    mbPath <- findExample "space-oxygen.yaml"
    case mbPath of
        Nothing -> do
            putStrLn "  space-oxygen.yaml not found"
            pure False
        Just path -> do
            advResult <- parseAdventureFile path
            case advResult of
                Left err -> do
                    putStrLn $ "  failed to parse space-oxygen.yaml: " ++ err
                    pure False
                Right adv -> case compileAdventure adv of
                    Left errs -> do
                        putStrLn $ "  compile errors: " ++ show errs
                        pure False
                    Right cr -> do
                        let defs = E.varDefs (crWorld cr)
                            vars = E.variables (crSave cr)
                        -- Check oxygen variable
                        case Map.lookup "oxygen" defs of
                            Nothing -> do
                                putStrLn "  variable 'oxygen' not in varDefs"
                                pure False
                            Just vd -> do
                                r1 <- expectEqual "oxygen" (E.vdVarName vd)
                                -- Check initial value in save
                                case Map.lookup "oxygen" vars of
                                    Nothing -> do
                                        putStrLn "  variable 'oxygen' not in save variables"
                                        pure False
                                    Just (E.VVInt n) -> do
                                        r2 <- expectEqual 100 n
                                        -- Check hull_integrity
                                        r3 <- expectTrue "hull_integrity in varDefs" (Map.member "hull_integrity" defs)
                                        r4 <- expectTrue "gravity_active is bool type" True
                                        pure (r1 && r2 && r3 && r4)
                                    Just _ -> do
                                        putStrLn "  expected VVInt for oxygen"
                                        pure False

-- | Mini-Fixture: trigger-test with rules (Phase 3f)
testTriggerFixtureCompiles :: IO Bool
testTriggerFixtureCompiles = do
    mbPath <- findExample "trigger-test.yaml"
    case mbPath of
        Nothing -> do
            putStrLn "  trigger-test.yaml not found"
            pure False
        Just path -> do
            advResult <- parseAdventureFile path
            case advResult of
                Left err -> do
                    putStrLn $ "  failed to parse trigger-test.yaml: " ++ err
                    pure False
                Right adv -> case compileAdventure adv of
                    Left errs -> do
                        putStrLn $ "  compile errors: " ++ show errs
                        pure False
                    Right cr -> do
                        let triggers = E.triggerDefs (crWorld cr)
                            ids = map E.trId triggers
                        r1 <- expectTrue "has 2 triggers" (length triggers == 2)
                        r2 <- expectTrue "has pit_falls" ("pit_falls" `elem` ids)
                        r3 <- expectTrue "has bridge_built" ("bridge_built" `elem` ids)
                        -- Check bridge_built uses item-use event
                        let bridge = head [t | t <- triggers, E.trId t == "bridge_built"]
                        r4 <- expectEqual (E.OnUse "crystal") (E.trEvent bridge)
                        pure (r1 && r2 && r3 && r4)

-- | Mini-Fixture: player-config with player stats, initial_variables, flags, active_quests (Phase 4d)
testPlayerConfigFixtureCompiles :: IO Bool
testPlayerConfigFixtureCompiles = do
    mbPath <- findExample "player-config.yaml"
    case mbPath of
        Nothing -> do
            putStrLn "  player-config.yaml not found"
            pure False
        Just path -> do
            advResult <- parseAdventureFile path
            case advResult of
                Left err -> do
                    putStrLn $ "  failed to parse player-config.yaml: " ++ err
                    pure False
                Right adv -> case compileAdventure adv of
                    Left errs -> do
                        putStrLn $ "  compile errors: " ++ show errs
                        pure False
                    Right cr -> do
                        let saved = crSave cr
                            p = E.player saved
                        r1 <- expectEqual 50 (E.playerMaxHealth p)
                        r2 <- expectEqual 8 (E.playerAttack p)
                        r3 <- expectEqual 3 (E.playerDefense p)
                        r4 <- expectEqual (Just 5) (Map.lookup "lockpick" (E.playerSkills p))
                        r5 <- expectEqual (Just (E.VVInt 30)) (Map.lookup "mana" (E.variables saved))
                        r6 <- expectEqual (Just "true") (Map.lookup "started" (E.flags saved))
                        r7 <- expectEqual (Just 0) (Map.lookup "find_treasure" (E.activeQuests saved))
                        pure (r1 && r2 && r3 && r4 && r5 && r6 && r7)

-- | Phase 5h: item with in_container starts inside that container.
testInContainerCompiles :: IO Bool
testInContainerCompiles = do
    let chest = minItem "chest"
        crystal = (minItem "crystal") { aiInContainer = Just "chest" }
        adv = (minAdventure (minRoom "loc_0")) { advItems = [chest, crystal] }
    case compileAdventure adv of
        Left errs -> do
            putStrLn $ "  compile errors: " ++ show errs
            pure False
        Right cr -> do
            let st = E.itemStates (crSave cr)
            expectEqual (Just (E.InContainer "chest"))
                        (E.itemLocation <$> Map.lookup "crystal" st)

-- | Phase 5h: if/then/else outcome compiles to a Conditional effect.
testConditionalOutcomeCompiles :: IO Bool
testConditionalOutcomeCompiles = do
    let cond = AOConditional (E.PlayerHas "crystal") [AOMessage "yes"] [AOMessage "no"]
        vm = Map.fromList [("activate,intact", [cond])]
        item = (minItem "shrine") { aiVerbMap = vm }
    case compileAdventure (advWithItem item) of
        Left errs -> do
            putStrLn $ "  compile errors: " ++ show errs
            pure False
        Right cr -> do
            let def = Map.findWithDefault (error "missing") "shrine" (E.itemDefs (crWorld cr))
            case Map.lookup (E.VUse, "intact") (E.itemVerbMap def) of
                Just (E.Conditional (E.PlayerHas "crystal") _ _) -> pure True
                other -> do
                    putStrLn $ "  unexpected effect: " ++ show other
                    pure False

-- | P1-17: the five effects that had no YAML form. Each JSON case checks the
--   *decoded constructor* (not merely that decoding succeeded) and the compiled
--   engine effect.
testP117EffectSugar :: IO Bool
testP117EffectSugar = do
    let dec = Aeson.decode :: BLC.ByteString -> Maybe AActionOutcome
    r1 <- expectEqual (Just (AONarrative ["a", "b"] [AOMessage "x"]))
              (dec (BLC.pack "{\"narrative\": [\"a\", \"b\"], \"then\": [{\"msg\": \"x\"}]}"))
    r2 <- expectEqual (Just (AOApplyCondition "poisoned" 3 [AODamagePlayer 1] [AOMessage "over"]))
              (dec (BLC.pack "{\"condition\": {\"name\": \"poisoned\", \"turns\": 3, \"tick\": [{\"damage\": 1}], \"end\": [{\"msg\": \"over\"}]}}"))
    r3 <- expectEqual (Just (AOClearCondition "poisoned"))
              (dec (BLC.pack "{\"clear_condition\": \"poisoned\"}"))
    r4 <- expectEqual (Just (AOModifySkill "lockpick" 1))
              (dec (BLC.pack "{\"skill\": {\"name\": \"lockpick\", \"delta\": 1}}"))
    r5 <- expectEqual (Just (AORandomChoice [(3, [AOMessage "a"]), (1, [AOMessage "b"])]))
              (dec (BLC.pack "{\"random\": [[3, [{\"msg\": \"a\"}]], [1, [{\"msg\": \"b\"}]]]}"))
    c1 <- expectEqual (E.Narrative ["a"] (E.SendMessage "x"))
              (compileAActionOutcome (AONarrative ["a"] [AOMessage "x"]))
    c2 <- expectEqual (E.ApplyCondition "p" 2 (Just (E.SendMessage "t")) (Just (E.SendMessage "e")))
              (compileAActionOutcome (AOApplyCondition "p" 2 [AOMessage "t"] [AOMessage "e"]))
    c3 <- expectEqual (E.ClearCondition "p") (compileAActionOutcome (AOClearCondition "p"))
    c4 <- expectEqual (E.ModifySkill "s" 2) (compileAActionOutcome (AOModifySkill "s" 2))
    c5 <- expectEqual (E.RandomChoice [(2, E.SendMessage "a")])
              (compileAActionOutcome (AORandomChoice [(2, [AOMessage "a"])]))
    pure (and [r1, r2, r3, r4, r5, c1, c2, c3, c4, c5])

-- | P1-18: the never-decodable `check_flag` shortcut is gone — flag tests now
--   go through `if: { has_flag: … }` (and a stale `check_flag` is a parse error
--   instead of a silently wrong compile).
testP118CheckFlagRejected :: IO Bool
testP118CheckFlagRejected = do
    let dec = Aeson.decode :: BLC.ByteString -> Maybe AActionOutcome
    r1 <- expectEqual Nothing
              (dec (BLC.pack "{\"check_flag\": \"x\", \"then\": [], \"else\": []}"))
    r2 <- expectEqual (Just (AOConditional (E.HasFlag "x") [AOMessage "y"] []))
              (dec (BLC.pack "{\"if\": {\"has_flag\": \"x\"}, \"then\": [{\"msg\": \"y\"}]}"))
    pure (r1 && r2)

-- | P1-20: `raise:` decodes to `AORaiseEvent` and compiles to `RaiseEvent`,
--   which fires a matching `on: custom <name>` rule at runtime.
testP120RaiseEvent :: IO Bool
testP120RaiseEvent = do
    let dec = Aeson.decode :: BLC.ByteString -> Maybe AActionOutcome
        adv = (minAdventure (minRoom "loc_0"))
                { advTriggers =
                    [ ATrigger "t_raise" "custom ritual_done"
                        Nothing [AORaiseEvent "ritual_done"] False 0 ] }
    r1 <- expectEqual (Just (AORaiseEvent "ritual_done"))
              (dec (BLC.pack "{\"raise\": \"ritual_done\"}"))
    r2 <- case compileAdventure adv of
        Right cr -> expectTrue "raise compiles to RaiseEvent"
                        (any (\t -> E.RaiseEvent "ritual_done" `elem` E.trEffects t)
                             (E.triggerDefs (crWorld cr)))
        Left errs -> do
            putStrLn $ "  unexpected errors: " ++ issuesText errs
            pure False
    pure (r1 && r2)

-- | P2-15: a malformed adventure file must report the reason (and for YAML the
--   line/column) instead of a bare "failed to parse"; an unreadable path is
--   reported instead of throwing.
testParseErrorIsReported :: IO Bool
testParseErrorIsReported = do
    tmp <- getTemporaryDirectory
    let badPath = tmp </> "text-adventure-parse-error.yaml"
    writeFile badPath "rooms: [\n"
    result <- parseAdventureFile badPath
    -- removeFile badPath omitted on Windows to avoid file lock conflict
    r1 <- case result of
        Left err -> expectTrue ("mentions the YAML parse error: " ++ err)
                                 ("YAML parse error" `isInfixOf` err)
        Right _  -> expectTrue "expected a parse error" False
    r2 <- case result of
        Left err -> expectTrue "reports line/column"
                                 ("line" `isInfixOf` err && "column" `isInfixOf` err)
        Right _  -> pure False
    missing <- parseAdventureFile (tmp </> "text-adventure-does-not-exist.yaml")
    r3 <- case missing of
        Left err -> expectTrue "unreadable file is reported" ("Cannot read" `isInfixOf` err)
        Right _  -> expectTrue "expected an unreadable-file error" False
    pure (r1 && r2 && r3)

-- | P2-21: `fuel:` accepts the object form and the legacy array form, compiles
--   to a `FuelSpec`, and a fuelled vehicle starts with a **full** tank (it used
--   to start at `Nothing` = "0/<max>" until the player refuelled).
testP121FuelSpec :: IO Bool
testP121FuelSpec = do
    r1 <- expectEqual (Just (AFuel "hay" 10))
              (Aeson.decode (BLC.pack "{\"item\": \"hay\", \"max\": 10}") :: Maybe AFuel)
    r2 <- expectEqual (Just (AFuel "hay" 10))
              (Aeson.decode (BLC.pack "[\"hay\", 10]") :: Maybe AFuel)
    let cart = (minShip "cart") { avFuel = Just (AFuel "hay" 10) }
        adv  = (minAdventure (minRoom "loc_0")) { advVehicles = [cart] }
    case compileAdventure adv of
        Left errs -> do
            putStrLn $ "  compile errors: " ++ issuesText errs
            pure False
        Right cr -> do
            let vdef = Map.findWithDefault (error "missing") "cart" (E.vehicleDefs (crWorld cr))
                vst  = Map.lookup "cart" (E.vehicleStates (crSave cr))
            r3 <- expectEqual (Just (E.FuelSpec "hay" 10)) (E.vehicleFuelProp vdef)
            r4 <- expectEqual (Just (Just 10)) (E.vsFuel <$> vst)
            pure (r1 && r2 && r3 && r4)

-- | P2-18: `name:` reaches the engine's `worldName` (the game banner); duplicate
--   or unnamed faction level thresholds are rejected instead of being inert
--   decoration.
testP218NameAndLevels :: IO Bool
testP218NameAndLevels = do
    let named = (minAdventure (minRoom "loc_0")) { advName = Just "Der Turm" }
    r1 <- case compileAdventure named of
        Right cr  -> expectEqual "Der Turm" (E.worldName (crWorld cr))
        Left errs -> do
            putStrLn $ "  errors: " ++ issuesText errs
            pure False
    let dupLevels = (minAdventure (minRoom "loc_0"))
            { advFactions = [ AFaction "g" "Gilde" 0
                                [AFactionLevel 10 "a", AFactionLevel 10 "b"] ] }
    r2 <- case compileAdventure dupLevels of
        Left errs -> expectContains "DuplicateFactionLevel" (issuesText errs)
        Right _   -> expectTrue "expected DuplicateFactionLevel" False
    let unnamedLevel = (minAdventure (minRoom "loc_0"))
            { advFactions = [ AFaction "g" "Gilde" 0 [AFactionLevel 10 ""] ] }
    r3 <- case compileAdventure unnamedLevel of
        Left errs -> expectContains "BadFactionLevel" (issuesText errs)
        Right _   -> expectTrue "expected BadFactionLevel" False
    pure (r1 && r2 && r3)

-- | Phase 5h: in_container pointing at a missing item is detected.
testInvalidContainerDetected :: IO Bool
testInvalidContainerDetected = do
    let badSave = minSave
            { itemStates = Map.singleton "crystal"
                (E.ItemState (E.InContainer "missing_chest") "intact" Map.empty False) }
    expectTrue "InvalidContainer detected"
        (InvalidContainer "crystal" "missing_chest" `elem` validateGameState minWorld badSave)

-- | Phase 6: item-on-item (crafting) interaction compiles.
testItemInteractionCompiles :: IO Bool
testItemInteractionCompiles = do
    let ix = AInteractions
            { aiEntity = []
            , aiItem = [ AItemInteraction "herb" "mortar" [AOMessage "paste made"] ] }
        adv = (minAdventure (minRoom "loc_0")) { advInteractions = Just ix }
    case compileAdventure adv of
        Left errs -> do
            putStrLn $ "  compile errors: " ++ show errs
            pure False
        Right cr -> expectTrue "item-on-item interaction present"
            (Map.member ("herb", "mortar") (E.itemInteractions (crWorld cr)))

-- | Phase 6: entity interaction (use item on target) compiles to unlock state.
testEntityInteractionCompiles :: IO Bool
testEntityInteractionCompiles = do
    let ix = AInteractions
            { aiEntity = [ AEntityInteraction "key" "door" "unlocked" (Just "It opens.") ]
            , aiItem = [] }
        adv = (minAdventure (minRoom "loc_0")) { advInteractions = Just ix }
    case compileAdventure adv of
        Left errs -> do
            putStrLn $ "  compile errors: " ++ show errs
            pure False
        Right cr -> expectEqual (Just ("unlocked", "It opens."))
            (Map.lookup ("key", "door") (E.entityInteractions (crWorld cr)))

-- | Phase 6: every genre fixture compiles and validates without issues.
testGenreFixturesCompile :: IO Bool
testGenreFixturesCompile = do
    let fixtures =
            [ "pure-if.yaml", "fantasy.yaml", "cyberpunk.yaml"
            , "space-opera.yaml", "detective.yaml", "horror.yaml" ]
    results <- mapM checkOne fixtures
    pure (and results)
  where
    checkOne fname = do
        mbPath <- findExample fname
        case mbPath of
            Nothing -> do
                putStrLn $ "  genre fixture not found: " ++ fname
                pure False
            Just path -> do
                advResult <- parseAdventureFile path
                case advResult of
                    Left err -> do
                        putStrLn $ "  failed to parse " ++ fname ++ ": " ++ err
                        pure False
                    Right adv -> case compileAdventure adv of
                        Left errs -> do
                            putStrLn $ "  " ++ fname ++ " compile errors: " ++ issuesText errs
                            pure False
                        Right cr -> do
                            let errs = validateWorld (crWorld cr) ++ validateGameState (crWorld cr) (crSave cr)
                            if null errs
                                then pure True
                                else do
                                    putStrLn $ "  " ++ fname ++ " validation: " ++ show errs
                                    pure False

-- ---------------------------------------------------------------------------
-- Phase 7a: factions / standing / set_state
-- ---------------------------------------------------------------------------

-- | `factions:` seeds varDefs + initial variables `faction.<id>`.
testFactionsSeedVariables :: IO Bool
testFactionsSeedVariables = do
    let fac = [ AFaction "corp" "Arasaka" 0 []
              , AFaction "guild" "Thieves Guild" 5 [] ]
        adv = (minAdventure (minRoom "loc_0")) { advFactions = fac }
    case compileAdventure adv of
        Left errs -> do
            putStrLn $ "  compile errors: " ++ show errs
            pure False
        Right cr -> do
            let defs = E.varDefs (crWorld cr)
                vars = E.variables (crSave cr)
            r1 <- expectEqual (Just (E.VarDef "faction.corp" (E.VTInt Nothing Nothing) (E.VVInt 0)))
                      (Map.lookup "faction.corp" defs)
            r2 <- expectEqual (Just (E.VVInt 5)) (Map.lookup "faction.guild" vars)
            pure (r1 && r2)

-- | `standing: {faction, add/set}` compiles to ModifyValue/SetValue on the
--   `faction.<id>` variable.
testStandingOutcomeCompiles :: IO Bool
testStandingOutcomeCompiles = do
    let adv = (minAdventure (minRoom "loc_0"))
            { advFactions = [AFaction "corp" "Arasaka" 0 []]
            , advTriggers = [ ATrigger "quest_reward" "turn" Nothing
                                [ AOStandingAdd "corp" 10
                                , AOStandingSet "corp" 25
                                ] False 0 ] }
    case compileAdventure adv of
        Left errs -> do
            putStrLn $ "  compile errors: " ++ show errs
            pure False
        Right cr -> do
            let tr = head (E.triggerDefs (crWorld cr))
                effs = E.trEffects tr
            r1 <- expectTrue "add compiles to ModifyValue faction.corp"
                (E.ModifyValue (E.VRVariable "faction.corp") 10 `elem` effs)
            r2 <- expectTrue "set compiles to SetValue faction.corp"
                (E.SetValue (E.VRVariable "faction.corp") (E.EVInt 25) `elem` effs)
            -- initial standing of guild var is 0 (not seeded)
            pure (r1 && r2)

-- | `set_state: <entity>, to: <state>` compiles to the entity-state effect.
testSetEntityStateCompiles :: IO Bool
testSetEntityStateCompiles = do
    let adv = (minAdventure (minRoom "loc_0"))
            { advTriggers = [ ATrigger "open_gate" "turn" Nothing
                                [ AOSetEntityState "guild_gate" "unlocked" ] False 0 ] }
    case compileAdventure adv of
        Left errs -> do
            putStrLn $ "  compile errors: " ++ show errs
            pure False
        Right cr -> do
            let tr = head (E.triggerDefs (crWorld cr))
            expectTrue "set_state compiles to SetValue (VRActorProp (ActorEntity e) PState)"
                (E.SetValue (E.VRActorProp (E.ActorEntity "guild_gate") E.PState) (E.EVString "unlocked")
                    `elem` E.trEffects tr)

-- | P1-6: two `rules:` with the same `id` would share one runtime
--   `TriggerState` (fired/cooldown), so a duplicate id is a compile error.
testDuplicateTriggerIdFails :: IO Bool
testDuplicateTriggerIdFails = do
    let adv = (minAdventure (minRoom "loc_0"))
            { advTriggers =
                [ ATrigger "dup" "turn" Nothing [AOMessage "a"] False 0
                , ATrigger "dup" "turn" Nothing [AOMessage "b"] False 0 ] }
    case compileAdventure adv of
        Left errs -> expectContains "DuplicateTriggerId" (issuesText errs)
        Right _   -> expectTrue "expected DuplicateTriggerId" False

-- | P1-6: an author rule id must not use a compiler-owned prefix
--   (encounter./environment./stealth./ship./party.) — those ids belong to
--   generated module triggers in the same `triggerStates` namespace.
testReservedTriggerIdFails :: IO Bool
testReservedTriggerIdFails = do
    let adv = (minAdventure (minRoom "loc_0"))
            { advTriggers = [ ATrigger "stealth.decay" "turn" Nothing [AOMessage "x"] False 0 ] }
    case compileAdventure adv of
        Left errs -> expectContains "ReservedTriggerId" (issuesText errs)
        Right _   -> expectTrue "expected ReservedTriggerId" False

-- | P1-13: `swim`/`crawl`/`dig`/`game` are genre verbs, not core words, so an
--   adventure may now declare them (e.g. `verbs: [swim]`) without tripping
--   `ReservedVerbName`.
testGenreVerbDeclarable :: IO Bool
testGenreVerbDeclarable = do
    let adv = (minAdventure (minRoom "loc_0"))
            { advVerbs = [AVerb "swim" [], AVerb "game" []] }
    case compileAdventure adv of
        Right cr -> expectTrue "swim/game compile as custom verbs"
                        (Map.member "swim" (E.verbDefs (crWorld cr))
                          && Map.member "game" (E.verbDefs (crWorld cr)))
        Left errs -> do
            putStrLn $ "  unexpected errors: " ++ issuesText errs
            pure False

-- | P1-14: an `on: command <verb>` rule must name a core command or a declared
--   custom verb; a bogus name would validate clean and silently never fire.
testUnknownCommandVerbFails :: IO Bool
testUnknownCommandVerbFails = do
    let adv = (minAdventure (minRoom "loc_0"))
            { advTriggers =
                [ ATrigger "t_bad" "command teleport" Nothing [AOMessage "x"] False 0 ] }
    case compileAdventure adv of
        Left errs -> expectContains "UnknownCommandVerb" (issuesText errs)
        Right _   -> expectTrue "expected UnknownCommandVerb" False

-- | W5: `lineForPath` finds the source line of a dotted issue path. The matching
--   rule is nesting-based: first segment at indent 0, each following segment
--   strictly deeper and after its parent. Duplicate keys are disambiguated by
--   that rule, and an unmatched tail falls back to the deepest matched prefix.
testLocateLineForPath :: IO Bool
testLocateLineForPath = do
    let yaml = unlines
            [ "rooms:"
            , "  - id: hall"
            , "    ascii:"
            , "      default: |"
            , "        XXX"
            , "      hotspots:"
            , "        - { glyph: \"*\", target: lever }"
            , "  - id: cave"
            , "    ascii:"
            , "      default: |"
            , "        YYY"
            ]
    r1 <- expectEqual (Just (4, "      default: |")) (lineForPath yaml "rooms.hall.ascii.default")
    -- two `ascii:` blocks: the deeper-indent rule picks the one under cave
    r2 <- expectEqual (Just (9, "    ascii:")) (lineForPath yaml "rooms.cave.ascii")
    r3 <- expectEqual (Just (2, "  - id: hall")) (lineForPath yaml "rooms.hall")
    -- unknown segment falls back to the deepest matched prefix
    r4 <- expectEqual (Just (3, "    ascii:")) (lineForPath yaml "rooms.hall.ascii.nope")
    r5 <- expectTrue "unknown top segment -> Nothing" (isNothing (lineForPath yaml "monsters.x"))
    pure (and [r1, r2, r3, r4, r5])
  where isNothing = maybe True (const False)

-- | P1-14: core command names (`examine`) and declared custom verbs both pass
--   the command-verb check.
testKnownCommandVerbCompiles :: IO Bool
testKnownCommandVerbCompiles = do
    let adv = (minAdventure (minRoom "loc_0"))
            { advVerbs = [AVerb "sneak" []]
            , advTriggers =
                [ ATrigger "t_core"   "command examine" Nothing [AOMessage "a"] False 0
                , ATrigger "t_custom" "command sneak"   Nothing [AOMessage "b"] False 0 ] }
    case compileAdventure adv of
        Right _ -> pure True
        Left errs -> do
            putStrLn $ "  unexpected errors: " ++ issuesText errs
            pure False

-- | Any `faction.<id>` reference (standing outcome / predicate) must point at a
--   declared faction once the `factions:` segment is present.
testUnknownFactionFails :: IO Bool
testUnknownFactionFails = do
    let adv = (minAdventure (minRoom "loc_0"))
            { advFactions = [AFaction "corp" "Arasaka" 0 []]
            , advTriggers = [ ATrigger "bad" "turn" Nothing
                                [ AOStandingAdd "nonexistent" 10 ] False 0 ] }
    case compileAdventure adv of
        Right _ -> do
            putStrLn "  expected UnknownFaction compile error, got Right"
            pure False
        Left errs ->
            expectContains "UnknownFaction" (issuesText errs)

-- | The 7a mini-fixture compiles and validates clean.
testFactionsFixtureCompiles :: IO Bool
testFactionsFixtureCompiles = do
    mbPath <- findExampleModule "factions.yaml"
    case mbPath of
        Nothing -> do
            putStrLn "  examples/modules/factions.yaml not found"
            pure False
        Just path -> do
            advResult <- parseAdventureFile path
            case advResult of
                Left err -> do
                    putStrLn $ "  failed to parse examples/modules/factions.yaml: " ++ err
                    pure False
                Right adv -> case compileAdventure adv of
                    Left errs -> do
                        putStrLn $ "  compile errors: " ++ show errs
                        pure False
                    Right cr -> do
                        let wErrs = validateWorld (crWorld cr)
                            sErrs = validateGameState (crWorld cr) (crSave cr)
                        r1 <- expectEqual [] wErrs
                        r2 <- expectEqual [] sErrs
                        pure (r1 && r2)

-- | The 7b mini-fixture compiles and validates clean (trade module).
testTradeFixtureCompiles :: IO Bool
testTradeFixtureCompiles = do
    mbPath <- findExampleModule "trade.yaml"
    case mbPath of
        Nothing -> do
            putStrLn "  examples/modules/trade.yaml not found"
            pure False
        Just path -> do
            advResult <- parseAdventureFile path
            case advResult of
                Left err -> do
                    putStrLn $ "  failed to parse examples/modules/trade.yaml: " ++ err
                    pure False
                Right adv -> case compileAdventure adv of
                    Left errs -> do
                        putStrLn $ "  compile errors: " ++ show errs
                        pure False
                    Right cr -> do
                        let wErrs = validateWorld (crWorld cr)
                            sErrs = validateGameState (crWorld cr) (crSave cr)
                        r1 <- expectEqual [] wErrs
                        r2 <- expectEqual [] sErrs
                        pure (r1 && r2)

-- ---------------------------------------------------------------------------
-- Phase 7c: encounter tables
-- ---------------------------------------------------------------------------

-- | An encounter table compiles to one trigger carrying a RandomChoice over
--   the weighted entries; a per-entry `when` becomes a Conditional gate.
testEncounterTableCompiles :: IO Bool
testEncounterTableCompiles = do
    let tbl = AEncounterTable "wilds" "turn"
                (Just (E.PNot (E.HasFlag "camp_cleared"))) 5
                [ AEncounterEntry 3 Nothing
                    [ AOMessage "A wolf!" ]
                , AEncounterEntry 1 (Just (E.PlayerHas "torch"))
                    [ AOMessage "Fireflies." ]
                ]
        adv = (minAdventure (minRoom "loc_0")) { advEncounterTables = [tbl] }
    case compileAdventure adv of
        Left errs -> do
            putStrLn $ "  compile errors: " ++ show errs
            pure False
        Right cr -> do
            let tr = head (E.triggerDefs (crWorld cr))
            r1 <- expectEqual "encounter.wilds" (E.trId tr)
            r2 <- expectEqual E.OnTurn (E.trEvent tr)
            r3 <- expectEqual (Just (E.PNot (E.HasFlag "camp_cleared"))) (E.trCondition tr)
            r4 <- expectEqual 5 (E.trCooldown tr)
            r5 <- expectEqual [ E.RandomChoice
                                    [ (3, E.SendMessage "A wolf!")
                                    , (1, E.Conditional (E.PlayerHas "torch")
                                            (E.SendMessage "Fireflies.") E.Noop) ] ]
                    (E.trEffects tr)
            pure (r1 && r2 && r3 && r4 && r5)

-- | A table without entries is rejected; zero weights are rejected.
testEncounterTableValidation :: IO Bool
testEncounterTableValidation = do
    let emptyTbl = AEncounterTable "empty" "turn" Nothing 0 []
        advEmpty = (minAdventure (minRoom "loc_0")) { advEncounterTables = [emptyTbl] }
    r1 <- case compileAdventure advEmpty of
            Left errs -> expectContains "EmptyEncounterTable" (issuesText errs)
            Right _   -> expectTrue "expected EmptyEncounterTable" False
    let badTbl = AEncounterTable "bad" "turn" Nothing 0
                    [AEncounterEntry 0 Nothing [AOMessage "x"]]
        advBad = (minAdventure (minRoom "loc_0")) { advEncounterTables = [badTbl] }
    r2 <- case compileAdventure advBad of
            Left errs -> expectContains "BadEncounterWeight" (issuesText errs)
            Right _   -> expectTrue "expected BadEncounterWeight" False
    pure (r1 && r2)

-- | The 7c mini-fixture compiles and validates clean.
testEncounterFixtureCompiles :: IO Bool
testEncounterFixtureCompiles = do
    mbPath <- findExampleModule "encounters.yaml"
    case mbPath of
        Nothing -> do
            putStrLn "  examples/modules/encounters.yaml not found"
            pure False
        Just path -> do
            advResult <- parseAdventureFile path
            case advResult of
                Left err -> do
                    putStrLn $ "  failed to parse examples/modules/encounters.yaml: " ++ err
                    pure False
                Right adv -> case compileAdventure adv of
                    Left errs -> do
                        putStrLn $ "  compile errors: " ++ show errs
                        pure False
                    Right cr -> do
                        let wErrs = validateWorld (crWorld cr)
                            sErrs = validateGameState (crWorld cr) (crSave cr)
                        r1 <- expectEqual [] wErrs
                        r2 <- expectEqual [] sErrs
                        pure (r1 && r2)

-- | The 7d environment: a weather machine compiles to the `env.weather`
--   variable (initial state index), one OnTurn trigger per transition, and
--   each drain becomes a guarded OnTurn trigger with a zero-check.
testEnvironmentWeatherCompiles :: IO Bool
testEnvironmentWeatherCompiles = do
    let weather = AWeatherDef
            [ "clear", "storm" ] "clear"
            [ AWeatherTransition (Just (E.CompareVar "day" E.CGte 3)) "storm"
                [ AOMessage "Ein Sturm zieht auf!" ] ]
        adv = (minAdventure (minRoom "loc_0"))
            { advEnvironment = Just (AEnvironment (Just weather) []) }
    case compileAdventure adv of
        Left errs -> do
            putStrLn $ "  compile errors: " ++ show errs
            pure False
        Right cr -> do
            r1 <- expectEqual
                (Just (E.VVInt 0)) (Map.lookup "env.weather" (variables (crSave cr)))
            let trs = filter (\t -> E.trId t == "environment.weather.0") (E.triggerDefs (crWorld cr))
            r2 <- expectEqual 1 (length trs)
            r3 <- expectEqual
                (Just (E.CompareVar "day" E.CGte 3)) (E.trCondition (head trs))
            r4 <- expectEqual
                [ E.SetValue (E.VRVariable "env.weather") (E.EVInt 1)
                , E.SendMessage "Ein Sturm zieht auf!" ]
                (E.trEffects (head trs))
            pure (r1 && r2 && r3 && r4)

testEnvironmentDrainCompiles :: IO Bool
testEnvironmentDrainCompiles = do
    let drain = ADrainDef "hunger" (-1)
                (Just (E.PNot (E.HasFlag "fed")))
                [ AOGameEnd "death" (Just "Du verhungerst.") ]
        adv = (minAdventure (minRoom "loc_0"))
            { advEnvironment = Just (AEnvironment Nothing [drain])
            , advVariables = [ AVariable "hunger" "int" (Just (Aeson.Number 2)) Nothing Nothing ] }
    case compileAdventure adv of
        Left errs -> do
            putStrLn $ "  compile errors: " ++ show errs
            pure False
        Right cr -> do
            let tr = head (filter (\t -> E.trId t == "environment.drain.hunger") (E.triggerDefs (crWorld cr)))
            r1 <- expectEqual E.OnTurn (E.trEvent tr)
            r2 <- expectEqual
                (Just (E.PNot (E.HasFlag "fed"))) (E.trCondition tr)
            r3 <- expectEqual
                [ E.ModifyValue (E.VRVariable "hunger") (-1)
                , E.Conditional (E.CompareVar "hunger" E.CLte 0)
                    (E.GameEnd E.Death "Du verhungerst.") E.Noop ]
                (E.trEffects tr)
            pure (r1 && r2 && r3)

testEnvironmentValidation :: IO Bool
testEnvironmentValidation = do
    -- Unknown weather state in `to` / bad initial state
    let badTo = AWeatherDef [ "clear", "storm" ] "clear"
                    [ AWeatherTransition Nothing "blizzard" [ AOMessage "x" ] ]
        advBadTo = (minAdventure (minRoom "loc_0"))
            { advEnvironment = Just (AEnvironment (Just badTo) []) }
    r1 <- case compileAdventure advBadTo of
            Left errs -> expectContains "UnknownWeatherState" (issuesText errs)
            Right _   -> expectTrue "expected UnknownWeatherState" False
    let badInit = AWeatherDef [ "clear", "storm" ] "foggy" []
        advBadInit = (minAdventure (minRoom "loc_0"))
            { advEnvironment = Just (AEnvironment (Just badInit) []) }
    r2 <- case compileAdventure advBadInit of
            Left errs -> expectContains "UnknownWeatherState" (issuesText errs)
            Right _   -> expectTrue "expected UnknownWeatherState" False
    -- Drain variable must be declared
    let advNoVar = (minAdventure (minRoom "loc_0"))
            { advEnvironment = Just (AEnvironment Nothing [ ADrainDef "mana" (-2) Nothing [ AOMessage "x" ] ]) }
    r3 <- case compileAdventure advNoVar of
            Left errs -> expectContains "UnknownDrainVariable" (issuesText errs)
            Right _   -> expectTrue "expected UnknownDrainVariable" False
    pure (r1 && r2 && r3)

-- | The 7d mini-fixture compiles and validates clean.
testSurvivalFixtureCompiles :: IO Bool
testSurvivalFixtureCompiles = do
    mbPath <- findExampleModule "survival.yaml"
    case mbPath of
        Nothing -> do
            putStrLn "  examples/modules/survival.yaml not found"
            pure False
        Just path -> do
            advResult <- parseAdventureFile path
            case advResult of
                Left err -> do
                    putStrLn $ "  failed to parse examples/modules/survival.yaml: " ++ err
                    pure False
                Right adv -> case compileAdventure adv of
                    Left errs -> do
                        putStrLn $ "  compile errors: " ++ show errs
                        pure False
                    Right cr -> do
                        let wErrs = validateWorld (crWorld cr)
                            sErrs = validateGameState (crWorld cr) (crSave cr)
                        r1 <- expectEqual [] wErrs
                        r2 <- expectEqual [] sErrs
                        pure (r1 && r2)

-- | The stealth segment compiles to a noise variable, one OnEnter trigger
--   per room (gain + clamp), one OnTurn observer per NPC (threshold gate),
--   and one final decay trigger. Observers must precede decay.
testStealthCompiles :: IO Bool
testStealthCompiles = do
    let noise = ANoiseSpec "noise" 2 (-1) 10
        guard = AObserver "guard" 5 3
                    [ AOSetFlag "alarmed" "true", AOMessage "The guard heard you!" ]
        adv = (minAdventure (minRoom "loc_0"))
            { advStealth = Just (AStealth noise [guard])
            , advNPCs = [ ANPC "guard" "Guard" (ACondText "Guard" []) (AAscii (ACondText "" []) [] 0 [] Nothing) [] "loc_0" "alive" Nothing 5 2 Map.empty Map.empty Nothing ] }
    case compileAdventure adv of
        Left errs -> do
            putStrLn $ "  compile errors: " ++ show errs
            pure False
        Right cr -> do
            let trs = E.triggerDefs (crWorld cr)
                ids = map E.trId trs
                nmove = filter (\t -> E.trId t == "stealth.nmove.loc_0") trs
                observe = filter (\t -> E.trId t == "stealth.observe.guard") trs
                decay = filter (\t -> E.trId t == "stealth.decay") trs
            r1 <- expectEqual (Just (E.VVInt 0)) (Map.lookup "noise" (variables (crSave cr)))
            r2 <- expectEqual [ E.OnEnter "loc_0" ] (map E.trEvent nmove)
            r3 <- expectEqual [ E.ModifyValue (E.VRVariable "noise") 2
                              , E.Conditional (E.CompareVar "noise" E.CGte 10)
                                    (E.SetValue (E.VRVariable "noise") (E.EVInt 10)) E.Noop ]
                    (concatMap E.trEffects nmove)
            r4 <- expectEqual [E.OnTurn] (map E.trEvent observe)
            r5 <- expectEqual (Just (E.PAll
                                    [ E.CompareVar "noise" E.CGte 5
                                    -- F6-Nacharbeit: a body hears nothing
                                    , E.PNot (E.EntityHasState "guard" "dead") ]))
                    (E.trCondition (head observe))
            r6 <- expectEqual 3 (E.trCooldown (head observe))
            r7 <- expectEqual [E.OnTurn] (map E.trEvent decay)
            -- observer before decay in trigger order
            r8 <- expectTrue "observer precedes decay"
                (length (takeWhile (/= "stealth.observe.guard") ids) < length (takeWhile (/= "stealth.decay") ids))
            -- Behaviour, not just shape: same noise, living guard vs. corpse.
            let baseState = emptyGameState { world = crWorld cr, save = crSave cr }
                withNoise st = st { save = (save st)
                                      { variables = Map.insert "noise" (E.VVInt 9) (variables (save st)) } }
                asCorpse st = st { save = (save st)
                                     { npcStates = Map.adjust (\ns -> ns { npcStatus = "dead" })
                                                              "guard" (npcStates (save st)) } }
                condFires st = maybe False (\p -> evalPredicate p st) (E.trCondition (head observe))
            r9 <- expectTrue "a living observer hears the noise" (condFires (withNoise baseState))
            r10 <- expectTrue "a dead observer hears nothing"
                     (not (condFires (asCorpse (withNoise baseState))))
            pure (and [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10])

-- | Stealth validation: observer NPC must exist; the noise variable must not
--   be declared separately.
testStealthValidation :: IO Bool
testStealthValidation = do
    let advBadNpc = (minAdventure (minRoom "loc_0"))
            { advStealth = Just (AStealth (ANoiseSpec "noise" 2 (-1) 10)
                                    [ AObserver "ghost" 5 0 [ AOMessage "x" ] ]) }
    r1 <- case compileAdventure advBadNpc of
            Left errs -> expectContains "UnknownObserverNPC" (issuesText errs)
            Right _   -> expectTrue "expected UnknownObserverNPC" False
    let advClash = (minAdventure (minRoom "loc_0"))
            { advStealth = Just (AStealth (ANoiseSpec "noise" 2 (-1) 10) [])
            , advVariables = [ AVariable "noise" "int" (Just (Aeson.Number 0)) Nothing Nothing ] }
    r2 <- case compileAdventure advClash of
            Left errs -> expectContains "StealthVariableClash" (issuesText errs)
            Right _   -> expectTrue "expected StealthVariableClash" False
    pure (r1 && r2)

-- | The 7e mini-fixture compiles and validates clean.
testStealthFixtureCompiles :: IO Bool
testStealthFixtureCompiles = do
    mbPath <- findExampleModule "stealth.yaml"
    case mbPath of
        Nothing -> do
            putStrLn "  examples/modules/stealth.yaml not found"
            pure False
        Just path -> do
            advResult <- parseAdventureFile path
            case advResult of
                Left err -> do
                    putStrLn $ "  failed to parse examples/modules/stealth.yaml: " ++ err
                    pure False
                Right adv -> case compileAdventure adv of
                    Left errs -> do
                        putStrLn $ "  compile errors: " ++ show errs
                        pure False
                    Right cr -> do
                        let wErrs = validateWorld (crWorld cr)
                            sErrs = validateGameState (crWorld cr) (crSave cr)
                        r1 <- expectEqual [] wErrs
                        r2 <- expectEqual [] sErrs
                        pure (r1 && r2)

-- ---------------------------------------------------------------------------
-- Modul 7i — Patrol
-- ---------------------------------------------------------------------------

-- | The `patrol:` segment compiles to a per-hostile gate variable, one
--   `on: turn` clear trigger, one `on: turn` step trigger per path index, plus
--   warn/attack triggers. The clear trigger must precede the steps: trigger
--   conditions are evaluated live inside one linear fold, so without the gate a
--   single turn would fire every matching step and teleport the NPC along its
--   whole path.
testPatrolCompiles :: IO Bool
testPatrolCompiles = do
    let hostile = AHostile "wolf" ["loc_1", "loc_2"] 0 False
                    (Just "A wolf is nearby!")
                    [ AOMessage "The wolf bites!", AODamagePlayer 2 ]
        adv = (minAdventure (minRoom "loc_0"))
            { advRooms = [minRoom "loc_0", minRoom "loc_1", minRoom "loc_2"]
            , advPatrol = Just (APatrol [hostile])
            , advNPCs = [ wolfNPC "loc_1" ] }
    case compileAdventure adv of
        Left errs -> do
            putStrLn $ "  compile errors: " ++ show errs
            pure False
        Right cr -> do
            let trs = E.triggerDefs (crWorld cr)
                byId want = [ t | t <- trs, E.trId t == want ]
                clear = byId "patrol.clear.wolf"
                step0 = byId "patrol.step.wolf.0"
                warn  = byId "patrol.warn.wolf"
                atk   = byId "patrol.attack.wolf"
                roomCheck r = E.PAll [ E.Location "player" r, E.Location "wolf" r ]
            r1 <- expectEqual [ "patrol.clear.wolf"
                              , "patrol.step.wolf.0"
                              , "patrol.step.wolf.1"
                              , "patrol.warn.wolf"
                              , "patrol.attack.wolf" ]
                    (map E.trId trs)
            r2 <- expectEqual [ E.SetValue (E.VRVariable "patrol.wolf.moved") (E.EVInt 0) ]
                    (concatMap E.trEffects clear)
            r3 <- expectEqual (Just (E.PAll
                    [ E.PNot (E.EntityHasState "wolf" "dead")
                    , E.CompareVar "patrol.wolf.index" E.CEq 0
                    , E.CompareVar "patrol.wolf.moved" E.CEq 0 ]))
                    (E.trCondition (head step0))
            r4 <- expectEqual [ E.MoveEntity "wolf" (E.InRoom "loc_2")
                              , E.SetValue (E.VRVariable "patrol.wolf.index") (E.EVInt 1)
                              , E.SetValue (E.VRVariable "patrol.wolf.moved") (E.EVInt 1) ]
                    (concatMap E.trEffects step0)
            r5 <- expectEqual (Just (E.VVInt 0)) (Map.lookup "patrol.wolf.index" (variables (crSave cr)))
            r6 <- expectEqual (Just (E.VVInt 0)) (Map.lookup "patrol.wolf.moved" (variables (crSave cr)))
            r7 <- expectEqual (Just (E.PNot (E.EntityHasState "wolf" "dead")))
                    (E.trCondition (head warn))
            r8 <- expectEqual [ E.Conditional (roomCheck "loc_1") (E.SendMessage "A wolf is nearby!") E.Noop
                              , E.Conditional (roomCheck "loc_2") (E.SendMessage "A wolf is nearby!") E.Noop ]
                    (concatMap E.trEffects warn)
            r9 <- expectEqual [ E.Conditional (roomCheck "loc_1")
                                    (E.Sequence [ E.SendMessage "The wolf bites!"
                                                , E.ModifyValue E.VRPlayerHealth (-2) ]) E.Noop
                              , E.Conditional (roomCheck "loc_2")
                                    (E.Sequence [ E.SendMessage "The wolf bites!"
                                                , E.ModifyValue E.VRPlayerHealth (-2) ]) E.Noop ]
                    (concatMap E.trEffects atk)
            -- Behaviour, not just shape: the gate closes after one step.
            let st0 = emptyGameState { world = crWorld cr, save = crSave cr }
                gated st = st { save = (save st)
                                  { variables = Map.insert "patrol.wolf.moved" (E.VVInt 1)
                                                  (variables (save st)) } }
                condFires st = maybe False (\p -> evalPredicate p st) (E.trCondition (head step0))
            r10 <- expectTrue "step fires while the gate is open" (condFires st0)
            r11 <- expectTrue "step is blocked once the gate is set" (not (condFires (gated st0)))
            pure (and [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11])

-- | A guardian stays put: no clear/step triggers, only warn/attack.
testPatrolGuardian :: IO Bool
testPatrolGuardian = do
    let hostile = AHostile "wolf" ["loc_1"] 0 True Nothing [ AOMessage "The guardian strikes!" ]
        adv = (minAdventure (minRoom "loc_0"))
            { advRooms = [minRoom "loc_0", minRoom "loc_1"]
            , advPatrol = Just (APatrol [hostile])
            , advNPCs = [ wolfNPC "loc_1" ] }
    case compileAdventure adv of
        Left errs -> do
            putStrLn $ "  compile errors: " ++ show errs
            pure False
        Right cr -> do
            r1 <- expectEqual [ "patrol.attack.wolf" ] (map E.trId (E.triggerDefs (crWorld cr)))
            r2 <- expectEqual Nothing (Map.lookup "patrol.wolf.index" (variables (crSave cr)))
            pure (r1 && r2)

-- | Patrol validation: unknown NPC, empty path, unknown room, variable clash.
testPatrolValidation :: IO Bool
testPatrolValidation = do
    let base extraRooms npcList = (minAdventure (minRoom "loc_0"))
            { advRooms = minRoom "loc_0" : extraRooms
            , advNPCs = npcList }
    r1 <- case compileAdventure (base [] []) { advPatrol = Just (APatrol [AHostile "ghost" ["loc_0"] 0 False Nothing []]) } of
            Left errs -> expectContains "UnknownPatrolNPC" (issuesText errs)
            Right _   -> expectTrue "expected UnknownPatrolNPC" False
    r2 <- case compileAdventure (base [] [ wolfNPC "loc_0" ]) { advPatrol = Just (APatrol [AHostile "wolf" [] 0 False Nothing []]) } of
            Left errs -> expectContains "EmptyPatrolPath" (issuesText errs)
            Right _   -> expectTrue "expected EmptyPatrolPath" False
    r3 <- case compileAdventure (base [ minRoom "loc_1" ] [ wolfNPC "loc_1" ])
                { advPatrol = Just (APatrol [AHostile "wolf" ["loc_1", "nope"] 0 False Nothing []]) } of
            Left errs -> expectContains "UnknownPatrolRoom" (issuesText errs)
            Right _   -> expectTrue "expected UnknownPatrolRoom" False
    r4 <- case compileAdventure (base [ minRoom "loc_1" ] [ wolfNPC "loc_1" ])
                { advPatrol = Just (APatrol [AHostile "wolf" ["loc_1"] 0 False Nothing []])
                , advVariables = [ AVariable "patrol.wolf.moved" "int" (Just (Aeson.Number 0)) Nothing Nothing ] } of
            Left errs -> expectContains "PatrolVariableClash" (issuesText errs)
            Right _   -> expectTrue "expected PatrolVariableClash" False
    pure (or [r1, r2, r3, r4] && and [r1, r2, r3, r4])

-- | The 7i mini-fixture compiles and validates clean.
testPatrolFixtureCompiles :: IO Bool
testPatrolFixtureCompiles = do
    mbPath <- findExampleModule "patrol.yaml"
    case mbPath of
        Nothing -> do
            putStrLn "  examples/modules/patrol.yaml not found"
            pure False
        Just path -> do
            advResult <- parseAdventureFile path
            case advResult of
                Left err -> do
                    putStrLn $ "  failed to parse examples/modules/patrol.yaml: " ++ err
                    pure False
                Right adv -> case compileAdventure adv of
                    Left errs -> do
                        putStrLn $ "  compile errors: " ++ show errs
                        pure False
                    Right cr -> do
                        r1 <- expectEqual [] (validateWorld (crWorld cr))
                        r2 <- expectEqual [] (validateGameState (crWorld cr) (crSave cr))
                        pure (r1 && r2)

-- | The patrolling wolf the patrol tests declare.
wolfNPC :: String -> ANPC
wolfNPC loc = ANPC "wolf" "Wolf" (ACondText "Wolf" []) (AAscii (ACondText "" []) [] 0 [] Nothing)
                    [] loc "alive" Nothing 8 3 Map.empty Map.empty Nothing

-- | The 7f combat segment: default without a block is CombatClassic; off /
--   narrative compile to their profiles; tactical and unknown profiles are
--   rejected (Phase 7f-3 stays open).
testCombatCompiles :: IO Bool
testCombatCompiles = do
    -- default: no combat block -> classic
    r0 <- case compileAdventure (minAdventure (minRoom "loc_0")) of
            Left _ -> expectTrue "default compiles" False
            Right cr -> expectEqual (E.CombatClassic Nothing) (E.combatProfile (crWorld cr))
    -- off with custom refusal
    let advOff = (minAdventure (minRoom "loc_0"))
            { advCombat = Just (ACombat "off" (Just "Im Fokus: Gespräch, nicht Gewalt.") 0 [] [] Nothing Nothing Nothing Nothing Nothing) }
    r1 <- case compileAdventure advOff of
            Left _   -> expectTrue "off compiles" False
            Right cr -> expectEqual (E.CombatOff (Just "Im Fokus: Gespräch, nicht Gewalt."))
                                     (E.combatProfile (crWorld cr))
    -- narrative with on_win/on_lose effects
    let advNarr = (minAdventure (minRoom "loc_0"))
            { advCombat = Just (ACombat "narrative" Nothing 3
                                    [ AOMessage "Du überzeugst ihn." ]
                                    [ AODamagePlayer 4 ]
                                    Nothing Nothing Nothing Nothing Nothing) }
    r2 <- case compileAdventure advNarr of
            Left _   -> expectTrue "narrative compiles" False
            Right cr -> case E.combatProfile (crWorld cr) of
                E.CombatNarrative nc -> do
                    rA <- expectEqual 3 (E.ncDifficulty nc)
                    rB <- expectEqual (E.SendMessage "Du überzeugst ihn.") (E.ncOnWin nc)
                    rC <- expectEqual (E.ModifyValue E.VRPlayerHealth (-4)) (E.ncOnLose nc)
                    pure (rA && rB && rC)
                _ -> expectTrue "expected narrative profile" False
    -- tactical: custom options
    let advTac = (minAdventure (minRoom "loc_0"))
            { advCombat = Just (ACombat "tactical" Nothing 0 [] [] (Just "by_speed") (Just False) (Just 50) (Just "agility") Nothing) }
    r3 <- case compileAdventure advTac of
            Left _    -> expectTrue "tactical compiles" False
            Right cr  -> case E.combatProfile (crWorld cr) of
                E.CombatTactical tc -> do
                    rA <- expectEqual E.BySpeed (E.tcInitiative tc)
                    rB <- expectEqual False (E.tcFleeAllowed tc)
                    rC <- expectEqual 50 (E.tcMaxRounds tc)
                    rD <- expectEqual "agility" (E.tcSpeedAttribute tc)
                    pure (rA && rB && rC && rD)
                _ -> expectTrue "expected tactical profile" False
    -- tactical default options:
    let advTacDef = (minAdventure (minRoom "loc_0"))
            { advCombat = Just (ACombat "tactical" Nothing 0 [] [] Nothing Nothing Nothing Nothing Nothing) }
    r3Def <- case compileAdventure advTacDef of
            Left _    -> expectTrue "tactical defaults compile" False
            Right cr  -> case E.combatProfile (crWorld cr) of
                E.CombatTactical tc -> do
                    rA <- expectEqual E.PlayerFirst (E.tcInitiative tc)
                    rB <- expectEqual True (E.tcFleeAllowed tc)
                    rC <- expectEqual 100 (E.tcMaxRounds tc)
                    rD <- expectEqual "speed" (E.tcSpeedAttribute tc)
                    pure (rA && rB && rC && rD)
                _ -> expectTrue "expected tactical profile with defaults" False
    -- tactical unknown initiative
    let advTacBadInit = (minAdventure (minRoom "loc_0"))
            { advCombat = Just (ACombat "tactical" Nothing 0 [] [] (Just "random_dice") Nothing Nothing Nothing Nothing) }
    r3Bad <- case compileAdventure advTacBadInit of
            Left errs -> expectContains "UnknownInitiativeRule" (issuesText errs)
            Right _   -> expectTrue "expected UnknownInitiativeRule" False
    -- unknown profile
    let advBad = (minAdventure (minRoom "loc_0"))
            { advCombat = Just (ACombat "quantum" Nothing 0 [] [] Nothing Nothing Nothing Nothing Nothing) }
    r4 <- case compileAdventure advBad of
            Left errs -> expectContains "UnknownCombatProfile" (issuesText errs)
            Right _   -> expectTrue "expected unknown profile rejection" False
    -- player abilities compile cleanly
    let advAb = (minAdventure (minRoom "loc_0"))
            { advAbilities = [ AAbility "slam" (Just "Slam") (Just "player.stamina") (Just 5) (Just 2) [AOMessage "Slam!"] ] }
    rAb <- case compileAdventure advAb of
            Left _ -> expectTrue "abilities compile" False
            Right cr -> case Map.lookup "slam" (E.abilities (crWorld cr)) of
                Just ab -> do
                    rA <- expectEqual "Slam" (E.paName ab)
                    rB <- expectEqual "player.stamina" (E.paCostVar ab)
                    rC <- expectEqual 5 (E.paCost ab)
                    rD <- expectEqual 2 (E.paCooldown ab)
                    rE <- expectEqual [E.SendMessage "Slam!"] (E.paEffects ab)
                    pure (rA && rB && rC && rD && rE)
                Nothing -> expectTrue "expected ability 'slam'" False
    pure (r0 && r1 && r2 && r3 && r3Def && r3Bad && r4 && rAb)

-- | The 7f combat fixture(s) compile and validate clean.
testCombatFixturesCompile :: IO Bool
testCombatFixturesCompile = do
    r1 <- fixtureOk "combat-off.yaml"
    r2 <- fixtureOk "combat-narrative.yaml"
    r3 <- fixtureOk "combat-classic.yaml"
    r4 <- fixtureOk "combat-tactical.yaml"
    pure (r1 && r2 && r3 && r4)

  where
    fixtureOk fname = do
        mbPath <- findExampleModule fname
        case mbPath of
            Nothing -> do
                putStrLn $ "  examples/modules/" ++ fname ++ " not found"
                pure False
            Just path -> do
                advResult <- parseAdventureFile path
                case advResult of
                    Left err -> do
                        putStrLn $ "  failed to parse examples/modules/" ++ fname ++ ": " ++ err
                        pure False
                    Right adv -> case compileAdventure adv of
                        Left errs -> do
                            putStrLn $ "  compile errors (" ++ fname ++ "): " ++ show errs
                            pure False
                        Right cr -> do
                            let wErrs = validateWorld (crWorld cr)
                                sErrs = validateGameState (crWorld cr) (crSave cr)
                            rA <- expectEqual [] wErrs
                            rB <- expectEqual [] sErrs
                            pure (rA && rB)

-- ---------------------------------------------------------------------------
-- Phase 7g: party / companions
-- ---------------------------------------------------------------------------

-- | A party-capable NPC with an explicit state, for the party tests.
partySquire :: Maybe AParty -> ANPC
partySquire party
    = ANPC "squire" "Knappe" (ACondText "Knappe" []) (AAscii (ACondText "" []) [] 0 [] Nothing) [] "loc_0" "alive"
        (Just 20) 3 1 Map.empty Map.empty party

followVerb :: AVerb
followVerb = AVerb "follow" ["escort"]

-- | The expected membership toggle for `squire`.
expectedToggle :: E.Effect
expectedToggle = E.Conditional (E.CompareVar "party.squire" E.CGte 1)
    (E.Sequence [ E.SendMessage "Knappe stays behind."
                , E.SetValue (E.VRVariable "party.squire") (E.EVInt 0) ])
    (E.Sequence [ E.SendMessage "Knappe falls in behind you."
                , E.SetValue (E.VRVariable "party.squire") (E.EVInt 1) ])

-- | The `party:` block compiles to the follow variable `party.<npc>` plus one
--   order-verb entry in the NPC's compiled verb map.
testPartyCompiles :: IO Bool
testPartyCompiles = do
    let adv = (minAdventure (minRoom "loc_0"))
            { advVerbs = [followVerb]
            , advNPCs = [partySquire (Just (AParty True "follow" True Nothing Nothing))] }
    case compileAdventure adv of
        Left errs -> do
            putStrLn $ "  compile errors: " ++ show errs
            pure False
        Right cr -> do
            let vars = variables (crSave cr)
                def = Map.lookup "squire" (E.npcDefs (crWorld cr))
                entry = def >>= Map.lookup (E.VCustom "follow", "alive") . E.npcVerbMap
            r1 <- expectEqual (Just (E.VVInt 0)) (Map.lookup "party.squire" vars)
            r2 <- expectTrue "follow variable declared" (Map.member "party.squire" (E.varDefs (crWorld cr)))
            r3 <- expectEqual (Just expectedToggle) entry
            pure (r1 && r2 && r3)

-- | Without `can_join` the block is inert: no variable, no order verb, and a
--   plain NPC is untouched (module default invariant).
testPartyCanJoinFalseIsInert :: IO Bool
testPartyCanJoinFalseIsInert = do
    let adv = (minAdventure (minRoom "loc_0"))
            { advVerbs = [followVerb]
            , advNPCs = [ partySquire (Just (AParty False "follow" True Nothing Nothing))
                        , (partySquire Nothing) { anId = "plainnpc" } ] }
    case compileAdventure adv of
        Left errs -> do
            putStrLn $ "  compile errors: " ++ show errs
            pure False
        Right cr -> do
            r1 <- expectEqual Nothing (Map.lookup "party.squire" (variables (crSave cr)))
            r2 <- expectTrue "no follow variable declared" (not (Map.member "party.squire" (E.varDefs (crWorld cr))))
            r3 <- expectEqual (Just []) (Map.lookup "squire" (E.npcDefs (crWorld cr)) >>= Just . Map.keys . E.npcVerbMap)
            pure (r1 && r2 && r3)

-- | Party validation: unknown/core order verb, hp tracked without max_hp and
--   an author-declared follow variable are all compile errors.
testPartyValidation :: IO Bool
testPartyValidation = do
    -- the order verb is not declared under `verbs:`
    let advNoVerb = (minAdventure (minRoom "loc_0"))
            { advNPCs = [partySquire (Just (AParty True "follow" True Nothing Nothing))] }
    r1 <- case compileAdventure advNoVerb of
            Left errs -> expectContains "PartyOrderVerbUnknown" (issuesText errs)
            Right _   -> expectTrue "expected PartyOrderVerbUnknown" False
    -- a core verb would shadow the built-in behaviour -> rejected as well
    let advCoreVerb = (minAdventure (minRoom "loc_0"))
            { advNPCs = [partySquire (Just (AParty True "look" True Nothing Nothing))] }
    r2 <- case compileAdventure advCoreVerb of
            Left errs -> expectContains "PartyOrderVerbUnknown" (issuesText errs)
            Right _   -> expectTrue "expected PartyOrderVerbUnknown for core verb" False
    -- hp_tracked needs a max_hp
    let advNoHp = (minAdventure (minRoom "loc_0"))
            { advVerbs = [followVerb]
            , advNPCs = [(partySquire (Just (AParty True "follow" True Nothing Nothing))) { anMaxHealth = Nothing }] }
    r3 <- case compileAdventure advNoHp of
            Left errs -> expectContains "PartyHealthMissing" (issuesText errs)
            Right _   -> expectTrue "expected PartyHealthMissing" False
    -- the follow variable is module-owned
    let advClash = (minAdventure (minRoom "loc_0"))
            { advVerbs = [followVerb]
            , advNPCs = [partySquire (Just (AParty True "follow" True Nothing Nothing))]
            , advVariables = [ AVariable "party.squire" "int" (Just (Aeson.Number 0)) Nothing Nothing ] }
    r4 <- case compileAdventure advClash of
            Left errs -> expectContains "PartyVariableClash" (issuesText errs)
            Right _   -> expectTrue "expected PartyVariableClash" False
    pure (r1 && r2 && r3 && r4)

-- | `damage_npc: { npc: X, amount: N }` is sugar over the existing NPC-HP
--   effect, and an unknown target is a compile error.
testDamageNpcCompiles :: IO Bool
testDamageNpcCompiles = do
    let stabber target = (partySquire Nothing)
            { anId = "trapper"
            , anVerbMap = Map.fromList [("stab,alive", [AODamageNPC target 25])] }
        advOk = (minAdventure (minRoom "loc_0"))
            { advVerbs = [AVerb "stab" []]
            , advNPCs = [partySquire Nothing, stabber "squire"] }
        advBad = (minAdventure (minRoom "loc_0"))
            { advVerbs = [AVerb "stab" []]
            , advNPCs = [partySquire Nothing, stabber "ghost"] }
    r1 <- case compileAdventure advOk of
            Left errs -> do
                putStrLn $ "  compile errors: " ++ show errs
                pure False
            Right cr -> expectEqual
                (Just (E.ModifyValue (E.VRActorProp (E.ActorNPC "squire") E.PHealth) (-25)))
                (Map.lookup "trapper" (E.npcDefs (crWorld cr))
                    >>= Map.lookup (E.VCustom "stab", "alive") . E.npcVerbMap)
    r2 <- case compileAdventure advBad of
            Left errs -> expectContains "UnknownDamageNPC" (issuesText errs)
            Right _   -> expectTrue "expected UnknownDamageNPC" False
    pure (r1 && r2)

-- | The 7g mini-fixture compiles and validates clean.
testPartyFixtureCompiles :: IO Bool
testPartyFixtureCompiles = do
    mbPath <- findExampleModule "party.yaml"
    case mbPath of
        Nothing -> do
            putStrLn "  examples/modules/party.yaml not found"
            pure False
        Just path -> do
            advResult <- parseAdventureFile path
            case advResult of
                Left err -> do
                    putStrLn $ "  failed to parse examples/modules/party.yaml: " ++ err
                    pure False
                Right adv -> case compileAdventure adv of
                    Left errs -> do
                        putStrLn $ "  compile errors (party.yaml): " ++ show errs
                        pure False
                    Right cr -> do
                        let wErrs = validateWorld (crWorld cr)
                            sErrs = validateGameState (crWorld cr) (crSave cr)
                        rA <- expectEqual [] wErrs
                        rB <- expectEqual [] sErrs
                        pure (rA && rB)

-- ---------------------------------------------------------------------------
-- Phase 7h: ship systems and stations
-- ---------------------------------------------------------------------------

-- | A minimal ship: two interior rooms, three systems, no stations.
minShip :: String -> AVehicle
minShip vid = AVehicle
    { avId = vid
    , avName = "Kestrel"
    , avDesc = "A lean courier."
    , avType = "player"
    , avInterior = [minRoom (vid ++ "_bridge"), minRoom (vid ++ "_guns")]
    , avEntryRoom = vid ++ "_bridge"
    , avCockpit = Just (vid ++ "_bridge")
    , avStops = Map.empty
    , avKeywords = ["ship", vid]
    , avFuel = Nothing
    , avConditions = Map.empty
    , avStartStop = Nothing
    , avSystems = Map.fromList [ ("power", ASystem 4 4)
                               , ("weapons", ASystem 2 5)
                               , ("hull", ASystem 6 6) ]
    , avStations = []
    }

-- | P1-19: a stop with a declared cost (long form) reaches the engine as
--   `VehicleStop … (Just (item, refused))`; an undeclared cost item is rejected.
testP119StopCost :: IO Bool
testP119StopCost = do
    let tram = (minShip "tram")
            { avType  = "paid"
            , avStops = Map.fromList
                [ ("Dock 7",  AStop "loc_1" (Just ("ticket", "Kein Ticket!")))
                , ("Zentrum", AStop "loc_0" Nothing) ] }
        adv = (minAdventure (minRoom "loc_0"))
                { advRooms = [minRoom "loc_0", minRoom "loc_1"]
                , advItems = [minItem "ticket"]
                , advVehicles = [tram] }
    case compileAdventure adv of
        Left errs -> do
            putStrLn $ "  compile errors: " ++ show errs
            pure False
        Right cr -> do
            let vdef = Map.findWithDefault (error "missing") "tram" (E.vehicleDefs (crWorld cr))
            r1 <- expectEqual (Just (Just ("ticket", "Kein Ticket!")))
                      (E.stopCost <$> Map.lookup "loc_1" (E.vehicleStops vdef))
            let bad = (minShip "tram2")
                    { avType  = "paid"
                    , avStops = Map.fromList
                        [ ("Dock 7", AStop "loc_1" (Just ("ghost_ticket", "no"))) ] }
                advBad = (minAdventure (minRoom "loc_0"))
                            { advRooms = [minRoom "loc_0", minRoom "loc_1"]
                            , advVehicles = [bad] }
            r2 <- case compileAdventure advBad of
                Left errs2 -> expectContains "UnknownStopCostItem" (issuesText errs2)
                Right _    -> expectTrue "expected UnknownStopCostItem" False
            pure (r1 && r2)

-- | `systems:` becomes VarMap entries, `stations:` one gated trigger per room.
testShipSystemsCompile :: IO Bool
testShipSystemsCompile = do
    let ship = (minShip "kestrel")
            { avStations = [ AStation "kestrel_guns" "feuern" [AOMessage "Die Waffen feuern."] Nothing ] }
        adv = (minAdventure (minRoom "loc_0"))
            { advVehicles = [ship]
            , advVerbs = [AVerb "feuern" []] }
    case compileAdventure adv of
        Left errs -> do
            putStrLn $ "  compile errors: " ++ show errs
            pure False
        Right cr -> do
            let vars = variables (crSave cr)
                stationTriggers = [ t | t <- E.triggerDefs (crWorld cr)
                                      , E.trId t == "ship.kestrel.station.kestrel_guns" ]
            r1 <- expectEqual (Just (E.VVInt 4)) (Map.lookup "ship.kestrel.power" vars)
            r2 <- expectEqual (Just (E.VVInt 2)) (Map.lookup "ship.kestrel.weapons" vars)
            r3 <- expectEqual (Just (E.VVInt 6)) (Map.lookup "ship.kestrel.hull" vars)
            r4 <- expectTrue "weapons system declared" (Map.member "ship.kestrel.weapons" (E.varDefs (crWorld cr)))
            r5 <- case stationTriggers of
                [t] -> do
                    a <- expectEqual [E.OnCommand "feuern"] (map E.trEvent stationTriggers)
                    b <- expectEqual (Just (E.PAll [E.Location "player" "kestrel_guns"])) (E.trCondition t)
                    c <- expectEqual [E.SendMessage "Die Waffen feuern."] (E.trEffects t)
                    pure (a && b && c)
                _ -> do
                    putStrLn $ "  expected exactly one station trigger, got " ++ show (length stationTriggers)
                    pure False
            pure (r1 && r2 && r3 && r4 && r5)

-- | Station validation: room must be an interior room, the verb must be a
--   declared custom verb, and systems are module-owned variables.
testShipSystemsValidation :: IO Bool
testShipSystemsValidation = do
    let withStation st ship = ship { avStations = [st] }
    -- unknown interior room
    let advRoom = (minAdventure (minRoom "loc_0"))
            { advVehicles = [withStation (AStation "nowhere" "feuern" [] Nothing) (minShip "kestrel")]
            , advVerbs = [AVerb "feuern" []] }
    r1 <- case compileAdventure advRoom of
            Left errs -> expectContains "UnknownStationRoom" (issuesText errs)
            Right _   -> expectTrue "expected UnknownStationRoom" False
    -- undeclared verb
    let advVerb = (minAdventure (minRoom "loc_0"))
            { advVehicles = [withStation (AStation "kestrel_guns" "salutieren" [] Nothing) (minShip "kestrel")] }
    r2 <- case compileAdventure advVerb of
            Left errs -> expectContains "UnknownStationVerb" (issuesText errs)
            Right _   -> expectTrue "expected UnknownStationVerb" False
    -- a core verb would shadow the built-in behaviour of that room
    let advCore = (minAdventure (minRoom "loc_0"))
            { advVehicles = [withStation (AStation "kestrel_guns" "look" [] Nothing) (minShip "kestrel")] }
    r3 <- case compileAdventure advCore of
            Left errs -> expectContains "UnknownStationVerb" (issuesText errs)
            Right _   -> expectTrue "expected UnknownStationVerb for core verb" False
    -- the system variable is module-owned
    let advClash = (minAdventure (minRoom "loc_0"))
            { advVehicles = [minShip "kestrel"]
            , advVariables = [AVariable "ship.kestrel.hull" "int" (Just (Aeson.Number 0)) Nothing Nothing] }
    r4 <- case compileAdventure advClash of
            Left errs -> expectContains "ShipVariableClash" (issuesText errs)
            Right _   -> expectTrue "expected ShipVariableClash" False
    pure (r1 && r2 && r3 && r4)

-- | The 7h mini-fixture compiles and validates clean.
testStarshipFixtureCompiles :: IO Bool
testStarshipFixtureCompiles = do
    mbPath <- findExampleModule "starship.yaml"
    case mbPath of
        Nothing -> do
            putStrLn "  examples/modules/starship.yaml not found"
            pure False
        Just path -> do
            advResult <- parseAdventureFile path
            case advResult of
                Left err -> do
                    putStrLn $ "  failed to parse examples/modules/starship.yaml: " ++ err
                    pure False
                Right adv -> case compileAdventure adv of
                    Left errs -> do
                        putStrLn $ "  compile errors (starship.yaml): " ++ show errs
                        pure False
                    Right cr -> do
                        let wErrs = validateWorld (crWorld cr)
                            sErrs = validateGameState (crWorld cr) (crSave cr)
                        rA <- expectEqual [] wErrs
                        rB <- expectEqual [] sErrs
                        pure (rA && rB)

-- | Compile + validate a shipped module fixture.
fixtureCompilesAndValidates :: String -> IO Bool
fixtureCompilesAndValidates fname = do
    mbPath <- findExampleModule fname
    case mbPath of
        Nothing -> do
            putStrLn $ "  examples/modules/" ++ fname ++ " not found"
            pure False
        Just path -> do
            advResult <- parseAdventureFile path
            case advResult of
                Left err -> do
                    putStrLn $ "  failed to parse examples/modules/" ++ fname ++ ": " ++ err
                    pure False
                Right adv -> case compileAdventure adv of
                    Left errs -> do
                        putStrLn $ "  compile errors (" ++ fname ++ "): " ++ show errs
                        pure False
                    Right cr -> do
                        rA <- expectEqual [] (validateWorld (crWorld cr))
                        rB <- expectEqual [] (validateGameState (crWorld cr) (crSave cr))
                        pure (rA && rB)

-- | The Phase-7 composition proof: one game using 7a + 7b + 7d + 7g + 7h at
--   the same time, coupled only by authored rules.
testComboFixtureCompiles :: IO Bool
testComboFixtureCompiles = fixtureCompilesAndValidates "combo.yaml"

-- | Try candidate paths for the modules directory.
findExampleModule :: String -> IO (Maybe FilePath)
findExampleModule fname = firstExisting
    [ "examples/modules" </> fname
    , "../examples/modules" </> fname
    , "../../examples/modules" </> fname
    ]
  where
    firstExisting [] = pure Nothing
    firstExisting (p : rest) = do
        ok <- doesFileExist p
        if ok then pure (Just p) else firstExisting rest

-- ---------------------------------------------------------------------------
-- Phase B: state-dependent ASCII art
-- ---------------------------------------------------------------------------

-- | `ascii:` on rooms, items and NPCs accepts the plain string shorthand and
--   the object form, and compiles to the engine's CondText.
testAsciiCondTextCompiles :: IO Bool
testAsciiCondTextCompiles = do
    -- JSON shorthand and object form both decode to ACondText.
    let shorthand = Aeson.decode (BLC.pack "\"just a string\"") :: Maybe ACondText
        objectForm = Aeson.decode (BLC.pack
            "{\"default\":\"D\",\"variants\":[{\"when\":{\"has_flag\":\"lit\"},\"text\":\"L\"}]}")
            :: Maybe ACondText
    r0a <- expectEqual (Just (ACondText "just a string" [])) shorthand
    r0b <- expectEqual (Just (ACondText "D" [ATextVariant (E.HasFlag "lit") "L"])) objectForm
    -- Compilation maps them 1:1 onto the engine AsciiArt.
    let room = (minRoom "loc_0") { arAscii = AAscii (ACondText "DARK" [ATextVariant (E.HasFlag "lit") "LIT"]) [] 0 [] Nothing }
        item = (minItem "lamp") { aiAscii = AAscii (ACondText "LAMP" []) [] 0 [] Nothing }
        npc  = (partySquire Nothing) { anAscii = AAscii (ACondText "NPC" []) [] 0 [] Nothing }
        adv  = (minAdventure room) { advItems = [item], advNPCs = [npc] }
    case compileAdventure adv of
        Left errs -> do
            putStrLn $ "  compile errors: " ++ show errs
            pure False
        Right cr -> do
            let rm = Map.findWithDefault (error "room") "loc_0" (E.rooms (crWorld cr))
                it = Map.findWithDefault (error "item") "lamp" (E.itemDefs (crWorld cr))
                np = Map.findWithDefault (error "npc") "squire" (E.npcDefs (crWorld cr))
            r1 <- expectEqual (E.AsciiArt (E.CondText "DARK" [E.TextVariant (E.HasFlag "lit") "LIT"]) [] 0 [] Nothing) (E.roomAscii rm)
            r2 <- expectEqual (E.AsciiArt (E.CondText "LAMP" []) [] 0 [] Nothing) (E.itemAscii it)
            r3 <- expectEqual (E.AsciiArt (E.CondText "NPC" []) [] 0 [] Nothing) (E.npcAscii np)
            pure (r0a && r0b && r1 && r2 && r3)

-- | Animated `ascii:` compiles frames + `every` (Phase D).
testAnimatedAsciiCompiles :: IO Bool
testAnimatedAsciiCompiles = do
    let art = AAscii (ACondText "" []) [ACondText "F0" [], ACondText "F1" []] 2 [] Nothing
        room = (minRoom "loc_0") { arAscii = art }
    case compileAdventure (minAdventure room) of
        Left errs -> do
            putStrLn $ "  compile errors: " ++ show errs
            pure False
        Right cr -> do
            let rm = Map.findWithDefault (error "room") "loc_0" (E.rooms (crWorld cr))
            r1 <- expectEqual [E.CondText "F0" [], E.CondText "F1" []] (E.aaFrames (E.roomAscii rm))
            r2 <- expectEqual 2 (E.aaEvery (E.roomAscii rm))
            pure (r1 && r2)

-- | An `ambient` block compiles into the engine AsciiArt (Phase H/H1).
testAmbientCompiles :: IO Bool
testAmbientCompiles = do
    let art = AAscii (ACondText "" []) [] 0 [] (Just (E.Ambient ["W0", "W1"] 4))
        room = (minRoom "loc_0") { arAscii = art }
    case compileAdventure (minAdventure room) of
        Left errs -> do
            putStrLn $ "  compile errors: " ++ show errs
            pure False
        Right cr -> do
            let rm = Map.findWithDefault (error "room") "loc_0" (E.rooms (crWorld cr))
            r1 <- expectEqual (Just (E.Ambient ["W0", "W1"] 4)) (E.aaAmbient (E.roomAscii rm))
            pure r1

-- | Ambient validation (Phase H/H1): a non-positive fps and an empty frame
--   list are compile errors with stable codes.
testAmbientValidation :: IO Bool
testAmbientValidation = do
    let badFps    = (minRoom "loc_0") { arAscii = AAscii (ACondText "" []) [] 0 [] (Just (E.Ambient ["F"] 0)) }
        badFrames = (minRoom "loc_0") { arAscii = AAscii (ACondText "" []) [] 0 [] (Just (E.Ambient [] 4)) }
    case (compileAdventure (minAdventure badFps), compileAdventure (minAdventure badFrames)) of
        (Left e1, Left e2) -> do
            r1 <- expectTrue "fps<=0 is AmbientFpsInvalid" ("AmbientFpsInvalid" `isInfixOf` issuesText e1)
            r2 <- expectTrue "empty frames is AmbientFramesEmpty" ("AmbientFramesEmpty" `isInfixOf` issuesText e2)
            pure (r1 && r2)
        (a, b) -> do
            putStrLn $ "  unexpected: " ++ show (fmap (const ()) a, fmap (const ()) b)
            pure False

-- | Clips compile into the GameWorld (Phase H/H4).
testClipsCompile :: IO Bool
testClipsCompile = do
    let adv = (minAdventure (minRoom "loc_0"))
            { advClips = [ AClip "pan" Nothing ["F0", "F1"] 6 ] }
    case compileAdventure adv of
        Left errs -> do
            putStrLn $ "  compile errors: " ++ show errs
            pure False
        Right cr -> do
            r1 <- expectEqual (Map.fromList [("pan", E.Clip ["F0", "F1"] 6)]) (E.worldClips (crWorld cr))
            pure r1

-- | Clip validation (Phase H/H4): duplicate ids, unknown `intro:` targets,
--   unknown `play_clip` references and unusable clips are compile errors.
testClipValidation :: IO Bool
testClipValidation = do
    let roomUnknown  = (minRoom "loc_0") { arIntro = Just "ghost" }
        dupAdv       = (minAdventure (minRoom "loc_0"))
            { advClips = [ AClip "x" Nothing ["F"] 4, AClip "x" Nothing ["F"] 4 ] }
        emptyAdv     = (minAdventure (minRoom "loc_0"))
            { advClips = [ AClip "y" Nothing [] 4 ] }
        fpsAdv       = (minAdventure (minRoom "loc_0"))
            { advClips = [ AClip "z" Nothing ["F"] 0 ] }
        playAdv      = (minAdventure (minRoom "loc_0"))
            { advClips = []
            , advTriggers = [ ATrigger "t" "turn" Nothing [ AOPlayClip "ghost" ] False 0 ] }
    results <- forM [(compileAdventure (minAdventure roomUnknown), "unknown intro", "UnknownClip")
                    ,(compileAdventure dupAdv, "duplicate id", "DuplicateClip")
                    ,(compileAdventure emptyAdv, "empty frames", "ClipFramesEmpty")
                    ,(compileAdventure fpsAdv, "bad fps", "ClipFpsInvalid")
                    ,(compileAdventure playAdv, "unknown play_clip", "UnknownClip")] $ \(r, label, code) ->
        case r of
            Left errs -> expectTrue (label ++ " -> " ++ code) (code `isInfixOf` issuesText errs)
            Right _ -> do
                putStrLn $ "  expected failure: " ++ label
                pure False
    pure (and results)

-- | A clip with `file:` embeds its frames from the companion file (D14):
--   parseAdventureFile resolves the path relative to the adventure.
testClipFileEmbedding :: IO Bool
testClipFileEmbedding = do
    tmp <- getTemporaryDirectory
    let dir = tmp ++ "/wbclip-" ++ show (length [()] :: Int)
        clipJson = dir </> "pan.json"
        yamlPath = dir </> "adv.yaml"
        yaml = unlines
            [ "name: ClipTest"
            , "start_room: loc_0"
            , "rooms:"
            , "  - id: loc_0"
            , "    name: R"
            , "    desc: A room."
            , "clips:"
            , "  - id: pan"
            , "    fps: 5"
            , "    file: pan.json"
            ]
    createDirectoryIfMissing True dir
    writeFile clipJson "[\"A\", \"B\" ]"
    writeFile yamlPath yaml
    r <- parseAdventureFile yamlPath
    result <- case r of
        Right adv -> expectEqual (Just (AClip "pan" Nothing ["A", "B"] 5))
                                 (listToMaybe (advClips adv))
        Left err -> do
            putStrLn $ "  parse failed: " ++ err
            pure False
    mapM_ removeFile [clipJson, yamlPath]
    pure result

-- | `end_art` and `title_art` compile into the GameWorld (Phase G).
testEndTitleArtCompiles :: IO Bool
testEndTitleArtCompiles = do
    let adv = (minAdventure (minRoom "loc_0"))
            { advEndArt = Map.fromList
                [ ("death", AAscii (ACondText "D" []) [] 1 [] Nothing)
                , ("victory", AAscii (ACondText "V" []) [] 1 [] Nothing) ]
            , advTitleArt = AAscii (ACondText "T" []) [] 1 [] Nothing }
        plainAdv = minAdventure (minRoom "loc_0")
    case (compileAdventure adv, compileAdventure plainAdv) of
        (Right cr, Right crPlain) -> do
            let gw = crWorld cr
            r1 <- expectEqual (Just (E.AsciiArt (E.CondText "D" []) [] 1 [] Nothing))
                      (Map.lookup "death" (E.worldEndArt gw))
            r2 <- expectEqual (E.AsciiArt (E.CondText "T" []) [] 1 [] Nothing) (E.worldTitleArt gw)
            r3 <- expectEqual (E.AsciiArt (E.CondText "" []) [] 1 [] Nothing) (E.worldTitleArt (crWorld crPlain))
            r4 <- expectEqual Map.empty (E.worldEndArt (crWorld crPlain))
            pure (r1 && r2 && r3 && r4)
        _ -> expectTrue "end/title art compile failed" False

-- | The shipped banner fixture compiles, validates and carries both banners.
testBannerArtFixtureCompiles :: IO Bool
testBannerArtFixtureCompiles = do
    mbPath <- findExample "banner-art.yaml"
    case mbPath of
        Nothing -> do
            putStrLn "  banner-art.yaml not found"
            pure False
        Just path -> do
            advResult <- parseAdventureFile path
            case advResult of
                Left err -> do
                    putStrLn $ "  parse error: " ++ err
                    pure False
                Right adv -> case compileAdventure adv of
                    Left errs -> do
                        putStrLn $ "  compile errors: " ++ show errs
                        pure False
                    Right cr -> do
                        let gw = crWorld cr
                            verrs = validateWorld gw
                            victory = Map.lookup "victory" (E.worldEndArt gw)
                            title = E.worldTitleArt gw
                        r1 <- expectTrue "banner-art fixture validates" (null verrs)
                        r2 <- expectTrue "victory end_art compiled"
                                  (maybe False (isInfixOf "*** YOU WIN ***" . E.ctDefault . E.aaStatic) victory)
                        r3 <- expectTrue "title_art compiled"
                                  (not (E.isEmptyAscii title))
                        pure (r1 && r2 && r3)

-- | Phase E: hotspot validation reports unknown targets, missing/duplicate
--   glyphs and reserved characters at compile time.
testHotspotValidation :: IO Bool
testHotspotValidation = do
    let mkAdv glyphs artText =
            (minAdventure ((minRoom "loc_0") { arAscii = AAscii (ACondText artText []) [] 0 glyphs Nothing }))
                { advItems = [minItem "lamp"]
                , advNPCs = [ (partySquire Nothing) { anId = "troll" } ] }
        hs g t = E.Hotspot g t
        codeOf outcome = case outcome of
            Left errs -> issuesText errs
            Right _   -> ""
    r1 <- expectContains "UnknownHotspotTarget"
              (codeOf (compileAdventure (mkAdv [hs '*' "ghost"] "*")))
    r2 <- expectContains "HotspotGlyphMissing"
              (codeOf (compileAdventure (mkAdv [hs 'X' "lamp"] "*")))
    r3 <- expectContains "DuplicateHotspotGlyph"
              (codeOf (compileAdventure (mkAdv [hs '*' "lamp", hs '*' "troll"] "*")))
    r4 <- expectContains "ReservedHotspotGlyph"
              (codeOf (compileAdventure (mkAdv [hs '1' "lamp"] "1")))
    r5 <- case compileAdventure (mkAdv [hs '*' "lamp"] "*") of
              Left errs -> expectTrue ("valid hotspots rejected: " ++ issuesText errs) False
              Right _   -> pure True
    pure (r1 && r2 && r3 && r4 && r5)

-- | The shipped hotspot fixture compiles, validates and carries its markers.
testHotspotFixtureCompiles :: IO Bool
testHotspotFixtureCompiles = do
    mbPath <- findExample "hotspot.yaml"
    case mbPath of
        Nothing -> do
            putStrLn "  hotspot.yaml not found"
            pure False
        Just path -> do
            advResult <- parseAdventureFile path
            case advResult of
                Left err -> do
                    putStrLn $ "  parse error: " ++ err
                    pure False
                Right adv -> case compileAdventure adv of
                    Left errs -> do
                        putStrLn $ "  compile errors: " ++ show errs
                        pure False
                    Right cr -> do
                        let gw = crWorld cr
                            verrs = validateWorld gw
                            spots = maybe [] E.aaHotspots (E.roomAscii <$> Map.lookup "cave" (E.rooms gw))
                        r1 <- expectTrue "hotspot fixture validates" (null verrs)
                        r2 <- expectEqual [E.Hotspot '*' "lever", E.Hotspot 'T' "troll"] spots
                        pure (r1 && r2)

-- | The shipped state-dependent ASCII fixture compiles and validates clean.
testAsciiFixtureCompiles :: IO Bool
testAsciiFixtureCompiles = do
    mbPath <- findExample "ascii-state.yaml"
    case mbPath of
        Nothing -> do
            putStrLn "  ascii-state.yaml not found"
            pure False
        Just path -> do
            advResult <- parseAdventureFile path
            case advResult of
                Left err -> do
                    putStrLn $ "  parse error: " ++ err
                    pure False
                Right adv -> case compileAdventure adv of
                    Left errs -> do
                        putStrLn $ "  compile errors: " ++ show errs
                        pure False
                    Right cr -> do
                        let verrs = validateWorld (crWorld cr)
                            hall = Map.lookup "hall" (E.rooms (crWorld cr))
                            animated = maybe False (not . null . E.aaFrames . E.roomAscii) hall
                            every = maybe 0 (E.aaEvery . E.roomAscii) hall
                        r1 <- expectTrue "ascii-state fixture validates" (null verrs)
                        r2 <- expectTrue "the hall carries animation frames" animated
                        r3 <- expectEqual 2 every
                        pure (r1 && r2 && r3)

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------
tests :: [(String, IO Bool)]
tests =
    [ ("ascii: string/object compiles to AsciiArt (B)", testAsciiCondTextCompiles)
    , ("animated ascii compiles frames + every (D)", testAnimatedAsciiCompiles)
    , ("end_art/title_art compile (G)", testEndTitleArtCompiles)
    , ("banner-art fixture compiles + validates (G)", testBannerArtFixtureCompiles)
    , ("hotspot validation (E)", testHotspotValidation)
    , ("hotspot fixture compiles + validates (E)", testHotspotFixtureCompiles)
    , ("ascii-state fixture compiles + validates (B)", testAsciiFixtureCompiles)
    , ("ambient compiles into AsciiArt (H1)", testAmbientCompiles)
    , ("clips compile into worldClips (H4)", testClipsCompile)
    , ("clip validation: dup/unknown/empty/fps (H4)", testClipValidation)
    , ("clip file embedding (D14)", testClipFileEmbedding)
    , ("ambient validation: fps and frames (H1)", testAmbientValidation)
    , ("direction aliases ne/nw/se/sw work", testDirectionAliases)
    , ("unknown direction is a compile error", testUnknownDirectionFails)
    , ("'activate' verb maps to VUse", testActivateVerbMapsToUse)
    , ("unknown verb is a compile error", testUnknownVerbFails)
    , ("verb aliases (examine->VLookAt) work", testRepeatedVerbAliases)
    , ("on_take is merged into verb map", testOnTakeMergedIntoVerbMap)
    , ("on_take conflict with verb_map merges outcomes", testOnTakeConflictMerges)
    , ("unknown equip slot is a compile error", testInvalidSlotFails)
    , ("invalid equip effect is a compile error", testInvalidEffectFails)
    , ("AExitRef string scalar has no quotes", testExitRefStringNoQuotes)
    , ("AExitRef object parses to/locked_by", testExitRefObject)
    , ("AActionOutcome string scalar has no quotes", testActionOutcomeStringNoQuotes)
    , ("ADialogueChoice string scalar has no quotes", testDialogueChoiceStringNoQuotes)
    -- Phase 2: collision detection
    , ("duplicate direction (se + southeast) is a compile error", testDuplicateDirectionFails)
    , ("duplicate verb key (use + activate) is a compile error", testDuplicateVerbKeyFails)
    , ("compile issue points at exact YAML field path", testIssuePathPointsAtField)
    -- Phase 2: validateGameState
    , ("invalid start room detected", testInvalidStartRoomDetected)
    , ("invalid item location detected", testInvalidItemLocationDetected)
    , ("invalid NPC location detected", testInvalidNPCLocationDetected)
    , ("empty quest stages detected", testEmptyQuestStagesDetected)
    , ("unknown quest prereq flag detected", testUnknownQuestPrereqDetected)
    , ("valid minimal world+save has no validateGameState errors", testValidSaveStateHasNoNewErrors)
    , ("demo.yaml compiles and validates clean", testDemoYamlCompiles)
    , ("thefog.yaml compiles with strict compiler", testTheFogYamlCompiles)
    -- Phase 3a: custom verb fixture
    , ("fantasy-magic fixture compiles with cast verb", testFantasyMagicFixtureCompiles)
    -- Phase 3b: variable fixture
    , ("space-oxygen fixture compiles with variables", testSpaceOxygenFixtureCompiles)
    -- Phase 3f: trigger fixture
    , ("trigger-test fixture compiles with rules", testTriggerFixtureCompiles)
    -- Phase 4d: player config fixture
    , ("player-config fixture compiles with stats/flags/quests", testPlayerConfigFixtureCompiles)
    -- Phase 5h: containers + conditional outcomes
    , ("in_container places item inside container", testInContainerCompiles)
    , ("if/then/else outcome compiles to Conditional", testConditionalOutcomeCompiles)
    , ("P1-17 effect sugar: narrative/condition/skill/random", testP117EffectSugar)
    , ("P1-18 check_flag is rejected, has_flag works", testP118CheckFlagRejected)
    , ("P1-19 paid stop cost reaches the engine", testP119StopCost)
    , ("P1-20 raise: fires a custom event", testP120RaiseEvent)
    , ("P2-15 parse error reports reason + position", testParseErrorIsReported)
    , ("P2-21 fuel object form + full tank", testP121FuelSpec)
    , ("P2-18 world name + faction level validation", testP218NameAndLevels)
    , ("in_container at missing item is detected", testInvalidContainerDetected)
    , ("item-on-item (crafting) interaction compiles", testItemInteractionCompiles)
    , ("entity interaction (use on target) compiles", testEntityInteractionCompiles)
    , ("all 6 genre fixtures compile + validate clean", testGenreFixturesCompile)
    -- Phase 7a: factions / standing / set_state
    , ("factions segment seeds faction.* variables", testFactionsSeedVariables)
    , ("standing add/set outcome compiles to faction var", testStandingOutcomeCompiles)
    , ("set_state outcome compiles to entity state effect", testSetEntityStateCompiles)
    , ("duplicate rule id is a compile error (P1-6)", testDuplicateTriggerIdFails)
    , ("reserved rule id prefix is a compile error (P1-6)", testReservedTriggerIdFails)
    , ("genre verb (swim/game) is declarable (P1-13)", testGenreVerbDeclarable)
    , ("unknown command verb is a compile error (P1-14)", testUnknownCommandVerbFails)
    , ("known command verb compiles (P1-14)", testKnownCommandVerbCompiles)
    , ("standing reference to unknown faction fails", testUnknownFactionFails)
    , ("factions fixture compiles + validates", testFactionsFixtureCompiles)
    , ("trade fixture compiles + validates", testTradeFixtureCompiles)
    -- Phase 7c: encounter tables
    , ("encounter table compiles to weighted RandomChoice trigger", testEncounterTableCompiles)
    , ("encounter table validation (empty / zero weight)", testEncounterTableValidation)
    , ("encounter fixture compiles + validates", testEncounterFixtureCompiles)
    -- Phase 7d: environment (weather + drains)
    , ("weather machine compiles to env.weather + OnTurn transitions", testEnvironmentWeatherCompiles)
    , ("drains compile to guarded OnTurn triggers", testEnvironmentDrainCompiles)
    , ("environment validation (unknown state / unknown drain var)", testEnvironmentValidation)
    , ("survival fixture compiles + validates", testSurvivalFixtureCompiles)
    -- Phase 7e: stealth
    , ("stealth compiles to noise/observer/decay triggers", testStealthCompiles)
    , ("stealth validation (unknown npc / var clash)", testStealthValidation)
    , ("stealth fixture compiles + validates", testStealthFixtureCompiles)
    , ("patrol compiles to gate + steps + warn/attack", testPatrolCompiles)
    , ("patrol guardian stays put", testPatrolGuardian)
    , ("patrol validation (npc / path / room / variable clash)", testPatrolValidation)
    , ("patrol fixture compiles + validates", testPatrolFixtureCompiles)
    -- Phase 7f: combat profiles
    , ("combat segment compiles (default/off/narrative/tactical)", testCombatCompiles)
    , ("combat fixtures compile + validate", testCombatFixturesCompile)
    -- Rogue Phase 1: authored game policy
    , ("game policy compiles (default/ironman/warning/missing room)", testGamePolicyCompiles)
    , ("set_exit/remove_exit compile + validate (Rogue P3)", testSetExitCompiles)
    -- Phase 7g: party / companions
    , ("party block compiles to follow var + order verb", testPartyCompiles)
    , ("party block with can_join false is inert", testPartyCanJoinFalseIsInert)
    , ("party validation (verb / hp / variable clash)", testPartyValidation)
    , ("damage_npc compiles and validates its target", testDamageNpcCompiles)
    , ("party fixture compiles + validates", testPartyFixtureCompiles)
    -- Phase 7h: ship systems + stations
    , ("ship systems compile to VarMap + station triggers", testShipSystemsCompile)
    , ("ship validation (station room / verb / variable clash)", testShipSystemsValidation)
    , ("starship fixture compiles + validates", testStarshipFixtureCompiles)
    -- Phase 7 acceptance: composition proof
    , ("combo fixture (5 modules) compiles + validates", testComboFixtureCompiles)
    , ("combat. namespace is reserved (7f-3 A1)", testCombatVariableClash)
    , ("cooldown_ condition namespace is reserved (F6)", testCooldownConditionClash)
    , ("lineForPath finds issue source lines (W5)", testLocateLineForPath)
    -- Rogue Phase 4a: SplitMix64 generator RNG
    , ("rng: SplitMix64 known-answer vectors", testRngKnownVectors)
    , ("rng: same seed, identical stream (determinism)", testRngDeterminism)
    , ("rng: randInt bounds + coverage", testRngIntBounds)
    , ("rng: pickWeighted respects weights (smoke)", testRngPickWeighted)
    , ("rng: shuffle is a seeded permutation", testRngShuffle)
    , ("rng: deriveRuntimeSeed matches seed * GOLDEN", testRngDeriveRuntimeSeed)
    -- Rogue Phase 4b: dungeon template schema + validation
    , ("template: valid minimal template parses without issues", testTemplateValidMinimal)
    , ("template: unknown special.*.template is rejected", testTemplateUnknownSpecial)
    , ("template: depth_range outside layout.depth is rejected", testTemplateDepthRangeOutside)
    , ("template: locked treasure without boss_lock key is rejected", testTemplateBossLockMissingKey)
    , ("template: room.id is generator-assigned (RoomTemplateIdForbidden)", testTemplateRoomIdForbidden)
    , ("template: boss depth must be 'max'", testTemplateBossDepthSpec)
    , ("template: rooms.min < 3 is rejected", testTemplateLayoutTooSmall)
    , ("template: oneway hint direction is validated", testTemplateOnewayDirection)
    , ("template: at most one boss npc (Review-Frage 4)", testTemplateMultipleBossNpcs)
    , ("template: invalid count range is rejected", testTemplateInvalidCount)
    , ("template: seed parses from number and string", testTemplateSeedParse)
    , ("template: invalid combat fragment fails parsing", testTemplateInvalidCombat)
    , ("template: levels block parses with all fields", testTemplateLevelsParse)
    , ("template: levels count exceeding layout.depth is rejected", testTemplateLevelsExceedDepth)
    , ("template: levels invalid field values are rejected", testTemplateLevelsInvalidValues)
    -- Rogue Phase 4c: pure generation core
    , ("gen: same seed, identical plan (determinism)", testGenDeterminism)
    , ("gen: rooms.min unreachable -> GENoSpace", testGenNoSpace)
    , ("gen: every cell reaches the boss (directed BFS)", testGenReachability)
    , ("gen: early abort delivers dungeon + warning", testGenEarlyAbort)
    , ("gen: one-way hint converts exactly one edge", testGenOneway)
    , ("gen: start cell + savezone archetype", testGenStartCell)
    , ("gen: boss sits at maximum depth", testGenBossDepth)
    , ("gen: loops create cycles (edges > nodes - 1)", testGenCycles)
    , ("gen: bidirectional edges are unique pairs", testGenEdgeUniqueness)
    , ("gen: different seeds diverge (smoke)", testGenSeedDivergence)
    -- Rogue Phase 4d: emission, locks, pools
    , ("gen: adventure compiles + validates clean (multi-seed)", testGenAdventureClean)
    , ("gen: combat passthrough reaches advCombat", testGenCombatPassthrough)
    , ("gen: save_zones from savezone instances", testGenSavezones)
    , ("gen: treasure lock + key ordering (keyDepth < lockDepth)", testGenLockKey)
    , ("gen: pool placement lands in non-special rooms", testGenPools)
    -- Rogue Phase 4b: multi-level layout & stairs
    , ("gen: multi-level layout places rooms on all levels (Ebenen 1..N)", testGenMultiLevelRoomsOnAllLevels)
    , ("gen: multi-level descent edges connect deepest cell of level i to (0,0,i+1)", testGenMultiLevelStairEdges)
    , ("gen: return_stairs true adds upward edge", testGenMultiLevelReturnStairs)
    , ("gen: boss sits on the last level at maximum depth", testGenMultiLevelBossOnLastLevel)
    , ("gen: depth_range filters room templates by level in multi-level layout", testGenMultiLevelDepthRangeFiltering)
    , ("gen: auto-distribution of budget when levels entries are omitted", testGenMultiLevelAutoDistribution)
    , ("gen: multi-level adventure compiles and validates clean", testGenMultiLevelAdventureValid)
    , ("gen: rooms carry roomFloor matching level", testGenMultiLevelRoomFloors)
    , ("engine: roomFloor JSON default-invariante holds", testRoomFloorJsonDefaultInvariant)
    , ("gen: multilevel_dungeon_template.yaml fixture compiles and validates clean", testMultiLevelFixtureCompiles)
    , ("run: run-seed derivation and fresh runs across runs", testRunPreparationDeterministicSeedAndDifferentRuns)
    , ("run: seed override is respected", testRunPreparationSeedOverride)
    , ("run: pruneOldRuns removes oldest runs beyond keepCount", testPruneOldRuns)
    , ("run: checkpoint checksum binds to world of that run", testCheckpointBindingAcrossRuns)
    ]

-- | 7f-3 A1: `combat.` is the engine's namespace for the combat round state — an
--   author-declared variable in it is rejected, the same rule that guards the
--   `ship.` systems (and for the same reason: the engine owns those entries).
testCombatVariableClash :: IO Bool
testCombatVariableClash = do
    let withVar n = (minAdventure (minRoom "loc_0"))
            { advVariables = [AVariable n "int" (Just (Aeson.Number 0)) Nothing Nothing] }
    r1 <- case compileAdventure (withVar "combat.round") of
            Left errs -> expectContains "CombatVariableClash" (issuesText errs)
            Right _   -> expectTrue "expected CombatVariableClash for combat.round" False
    r2 <- case compileAdventure (withVar "combat.engaged") of
            Left errs -> expectContains "CombatVariableClash" (issuesText errs)
            Right _   -> expectTrue "expected CombatVariableClash for combat.engaged" False
    -- an ordinary variable is unaffected
    r3 <- case compileAdventure (withVar "hunger") of
            Left errs -> expectTrue "an ordinary variable is not a combat clash"
                            (not ("CombatVariableClash" `isInfixOf` issuesText errs))
            Right _   -> pure True
    pure (r1 && r2 && r3)

-- | F6: the engine marks an ability cooldown as a condition named
--   `cooldown_<abilityId>` (7f-3, A3). That prefix belongs to the engine — an
--   author condition of the same name would silently share the marker. Variables
--   have the same guard (`CombatVariableClash`); this is the condition-side twin.
testCooldownConditionClash :: IO Bool
testCooldownConditionClash = do
    let withHook outcomes = (minAdventure (minRoom "loc_0"))
            { advRooms = [ (minRoom "loc_0") { arOnEnter = Just outcomes } ] }
    r1 <- case compileAdventure (withHook [AOApplyCondition "cooldown_slash" 2 [] []]) of
            Left errs -> expectContains "CooldownConditionClash" (issuesText errs)
            Right _   -> expectTrue "expected CooldownConditionClash for apply_condition" False
    r2 <- case compileAdventure (withHook [AOClearCondition "cooldown_slash"]) of
            Left errs -> expectContains "CooldownConditionClash" (issuesText errs)
            Right _   -> expectTrue "expected CooldownConditionClash for clear_condition" False
    -- an ordinary condition name is unaffected (control case)
    r3 <- case compileAdventure (withHook [AOApplyCondition "poisoned" 3 [] []]) of
            Left errs -> expectTrue "an ordinary condition is not a cooldown clash"
                            (not ("CooldownConditionClash" `isInfixOf` issuesText errs))
            Right _   -> pure True
    pure (r1 && r2 && r3)

-- ---------------------------------------------------------------------------
-- Rogue Phase 4a: SplitMix64 generator RNG (Worldbuilder.Rng)
-- ---------------------------------------------------------------------------

-- | Pin the exact SplitMix64 algorithm against independently computed
--   reference vectors (standard spec: state += 0x9E3779B97F4A7C15, then the
--   two multipliers and the final xor-shift). If any constant changes, this
--   test breaks — which is the point: generated dungeons must stay
--   bit-identical across compiler versions.
testRngKnownVectors :: IO Bool
testRngKnownVectors = do
    let draws0 = take 3 (iterate (stepRng . snd) (stepRng (newRng 0)))
        vals0  = map fst draws0
        expect = [0xE220A8397B1DCDAF, 0x6E789E6AA1B965F4, 0x06C45D188009454F]
        (w42a, _) = stepRng (newRng 42)
        (w42b, _) = stepRng . snd $ stepRng (newRng 42)
    r1 <- expectEqual expect vals0
    r2 <- expectTrue "seed 42 first draw matches"
                     (w42a == 0xBDD732262FEB6E95)
    r3 <- expectTrue "seed 42 second draw matches"
                     (w42b == 0x28EFE333B266F103)
    pure (r1 && r2 && r3)

-- | The core Phase 4 invariant in miniature: the same seed must produce the
--   same stream of decisions (randInt, pickWeighted, shuffle) every time.
testRngDeterminism :: IO Bool
testRngDeterminism = do
    let stream seed =
            let (a, r1) = randInt 0 999 (newRng seed)
                (b, r2) = randInt 1 6 r1
                (c, r3) = pickWeighted [(3, 'x'), (5, 'y'), (1, 'z')] r2
                (s, r4) = shuffle [1 .. 20 :: Int] r3
            in (a, b, c, s)
    r1 <- expectEqual (stream 12345) (stream 12345)
    r2 <- expectTrue "different seeds diverge (smoke)"
                     (stream 12345 /= stream 54321)
    pure (r1 && r2)

-- | randInt must stay within [lo, hi] and (smoke-level) cover small ranges
--   entirely; a degenerate range must not advance the state.
testRngIntBounds :: IO Bool
testRngIntBounds = do
    let (vs, finalR) = go (newRng 777) 1000 []
        go r 0 acc = (reverse acc, r)
        go r n acc = let (v, r') = randInt 3 7 r in go r' (n - 1 :: Int) (v : acc)
    r1 <- expectTrue "all draws within [3,7]" (all (\v -> v >= 3 && v <= 7) vs)
    r2 <- expectTrue "small range fully covered (smoke)"
                     (length (nub vs) == 5)
    -- degenerate range: single value, and the state must be untouched
    let (one, rSame) = randInt 5 5 (newRng 42)
    r3 <- expectEqual (5, newRng 42) (one, rSame)
    _ <- pure finalR
    pure (r1 && r2 && r3)

-- | Weighted picks must stay inside the pool and (smoke-level) follow the
--   weights: 90/10 must favor the heavy side decisively over 1000 draws.
testRngPickWeighted :: IO Bool
testRngPickWeighted = do
    let go r 0 acc = (reverse acc, r)
        go r n acc =
            let (v, r') = pickWeighted [(9, 'a'), (1, 'b')] r
            in go r' (n - 1 :: Int) (v : acc)
        (picks, _) = go (newRng 2024) 1000 []
        countA = length (filter (== 'a') picks)
    r1 <- expectTrue "pool coverage: only 'a'/'b' drawn" (all (`elem` ['a', 'b']) picks)
    r2 <- expectTrue "9:1 weights respected (smoke, a >= 800/1000)" (countA >= 800)
    r3 <- expectTrue "both sides reachable" (countA < 1000)
    pure (r1 && r2 && r3)

-- | shuffle must return a true permutation (same multiset) and be seeded:
--   two different seeds almost certainly produce different orders.
testRngShuffle :: IO Bool
testRngShuffle = do
    let xs = [1 .. 50 :: Int]
        (s1, _) = shuffle xs (newRng 1)
        (s2, _) = shuffle xs (newRng 2)
    r1 <- expectTrue "shuffle preserves the multiset" (sort s1 == xs)
    r2 <- expectTrue "shuffle is seeded (smoke)" (s1 /= s2)
    r3 <- expectTrue "empty shuffle stays empty" (null (fst (shuffle [] (newRng 9))))
    r4 <- expectTrue "single element stays fixed"
                     (fst (shuffle [7 :: Int] (newRng 9)) == [7])
    pure (r1 && r2 && r3 && r4)

testRngDeriveRuntimeSeed :: IO Bool
testRngDeriveRuntimeSeed = do
    r1 <- expectTrue "seed 7 * GOLDEN mod 2^64"
        (deriveRuntimeSeed 7 == 0x538454127B096493)
    r2 <- expectTrue "seed 0 derives 0 (documented edge)" (deriveRuntimeSeed 0 == 0)
    pure (r1 && r2)

-- ---------------------------------------------------------------------------
-- Rogue Phase 4b: dungeon template schema + validation
-- ---------------------------------------------------------------------------

-- | Replace the first occurrence of @needle@ in a template YAML snippet.
replace1 :: String -> String -> String -> String
replace1 needle repl = go
  where
    go [] = []
    go s@(c:cs) = case stripPrefix needle s of
        Just rest -> repl ++ go rest
        Nothing   -> c : go cs

-- | Parse a template through the production path: YAML -> Aeson.Value ->
--   DTemplate (the same bridge the adventure parser uses).
parseYamlTemplate :: String -> Either String DTemplate
parseYamlTemplate yaml = case decode1 (BLC.pack yaml) of
    Left (pos, err) -> Left ("YAML line " ++ show (posLine pos) ++ ": " ++ err)
    Right v         -> parseTemplate v

-- | A valid minimal template: two archetypes, start + boss, savezone camp.
validTemplateYaml :: String
validTemplateYaml = unlines
    [ "template:"
    , "  name: \"Test-Dungeon\""
    , "layout:"
    , "  rooms: { min: 3, max: 6 }"
    , "  depth: 2"
    , "room_templates:"
    , "  - id: junction"
    , "    weight: 3"
    , "    depth_range: [1, 2]"
    , "    room:"
    , "      name: \"Kreuzung\""
    , "      desc: \"Ein gewoelbter Kreuzgang.\""
    , "  - id: camp"
    , "    weight: 1"
    , "    depth_range: [1, 1]"
    , "    savezone: true"
    , "    room:"
    , "      name: \"Lager\""
    , "      desc: \"Feuer.\""
    , "special:"
    , "  start: { template: camp }"
    , "  boss: { template: junction, depth: max }"
    ]

-- | Run validation and collect the codes of all resulting issues.
templateIssueCodes :: Either String DTemplate -> [String]
templateIssueCodes (Left _)  = ["<parse-error>"]
templateIssueCodes (Right t) = map ciCode (validateTemplate t)

-- | The happy path: valid template parses, validates cleanly and carries the
--   expected structure through.
testTemplateValidMinimal :: IO Bool
testTemplateValidMinimal = case parseYamlTemplate validTemplateYaml of
    Left err -> do
        putStrLn $ "  unexpected parse error: " ++ err
        pure False
    Right t -> do
        r1 <- expectEqual [] (validateTemplate t)
        r2 <- expectEqual "Test-Dungeon" (dtName t)
        r3 <- expectEqual 2 (dlDepth (dtLayout t))
        r4 <- expectTrue "seed absent" (dtSeed t == Nothing)
        pure (r1 && r2 && r3 && r4)

-- | Pairing rule: every special.*.template must name an existing
--   room_templates id — treasure and boss paths are both checked.
testTemplateUnknownSpecial :: IO Bool
testTemplateUnknownSpecial = do
    let yamlTreasure = validTemplateYaml ++ "  treasure: { template: vault }\n"
        yamlBoss = replace1 "boss: { template: junction, depth: max }"
                            "boss: { template: vault, depth: max }"
                            validTemplateYaml
    r1 <- expectTrue "UnknownRoomTemplate for special.treasure"
        ("UnknownRoomTemplate" `elem` templateIssueCodes (parseYamlTemplate yamlTreasure))
    r2 <- expectTrue "UnknownRoomTemplate for special.boss"
        ("UnknownRoomTemplate" `elem` templateIssueCodes (parseYamlTemplate yamlBoss))
    pure (r1 && r2)

testTemplateDepthRangeOutside :: IO Bool
testTemplateDepthRangeOutside =
    let yaml = replace1 "depth_range: [1, 2]" "depth_range: [1, 5]" validTemplateYaml
    in expectTrue "DepthRangeOutside reported"
        ("DepthRangeOutside" `elem` templateIssueCodes (parseYamlTemplate yaml))

testTemplateBossLockMissingKey :: IO Bool
testTemplateBossLockMissingKey =
    let yaml = validTemplateYaml ++ "  treasure: { template: junction, locked: true }\n"
    in expectTrue "BossLockMissingKey reported"
        ("BossLockMissingKey" `elem` templateIssueCodes (parseYamlTemplate yaml))

testTemplateRoomIdForbidden :: IO Bool
testTemplateRoomIdForbidden =
    let yaml = replace1 "      name: \"Kreuzung\""
                        "      id: hand_set\n      name: \"Kreuzung\""
                        validTemplateYaml
    in expectTrue "RoomTemplateIdForbidden reported"
        ("RoomTemplateIdForbidden" `elem` templateIssueCodes (parseYamlTemplate yaml))

testTemplateBossDepthSpec :: IO Bool
testTemplateBossDepthSpec =
    let yaml = replace1 "boss: { template: junction, depth: max }"
                        "boss: { template: junction, depth: 3 }"
                        validTemplateYaml
    in expectTrue "BossDepthSpec reported"
        ("BossDepthSpec" `elem` templateIssueCodes (parseYamlTemplate yaml))

testTemplateLayoutTooSmall :: IO Bool
testTemplateLayoutTooSmall =
    let yaml = replace1 "rooms: { min: 3, max: 6 }" "rooms: { min: 2, max: 6 }"
                        validTemplateYaml
    in expectTrue "LayoutTooSmall reported"
        ("LayoutTooSmall" `elem` templateIssueCodes (parseYamlTemplate yaml))

testTemplateOnewayDirection :: IO Bool
testTemplateOnewayDirection = do
    let yamlBad = validTemplateYaml
            ++ "oneway_hints:\n"
            ++ "  - { from: junction, to: any, dir: sideways }\n"
        yamlGood = validTemplateYaml
            ++ "oneway_hints:\n"
            ++ "  - { from: junction, to: any, dir: down }\n"
    r1 <- expectTrue "UnknownDirection for 'sideways'"
        ("UnknownDirection" `elem` templateIssueCodes (parseYamlTemplate yamlBad))
    r2 <- expectEqual [] (templateIssueCodes (parseYamlTemplate yamlGood))
    pure (r1 && r2)

testTemplateMultipleBossNpcs :: IO Bool
testTemplateMultipleBossNpcs =
    let yaml = validTemplateYaml ++ unlines
            [ "npc_pool:"
            , "  - { npc: { id: ogre, name: \"Ogre\", max_health: 20 }, boss: true }"
            , "  - { npc: { id: drake, name: \"Drake\", max_health: 30 }, boss: true }"
            ]
    in expectTrue "MultipleBossNpcs reported"
        ("MultipleBossNpcs" `elem` templateIssueCodes (parseYamlTemplate yaml))

testTemplateInvalidCount :: IO Bool
testTemplateInvalidCount =
    let yaml = validTemplateYaml ++ unlines
            [ "item_pool:"
            , "  - { item: { id: potion, name: \"Heiltrank\" }, count: { min: 3, max: 2 } }"
            ]
    in expectTrue "InvalidCount reported"
        ("InvalidCount" `elem` templateIssueCodes (parseYamlTemplate yaml))

testTemplateSeedParse :: IO Bool
testTemplateSeedParse = do
    let withSeed s = validTemplateYaml ++ "seed: " ++ s ++ "\n"
    r1 <- case parseYamlTemplate (withSeed "42") of
        Left err -> expectTrue ("numeric seed parse failed: " ++ err) False
        Right t  -> expectEqual (Just 42) (dtSeed t)
    r2 <- case parseYamlTemplate (withSeed "\"42\"") of
        Left err -> expectTrue ("string seed parse failed: " ++ err) False
        Right t  -> expectEqual (Just 42) (dtSeed t)
    pure (r1 && r2)

testTemplateInvalidCombat :: IO Bool
testTemplateInvalidCombat =
    -- combat must be an ACombat object; a scalar is a parse error (which the
    -- CLI in 4d turns into Exit 1 — same outcome as a TemplateIssue).
    let yaml = validTemplateYaml ++ "combat: \"nope\"\n"
    in case parseYamlTemplate yaml of
        Left _  -> expectTrue "invalid combat fragment is a parse error" True
        Right _ -> expectTrue "expected parse failure for combat: \"nope\"" False

testTemplateLevelsParse :: IO Bool
testTemplateLevelsParse = do
    let yaml = validTemplateYaml ++ unlines
            [ "levels:"
            , "  - rooms: { min: 8, max: 14 }"
            , "    branching: 0.5"
            , "    return_stairs: true"
            , "    depth: 3"
            , "  - rooms: 4"
            , "    branching: 0.0"
            ]
    case parseYamlTemplate yaml of
        Left err -> do
            putStrLn $ "  parse error: " ++ err
            pure False
        Right t -> do
            r1 <- expectEqual 2 (length (dtLevels t))
            let lv1 = head (dtLevels t)
                lv2 = dtLevels t !! 1
            r2 <- expectEqual (Just (DRange 8 14)) (dlvRooms lv1)
            r3 <- expectEqual (Just 0.5) (dlvBranching lv1)
            r4 <- expectEqual True (dlvReturnStairs lv1)
            r5 <- expectEqual (Just 3) (dlvDepth lv1)
            r6 <- expectEqual (Just (DRange 4 4)) (dlvRooms lv2)
            r7 <- expectEqual False (dlvReturnStairs lv2)
            pure (r1 && r2 && r3 && r4 && r5 && r6 && r7)

testTemplateLevelsExceedDepth :: IO Bool
testTemplateLevelsExceedDepth = do
    -- validTemplateYaml has layout.depth = 2; adding 3 levels should trigger LevelsExceedDepth
    let yaml = validTemplateYaml ++ unlines
            [ "levels:"
            , "  - { rooms: 2 }"
            , "  - { rooms: 2 }"
            , "  - { rooms: 2 }"
            ]
    expectTrue "LevelsExceedDepth reported"
        ("LevelsExceedDepth" `elem` templateIssueCodes (parseYamlTemplate yaml))

testTemplateLevelsInvalidValues :: IO Bool
testTemplateLevelsInvalidValues = do
    let yamlRooms = validTemplateYaml ++ unlines
            [ "levels:"
            , "  - { rooms: { min: 5, max: 2 } }"
            ]
        yamlRoomsZero = validTemplateYaml ++ unlines
            [ "levels:"
            , "  - { rooms: 0 }"
            ]
        yamlBranch = validTemplateYaml ++ unlines
            [ "levels:"
            , "  - { branching: 1.5 }"
            ]
        yamlDepth = validTemplateYaml ++ unlines
            [ "levels:"
            , "  - { depth: 0 }"
            ]
    r1 <- expectTrue "InvalidCount for rooms bounds"
        ("InvalidCount" `elem` templateIssueCodes (parseYamlTemplate yamlRooms))
    r2 <- expectTrue "InvalidCount for rooms < 1"
        ("InvalidCount" `elem` templateIssueCodes (parseYamlTemplate yamlRoomsZero))
    r3 <- expectTrue "LayoutBranching for branching > 1"
        ("LayoutBranching" `elem` templateIssueCodes (parseYamlTemplate yamlBranch))
    r4 <- expectTrue "LayoutDepth for depth < 1"
        ("LayoutDepth" `elem` templateIssueCodes (parseYamlTemplate yamlDepth))
    pure (r1 && r2 && r3 && r4)

-- ---------------------------------------------------------------------------
-- Rogue Phase 4c: pure generation core
-- ---------------------------------------------------------------------------

-- | A directly built template (no YAML detour) for the generation tests.
genTemplate :: DTemplate
genTemplate = DTemplate
    { dtName          = "Gen"
    , dtDescription   = Nothing
    , dtLayout        = DLayout 6 12 4 0.5 2
    , dtLevels        = []
    , dtRoomTemplates =
        [ DRoomTemplate "junction" 3 (DRange 1 4) False (minRoom "x") False
        , DRoomTemplate "camp" 1 (DRange 1 2) True (minRoom "y") False ]
    , dtSpecial = DSpecial (Just (DStartSpec (Just "camp")))
                           (Just (DBossSpec "junction" (Just (Aeson.String (T.pack "max")))))
                           Nothing
    , dtOnewayHints = []
    , dtItemPool = []
    , dtNpcPool = []
    , dtCombat = Nothing
    , dtPlayer = Nothing
    , dtVariables = []
    , dtSeed = Nothing
    }

-- | Directed BFS over the plan's edges (locks ignored, one-way followed).
genReachable :: DungeonPlan -> Set.Set Cell
genReachable plan = go [dpStartCell plan] (Set.singleton (dpStartCell plan))
  where
    succs = Map.fromListWith (++) (concatMap f (dpEdges plan))
    f e | deOneway e = [(deFrom e, [deTo e])]
        | otherwise  = [(deFrom e, [deTo e]), (deTo e, [deFrom e])]
    go [] acc = acc
    go (c:cs) acc =
        let next = [ n | n <- Map.findWithDefault [] c succs, not (Set.member n acc) ]
        in go (cs ++ next) (foldr Set.insert acc next)

testGenDeterminism :: IO Bool
testGenDeterminism =
    expectEqual (generateDungeonLayout genTemplate 42)
                (generateDungeonLayout genTemplate 42)

testGenNoSpace :: IO Bool
testGenNoSpace =
    -- branching 0 + depth 3 => exactly 3 cells; min 5 is unsatisfiable
    let t = genTemplate { dtLayout = DLayout 5 5 3 0.0 0 }
    in expectEqual (Left GENoSpace) (generateDungeonLayout t 42)

testGenReachability :: IO Bool
testGenReachability = case generateDungeonLayout genTemplate 42 of
    Left err -> expectTrue ("unexpected: " ++ show err) False
    Right plan ->
        let reach = genReachable plan
        in expectTrue "all cells (incl. boss) reachable from start"
            (Map.keys (dpGrid plan) `allElem` reach)
  where
    allElem xs s = all (`Set.member` s) xs

testGenEarlyAbort :: IO Bool
testGenEarlyAbort =
    let t = genTemplate { dtLayout = DLayout 3 4 6 0.5 0 }
    in case generateDungeonLayout t 42 of
        Left err -> expectTrue ("unexpected: " ++ show err) False
        Right plan -> do
            r1 <- expectTrue "early abort detected"
                             (dpEarlyAbort plan && dpMaxDepth plan < 6)
            r2 <- expectTrue "GeneratorEarlyAbort warning attached"
                ("GeneratorEarlyAbort" `elem` map ciCode (dpWarnings plan))
            r3 <- expectTrue "boss sits at the reached maximum"
                (prDepth (dpGrid plan Map.! dpBossCell plan) == dpMaxDepth plan)
            pure (r1 && r2 && r3)

testGenOneway :: IO Bool
testGenOneway =
    let t = genTemplate
            { dtOnewayHints = [ DOnewayHint "junction" "any" "down" True ] }
    in case generateDungeonLayout t 42 of
        Left err -> expectTrue ("unexpected: " ++ show err) False
        Right plan -> do
            let oneways = filter deOneway (dpEdges plan)
            r1 <- expectTrue "exactly one one-way edge" (length oneways == 1)
            r2 <- expectTrue "forward direction renamed to 'down'"
                (all (\e -> deDir e == "down") oneways)
            r3 <- expectTrue "boss still reachable (directed)"
                (Set.member (dpBossCell plan) (genReachable plan))
            pure (r1 && r2 && r3)

testGenStartCell :: IO Bool
testGenStartCell = case generateDungeonLayout genTemplate 42 of
    Left err -> expectTrue ("unexpected: " ++ show err) False
    Right plan -> do
        let sr = dpGrid plan Map.! dpStartCell plan
        r1 <- expectEqual (0, 0, 1) (dpStartCell plan)
        r2 <- expectEqual 1 (prDepth sr)
        r3 <- expectEqual "camp" (prArch sr)  -- special.start.template wins
        pure (r1 && r2 && r3)

testGenBossDepth :: IO Bool
testGenBossDepth =
    -- with enough headroom the boss sits at layout.depth; otherwise the
    -- early-abort maximum — both cases: boss depth == dpMaxDepth
    let checks t seed = case generateDungeonLayout t seed of
            Left _   -> False
            Right p  -> prDepth (dpGrid p Map.! dpBossCell p) == dpMaxDepth p
                     && (dpMaxDepth p == dlDepth (dtLayout t) || dpEarlyAbort p)
    in expectTrue "boss depth == max depth (both templates)"
        (checks genTemplate 42 && checks genTemplate 7
         && checks (genTemplate { dtLayout = DLayout 3 20 5 0.8 1 }) 99)

testGenCycles :: IO Bool
testGenCycles = case generateDungeonLayout genTemplate 42 of
    Left err -> expectTrue ("unexpected: " ++ show err) False
    Right plan ->
        expectTrue "edge count exceeds a tree (cycles exist)"
            (length (dpEdges plan) > Map.size (dpGrid plan) - 1)

testGenEdgeUniqueness :: IO Bool
testGenEdgeUniqueness = case generateDungeonLayout genTemplate 42 of
    Left err -> expectTrue ("unexpected: " ++ show err) False
    Right plan ->
        let undirected = sort [ sort [deFrom e, deTo e] | e <- dpEdges plan ]
        in expectTrue "no duplicate undirected pairs"
            (length undirected == length (nub' undirected))
  where
    nub' = Set.toList . Set.fromList

testGenSeedDivergence :: IO Bool
testGenSeedDivergence =
    expectTrue "seed 42 and seed 7 differ (smoke)"
        (generateDungeonLayout genTemplate 42 /= generateDungeonLayout genTemplate 7)

-- ---------------------------------------------------------------------------
-- Rogue Phase 4d: emission, locks, pools
-- ---------------------------------------------------------------------------

-- | The core pipeline guarantee: every generated adventure must pass
--   compileAdventure without issues and both engine validators cleanly.
testGenAdventureClean :: IO Bool
testGenAdventureClean = do
    let go t seed = case generateDungeon t seed of
            Left err -> expectTrue ("unexpected: " ++ show err) False
            Right (adv, _) -> case compileAdventure adv of
                Left errs -> expectTrue ("compile errors: " ++ issuesText errs) False
                Right cr ->
                    let werrs = validateWorld (crWorld cr)
                        serrs = validateGameState (crWorld cr) (crSave cr)
                    in expectTrue ("validation clean ("
                                   ++ show (length (werrs ++ serrs)) ++ " issues: "
                                   ++ show (take 2 (werrs ++ serrs)) ++ ")")
                            (null (werrs ++ serrs))
    r1 <- go genTemplate 42
    r2 <- go genTemplate 7
    r3 <- go (genTemplate { dtLayout = DLayout 8 14 5 0.6 2 }) 123
    r4 <- go (genTemplate { dtLayout = DLayout 3 4 6 0.5 0 }) 42  -- early abort
    pure (r1 && r2 && r3 && r4)

-- | The combat passthrough (combination lever with plan-kampfbildschirm.md):
--   the authored combat: block reaches advCombat unchanged.
testGenCombatPassthrough :: IO Bool
testGenCombatPassthrough = case parseYamlTemplate (validTemplateYaml ++ unlines
        [ "combat:"
        , "  profile: classic"
        , "  screen:"
        , "    art: \"fight!\""
        , "    bar_width: 20"
        ]) of
    Left err -> expectTrue ("parse failed: " ++ err) False
    Right t -> case generateDungeon t 42 of
        Left err -> expectTrue ("unexpected: " ++ show err) False
        Right (adv, _) -> do
            r1 <- expectTrue "advCombat present" (advCombat adv /= Nothing)
            r2 <- expectTrue "classic profile with screen"
                (case advCombat adv of
                    Just c -> acProfile c == "classic" && acScreen c /= Nothing
                    Nothing -> False)
            pure (r1 && r2)

-- | Phase-1 synergy: every savezone-archetype instance becomes a save zone
--   and the generator implies ironman (otherwise save_zones would be dead).
testGenSavezones :: IO Bool
testGenSavezones = case generateDungeon genTemplate 42 of
    Left err -> expectTrue ("unexpected: " ++ show err) False
    Right (adv, _) -> case advGame adv of
        Nothing -> expectTrue "expected a game block" False
        Just gp -> do
            r1 <- expectTrue "ironman implied" (agpIronman gp == Just True)
            r2 <- expectTrue "start room is a savezone"
                (advStartRoom adv `elem` agpSaveZones gp)
            pure (r1 && r2)

-- | The locked treasure: at least one approach exit is locked, the save
--   seeds the lock entity as "locked", and the key carries the standard
--   set_state rule (generated into its on_take by the generator).
testGenLockKey :: IO Bool
testGenLockKey =
    let lockT = genTemplate
            { dtSpecial = (dtSpecial genTemplate)
                { dspTreasure = Just (DTreasureSpec "junction" True) }
            , dtItemPool = [ DItemPoolEntry (minItemKey "key_iron") (DRange 1 1) (Just "key_iron") ]
            }
    in case generateDungeon lockT 42 of
        Left err -> expectTrue ("unexpected: " ++ show err) False
        Right (adv, warns) -> case compileAdventure adv of
            Left errs -> expectTrue ("compile: " ++ issuesText errs) False
            Right cr -> do
                let w = crWorld cr
                    s = crSave cr
                    lockedExits = [ (rid, dir)
                                  | (rid, room) <- Map.toList (E.rooms w)
                                  , (dir, ex) <- Map.toList (E.roomConnections room)
                                  , E.Locked _ _ <- [ex] ]
                r1 <- expectTrue "at least one locked approach exit" (not (null lockedExits))
                r2 <- expectTrue "lock entity seeded as locked in save"
                    (elem "lock_junction" (Map.keys (E.entityStates s)))
                r3 <- expectTrue "key carries set_state (unlock) rule"
                    (case Map.lookup "key_iron" (E.itemDefs w) of
                        Just it -> Map.member (E.VTake, "intact") (E.itemVerbMap it)
                                   && not (null [ () | E.SetValue _ _ <- [e | (_, e) <- Map.toList (E.itemVerbMap it)] ])
                        Nothing -> False)
                pure (r1 && r2 && r3)

-- | Pool placement: item and NPC instances land as item/npc defs and none
--   of the skip warnings fire (the template has enough non-special rooms).
testGenPools :: IO Bool
testGenPools = case generateDungeon poolT 42 of
    Left err -> expectTrue ("unexpected: " ++ show err) False
    Right (adv, warns) -> do
        r1 <- expectTrue "items placed" (not (null (advItems adv)))
        r2 <- expectTrue "npcs placed" (not (null (advNPCs adv)))
        r3 <- expectTrue "no GeneratorPopulationSkipped"
            (not ("GeneratorPopulationSkipped" `elem` map ciCode warns))
        pure (r1 && r2 && r3)
  where
    poolT = genTemplate
        { dtItemPool = [ DItemPoolEntry (minItemKey "potion") (DRange 2 3) Nothing ]
        , dtNpcPool = [ DNpcPoolEntry (minNpcKey "skeleton") (DRange 1 2) (Just (DRange 2 4)) False ]
        }

-- ---------------------------------------------------------------------------
-- Rogue Phase 4b: multi-level layout & stairs tests
-- ---------------------------------------------------------------------------

-- | Multi-level template for Phase 4b generation tests.
multiLevelTemplate :: DTemplate
multiLevelTemplate = genTemplate
    { dtLayout = DLayout 12 20 3 0.4 1
    , dtLevels =
        [ DLevel (Just (DRange 4 6)) (Just 6) (Just 0.4) True
        , DLevel (Just (DRange 4 6)) (Just 6) (Just 0.3) False
        , DLevel (Just (DRange 4 6)) (Just 6) (Just 0.0) False
        ]
    , dtRoomTemplates =
        [ DRoomTemplate "junction" 3 (DRange 1 3) False (minRoom "x") False
        , DRoomTemplate "camp" 1 (DRange 1 1) True (minRoom "y") False
        , DRoomTemplate "mid" 2 (DRange 2 2) False (minRoom "z") False
        ]
    }

testGenMultiLevelRoomsOnAllLevels :: IO Bool
testGenMultiLevelRoomsOnAllLevels = case generateDungeonLayout multiLevelTemplate 42 of
    Left err -> expectTrue ("unexpected layout error: " ++ show err) False
    Right plan -> do
        let grid = dpGrid plan
            levels = Set.fromList [ z | (_, _, z) <- Map.keys grid ]
            roomsL1 = length [ () | (_, _, z) <- Map.keys grid, z == 1 ]
            roomsL2 = length [ () | (_, _, z) <- Map.keys grid, z == 2 ]
            roomsL3 = length [ () | (_, _, z) <- Map.keys grid, z == 3 ]
        r1 <- expectEqual (Set.fromList [1, 2, 3]) levels
        r2 <- expectTrue "level 1 rooms in range [4, 6]" (roomsL1 >= 4 && roomsL1 <= 6)
        r3 <- expectTrue "level 2 rooms in range [4, 6]" (roomsL2 >= 4 && roomsL2 <= 6)
        r4 <- expectTrue "level 3 rooms in range [4, 6]" (roomsL3 >= 4 && roomsL3 <= 6)
        pure (r1 && r2 && r3 && r4)

testGenMultiLevelStairEdges :: IO Bool
testGenMultiLevelStairEdges = case generateDungeonLayout multiLevelTemplate 42 of
    Left err -> expectTrue ("unexpected layout error: " ++ show err) False
    Right plan -> do
        let edges = dpEdges plan
            downEdges = [ e | e <- edges, deDir e == "down" ]
            grid = dpGrid plan
            cellsL1 = [ c | c@(_, _, z) <- Map.keys grid, z == 1 ]
            deepest1 = head (sortOn (\c -> (negate (prDepth (grid Map.! c)), c)) cellsL1)
            cellsL2 = [ c | c@(_, _, z) <- Map.keys grid, z == 2 ]
            deepest2 = head (sortOn (\c -> (negate (prDepth (grid Map.! c)), c)) cellsL2)
        r1 <- expectEqual 2 (length downEdges)
        r2 <- expectTrue "down edge 1 connects deepest L1 to (0,0,2)"
            (any (\e -> deFrom e == deepest1 && deTo e == (0, 0, 2) && deOneway e) downEdges)
        r3 <- expectTrue "down edge 2 connects deepest L2 to (0,0,3)"
            (any (\e -> deFrom e == deepest2 && deTo e == (0, 0, 3) && deOneway e) downEdges)
        pure (r1 && r2 && r3)

testGenMultiLevelReturnStairs :: IO Bool
testGenMultiLevelReturnStairs = case generateDungeonLayout multiLevelTemplate 42 of
    Left err -> expectTrue ("unexpected layout error: " ++ show err) False
    Right plan -> do
        let edges = dpEdges plan
            upEdges = [ e | e <- edges, deDir e == "up" ]
            grid = dpGrid plan
            cellsL1 = [ c | c@(_, _, z) <- Map.keys grid, z == 1 ]
            deepest1 = head (sortOn (\c -> (negate (prDepth (grid Map.! c)), c)) cellsL1)
        r1 <- expectEqual 1 (length upEdges)
        r2 <- expectTrue "up edge from (0,0,2) to deepest L1"
            (any (\e -> deFrom e == (0, 0, 2) && deTo e == deepest1 && deOneway e) upEdges)
        r3 <- expectTrue "no return stairs from level 3"
            (null [ e | e <- upEdges, deFrom e == (0, 0, 3) ])
        pure (r1 && r2 && r3)

testGenMultiLevelBossOnLastLevel :: IO Bool
testGenMultiLevelBossOnLastLevel = case generateDungeonLayout multiLevelTemplate 42 of
    Left err -> expectTrue ("unexpected layout error: " ++ show err) False
    Right plan -> do
        let (_, _, bossZ) = dpBossCell plan
            grid = dpGrid plan
            maxZ = maximum [ z | (_, _, z) <- Map.keys grid ]
            cellsOnMaxZ = [ c | c@(_, _, z) <- Map.keys grid, z == maxZ ]
            maxDepthOnMaxZ = maximum [ prDepth (grid Map.! c) | c <- cellsOnMaxZ ]
        r1 <- expectEqual maxZ bossZ
        r2 <- expectEqual maxDepthOnMaxZ (prDepth (grid Map.! dpBossCell plan))
        pure (r1 && r2)

testGenMultiLevelDepthRangeFiltering :: IO Bool
testGenMultiLevelDepthRangeFiltering = case generateDungeonLayout multiLevelTemplate 42 of
    Left err -> expectTrue ("unexpected layout error: " ++ show err) False
    Right plan -> do
        let grid = dpGrid plan
            midCells = [ c | (c, pr) <- Map.toList grid, prArch pr == "mid" ]
        r1 <- expectTrue "mid room template was placed" (not (null midCells))
        r2 <- expectTrue "all mid rooms are on level 2" (all (\(_, _, z) -> z == 2) midCells)
        pure (r1 && r2)

testGenMultiLevelAutoDistribution :: IO Bool
testGenMultiLevelAutoDistribution = do
    let partialT = genTemplate
            { dtLayout = DLayout 12 18 3 0.4 1
            , dtLevels = [ DLevel (Just (DRange 4 6)) Nothing Nothing False ]
            }
        resolved = resolveLevels partialT
    r1 <- expectEqual 3 (length resolved)
    let rl1 = head resolved
        rl2 = resolved !! 1
        rl3 = resolved !! 2
    r2 <- expectEqual (4, 6) (rlRoomsMin rl1, rlRoomsMax rl1)
    r3 <- expectEqual (4, 6) (rlRoomsMin rl2, rlRoomsMax rl2)
    r4 <- expectEqual (4, 6) (rlRoomsMin rl3, rlRoomsMax rl3)
    case generateDungeonLayout partialT 42 of
        Left err -> expectTrue ("layout error: " ++ show err) False
        Right plan -> do
            let levels = Set.fromList [ z | (_, _, z) <- Map.keys (dpGrid plan) ]
            r5 <- expectEqual (Set.fromList [1, 2, 3]) levels
            pure (r1 && r2 && r3 && r4 && r5)

testGenMultiLevelReachability :: IO Bool
testGenMultiLevelReachability = case generateDungeonLayout multiLevelTemplate 42 of
    Left err -> expectTrue ("unexpected layout error: " ++ show err) False
    Right plan -> do
        let reachable = genReachable plan
            grid = dpGrid plan
        expectEqual (Map.keysSet grid) reachable

testGenMultiLevelAdventureValid :: IO Bool
testGenMultiLevelAdventureValid = case generateDungeon multiLevelTemplate 42 of
    Left err -> expectTrue ("generation error: " ++ show err) False
    Right (adv, _warns) -> case compileAdventure adv of
        Left errs -> expectTrue ("compile error: " ++ issuesText errs) False
        Right cr -> do
            let werrs = validateWorld (crWorld cr)
                serrs = validateGameState (crWorld cr) (crSave cr)
            expectTrue ("validation clean (" ++ show (length (werrs ++ serrs)) ++ " issues)")
                       (null (werrs ++ serrs))

testGenMultiLevelRoomFloors :: IO Bool
testGenMultiLevelRoomFloors = case generateDungeon multiLevelTemplate 42 of
    Left err -> expectTrue ("generation error: " ++ show err) False
    Right (adv, _warns) -> case compileAdventure adv of
        Left errs -> expectTrue ("compile error: " ++ issuesText errs) False
        Right cr -> do
            let w = crWorld cr
                advRoomsList = advRooms adv
                floors = [ arFloor r | r <- advRoomsList ]
                compiledFloors = [ E.roomFloor r | (_, r) <- Map.toList (E.rooms w) ]
            r1 <- expectTrue "every room in multi-level has Just floor"
                    (all (\fl -> case fl of Just f -> f >= 1 && f <= 3; Nothing -> False) floors)
            r2 <- expectTrue "compiled rooms keep roomFloor"
                    (all (\fl -> case fl of Just f -> f >= 1 && f <= 3; Nothing -> False) compiledFloors)
            r3 <- expectTrue "rooms exist on each floor 1, 2, 3"
                    (Set.fromList [ f | Just f <- compiledFloors ] == Set.fromList [1, 2, 3])
            pure (r1 && r2 && r3)

testRoomFloorJsonDefaultInvariant :: IO Bool
testRoomFloorJsonDefaultInvariant = do
    let rNoFloor = E.Room "r1" "Room 1" (E.plainText "desc") Map.empty Set.empty Nothing
                    Nothing Nothing Nothing Nothing E.emptyAscii Nothing Nothing
        rWithFloor = rNoFloor { E.roomFloor = Just 2 }
        sNoFloor = BLC.unpack (Aeson.encode rNoFloor)
        sWithFloor = BLC.unpack (Aeson.encode rWithFloor)
    r1 <- expectTrue "roomFloor Nothing is omitted from JSON (Default-Invariante)"
            (not ("\"roomFloor\"" `isInfixOf` sNoFloor) && not ("\"floor\"" `isInfixOf` sNoFloor))
    r2 <- expectTrue "roomFloor Just 2 is serialized into JSON as roomFloor"
            ("\"roomFloor\":2" `isInfixOf` sWithFloor)
    r3 <- case Aeson.decode (Aeson.encode rNoFloor) :: Maybe E.Room of
        Just decoded -> expectEqual Nothing (E.roomFloor decoded)
        Nothing      -> expectTrue "decode no floor failed" False
    r4 <- case Aeson.decode (Aeson.encode rWithFloor) :: Maybe E.Room of
        Just decoded -> expectEqual (Just 2) (E.roomFloor decoded)
        Nothing      -> expectTrue "decode with floor failed" False
    -- Also verify fallback parsing from "floor": 2
    let sManualFloor = BLC.pack "{\"roomId\":\"r1\",\"roomName\":\"Room 1\",\"roomDescription\":{\"default\":\"desc\"},\"roomConnections\":[],\"roomTags\":[],\"floor\":2}"
    r5 <- case Aeson.decode sManualFloor :: Maybe E.Room of
        Just decoded -> expectEqual (Just 2) (E.roomFloor decoded)
        Nothing      -> expectTrue "decode fallback floor:2 failed" False
    pure (r1 && r2 && r3 && r4 && r5)

testMultiLevelFixtureCompiles :: IO Bool
testMultiLevelFixtureCompiles = do
    mbPath <- findExample "multilevel_dungeon_template.yaml"
    case mbPath of
        Nothing -> do
            putStrLn "  multilevel_dungeon_template.yaml not found"
            pure False
        Just path -> do
            bytes <- BLC.readFile path
            case decode1 bytes of
                Left (_, err) -> do
                    putStrLn $ "  failed to decode YAML: " ++ err
                    pure False
                Right val -> case parseTemplate val of
                    Left err -> do
                        putStrLn $ "  failed to parse template: " ++ err
                        pure False
                    Right tmpl -> do
                        let tIssues = validateTemplate tmpl
                            tErrors = filter ((== SError) . ciSeverity) tIssues
                        r1 <- expectTrue ("template validation clean (" ++ show (length tErrors) ++ " errors)")
                                         (null tErrors)
                        r2 <- expectEqual 3 (length (dtLevels tmpl))
                        case generateDungeon tmpl 42 of
                            Left err -> do
                                putStrLn $ "  generation failed: " ++ show err
                                pure False
                            Right (adv, _warns) -> case compileAdventure adv of
                                Left errs -> do
                                    putStrLn $ "  compilation failed: " ++ issuesText errs
                                    pure False
                                Right cr -> do
                                    let gw = crWorld cr
                                        werrs = validateWorld gw
                                        serrs = validateGameState gw (crSave cr)
                                        roomList = Map.elems (E.rooms gw)
                                        floors = [ fl | Just fl <- map E.roomFloor roomList ]
                                    r3 <- expectTrue ("validation clean (" ++ show (length (werrs ++ serrs)) ++ " issues)")
                                                     (null (werrs ++ serrs))
                                    r4 <- expectTrue "rooms exist on each floor [1, 2, 3]"
                                                     (Set.fromList floors == Set.fromList [1, 2, 3])
                                    let allExits = [ (rid, dir, ex)
                                                   | (rid, room) <- Map.toList (E.rooms gw)
                                                   , (dir, ex) <- Map.toList (E.roomConnections room) ]
                                        upExits = [ (rid, ex) | (rid, dir, ex) <- allExits, dir == E.Up ]
                                        downExits = [ (rid, ex) | (rid, dir, ex) <- allExits, dir == E.Down ]
                                    r5 <- expectTrue "down stairs exist between levels" (length downExits >= 2)
                                    r6 <- expectTrue "return stairs (up) exist" (length upExits >= 2)
                                    pure (r1 && r2 && r3 && r4 && r5 && r6)

testRunPreparationDeterministicSeedAndDifferentRuns :: IO Bool
testRunPreparationDeterministicSeedAndDifferentRuns = do
    mbPath <- findExample "dungeon_template.yaml"
    case mbPath of
        Nothing -> expectTrue "dungeon_template.yaml found" False
        Just tmplPath -> do
            tmpBase <- getTemporaryDirectory
            let savesD = tmpBase </> "ta-run-test-1"
                slug = "katakomben_von_vhal"
                metaFile = savesD </> (slug ++ "_meta.json")
            createDirectoryIfMissing True savesD
            ex <- doesFileExist metaFile
            when ex (removeFile metaFile)
            -- 1. First run with empty/missing meta file -> produces run_1
            let cfg1 = (defaultRunConfig tmplPath)
                    { rcSavesDir = Just savesD
                    , rcNoLaunch = True
                    }
            res1 <- prepareRun cfg1
            case res1 of
                Left err -> expectTrue ("run 1 failed: " ++ err) False
                Right r1 -> do
                    let expectedSeed1 = SaveLoad.deriveRunSeedFromSlug slug 1
                    b1 <- expectEqual 1 (rrRunIndex r1)
                    b2 <- expectEqual expectedSeed1 (rrSeed r1)
                    b3 <- doesFileExist (rrWorldPath r1)
                    b4 <- doesFileExist (rrSavePath r1)
                    -- Check save.json has meta.runs = 0 (bumped to 1 when engine starts)
                    b5 <- expectEqual (Just (E.VVInt 0)) (Map.lookup "meta.runs" (E.variables (rrSave r1)))
                    -- 2. Simulate run 1 finished: write meta file with meta.runs = 1
                    SaveLoad.saveMeta (rrWorld r1) (Map.singleton "meta.runs" (E.VVInt 1))
                    -- 3. Second run with meta.runs = 1 on disk -> produces run_2
                    let cfg2 = cfg1
                    res2 <- prepareRun cfg2
                    case res2 of
                        Left err -> expectTrue ("run 2 failed: " ++ err) False
                        Right r2 -> do
                            let expectedSeed2 = SaveLoad.deriveRunSeedFromSlug slug 2
                            b6 <- expectEqual 2 (rrRunIndex r2)
                            b7 <- expectEqual expectedSeed2 (rrSeed r2)
                            b8 <- doesFileExist (rrWorldPath r2)
                            b9 <- doesFileExist (rrSavePath r2)
                            -- Different seeds => different world checksums!
                            let cs1 = SaveLoad.computeWorldChecksum (rrWorld r1)
                                cs2 = SaveLoad.computeWorldChecksum (rrWorld r2)
                            b10 <- expectTrue "run 1 and run 2 have different worlds" (cs1 /= cs2)
                            -- Cleanup
                            _ <- try (removeDirectoryRecursive savesD) :: IO (Either SomeException ())
                            pure (b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10)

testRunPreparationSeedOverride :: IO Bool
testRunPreparationSeedOverride = do
    mbPath <- findExample "dungeon_template.yaml"
    case mbPath of
        Nothing -> expectTrue "dungeon_template.yaml found" False
        Just tmplPath -> do
            tmpBase <- getTemporaryDirectory
            let savesD = tmpBase </> "ta-run-test-seed"
            createDirectoryIfMissing True savesD
            let cfg = (defaultRunConfig tmplPath)
                    { rcSavesDir = Just savesD
                    , rcSeedOverride = Just 777777
                    , rcNoLaunch = True
                    }
            res <- prepareRun cfg
            case res of
                Left err -> expectTrue ("run failed: " ++ err) False
                Right r -> do
                    b1 <- expectEqual 777777 (rrSeed r)
                    _ <- try (removeDirectoryRecursive savesD) :: IO (Either SomeException ())
                    pure b1

testPruneOldRuns :: IO Bool
testPruneOldRuns = do
    tmpBase <- getTemporaryDirectory
    let testD = tmpBase </> "ta-prune-test"
    createDirectoryIfMissing True testD
    createDirectoryIfMissing True (testD </> "run_1")
    createDirectoryIfMissing True (testD </> "run_2")
    createDirectoryIfMissing True (testD </> "run_3")
    createDirectoryIfMissing True (testD </> "run_4")
    writeFile (testD </> "other.txt") "keep me"
    pruned <- pruneOldRuns testD 2
    b1 <- expectEqual 2 (length pruned)
    e1 <- doesDirectoryExist (testD </> "run_1")
    e2 <- doesDirectoryExist (testD </> "run_2")
    e3 <- doesDirectoryExist (testD </> "run_3")
    e4 <- doesDirectoryExist (testD </> "run_4")
    otherOk <- doesFileExist (testD </> "other.txt")
    _ <- try (removeDirectoryRecursive testD) :: IO (Either SomeException ())
    pure (b1 && not e1 && not e2 && e3 && e4 && otherOk)

testCheckpointBindingAcrossRuns :: IO Bool
testCheckpointBindingAcrossRuns = do
    mbPath <- findExample "dungeon_template.yaml"
    case mbPath of
        Nothing -> expectTrue "dungeon_template.yaml found" False
        Just tmplPath -> do
            tmpBase <- getTemporaryDirectory
            let savesD = tmpBase </> "ta-run-test-cp"
            createDirectoryIfMissing True savesD
            res1 <- prepareRun (defaultRunConfig tmplPath) { rcSavesDir = Just savesD, rcSeedOverride = Just 111, rcNoLaunch = True }
            res2 <- prepareRun (defaultRunConfig tmplPath) { rcSavesDir = Just savesD, rcSeedOverride = Just 222, rcNoLaunch = True }
            case (res1, res2) of
                (Right r1, Right r2) -> do
                    let w1 = rrWorld r1
                        w2 = rrWorld r2
                        cs1 = SaveLoad.computeWorldChecksum w1
                        cs2 = SaveLoad.computeWorldChecksum w2
                    b1 <- expectTrue "different seeds -> different world checksums" (cs1 /= cs2)
                    -- Checkpoint saved for r1 carries cs1
                    let cpFile = E.SaveFile 3 "2026-09-23T00:00:00" cs1 "checkpoint" (rrSave r1)
                    b2 <- expectEqual cs1 (E.worldChecksum cpFile)
                    b3 <- expectTrue "checkpoint of run 1 mismatches world of run 2" (E.worldChecksum cpFile /= cs2)
                    _ <- try (removeDirectoryRecursive savesD) :: IO (Either SomeException ())
                    pure (b1 && b2 && b3)
                _ -> expectTrue "runs preparation succeeded" False

-- helpers -------------------------------------------------------------------

-- | Minimal usable AItem for pool entries.
minItemKey :: String -> AItem
minItemKey iid = AItem
    { aiId = iid
    , aiName = iid
    , aiTexts = ACondText "" []
    , aiAscii = AAscii (ACondText "" []) [] 0 [] Nothing
    , aiKeywords = []
    , aiTags = []
    , aiLocation = "start"
    , aiState = "intact"
    , aiEquipSlot = Nothing
    , aiEquipEffects = []
    , aiHidden = False
    , aiDiscover = Nothing
    , aiProps = Map.empty
    , aiOnTake = Nothing
    , aiVerbMap = Map.empty
    , aiPortable = Just True
    , aiTakeFailure = Nothing
    , aiInContainer = Nothing
    }

-- | Minimal usable ANPC for pool entries.
minNpcKey :: String -> ANPC
minNpcKey nid = ANPC
    { anId = nid
    , anName = nid
    , anTexts = ACondText "" []
    , anAscii = AAscii (ACondText "" []) [] 0 [] Nothing
    , anKeywords = []
    , anLocation = "start"
    , anState = "idle"
    , anMaxHealth = Just 10
    , anAttack = 2
    , anDefense = 1
    , anDialogue = Map.empty
    , anVerbMap = Map.empty
    , anParty = Nothing
    }

main :: IO ()
main = do
    hSetEncoding stdout utf8
    results <- mapM (\(name, test) -> runTest name test) tests
    let failed = length (filter not results)
    putStrLn ""
    putStrLn $ show (length results - failed) ++ " passed, " ++ show failed ++ " failed"
    when (failed > 0) exitFailure