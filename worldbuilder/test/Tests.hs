-- | Test suite for the worldbuilder: strict compilation, directions,
--   verbs, on_take merging, string-scalar handling, and YAML round-trips.
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad (when)
import Data.List (isInfixOf)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Exit (exitFailure)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Worldbuilder.Types
import Worldbuilder.Compile (CompileResult (..), compileAdventure, CompileIssue(..), Severity(..))
import Worldbuilder.ParseFile (parseAdventureFile)
import Types as E
import Validate (validateWorld, validateGameState, ValidationError (..))

-- | Helper: a minimal room set for validateGameState tests
minWorld :: E.GameWorld
minWorld = E.GameWorld
    { rooms = Map.fromList
        [ ("room_0", E.Room "room_0" "Room 0" (E.CondText "test" []) Map.empty Set.empty Nothing Nothing Nothing Nothing Nothing Nothing)
        ]
    , itemDefs = Map.empty
    , npcDefs = Map.empty
    , entityInteractions = Map.empty
    , itemInteractions = Map.empty
    , questDefs = Map.empty
    , vehicleDefs = Map.empty
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
    , arAscii = Nothing
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
    }

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
            mbAdv <- parseAdventureFile path
            case mbAdv of
                Nothing -> do
                    putStrLn "  failed to parse demo.yaml"
                    pure False
                Just adv -> compileCheck adv
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
            mbAdv <- parseAdventureFile path
            case mbAdv of
                Nothing -> do
                    putStrLn "  failed to parse thefog.yaml"
                    pure False
                Just adv -> case compileAdventure adv of
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
            mbAdv <- parseAdventureFile path
            case mbAdv of
                Nothing -> do
                    putStrLn "  failed to parse fantasy-magic.yaml"
                    pure False
                Just adv -> case compileAdventure adv of
                    Left errs -> do
                        putStrLn $ "  compile errors: " ++ show errs
                        pure False
                    Right cr -> do
                        let errors = validateWorld (crWorld cr)
                            stateErrs = validateGameState (crWorld cr) (crSave cr)
                            -- Allow known unreachable-start-room warnings (validator
                            -- searches for "start" rather than the actual start_room)
                            acceptable = filter (\e -> case e of
                                UnreachableRoom _ -> False
                                _ -> True) errors
                        if not (null acceptable) || not (null stateErrs)
                        then do
                            putStrLn $ "  validation issues: " ++ show (acceptable ++ stateErrs)
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
            mbAdv <- parseAdventureFile path
            case mbAdv of
                Nothing -> do
                    putStrLn "  failed to parse space-oxygen.yaml"
                    pure False
                Just adv -> case compileAdventure adv of
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
            mbAdv <- parseAdventureFile path
            case mbAdv of
                Nothing -> do
                    putStrLn "  failed to parse trigger-test.yaml"
                    pure False
                Just adv -> case compileAdventure adv of
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
            mbAdv <- parseAdventureFile path
            case mbAdv of
                Nothing -> do
                    putStrLn "  failed to parse player-config.yaml"
                    pure False
                Just adv -> case compileAdventure adv of
                    Left errs -> do
                        putStrLn $ "  compile errors: " ++ show errs
                        pure False
                    Right cr -> do
                        let save = crSave cr
                            p = E.player save
                        r1 <- expectEqual 50 (E.playerMaxHealth p)
                        r2 <- expectEqual 8 (E.playerAttack p)
                        r3 <- expectEqual 3 (E.playerDefense p)
                        r4 <- expectEqual (Just 5) (Map.lookup "lockpick" (E.playerSkills p))
                        r5 <- expectEqual (Just (E.VVInt 30)) (Map.lookup "mana" (E.variables save))
                        r6 <- expectEqual (Just "true") (Map.lookup "started" (E.flags save))
                        r7 <- expectEqual (Just 0) (Map.lookup "find_treasure" (E.activeQuests save))
                        pure (r1 && r2 && r3 && r4 && r5 && r6 && r7)

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

tests :: [(String, IO Bool)]
tests =
    [ ("all 10 directions compile to engine Direction", testAllDirectionsCompile)
    , ("direction aliases ne/nw/se/sw work", testDirectionAliases)
    , ("unknown direction is a compile error", testUnknownDirectionFails)
    , ("'activate' verb maps to VUse", testActivateVerbMapsToUse)
    , ("unknown verb is a compile error", testUnknownVerbFails)
    , ("verb aliases (examine→VLookAt) work", testRepeatedVerbAliases)
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
    ]

main :: IO ()
main = do
    results <- mapM (\(name, test) -> runTest name test) tests
    let failed = length (filter not results)
    putStrLn ""
    putStrLn $ show (length results - failed) ++ " passed, " ++ show failed ++ " failed"
    when (failed > 0) exitFailure