-- | Test suite for the worldbuilder: strict compilation, directions,
--   verbs, on_take merging, string-scalar handling, and YAML round-trips.
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad (forM, when)
import Data.List (isInfixOf, isPrefixOf, nub, find)
import qualified Data.Aeson as Aeson
import Data.Maybe (isJust, listToMaybe)
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Exit (exitFailure)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getTemporaryDirectory, removeDirectoryRecursive, removeFile, getPermissions, Permissions (..))
import System.FilePath ((</>))
import System.IO (hSetEncoding, stdout, utf8, openTempFile, hClose)
import qualified System.Info as Info
import Control.Exception (try, SomeException)
import Worldbuilder.Types
import Worldbuilder.Locate (lineForPath)
import Worldbuilder.YamlDoc (parseYamlDoc, ydResolveIssuePath, ydScalarSpan,
                             setScalarAt, setScalarsAt, insertKeys, splitIssuePath,
                             ssText, YamlSeg (..))
import Worldbuilder.MapLayout (MapCell (..), roomLayout)
import Worldbuilder.QuestCheck (questDiagnostics, QuestDiagnostic (..))
import Worldbuilder.ProjectView (ProjectView (..), PVRoom (..), PVEdge (..),
                                 PVReachability (..), PVQuest (..),
                                 buildProjectView, encodeProjectView, renderMapGrid)
import Worldbuilder.Compile (CompileResult (..), compileAdventure, CompileIssue(..), Severity(..), compileAActionOutcome, allWorldEffects, allAOutcomes, checkUnknownYamlKeys)
import Worldbuilder.Test (checkMarkers, executeContentTest)
import Worldbuilder.Fuzz (FindingKind (..), FuzzFinding (..), FuzzVocab (..),
                          frozenWindow, fuzzRun, fuzzVocab, genInputs, runSeedFor)
import Worldbuilder.Export (collectAssetRefs, bundleFiles, exportBundle, launcherSh, launcherBat)
import Worldbuilder.ParseFile (parseAdventureFile)
import Worldbuilder.Rng
import Worldbuilder.Generate
import Worldbuilder.Run (RunConfig (..), RunResult (..), defaultRunConfig, prepareRun, pruneOldRuns)
import qualified SaveLoad
import Data.List (sort, sortOn, stripPrefix)
import Data.YAML.Aeson (decode1)
import Data.YAML (posLine, posColumn)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Types as E
import Game (emptyGameState, evalPredicate)
import Validate (validateWorld, validateWorldWithFlags, validateGameState, ValidationError (..))

-- | Helper: a minimal room set for validateGameState tests
minWorld :: E.GameWorld
minWorld = E.GameWorld
    { rooms = Map.fromList
        [ ("room_0", E.Room "room_0" "Room 0" (E.CondText "test" []) Map.empty Set.empty Nothing Nothing Nothing Nothing Nothing Nothing (E.AsciiArt (E.CondText "" []) [] 0 [] Nothing) Nothing Nothing Nothing)
        ]
    , itemDefs = Map.empty
    , npcDefs = Map.empty
    , entityInteractions = Map.empty
    , itemInteractions = Map.empty
    , npcInteractions = Map.empty
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
    , cardDefs = Map.empty
    , sandboxZones = Map.empty
    , factDefs = []
    , combineDefs = []
    , procDefs = Map.empty
    , chapterDefs = []
    , deviceDefs = Map.empty, containerDefs = Map.empty
    , progressionDef = Nothing
    , worldLanguage = Nothing
    , worldMessages = Map.empty
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
    , deckState = Nothing
    , dynamicRooms = Map.empty
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
    , arDarkMsg = Nothing
    , arOnEnter = Nothing
    , arOnLook = Nothing
    , arOnExit = Nothing
    , arSearch = Nothing
    , arAscii = AAscii (ACondText "" []) [] 0 [] Nothing
    , arIntro = Nothing
    , arFloor = Nothing
    , arMapPos = Nothing
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
    , advCards = []
    , advDeck = Nothing
    , advHandLimit = Nothing
    , advSandboxZones = []
    , advProcedures = []
    , advCombineVerb = Nothing
    , advJournal = Nothing
    , advChapters = []
    , advPursuit = [], advContainers = []
    , advInclude = []
    , advFacts = []
    , advCombines = []
    , advDevices = []
    , advProgression = Nothing
    , advTests = []
    , advRawValue = Nothing
    , advAssets = []
    , advLanguage = Nothing
    , advMessages = Map.empty
    }

-- ===== Rogue Phase 1: authored game policy =====

-- ---------------------------------------------------------------------------
-- Phase 2.5: procedures (D2)
-- ---------------------------------------------------------------------------

-- | `procedures:` compiles into procDefs; an empty proc map is omitted from
--   world.json — the byte-stability guarantee for every existing adventure.
testProceduresCompile :: IO Bool
testProceduresCompile = do
    let empty = minAdventure (minRoom "loc_0")
    r0 <- case compileAdventure empty of
            Left _ -> expectTrue "default compiles" False
            Right cr -> do
                a <- expectTrue "no procs by default" (Map.null (E.procDefs (crWorld cr)))
                b <- expectTrue "world.json omits empty procDefs"
                        (not ("procDefs" `isInfixOf` BLC.unpack (Aeson.encode (crWorld cr))))
                pure (a && b)
    let adv = empty
            { advProcedures = [AProcDef "belohnen" ["betrag"] [AOMessage "hi"]] }
    r1 <- case compileAdventure adv of
            Left errs -> expectTrue ("procs compile, got: " ++ issuesText errs) False
            Right cr -> case Map.lookup "belohnen" (E.procDefs (crWorld cr)) of
                Nothing -> expectTrue "procDefs contains belohnen" False
                Just pd -> do
                    a <- expectEqual ["betrag"] (E.procParams pd)
                    b <- expectTrue "body compiled" (not (null (E.procEffects pd)))
                    pure (a && b)
    pure (r0 && r1)

-- | `call:` sites are checked statically — unknown names and arity
--   mismatches are compile errors; matching calls compile.
testProcCallSiteChecks :: IO Bool
testProcCallSiteChecks = do
    let mk call = (minAdventure (minRoom "loc_0"))
            { advProcedures =
                [ AProcDef "f" ["a"] [AOMessage "x"]
                , AProcDef "caller" [] [call] ] }
    r1 <- case compileAdventure (mk (AOCallProc "nope" [])) of
            Left errs -> expectTrue "unknown is UnknownProc"
                (any (\i -> ciCode i == "UnknownProc") errs)
            Right _ -> expectTrue "unknown call must fail" False
    r2 <- case compileAdventure (mk (AOCallProc "f" [E.EVInt 1, E.EVInt 2])) of
            Left errs -> expectTrue "mismatch is ProcArity"
                (any (\i -> ciCode i == "ProcArity") errs)
            Right _ -> expectTrue "arity mismatch must fail" False
    r3 <- case compileAdventure (mk (AOCallProc "f" [E.EVInt 1])) of
            Left errs -> expectTrue ("valid call compiles, got: " ++ issuesText errs) False
            Right _ -> expectTrue "valid call ok" True
    pure (r1 && r2 && r3)

-- | Recursion is statically forbidden (D2): direct and indirect call cycles
--   are compile errors — `maxOutcomeDepth` is deliberately not relied upon.
testProcRecursionForbidden :: IO Bool
testProcRecursionForbidden = do
    let direct = (minAdventure (minRoom "loc_0"))
            { advProcedures = [AProcDef "p" [] [AOCallProc "p" []]] }
        indirect = (minAdventure (minRoom "loc_0"))
            { advProcedures =
                [ AProcDef "p" [] [AOCallProc "q" []]
                , AProcDef "q" [] [AOCallProc "p" []] ] }
    r1 <- case compileAdventure direct of
            Left errs -> expectTrue "direct cycle is ProcRecursion"
                (any (\i -> ciCode i == "ProcRecursion") errs)
            Right _ -> expectTrue "direct recursion must fail" False
    r2 <- case compileAdventure indirect of
            Left errs -> expectTrue "indirect cycle is ProcRecursion"
                (any (\i -> ciCode i == "ProcRecursion") errs)
            Right _ -> expectTrue "indirect recursion must fail" False
    pure (r1 && r2)

-- | Parameters must not shadow engine-owned variable namespaces; duplicate
--   procedure ids are rejected.
testProcParamAndIdChecks :: IO Bool
testProcParamAndIdChecks = do
    let reserved = (minAdventure (minRoom "loc_0"))
            { advProcedures = [AProcDef "p" ["cmd.target"] [AOMessage "x"]] }
        dup = (minAdventure (minRoom "loc_0"))
            { advProcedures =
                [AProcDef "p" [] [AOMessage "x"], AProcDef "p" [] [AOMessage "y"]] }
    r1 <- case compileAdventure reserved of
            Left errs -> expectTrue "reserved param is ProcParamReserved"
                (any (\i -> ciCode i == "ProcParamReserved") errs)
            Right _ -> expectTrue "reserved param must fail" False
    r2 <- case compileAdventure dup of
            Left errs -> expectTrue "duplicate id is DuplicateProc"
                (any (\i -> ciCode i == "DuplicateProc") errs)
            Right _ -> expectTrue "duplicate id must fail" False
    pure (r1 && r2)

-- | `call:` parses from the two YAML shapes: bare name (no args) and
--   {proc: name, args: [literal, ...]}.
testCallParsesFromJson :: IO Bool
testCallParsesFromJson = do
    r1 <- case Aeson.decode (BLC.pack "{\"call\":\"belohnen\"}") of
        Just ao -> expectEqual (AOCallProc "belohnen" []) ao
        Nothing -> expectTrue "bare call parses" False
    r2 <- case Aeson.decode
            (BLC.pack "{\"call\":{\"proc\":\"belohnen\",\"args\":[7,\"hi\",true]}}") of
        Just ao -> expectEqual
            (AOCallProc "belohnen" [E.EVInt 7, E.EVString "hi", E.EVBool True]) ao
        Nothing -> expectTrue "call with args parses" False
    pure (r1 && r2)

-- ---------------------------------------------------------------------------
-- B1: content tests as data
-- ---------------------------------------------------------------------------

-- | Ordered marker semantics + YAML parsing of `tests:` entries.
testContentTestBasics :: IO Bool
testContentTestBasics = do
    r1 <- case Aeson.decode (BLC.pack "{\"input\":[\"look\"],\"expect\":[\"x\",\"y\"]}") of
        Just ct -> expectEqual (AContentTest "unnamed" ["look"] ["x", "y"]) ct
        Nothing -> expectTrue "test entry parses (name optional)" False
    r2 <- expectEqual (Nothing :: Maybe String)
            (checkMarkers "alpha\nbeta\ngamma" ["alpha", "gamma"])
    r3 <- expectEqual (Just ("gamma" :: String))
            (checkMarkers "gamma\nbeta\nalpha" ["beta", "gamma"])
    r4 <- expectEqual (Just ("delta" :: String))
            (checkMarkers "alpha" ["alpha", "delta"])
    pure (r1 && r2 && r3 && r4)

-- | Full round trip: YAML file -> parse -> compile -> execute the authored
--   tests; one passing and one failing case.
testContentTestRunner :: IO Bool
testContentTestRunner = do
    tmpDir <- getTemporaryDirectory
    let path = tmpDir </> "b1-content-tests.yaml"
        yaml = unlines
            [ "name: B1 Fixture"
            , "start_room: r0"
            , "verbs:"
            , "  - name: hallo"
            , "rooms:"
            , "  - id: r0"
            , "    name: Room"
            , "    desc: A room."
            , "rules:"
            , "  - id: greet"
            , "    on: \"command hallo\""
            , "    effects:"
            , "      - msg: \"HALLO-DU\""
            , "tests:"
            , "  - name: passt"
            , "    input: [hallo]"
            , "    expect: [\"HALLO-DU\"]"
            , "  - name: schlaegt_fehl"
            , "    input: [hallo]"
            , "    expect: [\"FEHLT\"]"
            ]
    writeFile path yaml
    parsed <- parseAdventureFile path
    case parsed of
        Left err -> do
            removeFile path
            expectTrue ("fixture parses: " ++ show err) False
        Right adv -> case compileAdventure adv of
            Left errs -> do
                removeFile path
                expectTrue ("fixture compiles: " ++ issuesText errs) False
            Right cr -> do
                let results = [ executeContentTest ct (crWorld cr) (crSave cr)
                              | ct <- advTests adv ]
                r1 <- expectEqual (2 :: Int) (length results)
                r2 <- expectEqual (Nothing :: Maybe String) (head results)
                r3 <- expectEqual (Just ("FEHLT" :: String)) (last results)
                removeFile path
                pure (r1 && r2 && r3)

-- ---------------------------------------------------------------------------
-- W1: knowledge model (facts: / combine:)
-- ---------------------------------------------------------------------------

-- | `facts:` compiles in declaration order; empty fact/combine lists are
--   omitted from world.json (byte-stability).
testFactsCompile :: IO Bool
testFactsCompile = do
    let empty = minAdventure (minRoom "loc_0")
    r0 <- case compileAdventure empty of
            Left _ -> expectTrue "default compiles" False
            Right cr -> do
                a <- expectTrue "no facts by default" (null (E.factDefs (crWorld cr)))
                b <- expectTrue "world.json omits empty factDefs"
                        (not ("factDefs" `isInfixOf` BLC.unpack (Aeson.encode (crWorld cr))))
                c <- expectTrue "world.json omits empty combineDefs"
                        (not ("combineDefs" `isInfixOf` BLC.unpack (Aeson.encode (crWorld cr))))
                pure (a && b && c)
    let adv = empty
            { advFacts =
                [ AFactDef "brief" ["brief"] "Der Brief." Nothing Nothing Nothing Nothing
                , AFactDef "brief_gelesen" [] "" Nothing Nothing Nothing Nothing ]
            , advCombines = [ACombineDef ["brief"] "brief_gelesen" Nothing] }
    r1 <- case compileAdventure adv of
            Left errs -> expectTrue ("facts compile, got: " ++ issuesText errs) False
            Right cr -> do
                a <- expectEqual ["brief", "brief_gelesen"] (map E.factId (E.factDefs (crWorld cr)))
                b <- expectTrue "combine compiled"
                        (any (\c -> E.cdYields c == "brief_gelesen") (E.combineDefs (crWorld cr)))
                pure (a && b)
    pure (r0 && r1)

-- | The knowledge model is checked statically: unknown fact references
--   (learn/forget outcomes, combine premises/yields, `knows:` predicates),
--   duplicate ids and yields without premises are compile errors.
testFactChecks :: IO Bool
testFactChecks = do
    let advWith extraFacts extraRules extraCombines = (minAdventure (minRoom "loc_0"))
            { advFacts = [AFactDef "brief" ["brief"] "Der Brief." Nothing Nothing Nothing Nothing] ++ extraFacts
            , advTriggers = [ ATrigger "t" "turn" Nothing extraRules False 0 ]
            , advCombines = extraCombines }
    r1 <- case compileAdventure (advWith [] [AOLearn "nope" "player"] []) of
            Left errs -> expectTrue "learn of unknown fact is UnknownFact"
                (any (\i -> ciCode i == "UnknownFact") errs)
            Right _ -> expectTrue "unknown learn must fail" False
    r2 <- case compileAdventure (advWith [] [] [ACombineDef ["brief"] "nope" Nothing]) of
            Left errs -> expectTrue "yields of unknown fact is UnknownFact"
                (any (\i -> ciCode i == "UnknownFact") errs)
            Right _ -> expectTrue "unknown yields must fail" False
    r3 <- case compileAdventure (advWith [] [] [ACombineDef [] "brief" Nothing]) of
            Left errs -> expectTrue "premise-less combine is YieldsWithoutPremises"
                (any (\i -> ciCode i == "YieldsWithoutPremises") errs)
            Right _ -> expectTrue "premise-less combine must fail" False
    r4 <- case compileAdventure (advWith [AFactDef "brief" [] "x" Nothing Nothing Nothing Nothing] [] []) of
            Left errs -> expectTrue "duplicate fact is DuplicateFact"
                (any (\i -> ciCode i == "DuplicateFact") errs)
            Right _ -> expectTrue "duplicate fact must fail" False
    r5 <- case compileAdventure (advWith [] [] []) of
            Left errs -> expectTrue ("clean knowledge model compiles, got: " ++ issuesText errs) False
            Right _ -> expectTrue "clean compiles" True
    pure (r1 && r2 && r3 && r4 && r5)

-- | `knows:` predicates and `learn:`/`forget:` outcomes parse and compile.
testKnowsSugar :: IO Bool
testKnowsSugar = do
    r1 <- case Aeson.decode (BLC.pack "{\"knows\":\"brief\"}") of
        Just p -> expectEqual (E.Knows E.ActorPlayer "brief") p
        Nothing -> expectTrue "knows: fact parses (player default)" False
    r2 <- case Aeson.decode (BLC.pack "{\"learn\":\"brief\"}") of
        Just o -> expectEqual (AOLearn "brief" "player") o
        Nothing -> expectTrue "learn: fact parses (player default)" False
    r3 <- case Aeson.decode (BLC.pack "{\"learn\":{\"fact\":\"brief\",\"actor\":\"butler\"}}") of
        Just o -> expectEqual (AOLearn "brief" "butler") o
        Nothing -> expectTrue "learn: {fact, actor} parses" False
    r4 <- case Aeson.decode (BLC.pack "{\"forget\":\"brief\"}") of
        Just o -> expectEqual (AOForget "brief" "player") o
        Nothing -> expectTrue "forget: parses" False
    pure (r1 && r2 && r3 && r4)

-- ---------------------------------------------------------------------------
-- W3: chapters
-- ---------------------------------------------------------------------------

-- | `chapters:` compiles in declaration order; empty chapterDefs are omitted
--   from world.json (byte-stability).
testChaptersCompile :: IO Bool
testChaptersCompile = do
    let empty = minAdventure (minRoom "loc_0")
    r0 <- case compileAdventure empty of
            Left _ -> expectTrue "default compiles" False
            Right cr -> do
                a <- expectTrue "no chapters by default" (null (E.chapterDefs (crWorld cr)))
                b <- expectTrue "world.json omits empty chapterDefs"
                        (not ("chapterDefs" `isInfixOf` BLC.unpack (Aeson.encode (crWorld cr))))
                pure (a && b)
    let adv = empty
            { advChapters =
                [ AChapterDef "kap1" Nothing Nothing
                , AChapterDef "kap2" (Just "Zwei.") (Just (HasFlag "tor")) ] }
    r1 <- case compileAdventure adv of
            Left errs -> expectTrue ("chapters compile, got: " ++ issuesText errs) False
            Right cr -> do
                a <- expectEqual ["kap1", "kap2"] (map E.chId (E.chapterDefs (crWorld cr)))
                b <- expectTrue "gates compiled"
                        (all (\cd -> E.chWhen cd /= Nothing) (drop 1 (E.chapterDefs (crWorld cr))))
                pure (a && b)
    pure (r0 && r1)

-- | Static checks: duplicate ids, unknown goto targets, backward jumps
--   (error), unreachable chapters (warning).
testChapterChecks :: IO Bool
testChapterChecks = do
    let mk chs rules = (minAdventure (minRoom "loc_0"))
            { advChapters = chs, advTriggers = rules }
        back = ATrigger "sprung" "chapter kap2" Nothing [AOGotoChapter "kap1"] False 0
        fwd  = ATrigger "sprung" "chapter kap1" Nothing [AOGotoChapter "kap2"] False 0
    r1 <- case compileAdventure (mk [AChapterDef "k" Nothing Nothing, AChapterDef "k" Nothing Nothing] []) of
            Left errs -> expectTrue "duplicate is DuplicateChapter"
                (any (\i -> ciCode i == "DuplicateChapter") errs)
            Right _ -> expectTrue "duplicate must fail" False
    r2 <- case compileAdventure (mk [AChapterDef "k" Nothing Nothing] [ATrigger "s" "turn" Nothing [AOGotoChapter "nope"] False 0]) of
            Left errs -> expectTrue "unknown target is UnknownChapter"
                (any (\i -> ciCode i == "UnknownChapter") errs)
            Right _ -> expectTrue "unknown target must fail" False
    r3 <- case compileAdventure (mk [AChapterDef "kap1" Nothing Nothing, AChapterDef "kap2" Nothing Nothing] [back]) of
            Left errs -> expectTrue "backward jump is ChapterBackwardsJump"
                (any (\i -> ciCode i == "ChapterBackwardsJump") errs)
            Right _ -> expectTrue "backward jump must fail" False
    r4 <- case compileAdventure (mk [AChapterDef "kap1" Nothing Nothing, AChapterDef "kap2" Nothing Nothing] [fwd]) of
            Left errs -> expectTrue ("forward jump compiles, got: " ++ issuesText errs) False
            Right _ -> expectTrue "forward jump compiles" True
    r5 <- case compileAdventure (mk [AChapterDef "k1" Nothing Nothing, AChapterDef "k2" Nothing Nothing] []) of
            Right cr -> expectTrue "unreachable chapter warns"
                (any (\i -> ciCode i == "UnreachableChapter") (crWarnings cr))
            Left errs -> expectTrue ("warn-only case compiles, got: " ++ issuesText errs) False
    pure (r1 && r2 && r3 && r4 && r5)

-- | `next_chapter` / `goto_chapter` sugar compiles.
testChapterSugar :: IO Bool
testChapterSugar = do
    r1 <- case compileAActionOutcome AONextChapter of
            E.NextChapter -> expectTrue "next_chapter compiles" True
            _ -> expectTrue "next_chapter compiles" False
    r2 <- case compileAActionOutcome (AOGotoChapter "finale") of
            E.GotoChapter "finale" -> expectTrue "goto_chapter compiles" True
            _ -> expectTrue "goto_chapter compiles" False
    pure (r1 && r2)

-- | B3: the five mass-operation sugars compile to the engine effects.
testMassOpSugar :: IO Bool
testMassOpSugar = do
    let spec = E.CountSpec E.CountItems (E.CountInRoom "halle") (Just "licht")
    r1 <- case compileAActionOutcome (AODamageAll spec 5) of
            E.DamageAll _ 5 -> expectTrue "damage_all compiles" True
            _ -> expectTrue "damage_all compiles" False
    r2 <- case compileAActionOutcome (AOMoveAll spec (E.CountInRoom "keller")) of
            E.MoveAll _ (E.CountInRoom "keller") -> expectTrue "move_all compiles" True
            _ -> expectTrue "move_all compiles" False
    r3 <- case compileAActionOutcome (AORevealAll spec) of
            E.RevealAll _ -> expectTrue "reveal_all compiles" True
            _ -> expectTrue "reveal_all compiles" False
    r4 <- case compileAActionOutcome (AOConsumeAll spec) of
            E.ConsumeAll _ -> expectTrue "consume_all compiles" True
            _ -> expectTrue "consume_all compiles" False
    r5 <- case compileAActionOutcome (AOSetStateAll spec "brennend") of
            E.SetStateAll _ "brennend" -> expectTrue "set_state_all compiles" True
            _ -> expectTrue "set_state_all compiles" False
    pure (r1 && r2 && r3 && r4 && r5)

-- ---------------------------------------------------------------------------
-- Pursuit (Tür IV)
-- ---------------------------------------------------------------------------

-- | `step_toward:` / `step_away_from:` sugar compiles to the engine effects.
testPursuitSugar :: IO Bool
testPursuitSugar = do
    r1 <- case compileAActionOutcome (AOStepToward "wolf" (E.DTActor E.ActorPlayer) Nothing) of
            E.StepToward (E.ActorNPC "wolf") (E.DTActor E.ActorPlayer) _ Nothing ->
                expectTrue "step_toward compiles" True
            _ -> expectTrue "step_toward compiles" False
    r2 <- case compileAActionOutcome (AOStepAwayFrom "wolf" (E.DTRoom "halle") (Just "weg!")) of
            E.StepAwayFrom (E.ActorNPC "wolf") (E.DTRoom "halle") _ (Just "weg!") ->
                expectTrue "step_away_from compiles" True
            _ -> expectTrue "step_away_from compiles" False
    pure (r1 && r2)

-- | `pursuit:` compiles to one `on: turn` trigger per pursuer, emitted in
--   npc-id order; unknown pursuers and unknown `ignores:` values are errors.
testPursuitSection :: IO Bool
testPursuitSection = do
    let mk entries = (minAdventure (minRoom "loc_0"))
            { advPursuit = entries
            , advNPCs = [ (minNpcKey "wolf") { anLocation = "loc_0" }
                        , (minNpcKey "bird") { anLocation = "loc_0" } ] }
        eWolf = APursuitEntry "wolf" (E.DTActor E.ActorPlayer) ["locked"] (Just "Der Wolf folgt.")
        eBird = APursuitEntry "bird" (E.DTActor E.ActorPlayer) [] Nothing
    r1 <- case compileAdventure (mk [eWolf, eBird]) of
            Left errs -> expectTrue ("pursuit compiles, got: " ++ issuesText errs) False
            Right cr -> do
                let ts = [ t | t <- E.triggerDefs (crWorld cr)
                             , "pursuit_" `isPrefixOf` E.trId t ]
                a <- expectEqual ["pursuit_bird", "pursuit_wolf"] (map E.trId ts)
                b <- expectTrue "on: turn per pursuer" (all (\t -> E.trEvent t == E.OnTurn) ts)
                pure (a && b)
    r2 <- case compileAdventure (mk [APursuitEntry "ghost" (E.DTActor E.ActorPlayer) [] Nothing]) of
            Left errs -> expectTrue "unknown pursuer is MissingNPC"
                (any (\i -> ciCode i == "MissingNPC") errs)
            Right _ -> expectTrue "unknown pursuer must fail" False
    r3 <- case compileAdventure
                (mk [APursuitEntry "wolf" (E.DTActor E.ActorPlayer) ["invisible"] Nothing]) of
            Left errs -> expectTrue "unknown ignore is UnknownPursuitIgnore"
                (any (\i -> ciCode i == "UnknownPursuitIgnore") errs)
            Right _ -> expectTrue "unknown ignore must fail" False
    pure (r1 && r2 && r3)

-- ---------------------------------------------------------------------------
-- W4: devices (Hebel / Halterung)
-- ---------------------------------------------------------------------------

testDevicesCompile :: IO Bool
testDevicesCompile = do
    let empty = minAdventure (minRoom "loc_0")
    r0 <- case compileAdventure empty of
            Left errs -> expectTrue ("empty compile failed: " ++ issuesText errs) False
            Right cr -> do
                a <- expectEqual 0 (Map.size (E.deviceDefs (crWorld cr)))
                b <- expectTrue "world.json omits empty deviceDefs"
                        (not ("deviceDefs" `isInfixOf` BLC.unpack (Aeson.encode (crWorld cr))))
                pure (a && b)
    let dev = ADeviceDef "fackelhalterung" (Just "Fackelhalterung") ["halterung"] "loc_0" (Just "Eine Wandhalterung.") (Just "lichtquelle") ["fackel"] (Just "Knistern.") (Just "Erloschen.") [AOSetFlag "lit" "true"] [AOSetFlag "lit" "false"] (Just "umlegen") ["unten", "oben"] [("oben", [AOSetFlag "hebel_oben" "true"])]
        adv = empty
            { advItems = [ (minItem "fackel") { aiTags = ["lichtquelle"] } ]
            , advDevices = [dev] }
    r1 <- case compileAdventure adv of
            Left errs -> expectTrue ("devices compile, got: " ++ issuesText errs) False
            Right cr -> do
                let devs = E.deviceDefs (crWorld cr)
                a <- expectEqual 1 (Map.size devs)
                case Map.lookup "fackelhalterung" devs of
                    Nothing -> expectTrue "device fackelhalterung found" False
                    Just d -> do
                        b1 <- expectEqual "fackelhalterung" (E.devId d)
                        b2 <- expectEqual "Fackelhalterung" (E.devName d)
                        b3 <- expectEqual ["halterung"] (E.devKeys d)
                        b4 <- expectEqual "loc_0" (E.devLocation d)
                        b5 <- expectEqual (Just "Eine Wandhalterung.") (E.devDescription d)
                        b6 <- expectEqual (Just "lichtquelle") (E.devFitsTag d)
                        b7 <- expectEqual ["fackel"] (E.devFits d)
                        b8 <- expectEqual (Just "umlegen") (E.devFlipVerb d)
                        b9 <- expectEqual ["unten", "oben"] (E.devFlipStates d)
                        pure (a && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9)
    pure (r0 && r1)

testDeviceChecks :: IO Bool
testDeviceChecks = do
    let base = (minAdventure (minRoom "loc_0"))
            { advItems = [ (minItem "fackel") { aiTags = ["lichtquelle"] } ] }
        mkDevs ds = base { advDevices = ds }
        validDev = ADeviceDef "d1" Nothing [] "loc_0" Nothing (Just "lichtquelle") ["fackel"] Nothing Nothing [AOSetFlag "x" "true"] [] Nothing [] []

    -- 1. Duplicate device ID -> DuplicateDevice
    r1 <- case compileAdventure (mkDevs [validDev, validDev { adName = Just "Other" }]) of
            Left errs -> expectTrue "duplicate device is DuplicateDevice"
                (any (\i -> ciCode i == "DuplicateDevice") errs)
            Right _ -> expectTrue "duplicate device must fail" False

    -- 2. Unknown location -> UnknownDeviceLocation
    r2 <- case compileAdventure (mkDevs [validDev { adLocation = "nowhere" }]) of
            Left errs -> expectTrue "unknown location is UnknownDeviceLocation"
                (any (\i -> ciCode i == "UnknownDeviceLocation") errs)
            Right _ -> expectTrue "unknown location must fail" False

    -- 3. Unknown item in fits -> UnknownDeviceItem
    r3 <- case compileAdventure (mkDevs [validDev { adFits = ["ghost_item"] }]) of
            Left errs -> expectTrue "unknown item in fits is UnknownDeviceItem"
                (any (\i -> ciCode i == "UnknownDeviceItem") errs)
            Right _ -> expectTrue "unknown item in fits must fail" False

    -- 4. Fewer than 2 flip states -> DeviceFlipStateCount
    r4 <- case compileAdventure (mkDevs [validDev { adFlipVerb = Just "flip", adFlipStates = ["single"] }]) of
            Left errs -> expectTrue "fewer than 2 flip states is DeviceFlipStateCount"
                (any (\i -> ciCode i == "DeviceFlipStateCount") errs)
            Right _ -> expectTrue "fewer than 2 flip states must fail" False

    -- 5. Unknown tag warning -> UnknownDeviceTag
    r5 <- case compileAdventure (mkDevs [validDev { adFitsTag = Just "nonexistent_tag" }]) of
            Right cr -> expectTrue "unknown fits_tag produces UnknownDeviceTag warning"
                (any (\i -> ciCode i == "UnknownDeviceTag") (crWarnings cr))
            Left errs -> expectTrue ("warn-only case must compile, got: " ++ issuesText errs) False

    -- 6. Device without effects warning -> DeviceWithoutEffects
    let noEffDev = ADeviceDef "d2" Nothing [] "loc_0" Nothing Nothing [] Nothing Nothing [] [] Nothing [] []
    r6 <- case compileAdventure (mkDevs [noEffDev]) of
            Right cr -> expectTrue "device without effects produces DeviceWithoutEffects warning"
                (any (\i -> ciCode i == "DeviceWithoutEffects") (crWarnings cr))
            Left errs -> expectTrue ("warn-only case must compile, got: " ++ issuesText errs) False

    pure (r1 && r2 && r3 && r4 && r5 && r6)

-- ---------------------------------------------------------------------------
-- W2: progression (XP & Level)
-- ---------------------------------------------------------------------------

testProgressionCompile :: IO Bool
testProgressionCompile = do
    let empty = minAdventure (minRoom "loc_0")
    -- 1. Adventure without progression: progressionDef is Nothing, no progression variables
    r0 <- case compileAdventure empty of
            Left errs -> expectTrue ("empty compile failed: " ++ issuesText errs) False
            Right cr -> do
                a <- expectEqual Nothing (E.progressionDef (crWorld cr))
                b <- expectTrue "world.json omits progression when Nothing"
                        (not ("progression" `isInfixOf` BLC.unpack (Aeson.encode (crWorld cr))))
                c <- expectTrue "xp.current not generated when progression is Nothing"
                        (not (Map.member "xp.current" (E.varDefs (crWorld cr))))
                pure (a && b && c)

    -- 2. Adventure with progression: compiles levels, effects, variables, initials
    let prog = AProgressionDef
            [ ALevelDef (Just 1) 0 "Novize" Nothing []
            , ALevelDef (Just 2) 100 "Krieger" (Just "Du bist nun Krieger!") [AOSetVar "bonus.attack" 2]
            ]
        adv = empty { advProgression = Just prog }
    r1 <- case compileAdventure adv of
            Left errs -> expectTrue ("progression compile, got: " ++ issuesText errs) False
            Right cr -> do
                let mPdef = E.progressionDef (crWorld cr)
                case mPdef of
                    Nothing -> expectTrue "progressionDef compiled" False
                    Just pdef -> do
                        let lvls = E.progLevels pdef
                        a1 <- expectEqual 2 (length lvls)
                        let l1 = head lvls
                            l2 = lvls !! 1
                        b1 <- expectEqual 1 (E.lvlNumber l1)
                        b2 <- expectEqual 0 (E.lvlXp l1)
                        b3 <- expectEqual "Novize" (E.lvlName l1)
                        b4 <- expectEqual 2 (E.lvlNumber l2)
                        b5 <- expectEqual 100 (E.lvlXp l2)
                        b6 <- expectEqual (Just "Du bist nun Krieger!") (E.lvlMsg l2)

                        -- Variables merged
                        let vDefs = E.varDefs (crWorld cr)
                            vInits = E.variables (crSave cr)
                        c1 <- expectTrue "xp.current def exists" (Map.member "xp.current" vDefs)
                        c2 <- expectTrue "level.current def exists" (Map.member "level.current" vDefs)
                        c3 <- expectTrue "bonus.attack def exists" (Map.member "bonus.attack" vDefs)
                        c4 <- expectTrue "bonus.defense def exists" (Map.member "bonus.defense" vDefs)
                        c5 <- expectTrue "bonus.hp def exists" (Map.member "bonus.hp" vDefs)

                        d1 <- expectEqual (Just (E.VVInt 0)) (Map.lookup "xp.current" vInits)
                        d2 <- expectEqual (Just (E.VVInt 1)) (Map.lookup "level.current" vInits)
                        d3 <- expectEqual (Just (E.VVInt 0)) (Map.lookup "bonus.attack" vInits)

                        pure (a1 && b1 && b2 && b3 && b4 && b5 && b6 && c1 && c2 && c3 && c4 && c5 && d1 && d2 && d3)
    pure (r0 && r1)

testProgressionChecks :: IO Bool
testProgressionChecks = do
    let base = minAdventure (minRoom "loc_0")
        mkProg ps = base { advProgression = Just (AProgressionDef ps) }

    -- 1. Empty levels -> EmptyLevels error
    r1 <- case compileAdventure (mkProg []) of
            Left errs -> expectTrue "empty levels is EmptyLevels"
                (any (\i -> ciCode i == "EmptyLevels") errs)
            Right _ -> expectTrue "empty levels must fail" False

    -- 2. First level XP /= 0 -> BadLevelXp error
    r2 <- case compileAdventure (mkProg [ALevelDef (Just 1) 50 "Novize" Nothing []]) of
            Left errs -> expectTrue "first level xp /= 0 is BadLevelXp"
                (any (\i -> ciCode i == "BadLevelXp") errs)
            Right _ -> expectTrue "first level xp /= 0 must fail" False

    -- 3. Non-monotonic XP -> NonMonotonicXp error
    r3 <- case compileAdventure (mkProg [ ALevelDef (Just 1) 0 "Novize" Nothing []
                                        , ALevelDef (Just 2) 100 "Krieger" Nothing []
                                        , ALevelDef (Just 3) 80 "Meister" Nothing []
                                        ]) of
            Left errs -> expectTrue "decreasing xp is NonMonotonicXp"
                (any (\i -> ciCode i == "NonMonotonicXp") errs)
            Right _ -> expectTrue "decreasing xp must fail" False

    -- 4. Duplicate/equal XP -> NonMonotonicXp error
    r4 <- case compileAdventure (mkProg [ ALevelDef (Just 1) 0 "Novize" Nothing []
                                        , ALevelDef (Just 2) 100 "Krieger" Nothing []
                                        , ALevelDef (Just 3) 100 "Meister" Nothing []
                                        ]) of
            Left errs -> expectTrue "equal xp is NonMonotonicXp"
                (any (\i -> ciCode i == "NonMonotonicXp") errs)
            Right _ -> expectTrue "equal xp must fail" False

    -- 5. Reserved variable clash in author variables -> ProgressionVariableClash error
    let clashAdv = (mkProg [ALevelDef (Just 1) 0 "Novize" Nothing []])
            { advVariables = [ AVariable "xp.current" "int" (Just (Aeson.Number 0)) Nothing Nothing ] }
    r5 <- case compileAdventure clashAdv of
            Left errs -> expectTrue "declaring xp.current is ProgressionVariableClash"
                (any (\i -> ciCode i == "ProgressionVariableClash") errs)
            Right _ -> expectTrue "reserved var clash must fail" False

    -- 6. gain_xp outcome without progression section -> GainXpWithoutProgression warning
    let warnAdv = base
            { advTriggers = [ ATrigger "t1" "turn" Nothing [AOGainXp 50] False 0 ] }
    r6 <- case compileAdventure warnAdv of
            Right cr -> expectTrue "gain_xp without progression produces GainXpWithoutProgression warning"
                (any (\i -> ciCode i == "GainXpWithoutProgression") (crWarnings cr))
            Left errs -> expectTrue ("warn-only case must compile, got: " ++ issuesText errs) False

    pure (r1 && r2 && r3 && r4 && r5 && r6)

-- ---------------------------------------------------------------------------
-- 4.4: containers
-- ---------------------------------------------------------------------------

-- | `containers:` compiles to containerDefs plus the initial entity states;
--   `player: {inventory_limit}` seeds the VarMap; duplicate ids and unknown
--   rooms fail.
testContainersCompile :: IO Bool
testContainersCompile = do
    let mk cons = (minAdventure (minRoom "loc_0"))
            { advContainers = cons
            , advPlayer = Just (AAdventurePlayer Nothing Nothing Nothing Map.empty Nothing Nothing (Just 3)) }
        mkC i open locked = AContainerDef i "" "loc_0" (Just 5) open locked
    r1 <- case compileAdventure (mk [mkC "truhe" False True, mkC "schrank" True False]) of
            Left errs -> expectTrue ("containers compile, got: " ++ issuesText errs) False
            Right cr -> do
                let cd = E.containerDefs (crWorld cr)
                a <- expectEqual ["schrank", "truhe"] (Map.keys cd)
                b <- expectEqual (Just "locked") (Map.lookup "truhe" (E.entityStates (crSave cr)))
                c <- expectEqual (Just "open") (Map.lookup "schrank" (E.entityStates (crSave cr)))
                d <- expectEqual (Just (E.VVInt 3)) (Map.lookup "inventory.limit" (E.variables (crSave cr)))
                pure (a && b && c && d)
    r2 <- case compileAdventure (mk [mkC "doppelt" False False, mkC "doppelt" False False]) of
            Left errs -> expectTrue "duplicate container is DuplicateContainer"
                (any (\i -> ciCode i == "DuplicateContainer") errs)
            Right _ -> expectTrue "duplicate container must fail" False
    r3 <- case compileAdventure (mk [AContainerDef "ortlos" "" "nirgendwo" Nothing False False]) of
            Left errs -> expectTrue "unknown room is MissingRoom"
                (any (\i -> ciCode i == "MissingRoom") errs)
            Right _ -> expectTrue "unknown room must fail" False
    pure (r1 && r2 && r3)

-- | `set_inventory_limit:` compiles to the VarMap write.
testSetInventoryLimitSugar :: IO Bool
testSetInventoryLimitSugar = do
    case compileAActionOutcome (AOSetInventoryLimit 7) of
        E.SetValue (E.VRVariable "inventory.limit") (E.EVInt 7) ->
            expectTrue "set_inventory_limit compiles" True
        _ -> expectTrue "set_inventory_limit compiles" False

-- ---------------------------------------------------------------------------
-- 5.3: include: libraries
-- ---------------------------------------------------------------------------

-- | Throwaway directory for include: tests (real files, real parse paths).
withIncludeDir :: (FilePath -> IO Bool) -> IO Bool
withIncludeDir act = do
    tmp <- getTemporaryDirectory
    (probe, h) <- openTempFile tmp "ta-include"
    hClose h
    removeFile probe
    createDirectoryIfMissing True probe
    ok <- act probe
    removeDirectoryRecursive probe
    pure ok

-- | Merge contract: the main file's own sections come first, then includes
--   in list order; resolved `include:` lists end up empty.
testIncludeMerge :: IO Bool
testIncludeMerge = withIncludeDir $ \dir -> do
    writeFile (dir </> "main.yaml") $ unlines
        [ "start_room: halle"
        , "rooms:"
        , "  - id: halle"
        , "    name: Halle"
        , "    desc: Die Halle."
        , "include: [lib.yaml]"
        , "rules:"
        , "  - id: eigene"
        , "    on: \"turn\""
        , "    effects: [ {msg: \"eigene\"} ]"
        ]
    writeFile (dir </> "lib.yaml") $ unlines
        [ "rooms:"
        , "  - id: kammer"
        , "    name: Kammer"
        , "    desc: Die Kammer."
        , "rules:"
        , "  - id: libtrigger"
        , "    on: \"turn\""
        , "    effects: [ {msg: \"lib\"} ]"
        ]
    r <- parseAdventureFile (dir </> "main.yaml")
    case r of
        Left err -> expectTrue ("include merge parses, got: " ++ err) False
        Right adv -> do
            a <- expectEqual ["halle", "kammer"] (map arId (advRooms adv))
            b <- expectEqual ["eigene", "libtrigger"] (map atId (advTriggers adv))
            c <- expectEqual ([] :: [String]) (advInclude adv)
            pure (a && b && c)

-- | Single-value fields are reserved for the main adventure file.
testIncludeForbidden :: IO Bool
testIncludeForbidden = withIncludeDir $ \dir -> do
    writeFile (dir </> "main.yaml") $ unlines
        [ "start_room: halle"
        , "rooms: [ {id: halle, name: H, desc: D} ]"
        , "include: [lib.yaml]"
        ]
    writeFile (dir </> "lib.yaml") "name: Bibliothek\n"
    r <- parseAdventureFile (dir </> "main.yaml")
    case r of
        Left err -> expectTrue "forbidden key is named"
            ("'name' is reserved" `isInfixOf` err)
        Right _ -> expectTrue "forbidden key must fail" False

-- | Duplicate ids across merged sources fail naming both files.
testIncludeDuplicates :: IO Bool
testIncludeDuplicates = withIncludeDir $ \dir -> do
    writeFile (dir </> "main.yaml") $ unlines
        [ "start_room: halle"
        , "rooms: [ {id: halle, name: H, desc: D} ]"
        , "include: [lib.yaml]"
        ]
    writeFile (dir </> "lib.yaml") "rooms: [ {id: halle, name: H2, desc: D} ]\n"
    r <- parseAdventureFile (dir </> "main.yaml")
    case r of
        Left err -> do
            a <- expectTrue "duplicate id is named" ("'halle' defined in" `isInfixOf` err)
            b <- expectTrue "both files are named"
                    ("main.yaml" `isInfixOf` err && "lib.yaml" `isInfixOf` err)
            pure (a && b)
        Right _ -> expectTrue "duplicate id must fail" False

-- | Transitive includes merge depth-first: own sections before own includes.
testIncludeTransitive :: IO Bool
testIncludeTransitive = withIncludeDir $ \dir -> do
    writeFile (dir </> "main.yaml") $ unlines
        [ "start_room: r1"
        , "rooms: [ {id: r1, name: R1, desc: D} ]"
        , "include: [b.yaml]"
        ]
    writeFile (dir </> "b.yaml") $ unlines
        [ "rooms: [ {id: r2, name: R2, desc: D} ]"
        , "include: [c.yaml]"
        ]
    writeFile (dir </> "c.yaml") "rooms: [ {id: r3, name: R3, desc: D} ]\n"
    r <- parseAdventureFile (dir </> "main.yaml")
    case r of
        Left err -> expectTrue ("transitive include parses, got: " ++ err) False
        Right adv -> expectEqual ["r1", "r2", "r3"] (map arId (advRooms adv))

-- | Include cycles are a hard error naming the chain.
testIncludeCycle :: IO Bool
testIncludeCycle = withIncludeDir $ \dir -> do
    writeFile (dir </> "main.yaml") $ unlines
        [ "start_room: r1"
        , "rooms: [ {id: r1, name: R1, desc: D} ]"
        , "include: [b.yaml]"
        ]
    writeFile (dir </> "b.yaml") "include: [main.yaml]\n"
    r <- parseAdventureFile (dir </> "main.yaml")
    case r of
        Left err -> expectTrue "cycle is named" ("include cycle" `isInfixOf` err)
        Right _ -> expectTrue "cycle must fail" False

-- | A diamond (same library via two paths) loads once, at its first position.
testIncludeDiamond :: IO Bool
testIncludeDiamond = withIncludeDir $ \dir -> do
    writeFile (dir </> "main.yaml") $ unlines
        [ "start_room: r1"
        , "rooms: [ {id: r1, name: R1, desc: D} ]"
        , "include: [b.yaml, c.yaml]"
        ]
    writeFile (dir </> "b.yaml") $ unlines
        [ "rooms: [ {id: r2, name: R2, desc: D} ]"
        , "include: [d.yaml]"
        ]
    writeFile (dir </> "c.yaml") $ unlines
        [ "rooms: [ {id: r3, name: R3, desc: D} ]"
        , "include: [d.yaml]"
        ]
    writeFile (dir </> "d.yaml") "rooms: [ {id: rd, name: RD, desc: D} ]\n"
    r <- parseAdventureFile (dir </> "main.yaml")
    case r of
        Left err -> expectTrue ("diamond parses, got: " ++ err) False
        Right adv -> expectEqual ["r1", "r2", "rd", "r3"] (map arId (advRooms adv))

-- | Byte-identical compilation: the same content compiles to the identical
--   world whether it lives in one file or is split into libraries.
testIncludeIdenticalWorld :: IO Bool
testIncludeIdenticalWorld = withIncludeDir $ \dir -> do
    writeFile (dir </> "mono.yaml") $ unlines
        [ "start_room: r1"
        , "rooms:"
        , "  - {id: r1, name: R1, desc: D}"
        , "  - {id: r2, name: R2, desc: D}"
        , "rules:"
        , "  - id: t1"
        , "    on: \"turn\""
        , "    effects: [ {msg: \"x\"} ]"
        ]
    writeFile (dir </> "main.yaml") $ unlines
        [ "start_room: r1"
        , "rooms:"
        , "  - {id: r1, name: R1, desc: D}"
        , "include: [lib.yaml]"
        ]
    writeFile (dir </> "lib.yaml") $ unlines
        [ "rooms:"
        , "  - {id: r2, name: R2, desc: D}"
        , "rules:"
        , "  - id: t1"
        , "    on: \"turn\""
        , "    effects: [ {msg: \"x\"} ]"
        ]
    m <- parseAdventureFile (dir </> "mono.yaml")
    t <- parseAdventureFile (dir </> "main.yaml")
    case (m, t) of
        (Left err, _) -> expectTrue ("mono parses, got: " ++ err) False
        (_, Left err) -> expectTrue ("split parses, got: " ++ err) False
        (Right mono, Right split) ->
            expectEqual (fmap crWorld (compileAdventure mono))
                        (fmap crWorld (compileAdventure split))

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
    E.ApplyCondition _ _ (Just t) (Just el) _ -> carriesExitEffect t || carriesExitEffect el
    E.ApplyCondition _ _ (Just t) Nothing _   -> carriesExitEffect t
    E.ApplyCondition _ _ Nothing (Just el) _  -> carriesExitEffect el
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
                , (minRoom "loc_b") { arExits = Map.fromList [("west", AExitRef "loc_0" Nothing Nothing Nothing)] }
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
            [ ("north", AExitRef "loc_a" Nothing Nothing Nothing)
            , ("south", AExitRef "loc_a" Nothing Nothing Nothing)
            , ("east", AExitRef "loc_a" Nothing Nothing Nothing)
            , ("west", AExitRef "loc_a" Nothing Nothing Nothing)
            , ("up", AExitRef "loc_a" Nothing Nothing Nothing)
            , ("down", AExitRef "loc_a" Nothing Nothing Nothing)
            , ("northeast", AExitRef "loc_a" Nothing Nothing Nothing)
            , ("northwest", AExitRef "loc_a" Nothing Nothing Nothing)
            , ("southeast", AExitRef "loc_a" Nothing Nothing Nothing)
            , ("southwest", AExitRef "loc_a" Nothing Nothing Nothing)
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
            [ ("ne", AExitRef "loc_a" Nothing Nothing Nothing)
            , ("nw", AExitRef "loc_a" Nothing Nothing Nothing)
            , ("se", AExitRef "loc_a" Nothing Nothing Nothing)
            , ("sw", AExitRef "loc_a" Nothing Nothing Nothing)
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
    let exits = Map.fromList [("noth", AExitRef "loc_a" Nothing Nothing Nothing)]
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
    , aiCapacity = Nothing, aiPortable = Nothing
    , aiTakeFailure = Nothing
    , aiInContainer = Nothing
    , aiCarriedBy = Nothing
    , aiGrammar = E.emptyGrammar
    }

advWithItem :: AItem -> Adventure
advWithItem item =
    let adv = minAdventure (minRoom "loc_0")
    in adv { advItems = [item] }

-- | A minimal adventure carrying one NPC (B9: the `drops_on_death:` tests).
advWithNPC :: ANPC -> Adventure
advWithNPC npc =
    let adv = minAdventure (minRoom "loc_0")
    in adv { advNPCs = [npc { anLocation = "loc_0" }] }

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
            expectEqual (Just (E.PhaseAfter, E.VUse, "intact")) (Map.lookupMin (E.itemVerbMap def) >>= (\(k, _) -> Just k))

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
                firstVerb = (\((_, v, _), _) -> v) <$> Map.lookupMin (E.itemVerbMap def)
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
            case Map.lookup (E.PhaseAfter, E.VTake, "intact") (E.itemVerbMap def) of
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
            case Map.lookup (E.PhaseAfter, E.VTake, "intact") (E.itemVerbMap def) of
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
            [ ("se", AExitRef "loc_a" Nothing Nothing Nothing)
            , ("southeast", AExitRef "loc_b" Nothing Nothing Nothing)
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
    let exits = Map.fromList [("noth", AExitRef "loc_a" Nothing Nothing Nothing)]
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
    let badQuest = E.Quest "q1" "Empty Quest" "no stages" Map.empty [] Nothing Nothing
        badWorld = minWorld { questDefs = Map.singleton "q1" badQuest }
    expectTrue "EmptyQuestStages detected"
        (EmptyQuestStages "q1" `elem` validateGameState badWorld minSave)

-- | Quest-Prereq-Flag, das nie gesetzt wird, wird erkannt
testUnknownQuestPrereqDetected :: IO Bool
testUnknownQuestPrereqDetected = do
    let badQuest = E.Quest "q2" "Bad prereq" "flag never set"
                    (Map.singleton "never_set_flag" "true")
                    [E.QuestStage "s1" "step one" Nothing] Nothing Nothing
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
            case Map.lookup (E.PhaseAfter, E.VUse, "intact") (E.itemVerbMap def) of
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
    r2 <- expectEqual (Just (AOApplyCondition "poisoned" 3 [AODamagePlayer 1] [AOMessage "over"] False))
              (dec (BLC.pack "{\"condition\": {\"name\": \"poisoned\", \"turns\": 3, \"tick\": [{\"damage\": 1}], \"end\": [{\"msg\": \"over\"}]}}"))
    r2b <- expectEqual (Just (AOApplyCondition "bomb" 5 [] [] True))
              (dec (BLC.pack "{\"condition\": {\"name\": \"bomb\", \"turns\": 5, \"hidden\": true}}"))
    r3 <- expectEqual (Just (AOClearCondition "poisoned"))
              (dec (BLC.pack "{\"clear_condition\": \"poisoned\"}"))
    r4 <- expectEqual (Just (AOModifySkill "lockpick" 1))
              (dec (BLC.pack "{\"skill\": {\"name\": \"lockpick\", \"delta\": 1}}"))
    r5 <- expectEqual (Just (AORandomChoice "" [(3, [AOMessage "a"]), (1, [AOMessage "b"])]))
              (dec (BLC.pack "{\"random\": [[3, [{\"msg\": \"a\"}]], [1, [{\"msg\": \"b\"}]]]}"))
    -- B8: object form with a named stream (and without one = default stream)
    r5b <- expectEqual (Just (AORandomChoice "beute" [(2, [AOMessage "a"]), (1, [AOMessage "b"])]))
              (dec (BLC.pack "{\"random\": {\"stream\": \"beute\", \"choices\": [[2, [{\"msg\": \"a\"}]], [1, [{\"msg\": \"b\"}]]]}}"))
    r5c <- expectEqual (Just (AORandomChoice "" [(2, [AOMessage "a"])]))
              (dec (BLC.pack "{\"random\": {\"choices\": [[2, [{\"msg\": \"a\"}]]]}}"))
    c1 <- expectEqual (E.Narrative ["a"] (E.SendMessage "x"))
              (compileAActionOutcome (AONarrative ["a"] [AOMessage "x"]))
    c2 <- expectEqual (E.ApplyCondition "p" 2 (Just (E.SendMessage "t")) (Just (E.SendMessage "e")) False)
              (compileAActionOutcome (AOApplyCondition "p" 2 [AOMessage "t"] [AOMessage "e"] False))
    c2b <- expectEqual (E.ApplyCondition "b" 3 Nothing Nothing True)
              (compileAActionOutcome (AOApplyCondition "b" 3 [] [] True))
    c3 <- expectEqual (E.ClearCondition "p") (compileAActionOutcome (AOClearCondition "p"))
    c4 <- expectEqual (E.ModifySkill "s" 2) (compileAActionOutcome (AOModifySkill "s" 2))
    c5 <- expectEqual (E.RandomChoice [(2, E.SendMessage "a")])
              (compileAActionOutcome (AORandomChoice "" [(2, [AOMessage "a"])]))
    c5b <- expectEqual (E.RandomChoiceOn "beute" [(2, E.SendMessage "a")])
              (compileAActionOutcome (AORandomChoice "beute" [(2, [AOMessage "a"])]))
    pure (and [r1, r2, r2b, r3, r4, r5, r5b, r5c, c1, c2, c2b, c3, c4, c5, c5b])

-- | Audio Phase 1 & 2: `sfx:`, `music:`, `stop_music:` decode and compile.
testAudioOutcomes :: IO Bool
testAudioOutcomes = do
    let dec = Aeson.decode :: BLC.ByteString -> Maybe AActionOutcome
    r1 <- expectEqual (Just (AOPlaySfx "sword.wav"))
              (dec (BLC.pack "{\"sfx\": \"sword.wav\"}"))
    r2 <- expectEqual (Just (AOPlayMusic "theme.xm"))
              (dec (BLC.pack "{\"music\": \"theme.xm\"}"))
    r3 <- expectEqual (Just AOStopMusic)
              (dec (BLC.pack "{\"stop_music\": true}"))
    c1 <- expectEqual (E.PlaySfx "sword.wav")
              (compileAActionOutcome (AOPlaySfx "sword.wav"))
    c2 <- expectEqual (E.PlayMusic "theme.xm")
              (compileAActionOutcome (AOPlayMusic "theme.xm"))
    c3 <- expectEqual E.StopMusic
              (compileAActionOutcome AOStopMusic)
    pure (and [r1, r2, r3, c1, c2, c3])

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
            , aiItem = [ AItemInteraction "herb" "mortar" [AOMessage "paste made"] ]
            , aiNpc = [] }
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
            , aiItem = []
            , aiNpc = [] }
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

-- | Genre 4: economy fixture economy_hamurabi.yaml compiles and validates clean.
testEconomyFixtureCompiles :: IO Bool
testEconomyFixtureCompiles = do
    mbPath <- findExample "economy_hamurabi.yaml"
    case mbPath of
        Nothing -> do
            putStrLn "  economy_hamurabi.yaml fixture not found"
            pure False
        Just path -> do
            advResult <- parseAdventureFile path
            case advResult of
                Left err -> do
                    putStrLn $ "  failed to parse economy_hamurabi.yaml: " ++ err
                    pure False
                Right adv -> case compileAdventure adv of
                    Left errs -> do
                        putStrLn $ "  economy_hamurabi.yaml compile errors: " ++ issuesText errs
                        pure False
                    Right cr -> do
                        let errs = validateWorld (crWorld cr) ++ validateGameState (crWorld cr) (crSave cr)
                        if null errs
                            then pure True
                            else do
                                putStrLn $ "  economy_hamurabi.yaml validation: " ++ show errs
                                pure False

-- | Genre 5: deckbuilder fixture deckbuilder_spire.yaml compiles and validates clean.
testDeckbuilderFixtureCompiles :: IO Bool
testDeckbuilderFixtureCompiles = do
    mbPath <- findExample "deckbuilder_spire.yaml"
    case mbPath of
        Nothing -> do
            putStrLn "  deckbuilder_spire.yaml fixture not found"
            pure False
        Just path -> do
            advResult <- parseAdventureFile path
            case advResult of
                Left err -> do
                    putStrLn $ "  failed to parse deckbuilder_spire.yaml: " ++ err
                    pure False
                Right adv -> case compileAdventure adv of
                    Left errs -> do
                        putStrLn $ "  deckbuilder_spire.yaml compile errors: " ++ issuesText errs
                        pure False
                    Right cr -> do
                        let errs = validateWorld (crWorld cr) ++ validateGameState (crWorld cr) (crSave cr)
                        if null errs
                            then pure True
                            else do
                                putStrLn $ "  deckbuilder_spire.yaml validation: " ++ show errs
                                pure False

-- | Genre 3: sandbox fixture sandbox_wilderness.yaml compiles and validates clean.
testSandboxWildernessFixtureCompiles :: IO Bool
testSandboxWildernessFixtureCompiles = do
    mbPath <- findExample "sandbox_wilderness.yaml"
    case mbPath of
        Nothing -> do
            putStrLn "  sandbox_wilderness.yaml fixture not found"
            pure False
        Just path -> do
            advResult <- parseAdventureFile path
            case advResult of
                Left err -> do
                    putStrLn $ "  failed to parse sandbox_wilderness.yaml: " ++ err
                    pure False
                Right adv -> case compileAdventure adv of
                    Left errs -> do
                        putStrLn $ "  sandbox_wilderness.yaml compile errors: " ++ issuesText errs
                        pure False
                    Right cr -> do
                        let errs = validateWorld (crWorld cr) ++ validateGameState (crWorld cr) (crSave cr)
                        if null errs
                            then pure True
                            else do
                                putStrLn $ "  sandbox_wilderness.yaml validation: " ++ show errs
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

-- ---------------------------------------------------------------------------
-- W5 Stufe 2: exakte YAML-Positionen und der positionstreue Schreiber
-- ---------------------------------------------------------------------------

-- W5-Testdokument: Verschachtelung, Listen mit `id:`, Flow-Style und ein
-- Wert mit `": "` (die YAML-Falle aus 4.3.6), dazu Kommentare als
-- Erhaltungsprobe.
ydTestYaml :: String
ydTestYaml = unlines
    [ "# Kommentar oben"
    , "name: Doc"
    , "rooms:"
    , "  - id: keller"
    , "    name: Keller"
    , "    desc: \"Ein Keller: drei Faesser.\""      -- Wert mit Doppelpunkt
    , "    tags: [dark, safe]"
    , "    exits:"
    , "      east: { to: gange }"
    , "      west:"
    , "        to: gange"
    , "        locked_by: tuer"
    , "  - id: gange"
    , "    name: Gang"
    , "    desc: Ein Gang."
    , "items:"
    , "  - id: fass"
    , "    name: Fass"
    , "    location: keller"
    , "    keys: [fass, tonne]"
    , "    ascii: |"
    , "      +---+"
    , "      | X |"
    , "      +---+"
    ]

-- | W5 Stufe 2, Lesen: dotted Issue-Pfade lösen auf exakte Zeilen auf. Der
--   Punkt ist nicht nur "gefunden", sondern dass die Zeile die *richtige* ist —
--   die Stufe-1-Heuristik springt bei `rooms.gange` in der Sprach-Fixture in
--   einen 40 Zeilen späteren `topics:`-Eintrag.
testYamlDocPositions :: IO Bool
testYamlDocPositions = do
    let bytes = BLC.pack ydTestYaml
        lines' = ydTestYaml
    case parseYamlDoc bytes of
        Left err -> do
            putStrLn $ "  parse failed: " ++ err
            pure False
        Right doc -> do
            let at dotted = fmap (\p -> (posLine p, posColumn p)) (ydResolveIssuePath doc dotted)
            r1 <- expectEqual (Just (4, 4)) (at "rooms.keller")
            r2 <- expectEqual (Just (5, 4)) (at "rooms.keller.name")
            r3 <- expectEqual (Just (9, 6)) (at "rooms.keller.exits.east")
            r4 <- expectEqual (Just (11, 8)) (at "rooms.keller.exits.west.to")
            r5 <- expectEqual (Just (20, 17)) (at "items.fass.keys[1]")
            -- unknown paths stay Nothing so the caller falls back to Locate
            r6 <- expectTrue "unknown room -> Nothing" (isNothing' (at "rooms.nope"))
            r7 <- expectTrue "unknown top segment -> Nothing" (isNothing' (at "monsters.x"))
            -- the bracket form the B9 reference checks emit
            r8 <- expectEqual (Just (20, 11)) (at "items.fass.keys[0]")
            -- path splitting, including the bracket and index shapes
            r9 <- expectEqual [SegKey (T.pack "rooms"), SegKey (T.pack "keller")]
                            (splitIssuePath "rooms.keller")
            r10 <- expectEqual [SegKey (T.pack "items"), SegKey (T.pack "fass"),
                             SegKey (T.pack "keys"), SegIndex 1]
                            (splitIssuePath "items.fass.keys[1]")
            -- agreement with the Stufe-1 heuristic on a plain path: the exact
            -- line must at least be the same line Locate names
            r11 <- expectEqual
                            (fmap posLine (ydResolveIssuePath doc "items.fass.location"))
                            (fmap fst (lineForPath lines' "items.fass.location"))
            pure (and [r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11])
  where
    isNothing' = maybe True (const False)

-- | W5 Stufe 2, Schreiben: ein Wert wird ersetzt, **alle anderen Bytes bleiben
--   gleich** (Kommentare, Reihenfolge, Leerzeilen, Flow-Style). Die
--   Idempotenz ist der Kernvertrag: denselben Wert noch einmal setzen ergibt
--   das Original.
testYamlDocWriter :: IO Bool
testYamlDocWriter = do
    let bytes = BLC.pack ydTestYaml
    case parseYamlDoc bytes of
        Left err -> do
            putStrLn $ "  parse failed: " ++ err
            pure False
        Right doc -> do
            r1 <- case ydScalarSpan doc "items.fass.location" of
                Left e -> expectTrue ("span failed: " ++ show e) False
                Right sp -> do
                    r1a <- expectEqual "keller" (BLC.unpack (ssText sp))
                    -- idempotent by construction
                    r1b <- expectEqual
                        (Right bytes) (setScalarAt doc "items.fass.location" (ssText sp))
                    r1c <- expectEqual
                        (Right (BLC.pack (replaceIn ydTestYaml 19 "keller" "gange")))
                        (setScalarAt doc "items.fass.location" (BLC.pack "gange"))
                    pure (r1a && r1b && r1c)
            r2 <- expectTrue "comments survive a write"
                (contains "# Kommentar oben"
                    (either (const BLC.empty) id (setScalarAt doc "items.fass.location" (BLC.pack "gange"))))
            r3 <- expectTrue "the flow-style exit line is untouched by an unrelated write"
                (contains "east: { to: gange }"
                    (either (const BLC.empty) id (setScalarAt doc "items.fass.location" (BLC.pack "gange"))))
            r4 <- expectTrue "a quoted value is refused (re-quoting is the caller's job)"
                (case setScalarAt doc "rooms.keller.desc" (BLC.pack "\"Ein alter Keller.\"") of
                    Left _ -> True
                    Right _ -> False)
            -- UTF-8: the spans are BYTE offsets, so a multi-byte value earlier in
            -- the file must not shift what the writer splices. (The harness
            -- packs this one properly; BLC.pack would truncate the umlaut.)
            r5 <- case parseYamlDoc (utf8Bytes umlautYaml) of
                Left err -> expectTrue ("utf8 parse failed: " ++ err) False
                Right udoc -> do
                    r5a <- expectEqual (utf8Bytes "Keller")
                                (either (const BLC.empty) ssText (ydScalarSpan udoc "rooms.keller.name"))
                    r5b <- expectTrue "an earlier umlaut shifts nothing: the write hits the right bytes"
                        (case setScalarAt udoc "rooms.keller.name" (utf8Bytes "Foyer") of
                            Right out -> contains "name: Foyer" out
                                        && containsBytes (utf8Bytes "f\252r Fu\223bote") out
                                        && not (contains "name: Keller" out)
                            Left _ -> False)
                    pure (r5a && r5b)
            pure (and [r1, r2, r3, r4, r5])

-- | W5 Stufe 2: alles, was der Schreiber nicht *sicher* tun kann, lehnt er ab —
--   mit benanntem Grund und ohne die Datei anzufassen. Raten ist verboten:
--   ein halb geschriebener Block-Skalar wäre ein zerstörtes Abenteuer.
testYamlDocRefusals :: IO Bool
testYamlDocRefusals = do
    let bytes = BLC.pack ydTestYaml
    case parseYamlDoc bytes of
        Left err -> do
            putStrLn $ "  parse failed: " ++ err
            pure False
        Right doc -> do
            let refuses p = case ydScalarSpan doc p of
                    Left _ -> True
                    Right _ -> False
            r1 <- expectTrue "a mapping is not a scalar" (refuses "rooms.keller")
            r2 <- expectTrue "a sequence is not a scalar" (refuses "items.fass.keys")
            r3 <- expectTrue "a flow mapping is not a scalar" (refuses "rooms.keller.exits.east")
            r4 <- expectTrue "a block scalar is refused" (refuses "items.fass.ascii")
            r5 <- expectTrue "a quoted scalar is refused" (refuses "rooms.keller.desc")
            r6 <- expectTrue "an unknown path is refused" (refuses "rooms.nope.name")
            r7 <- expectTrue "a value with a line break is refused"
                (case setScalarAt doc "items.fass.location" (BLC.pack "a\nb") of
                    Left _ -> True
                    Right _ -> False)
            -- a folded continuation must not be spliced
            let folded = BLC.pack (unlines
                    [ "name: F"
                    , "desc: this value"
                    , "  continues on the next line"
                    , "start_room: x"
                    ])
            r8 <- case parseYamlDoc folded of
                Left _ -> expectTrue "folded parse failed" False
                Right fdoc -> expectTrue "a folded multi-line scalar is refused"
                    (case ydScalarSpan fdoc "desc" of { Left _ -> True; Right _ -> False })
            pure (and [r1,r2,r3,r4,r5,r6,r7,r8])

-- | Ein Dokument mit Mehrbyte-Zeichen *vor* dem Zielwert — genau der Fall, an
--   dem ein Schreiber mit Zeichen- statt Byte-Offsets verrutscht.
umlautYaml :: String
umlautYaml = unlines
    [ "name: Umlaut-Dokument f\252r Fu\223bote"
    , "rooms:"
    , "  - id: keller"
    , "    name: Keller"
    , "    desc: Ein Keller."
    ]

-- | UTF-8-korrekte Bytes. `BLC.pack` kappt auf 8 Bit und wuerde Umlaute zerstoeren.
utf8Bytes :: String -> BLC.ByteString
utf8Bytes = utf8FromStrict . TE.encodeUtf8 . T.pack
  where utf8FromStrict = BL.fromStrict

-- | Enthaelt-Operator fuer die Writer-Erwartungen (BLC hat kein isInfixOf).
contains :: String -> BLC.ByteString -> Bool
contains needle hay = needle `isInfixOf` BLC.unpack hay

-- | Byteweise Teilstring-Pruefung (fuer UTF-8: BLC.unpack zaeuert Bytes, nicht
--   Zeichen, deshalb reicht `contains` dort nicht).
containsBytes :: BLC.ByteString -> BLC.ByteString -> Bool
containsBytes needle hay = BS.isInfixOf (BL.toStrict needle) (BL.toStrict hay)

-- | Ersetzt Zeile @n@ (1-basiert) inhaltlich — nur fuer die Byte-Erwartung im
--   Writer-Test.
replaceIn :: String -> Int -> String -> String -> String
replaceIn content n old new =
    let ls = lines content
    in unlines [ if i == n then replaceOnce old new l else l | (i, l) <- zip [1 ..] ls ]
  where
    replaceOnce o nw l = case breakOn o l of
        Just (pre, post) -> pre ++ nw ++ post
        Nothing -> l
    breakOn needle hay = go "" hay
      where
        go _ [] = Nothing
        go acc s@(c:cs)
            | needle `isPrefixOf` s = Just (reverse acc, drop (length needle) s)
            | otherwise = go (c:acc) cs

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
            , advNPCs = [ ANPC "guard" "Guard" (ACondText "Guard" []) (AAscii (ACondText "" []) [] 0 [] Nothing) [] "loc_0" "alive" Nothing 5 2 Map.empty Map.empty Nothing Map.empty [] Nothing False E.emptyGrammar ] }
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
                roomCheck r = E.PAll [ E.Location E.ActorPlayer r, E.Location (E.ActorNPC "wolf") r ]
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
                    [] loc "alive" Nothing 8 3 Map.empty Map.empty Nothing Map.empty [] Nothing False E.emptyGrammar

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
        (Just 20) 3 1 Map.empty Map.empty party Map.empty [] Nothing False E.emptyGrammar

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
                entry = def >>= Map.lookup (E.PhaseAfter, E.VCustom "follow", "alive") . E.npcVerbMap
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
                    >>= Map.lookup (E.PhaseAfter, E.VCustom "stab", "alive") . E.npcVerbMap)
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
                    b <- expectEqual (Just (E.PAll [E.Location E.ActorPlayer "kestrel_guns"])) (E.trCondition t)
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
    , ("audio outcomes: sfx, music, stop_music (Audio Phase 1/2)", testAudioOutcomes)
    , ("P1-18 check_flag is rejected, has_flag works", testP118CheckFlagRejected)
    , ("P1-19 paid stop cost reaches the engine", testP119StopCost)
    , ("P1-20 raise: fires a custom event", testP120RaiseEvent)
    , ("P2-15 parse error reports reason + position", testParseErrorIsReported)
    , ("P2-21 fuel object form + full tank", testP121FuelSpec)
    , ("P2-18 world name + faction level validation", testP218NameAndLevels)
    , ("in_container at missing item is detected", testInvalidContainerDetected)
    , ("item-on-item (crafting) interaction compiles", testItemInteractionCompiles)
    , ("entity interaction (use on target) compiles", testEntityInteractionCompiles)
    , ("the 6 original genre fixtures compile + validate clean", testGenreFixturesCompile)
    , ("economy genre fixture compiles + validates clean", testEconomyFixtureCompiles)
    , ("deckbuilder genre fixture compiles + validates clean", testDeckbuilderFixtureCompiles)
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
    , ("exact YAML positions resolve dotted issue paths (W5)", testYamlDocPositions)
    , ("the YAML writer is position-exact and byte-faithful (W5)", testYamlDocWriter)
    , ("the YAML writer refuses what it cannot do safely (W5)", testYamlDocRefusals)
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
    -- Phase 2.5: procedures (D2)
    , ("proc: procedures compile and empty procDefs is omitted from world.json", testProceduresCompile)
    , ("proc: call sites are statically checked (UnknownProc, ProcArity)", testProcCallSiteChecks)
    , ("proc: recursion is statically forbidden (ProcRecursion)", testProcRecursionForbidden)
    , ("proc: reserved params and duplicate ids are rejected", testProcParamAndIdChecks)
    , ("proc: call parses from bare name and {proc, args} shapes", testCallParsesFromJson)
    -- B1: content tests as data
    , ("content-tests: parsing and ordered marker semantics", testContentTestBasics)
    , ("content-tests: runner round-trip with pass and fail", testContentTestRunner)
    -- W1: knowledge model
    , ("facts: facts compile in order; empty lists omitted from world.json", testFactsCompile)
    , ("facts: static checks (UnknownFact, DuplicateFact, YieldsWithoutPremises)", testFactChecks)
    , ("facts: knows/learn/forget YAML sugar parses", testKnowsSugar)
    -- W2: progression (XP & Level)
    , ("progression: levels compile, variables merge, empty omitted", testProgressionCompile)
    , ("progression: static checks (EmptyLevels, BadLevelXp, NonMonotonicXp, Clash, Warn)", testProgressionChecks)
    -- W3: chapters
    , ("chapters: compile in order; empty chapterDefs omitted", testChaptersCompile)
    , ("chapters: static checks (Duplicate, Unknown, Backwards, Unreachable)", testChapterChecks)
    , ("chapters: next_chapter/goto_chapter sugar compiles", testChapterSugar)
    -- Pursuit (Tür IV)
    , ("pursuit: step_toward/step_away_from sugar compiles", testPursuitSugar)
    , ("mass ops: damage/move/reveal/consume/set_state sugar (B3)", testMassOpSugar)
    , ("pursuit: section emits sorted on:turn triggers; checks", testPursuitSection)
    -- 5.3: include:
    , ("include: own sections first, then includes (5.3)", testIncludeMerge)
    -- 4.4: containers
    , ("containers: defs, initial states, inventory limit (4.4)", testContainersCompile)
    , ("containers: set_inventory_limit sugar (4.4)", testSetInventoryLimitSugar)
    , ("include: single-value fields are reserved (5.3)", testIncludeForbidden)
    , ("include: duplicate ids name both files (5.3)", testIncludeDuplicates)
    , ("include: transitive, depth-first order (5.3)", testIncludeTransitive)
    , ("include: cycle is a hard error (5.3)", testIncludeCycle)
    , ("include: diamond loads once at first position (5.3)", testIncludeDiamond)
    , ("include: split sources compile byte-identical (5.3)", testIncludeIdenticalWorld)
    -- W4: devices (Hebel / Halterung)
    , ("devices: compile in order; empty deviceDefs omitted", testDevicesCompile)
    , ("devices: static checks (Duplicate, Location, Item, FlipCount, Tag, NoEffects)", testDeviceChecks)
    -- Schritt 2 / Phase 2D: Cards & Deckbuilder
    , ("cards: map syntax and deck count-map compile (Phase 2D)", testCardGameYamlCompilation)
    , ("cards: list syntax and card outcomes compile (Phase 2D)", testCardGameListFormAndOutcomes)
    , ("cards: handLimit compiles to maxHandSize (Phase 2 / S2)", testCardGameHandLimitCompilation)
    , ("cards: unknown card in deck is rejected (Phase 2D)", testCardGameUnknownCardInDeck)
    , ("cards: duplicate card id is rejected (Phase 2D)", testCardGameDuplicateCardId)
    , ("cards: unknown card type and target are rejected (Phase 2D)", testCardGameUnknownTypeAndTarget)
    -- Schritt 3 / Phase 3E: Procedural Sandbox Zones & Dynamic Worldgen
    , ("sandbox: map syntax and biome templates compile (Phase 3E)", testSandboxZoneYamlCompilation)
    , ("sandbox: generate_room outcome compiles (Phase 3E)", testGenerateRoomOutcomeCompilation)
    , ("sandbox: empty biomes in zone is rejected (Phase 3E)", testSandboxZoneEmptyBiomesValidation)
    , ("sandbox: duplicate zone id is rejected (Phase 3E)", testSandboxZoneDuplicateIdValidation)
    , ("sandbox: unknown direction in passable_dirs is rejected (Phase 3E)", testSandboxZoneUnknownDirectionValidation)
    , ("sandbox: wilderness fixture compiles and validates clean (Phase 3E)", testSandboxWildernessFixtureCompiles)
    , ("sandbox: set_var text and biome ascii variants compile (Phase 2 / S3)", testSetTextVarAndBiomeAsciiVariants)
    -- Phase 1: Compiler-Härtung — Unbekannte YAML-Schlüssel warnen
    , ("schema: valid keys compile with zero warnings (Phase 1)", testKnownKeysClean)
    , ("schema: pinned W6 rewards typo produces warning (Phase 1)", testW6RewardsTypoWarningPinned)
    , ("schema: room key typo produces warning with suggestion (Phase 1)", testRoomTypoWarning)
    , ("schema: unknown key without close match produces warning (Phase 1)", testUnknownKeyNoSuggestion)
    , ("schema: top-level typo produces warning with suggestion (Phase 1)", testTopLevelTypoWarning)
    -- Phase 0.3: dark_msg and dark_message schema support
    , ("schema: dark_msg and dark_message compile without warnings (Phase 0.3)", testRoomDarkMsgYamlParsing)
    -- Phase 0.4: validation warnings (keyword collisions, unknown placeholders, dark room dead ends)
    , ("warnings: keyword collision between entities in the same room (Phase 0.4)", testWarningKeywordCollision)
    , ("warnings: unknown variable placeholder in texts (Phase 0.4)", testWarningUnknownPlaceholder)
    , ("warnings: dark room dead end without feelable or lightsource (Phase 0.4)", testWarningDarkRoomDeadEnd)
    -- Phase 2.2: Guarded exit with when-predicate and failure msg; on: before <verb> and block: outcomes
    , ("schema: guarded exit with when predicate and msg compiles (Phase 2.2)", testGuardedExitCompilation)
    , ("schema: on before verb and block outcomes compile (Phase 2.2)", testOnBeforeAndBlockCompilation)
    -- 4.5: conversation sugars
    , ("say_node compiles to set_var dialog_node (4.5)", testSayNodeDialogEndSugar)
    , ("barks: compile to turn triggers with cooldown (4.5)", testBarkTriggerSugar)
    , ("on_talk: compiles to talk.<npc>-trigger (4.5)", testOnTalkTriggerSugar)
    -- B7: NPC possession
    , ("carried_by: compiles to CarriedBy on the npc (B7)", testCarriedByCompiles)
    , ("carried_by: unknown npc is a hard error (B7)", testCarriedByUnknownNpcFails)
    , ("carried_by: with in_container is a conflict error (B7)", testCarriedByConflictFails)
    , ("give: string and object form compile (B7)", testGiveToSugar)
    , ("give: unknown to-npc is a hard error (B7)", testGiveToUnknownNpcFails)
    , ("give: equip: true is a variant of the give-sugar (B9)", testGiveEquipSugar)
    , ("on_complete: is parsed, wired and known (4.6)", testQuestOnCompleteParsed)
    , ("quest references must resolve at compile time (4.6)", testQuestRefErrors)
    , ("initial_flags satisfy the flag validation (4.6)", testInitialFlagsSatisfyValidation)
    , ("map: is parsed, known and written only when set (4.6)", testRoomMapPosRoundTrip)
    , ("the auto-layout is layered and honours anchors (4.6)", testMapLayout)
    , ("two rooms on one cell is a hard error (4.6)", testMapOverlapIsAnError)
    , ("map-set pins a position and keeps the file byte-faithful (4.6)", testMapSetInsertsAndReplaces)
    , ("dead quests: nothing starts it, nothing moves it (4.6)", testQuestDiagnostics)
    , ("the project view is sorted, stable and complete (4.6)", testProjectView)
    , ("the map grid never hides which room is where (4.6)", testMapGridTellsRoomsApart)
    , ("drops_on_death: known key, compiles, json omission (B9)", testDropsOnDeathFlag)
    , ("known keys: carried_by and capacity warn nowhere (B7)", testNpcPossessionKnownKeysClean)
    -- B9: item-on-NPC interactions
    , ("interactions npc: compiles to an outcome map (B9)", testNpcInteractionCompiles)
    , ("interactions npc: unknown item/npc refs fail (B9)", testNpcInteractionRefsFail)
    , ("known keys: interactions npc: warns nowhere (B9)", testNpcInteractionKnownKeysClean)
    -- B6: game export (bundle)
    , ("export: asset refs over every surface incl. nested (B6)", testCollectAssetRefs)
    , ("export: outcome traversal is recursive (B6)", testAllAOutcomesDeep)
    , ("export: bundle files, bytes and warnings (B6)", testExportBundle)
    , ("export: assets is a known key (B6)", testAssetsKnownKey)
    -- B4: Regel-Diagnostik (dead content)
    , ("dead content: trigger events that can never fire (B4)", testUnreachableTriggerEvents)
    , ("dead content: unsatisfiable conditions (B4)", testUnsatisfiableConditions)
    , ("dead content: exits that can never be taken (B4)", testDeadExits)
    , ("dead content: rooms unreachable from start (B4)", testUnreachableRooms)
    -- B5: Content-Fuzzer
    , ("fuzz: generated stream is seed-deterministic (B5)", testFuzzGenDeterministic)
    , ("fuzz: vocabulary comes from the world (B5)", testFuzzVocabExtraction)
    , ("fuzz: run seeds derive per run index (B5)", testFuzzRunSeeds)
    , ("fuzz: frozen-window detector rules (B5)", testFuzzFrozenWindow)
    , ("fuzz: engine exceptions are crash findings (B5)", testFuzzCrashDetection)
    , ("fuzz: non-terminating steps are hang findings (B5)", testFuzzHangDetection)
    , ("fuzz: veto soft-lock is a loop finding (B5)", testFuzzLoopEndToEnd)
    , ("fuzz: clean world fuzzes without findings (B5)", testFuzzSmoke)
    -- B8: named RNG streams
    , ("rng: authored writes on rng.* are hard errors (B8)", testRngVarWriteGuard)
    -- K1: dice pool
    , ("dice pool: roll_dice compile validation rejects die: 1 and invalid pool/keep (K1.1)", testRollDiceValidation)
    , ("dice pool: {var: dice.*} placeholders emit no warnings without declaration (K1.1)", testDicePlaceholderNoWarning)
    , ("verb_map: before:/instead: key phases (4.2)", testVerbMapPhaseKeys)
    , ("verb_map: one phase per (verb, state) pair (4.2)", testVerbMapPhaseClash)
    -- Phase 4.3: language packs (D4)
    , ("lang pack: language and messages compile into the world (4.3)", testLanguagePackCompile)
    , ("lang pack: message override key/value checks (4.3)", testMessageOverrideChecks)
    , ("lang pack: language and messages are known keys (4.3)", testLanguageKnownKeysClean)
    , ("grammar: article/gender compile into the world (4.3.5)", testGrammarFieldsCompile)
    , ("grammar: YAML forms and knownKeys clean (4.3.5)", testGrammarYamlKnownKeysClean)
    , ("grammar: gender tag validation (4.3.5)", testGrammarGenderValidation)
    , ("grammar: MissingGrammar warning for unannotated entities (4.3.5)", testMissingGrammarWarning)
    ]

-- ---------------------------------------------------------------------------
-- Phase 4.3: language packs (D4)
-- ---------------------------------------------------------------------------

-- | `language:` + `messages:` compile into the world's language fields and
--   world.json carries them (omitted when absent — byte-stability contract);
--   an unknown language code is a hard error ('UnknownLanguage').
testLanguagePackCompile :: IO Bool
testLanguagePackCompile = do
    let base = (minAdventure (minRoom "loc_0"))
            { advLanguage = Just "de"
            , advMessages = Map.fromList [("move.ok", "Eigen: {dir}.")] }
    r1 <- case compileAdventure base of
            Left errs -> expectTrue ("language compiles, got: " ++ issuesText errs) False
            Right cr -> do
                a <- expectEqual (Just "de") (E.worldLanguage (crWorld cr))
                b <- expectEqual (Map.fromList [("move.ok", "Eigen: {dir}.")]) (E.worldMessages (crWorld cr))
                c <- expectTrue "world.json carries the language field"
                        ("\"language\"" `isInfixOf` BLC.unpack (Aeson.encode (crWorld cr)))
                d <- expectTrue "world.json carries the messages field"
                        ("\"messages\"" `isInfixOf` BLC.unpack (Aeson.encode (crWorld cr)))
                pure (a && b && c && d)
    r2 <- case compileAdventure (base { advLanguage = Just "xx" }) of
            Left errs -> expectContains "UnknownLanguage" (issuesText errs)
            Right _   -> expectTrue "an unknown language must fail" False
    r3 <- case compileAdventure (minAdventure (minRoom "loc_0")) of
            Left errs -> expectTrue ("plain adventure compiles, got: " ++ issuesText errs) False
            Right cr -> expectTrue "world.json omits language and messages"
                            (not ("\"language\"" `isInfixOf` BLC.unpack (Aeson.encode (crWorld cr)))
                              && not ("\"messages\"" `isInfixOf` BLC.unpack (Aeson.encode (crWorld cr))))
    pure (r1 && r2 && r3)

-- | Message overrides: an unknown catalog key only warns ('UnknownMsgKey' —
--   the override silently does nothing), an empty value is a hard error
--   ('EmptyMessageOverride' — an empty template would change engine behaviour).
testMessageOverrideChecks :: IO Bool
testMessageOverrideChecks = do
    let withMsgs m = (minAdventure (minRoom "loc_0")) { advMessages = Map.fromList m }
    r1 <- case compileAdventure (withMsgs [("no.such.key", "x")]) of
            Left errs -> expectTrue ("unknown key must warn, not fail: " ++ issuesText errs) False
            Right cr -> expectTrue "unknown key warns with UnknownMsgKey"
                            (any (\i -> ciCode i == "UnknownMsgKey") (crWarnings cr))
    r2 <- case compileAdventure (withMsgs [("move.ok", "")]) of
            Left errs -> expectContains "EmptyMessageOverride" (issuesText errs)
            Right _   -> expectTrue "an empty override value must fail" False
    r3 <- case compileAdventure (withMsgs [("move.ok", "Eigen: {dir}.")]) of
            Left errs -> expectTrue ("valid override compiles, got: " ++ issuesText errs) False
            Right cr -> expectTrue "a valid override produces no warnings"
                            (null (filter (\i -> ciCode i `elem` ["UnknownMsgKey", "EmptyMessageOverride"])
                                          (crWarnings cr)))
    pure (r1 && r2 && r3)

-- | `language:` and `messages:` are known top-level keys — no UnknownYamlKey.
testLanguageKnownKeysClean :: IO Bool
testLanguageKnownKeysClean = do
    let yaml = unlines
            [ "start_room: loc_0"
            , "language: de"
            , "messages:"
            , "  move.ok: \"Eigen: {dir}.\""
            , "rooms:"
            , "  - id: loc_0"
            , "    name: Halle"
            , "    desc: Eine Halle."
            ]
    case decode1 (BLC.pack yaml) of
        Left err -> do
            putStrLn $ "  yaml parse failed: " ++ show err
            pure False
        Right (adv :: Adventure) -> case compileAdventure adv of
            Left errs -> do
                putStrLn $ "  compile failed: " ++ issuesText errs
                pure False
            Right cr -> do
                let keyWarns = filter (\i -> ciCode i == "UnknownYamlKey") (crWarnings cr)
                a <- expectTrue ("expected 0 unknown key warnings, got " ++ show (length keyWarns)) (null keyWarns)
                b <- expectEqual (Just "de") (E.worldLanguage (crWorld cr))
                pure (a && b)

-- ---------------------------------------------------------------------------
-- Phase 4.3.5: grammar fields article:/gender: (Variante A)
-- ---------------------------------------------------------------------------

-- | 4.3.5: `article:` (short form = nominative, object form = nom/acc/dat)
--   and `gender:` compile into the item/NPC grammar; world.json carries them
--   and omits them when empty (byte contract).
testGrammarFieldsCompile :: IO Bool
testGrammarFieldsCompile = do
    let item = (minItem "i") { aiGrammar = E.Grammar (Just "der") (Just "den") (Just "dem") (Just "m") }
        npc = (minNpcKey "n") { anGrammar = E.Grammar (Just "die") Nothing Nothing Nothing }
        adv = (minAdventure (minRoom "loc_0")) { advItems = [item], advNPCs = [npc] }
    r1 <- case compileAdventure adv of
            Left errs -> expectTrue ("grammar fields compile, got: " ++ issuesText errs) False
            Right cr -> do
                let it = E.itemDefs (crWorld cr) Map.! "i"
                    ni = E.npcDefs (crWorld cr) Map.! "n"
                a <- expectEqual (Just "den") (E.gAcc (E.itemGrammar it))
                b <- expectEqual (Just "die") (E.gNom (E.npcGrammar ni))
                c <- expectTrue "world.json carries article and gender"
                        (all (`isInfixOf` BLC.unpack (Aeson.encode (crWorld cr)))
                             ["\"article\"", "\"gender\"", "\"acc\":\"den\""])
                pure (a && b && c)
    r2 <- case compileAdventure (minAdventure (minRoom "loc_0")) of
            Left errs -> expectTrue ("plain adventure compiles, got: " ++ issuesText errs) False
            Right cr -> expectTrue "world.json omits article and gender"
                            (not ("\"article\"" `isInfixOf` BLC.unpack (Aeson.encode (crWorld cr)))
                              && not ("\"gender\"" `isInfixOf` BLC.unpack (Aeson.encode (crWorld cr))))
    pure (r1 && r2)

-- | 4.3.5: YAML accepts both `article:` forms and `gender:` on items and NPCs
--   (knownKeys clean — no UnknownYamlKey).
testGrammarYamlKnownKeysClean :: IO Bool
testGrammarYamlKnownKeysClean = do
    let yaml = unlines
            [ "start_room: loc_0"
            , "rooms:"
            , "  - id: loc_0"
            , "    name: Halle"
            , "    desc: Eine Halle."
            , "items:"
            , "  - id: schwert"
            , "    name: Schwert"
            , "    desc: Ein Schwert."
            , "    location: loc_0"
            , "    article:"
            , "      nom: das"
            , "      acc: das"
            , "      dat: dem"
            , "    gender: n"
            , "npcs:"
            , "  - id: waechter"
            , "    name: Waechter"
            , "    desc: Ein Waechter."
            , "    location: loc_0"
            , "    article: der"
            , "    gender: m"
            ]
    case decode1 (BLC.pack yaml) of
        Left err -> do
            putStrLn $ "  yaml parse failed: " ++ show err
            pure False
        Right (adv :: Adventure) -> case compileAdventure adv of
            Left errs -> do
                putStrLn $ "  compile failed: " ++ issuesText errs
                pure False
            Right cr -> do
                let keyWarns = filter (\i -> ciCode i == "UnknownYamlKey") (crWarnings cr)
                    it = E.itemDefs (crWorld cr) Map.! "schwert"
                    ni = E.npcDefs (crWorld cr) Map.! "waechter"
                a <- expectTrue ("expected 0 unknown key warnings, got " ++ show (length keyWarns)) (null keyWarns)
                b <- expectEqual (E.Grammar (Just "das") (Just "das") (Just "dem") (Just "n")) (E.itemGrammar it)
                c <- expectEqual (E.Grammar (Just "der") Nothing Nothing (Just "m")) (E.npcGrammar ni)
                pure (a && b && c)

-- | 4.3.5: `gender:` is a closed tag set — an unknown value is a hard error.
testGrammarGenderValidation :: IO Bool
testGrammarGenderValidation = do
    let item = (minItem "i") { aiGrammar = E.Grammar Nothing Nothing Nothing (Just "x") }
        npc = (minNpcKey "n") { anGrammar = E.Grammar Nothing Nothing Nothing (Just "d") }
        adv = (minAdventure (minRoom "loc_0")) { advItems = [item], advNPCs = [npc] }
    r1 <- case compileAdventure adv of
            Left errs -> expectContains "InvalidGender" (issuesText errs)
            Right _  -> expectTrue "an unknown gender tag must fail" False
    r2 <- case compileAdventure (adv { advNPCs = [] }) of
            Left errs -> expectTrue "the issue points at the item field"
                            (any (\i -> ciCode i == "InvalidGender" && ciPath i == "items.i.gender") errs)
            Right _  -> expectTrue "an unknown gender tag must fail" False
    pure (r1 && r2)

-- | 4.3.5: 'MissingGrammar' warns for unannotated items/NPCs when the
--   effective catalog templates reference the grammar placeholders
--   (language-pack worlds only); annotated entities stay silent, and so do
--   worlds without `language:`.
testMissingGrammarWarning :: IO Bool
testMissingGrammarWarning = do
    let langAdv = (minAdventure (minRoom "loc_0"))
            { advItems = [minItem "i"], advNPCs = [minNpcKey "n"], advLanguage = Just "de" }
        annotated = (minAdventure (minRoom "loc_0"))
            { advItems = [(minItem "i") { aiGrammar = E.Grammar (Just "der") Nothing Nothing Nothing }]
            , advNPCs = [(minNpcKey "n") { anGrammar = E.Grammar (Just "die") Nothing Nothing Nothing }]
            , advLanguage = Just "de" }
        plain = (minAdventure (minRoom "loc_0"))
            { advItems = [minItem "i"], advNPCs = [minNpcKey "n"] }
        warns c = map ciCode (filter (\i -> ciCode i == "MissingGrammar") (crWarnings c))
    r1 <- case compileAdventure langAdv of
            Left errs -> expectTrue ("compiles, got: " ++ issuesText errs) False
            Right cr -> expectTrue "unannotated entities warn in a language world"
                            (length (warns cr) == 2)
    r2 <- case compileAdventure annotated of
            Left errs -> expectTrue ("compiles, got: " ++ issuesText errs) False
            Right cr -> expectTrue "annotated entities stay silent"
                            (null (warns cr))
    r3 <- case compileAdventure plain of
            Left errs -> expectTrue ("compiles, got: " ++ issuesText errs) False
            Right cr -> expectTrue "no language: no warning"
                            (null (warns cr))
    pure (r1 && r2 && r3)

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
    r1 <- case compileAdventure (withHook [AOApplyCondition "cooldown_slash" 2 [] [] False]) of
            Left errs -> expectContains "CooldownConditionClash" (issuesText errs)
            Right _   -> expectTrue "expected CooldownConditionClash for apply_condition" False
    r2 <- case compileAdventure (withHook [AOClearCondition "cooldown_slash"]) of
            Left errs -> expectContains "CooldownConditionClash" (issuesText errs)
            Right _   -> expectTrue "expected CooldownConditionClash for clear_condition" False
    -- an ordinary condition name is unaffected (control case)
    r3 <- case compileAdventure (withHook [AOApplyCondition "poisoned" 3 [] [] False]) of
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
                (s, _r4) = shuffle [1 .. 20 :: Int] r3
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
        Right (adv, _warns) -> case compileAdventure adv of
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
                        Just it -> Map.member (E.PhaseAfter, E.VTake, "intact") (E.itemVerbMap it)
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
                    Nothing Nothing Nothing Nothing Nothing E.emptyAscii Nothing Nothing Nothing
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

-- ---------------------------------------------------------------------------
-- Cards & Deck Tests (Schritt 2 / Phase 2D)
-- ---------------------------------------------------------------------------

-- | Test compiling YAML with cards (map syntax) and deck (count-map syntax).
testCardGameYamlCompilation :: IO Bool
testCardGameYamlCompilation = do
    let yaml = unlines
            [ "name: Deck Adventure"
            , "start_room: arena"
            , "rooms:"
            , "  - id: arena"
            , "    name: Kampfarena"
            , "    texts: Der Kampf beginnt."
            , "cards:"
            , "  strike:"
            , "    name: Schlag"
            , "    cost: { energy: 1 }"
            , "    type: attack"
            , "    target: single_enemy"
            , "    description: Fuegt 6 Schaden zu."
            , "    outcomes:"
            , "      - damage_enemy: { target: chosen, amount: 6 }"
            , "  defend:"
            , "    name: Verteidigung"
            , "    cost: { energy: 1 }"
            , "    type: skill"
            , "    target: self"
            , "    description: Erhoeht Block um 5."
            , "    outcomes:"
            , "      - add_var: { variable: block, delta: 5 }"
            , "  tactics:"
            , "    name: Taktik"
            , "    cost: {}"
            , "    type: skill"
            , "    target: self"
            , "    description: Ziehe 2 Karten."
            , "    outcomes:"
            , "      - draw_cards: 2"
            , "deck:"
            , "  strike: 3"
            , "  defend: 2"
            , "  tactics: 1"
            ]
    case decode1 (BLC.pack yaml) of
        Left err -> do
            putStrLn $ "  yaml parse failed: " ++ show err
            pure False
        Right (adv :: Adventure) -> case compileAdventure adv of
            Left errs -> do
                putStrLn $ "  compile failed: " ++ issuesText errs
                pure False
            Right cr -> do
                let w = crWorld cr
                    s = crSave cr
                    cDefs = E.cardDefs w
                r1 <- expectEqual 3 (Map.size cDefs)
                r2 <- expectTrue "strike is attack" (maybe False (\c -> E.cardType c == E.CardAttack) (Map.lookup "strike" cDefs))
                r3 <- expectTrue "defend is skill" (maybe False (\c -> E.cardType c == E.CardSkill) (Map.lookup "defend" cDefs))
                r4 <- expectTrue "strike target is SingleEnemy" (maybe False (\c -> E.cardTarget c == E.TargetSingleEnemy) (Map.lookup "strike" cDefs))
                r5 <- expectTrue "deckState initialized" (case E.deckState s of
                    Nothing -> False
                    Just ds -> length (E.drawPile ds) == 6 && null (E.hand ds) && null (E.discardPile ds))
                pure (r1 && r2 && r3 && r4 && r5)

-- | Test compiling cards (list syntax), player.deck, and card outcomes (draw, discard, exhaust, add_card, shuffle).
testCardGameListFormAndOutcomes :: IO Bool
testCardGameListFormAndOutcomes = do
    let yaml = unlines
            [ "name: List Deck Adventure"
            , "start_room: arena"
            , "rooms:"
            , "  - id: arena"
            , "    name: Arena"
            , "    texts: Arena."
            , "player:"
            , "  deck: [bash, sweep, utility]"
            , "cards:"
            , "  - id: bash"
            , "    name: Schmettern"
            , "    cost: { energy: 2 }"
            , "    type: attack"
            , "    target: enemy"
            , "    description: 8 Schaden."
            , "    outcomes:"
            , "      - damage_enemy: { target: chosen, amount: 8 }"
            , "  - id: sweep"
            , "    name: Rundumschlag"
            , "    cost: { energy: 1 }"
            , "    type: attack"
            , "    target: all_enemies"
            , "    description: 4 Schaden an alle Feinde."
            , "    outcomes:"
            , "      - damage_all_enemies: 4"
            , "  - id: utility"
            , "    name: Nuetzlich"
            , "    cost: {}"
            , "    type: skill"
            , "    target: self"
            , "    description: Verschiedene Karteneffekte."
            , "    outcomes:"
            , "      - draw_cards: 2"
            , "      - discard_hand: true"
            , "      - discard_card: bash"
            , "      - exhaust_card: bash"
            , "      - add_card: { card: bash, destination: discard }"
            , "      - shuffle_deck: true"
            ]
    case decode1 (BLC.pack yaml) of
        Left err -> do
            putStrLn $ "  yaml parse failed: " ++ show err
            pure False
        Right (adv :: Adventure) -> case compileAdventure adv of
            Left errs -> do
                putStrLn $ "  compile failed: " ++ issuesText errs
                pure False
            Right cr -> do
                let w = crWorld cr
                    s = crSave cr
                    cDefs = E.cardDefs w
                r1 <- expectEqual 3 (Map.size cDefs)
                r2 <- expectTrue "deck has 3 cards" (case E.deckState s of
                    Nothing -> False
                    Just ds -> E.drawPile ds == ["bash", "sweep", "utility"])
                let mUtil = Map.lookup "utility" cDefs
                    expectedEffects =
                        [ E.DrawCards 2
                        , E.DiscardHand
                        , E.DiscardCard "bash"
                        , E.ExhaustCard "bash"
                        , E.AddCardToDeck "bash" E.DestDiscard
                        , E.ShuffleDeck
                        ]
                r3 <- expectTrue "utility effects match" (maybe False (\c -> E.cardEffects c == expectedEffects) mUtil)
                let mSweep = Map.lookup "sweep" cDefs
                r4 <- expectTrue "sweep has TargetAllEnemies" (maybe False (\c -> E.cardTarget c == E.TargetAllEnemies) mSweep)
                pure (r1 && r2 && r3 && r4)

-- | Phase 2 / S2: Test handLimit compilation (top-level, deck-level, and absent).
testCardGameHandLimitCompilation :: IO Bool
testCardGameHandLimitCompilation = do
    let yamlTop = unlines
            [ "name: Top Limit"
            , "start_room: arena"
            , "rooms: [{id: arena, name: Arena}]"
            , "cards: {strike: {name: Schlag, type: attack}}"
            , "deck: [strike, strike]"
            , "handLimit: 5"
            ]
        yamlDeck = unlines
            [ "name: Deck Limit"
            , "start_room: arena"
            , "rooms: [{id: arena, name: Arena}]"
            , "cards: {strike: {name: Schlag, type: attack}}"
            , "deck:"
            , "  handLimit: 4"
            , "  strike: 3"
            ]
        yamlNoLimit = unlines
            [ "name: No Limit"
            , "start_room: arena"
            , "rooms: [{id: arena, name: Arena}]"
            , "cards: {strike: {name: Schlag, type: attack}}"
            , "deck: [strike]"
            ]
    res1 <- case decode1 (BLC.pack yamlTop) of
        Right (adv :: Adventure) -> case compileAdventure adv of
            Right cr -> pure (maybe 0 E.maxHandSize (E.deckState (crSave cr)) == 5)
            _        -> pure False
        _ -> pure False
    res2 <- case decode1 (BLC.pack yamlDeck) of
        Right (adv :: Adventure) -> case compileAdventure adv of
            Right cr -> pure (maybe 0 E.maxHandSize (E.deckState (crSave cr)) == 4)
            _        -> pure False
        _ -> pure False
    res3 <- case decode1 (BLC.pack yamlNoLimit) of
        Right (adv :: Adventure) -> case compileAdventure adv of
            Right cr -> pure (maybe (-1) E.maxHandSize (E.deckState (crSave cr)) == 0)
            _        -> pure False
        _ -> pure False
    r1 <- expectTrue "top-level handLimit compiles to maxHandSize 5" res1
    r2 <- expectTrue "deck-level handLimit compiles to maxHandSize 4" res2
    r3 <- expectTrue "no handLimit compiles to maxHandSize 0 (unlimited)" res3
    pure (r1 && r2 && r3)

-- | Test validation: unknown card referenced in deck.
testCardGameUnknownCardInDeck :: IO Bool
testCardGameUnknownCardInDeck = do
    let yaml = unlines
            [ "name: Bad Deck"
            , "start_room: arena"
            , "rooms:"
            , "  - id: arena"
            , "    name: Arena"
            , "    texts: Arena."
            , "cards:"
            , "  strike:"
            , "    name: Schlag"
            , "    type: attack"
            , "deck: [strike, phantom_card]"
            ]
    case decode1 (BLC.pack yaml) of
        Left err -> do
            putStrLn $ "  yaml parse failed: " ++ show err
            pure False
        Right (adv :: Adventure) -> case compileAdventure adv of
            Left errs -> expectTrue "UnknownCardInDeck error found"
                            (any (\i -> ciCode i == "UnknownCardInDeck") errs)
            Right _ -> expectTrue "expected compile failure for unknown card in deck" False

-- | Test validation: duplicate card ID detected.
testCardGameDuplicateCardId :: IO Bool
testCardGameDuplicateCardId = do
    let yaml = unlines
            [ "name: Duplicate Cards"
            , "start_room: arena"
            , "rooms:"
            , "  - id: arena"
            , "    name: Arena"
            , "    texts: Arena."
            , "cards:"
            , "  - id: strike"
            , "    name: Schlag 1"
            , "  - id: strike"
            , "    name: Schlag 2"
            ]
    case decode1 (BLC.pack yaml) of
        Left err -> do
            putStrLn $ "  yaml parse failed: " ++ show err
            pure False
        Right (adv :: Adventure) -> case compileAdventure adv of
            Left errs -> expectTrue "DuplicateCardId error found"
                            (any (\i -> ciCode i == "DuplicateCardId") errs)
            Right _ -> expectTrue "expected compile failure for duplicate card id" False

-- | Test validation: unknown card type and target detected.
testCardGameUnknownTypeAndTarget :: IO Bool
testCardGameUnknownTypeAndTarget = do
    let yaml = unlines
            [ "name: Bad Card"
            , "start_room: arena"
            , "rooms:"
            , "  - id: arena"
            , "    name: Arena"
            , "    texts: Arena."
            , "cards:"
            , "  weird:"
            , "    name: Seltsam"
            , "    type: cosmic"
            , "    target: galaxy"
            ]
    case decode1 (BLC.pack yaml) of
        Left err -> do
            putStrLn $ "  yaml parse failed: " ++ show err
            pure False
        Right (adv :: Adventure) -> case compileAdventure adv of
            Left errs -> do
                r1 <- expectTrue "UnknownCardType found" (any (\i -> ciCode i == "UnknownCardType") errs)
                r2 <- expectTrue "UnknownCardTarget found" (any (\i -> ciCode i == "UnknownCardTarget") errs)
                pure (r1 && r2)
            Right _ -> expectTrue "expected compile failure for unknown card type and target" False

-- ---------------------------------------------------------------------------
-- Procedural Sandbox Zones & Dynamic Worldgen Tests (Schritt 3 / Phase 3E)
-- ---------------------------------------------------------------------------

-- | Test compiling YAML with sandbox_zones (map syntax) and verify GameWorld.sandboxZones.
testSandboxZoneYamlCompilation :: IO Bool
testSandboxZoneYamlCompilation = do
    let yaml = unlines
            [ "name: Sandbox Test"
            , "start_room: camp"
            , "rooms:"
            , "  - id: camp"
            , "    name: Base Camp"
            , "    texts: Base camp."
            , "    exits:"
            , "      north: sandbox_wildnis"
            , "sandbox_zones:"
            , "  wildnis:"
            , "    origin: [10, 20, 0]"
            , "    floor: 2"
            , "    biomes:"
            , "      - id: forest"
            , "        weight: 70"
            , "        name_pattern: 'Wald ({x}, {y})'"
            , "        description: 'Dichter Nadelwald.'"
            , "        tags: ['forest', 'outdoor']"
            , "        passable_dirs: ['north', 'south', 'east', 'west']"
            , "      - id: cliff"
            , "        weight: 30"
            , "        name_pattern: 'Klippe ({x}, {y})'"
            , "        description: 'Felswand.'"
            , "        tags: ['rock']"
            , "        passable_dirs: ['south', 'east']"
            ]
    case decode1 (BLC.pack yaml) of
        Left err -> do
            putStrLn $ "  yaml parse failed: " ++ show err
            pure False
        Right (adv :: Adventure) -> case compileAdventure adv of
            Left errs -> do
                putStrLn $ "  compile failed: " ++ issuesText errs
                pure False
            Right cr -> do
                let w = crWorld cr
                    zones = E.sandboxZones w
                r1 <- expectEqual 1 (Map.size zones)
                case Map.lookup "wildnis" zones of
                    Nothing -> expectTrue "wildnis zone found" False
                    Just sz -> do
                        b1 <- expectEqual (10, 20, 0) (E.szOrigin sz)
                        b2 <- expectEqual (Just 2) (E.szDefaultFloor sz)
                        b3 <- expectEqual 2 (length (E.szBiomes sz))
                        let bForest = head (E.szBiomes sz)
                        b4 <- expectEqual "forest" (E.btId bForest)
                        b5 <- expectEqual 70 (E.btWeight bForest)
                        b6 <- expectEqual "Wald ({x}, {y})" (E.btNamePattern bForest)
                        b7 <- expectEqual [E.North, E.South, E.East, E.West] (E.btPassableDirs bForest)
                        let bCliff = E.szBiomes sz !! 1
                        b8 <- expectEqual "cliff" (E.btId bCliff)
                        b9 <- expectEqual 30 (E.btWeight bCliff)
                        b10 <- expectEqual [E.South, E.East] (E.btPassableDirs bCliff)
                        pure (r1 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10)

-- | Test compiling generate_room action outcome in rules.
testGenerateRoomOutcomeCompilation :: IO Bool
testGenerateRoomOutcomeCompilation = do
    let yaml = unlines
            [ "name: Dig Test"
            , "start_room: entrance"
            , "verbs:"
            , "  - name: graben"
            , "rooms:"
            , "  - id: entrance"
            , "    name: Entrance"
            , "    texts: Entrance."
            , "rules:"
            , "  - id: dig_rule"
            , "    on: command graben"
            , "    effects:"
            , "      - generate_room:"
            , "          id: 'mine_{turn.count}'"
            , "          name: 'Tiefe Mine'"
            , "          description: 'Gegrabener Stollen.'"
            , "          connect_from: 'current_room'"
            , "          direction: 'down'"
            , "          return_direction: 'up'"
            ]
    case decode1 (BLC.pack yaml) of
        Left err -> do
            putStrLn $ "  yaml parse failed: " ++ show err
            pure False
        Right (adv :: Adventure) -> case compileAdventure adv of
            Left errs -> do
                putStrLn $ "  compile failed: " ++ issuesText errs
                pure False
            Right cr -> do
                let w = crWorld cr
                    trigs = E.triggerDefs w
                case trigs of
                    [t] -> do
                        let expected = E.GenerateRoom "mine_{turn.count}" "Tiefe Mine" "Gegrabener Stollen." "current_room" E.Down E.Up
                        expectEqual [expected] (E.trEffects t)
                    _ -> expectTrue "expected exactly 1 trigger" False

-- | Test validation: empty biomes in sandbox zone is rejected.
testSandboxZoneEmptyBiomesValidation :: IO Bool
testSandboxZoneEmptyBiomesValidation = do
    let yaml = unlines
            [ "name: Bad Zone"
            , "start_room: camp"
            , "rooms:"
            , "  - id: camp"
            , "    name: Camp"
            , "    texts: Camp."
            , "sandbox_zones:"
            , "  empty_zone:"
            , "    origin: [0, 0, 0]"
            , "    biomes: []"
            ]
    case decode1 (BLC.pack yaml) of
        Left err -> do
            putStrLn $ "  yaml parse failed: " ++ show err
            pure False
        Right (adv :: Adventure) -> case compileAdventure adv of
            Left errs -> expectTrue "EmptySandboxZone error found"
                            (any (\i -> ciCode i == "EmptySandboxZone") errs)
            Right _ -> expectTrue "expected compile failure for empty biomes in sandbox zone" False

-- | Test validation: duplicate sandbox zone id is rejected.
testSandboxZoneDuplicateIdValidation :: IO Bool
testSandboxZoneDuplicateIdValidation = do
    let yaml = unlines
            [ "name: Dup Zone"
            , "start_room: camp"
            , "rooms:"
            , "  - id: camp"
            , "    name: Camp"
            , "    texts: Camp."
            , "sandbox_zones:"
            , "  - id: wildnis"
            , "    biomes:"
            , "      - id: b1"
            , "  - id: wildnis"
            , "    biomes:"
            , "      - id: b2"
            ]
    case decode1 (BLC.pack yaml) of
        Left err -> do
            putStrLn $ "  yaml parse failed: " ++ show err
            pure False
        Right (adv :: Adventure) -> case compileAdventure adv of
            Left errs -> expectTrue "DuplicateSandboxZone error found"
                            (any (\i -> ciCode i == "DuplicateSandboxZone") errs)
            Right _ -> expectTrue "expected compile failure for duplicate sandbox zone" False

-- | Test validation: unknown direction in passable_dirs is rejected.
testSandboxZoneUnknownDirectionValidation :: IO Bool
testSandboxZoneUnknownDirectionValidation = do
    let yaml = unlines
            [ "name: Bad Dir"
            , "start_room: camp"
            , "rooms:"
            , "  - id: camp"
            , "    name: Camp"
            , "    texts: Camp."
            , "sandbox_zones:"
            , "  wildnis:"
            , "    biomes:"
            , "      - id: b1"
            , "        passable_dirs: ['nowhere']"
            ]
    case decode1 (BLC.pack yaml) of
        Left err -> do
            putStrLn $ "  yaml parse failed: " ++ show err
            pure False
        Right (adv :: Adventure) -> case compileAdventure adv of
            Left errs -> expectTrue "UnknownDirection error found"
                            (any (\i -> ciCode i == "UnknownDirection") errs)
            Right _ -> expectTrue "expected compile failure for unknown direction" False

-- | Test compiling set_var with text value and biome ascii variants (Phase 2 / S3).
testSetTextVarAndBiomeAsciiVariants :: IO Bool
testSetTextVarAndBiomeAsciiVariants = do
    let yaml = unlines
            [ "name: Variant Test"
            , "start_room: camp"
            , "variables:"
            , "  - name: weather"
            , "    type: text"
            , "    initial: sonne"
            , "verbs:"
            , "  - name: rasten"
            , "rooms:"
            , "  - id: camp"
            , "    name: Camp"
            , "    texts: Camp."
            , "    exits:"
            , "      north: sandbox_wildnis"
            , "rules:"
            , "  - id: rule_rain"
            , "    on: command rasten"
            , "    effects:"
            , "      - set_var: { var: weather, value: regen }"
            , "sandbox_zones:"
            , "  wildnis:"
            , "    origin: [0, 0, 0]"
            , "    biomes:"
            , "      - id: forest"
            , "        weight: 100"
            , "        name_pattern: 'Wald ({x}, {y})'"
            , "        ascii_art:"
            , "          default: 'DAY ({x}, {y})'"
            , "          variants:"
            , "            - when: { var: weather, is: regen }"
            , "              text: 'RAIN ({x}, {y})'"
            ]
    case decode1 (BLC.pack yaml) of
        Left err -> do
            putStrLn $ "  yaml parse failed: " ++ show err
            pure False
        Right (adv :: Adventure) -> case compileAdventure adv of
            Left errs -> do
                putStrLn $ "  compile failed: " ++ issuesText errs
                pure False
            Right cr -> do
                let w = crWorld cr
                    trigs = E.triggerDefs w
                    zones = E.sandboxZones w
                r1 <- case trigs of
                    [t] -> expectEqual [E.SetValue (E.VRVariable "weather") (E.EVString "regen")] (E.trEffects t)
                    _   -> expectTrue "expected 1 trigger" False
                case Map.lookup "wildnis" zones of
                    Nothing -> expectTrue "wildnis zone found" False
                    Just sz -> do
                        let b = head (E.szBiomes sz)
                            art = E.btAsciiArt b
                            stat = E.aaStatic art
                        r2 <- expectEqual "DAY ({x}, {y})" (E.ctDefault stat)
                        r3 <- expectEqual 1 (length (E.ctVariants stat))
                        let v = head (E.ctVariants stat)
                        r4 <- expectEqual (E.VarIs "weather" "regen") (E.tvWhen v)
                        r5 <- expectEqual "RAIN ({x}, {y})" (E.tvText v)
                        pure (r1 && r2 && r3 && r4 && r5)

-- ===========================================================================
-- Phase 1: Compiler-Härtung — Unbekannte YAML-Schlüssel warnen
-- ===========================================================================

testKnownKeysClean :: IO Bool
testKnownKeysClean = do
    let yaml = unlines
            [ "start_room: start"
            , "rooms:"
            , "  - id: start"
            , "    name: Start Room"
            , "    desc: A clean room."
            , "    exits:"
            , "      north: start"
            , "    tags: [safe]"
            , "quests:"
            , "  - id: sample_quest"
            , "    name: Sample"
            , "    desc: Just a test."
            , "    reward:"
            , "      - { msg: 'Done!' }"
            ]
    case decode1 (BLC.pack yaml) of
        Left err -> do
            putStrLn $ "  yaml parse failed: " ++ show err
            pure False
        Right (adv :: Adventure) -> case compileAdventure adv of
            Left errs -> do
                putStrLn $ "  compile failed: " ++ show errs
                pure False
            Right cr -> do
                let keyWarns = filter (\i -> ciCode i == "UnknownYamlKey") (crWarnings cr)
                expectTrue ("expected 0 unknown key warnings, got " ++ show (length keyWarns)) (null keyWarns)

-- | Pinned W6 test case: 'rewards:' (plural) instead of 'reward:' (singular) on a quest
-- produces an UnknownYamlKey warning pointing at quests.find_hermit with 'did you mean reward?'.
testW6RewardsTypoWarningPinned :: IO Bool
testW6RewardsTypoWarningPinned = do
    let yaml = unlines
            [ "start_room: start"
            , "rooms:"
            , "  - id: start"
            , "    name: Start Room"
            , "    desc: Starting point."
            , "quests:"
            , "  - id: find_hermit"
            , "    name: The Lost Hermit"
            , "    desc: Find the hermit in the forest."
            , "    rewards:"
            , "      - { msg: 'Hermit found!' }"
            ]
    case decode1 (BLC.pack yaml) of
        Left err -> do
            putStrLn $ "  yaml parse failed: " ++ show err
            pure False
        Right (adv :: Adventure) -> case compileAdventure adv of
            Left errs -> do
                putStrLn $ "  compile failed unexpectedly: " ++ show errs
                pure False
            Right cr -> do
                let warns = filter (\i -> ciCode i == "UnknownYamlKey") (crWarnings cr)
                r1 <- expectEqual 1 (length warns)
                case listToMaybe warns of
                    Nothing -> pure False
                    Just w -> do
                        r2 <- expectEqual "quests.find_hermit" (ciPath w)
                        r3 <- expectEqual SWarning (ciSeverity w)
                        r4 <- expectEqual "'rewards' is not a known key - did you mean 'reward'?" (ciMessage w)
                        let mLine = lineForPath yaml (ciPath w)
                        r5 <- expectTrue "lineForPath finds source line" (case mLine of Just (n, _) -> n > 0; Nothing -> False)
                        pure (r1 && r2 && r3 && r4 && r5)

testRoomTypoWarning :: IO Bool
testRoomTypoWarning = do
    let yaml = unlines
            [ "start_room: start"
            , "rooms:"
            , "  - id: start"
            , "    name: Start Room"
            , "    desc: Test."
            , "    exitz:"
            , "      north: start"
            ]
    case decode1 (BLC.pack yaml) of
        Left err -> do
            putStrLn $ "  yaml parse failed: " ++ show err
            pure False
        Right (adv :: Adventure) -> case compileAdventure adv of
            Left errs -> do
                putStrLn $ "  compile failed unexpectedly: " ++ show errs
                pure False
            Right cr -> do
                let warns = filter (\i -> ciCode i == "UnknownYamlKey") (crWarnings cr)
                r1 <- expectEqual 1 (length warns)
                case listToMaybe warns of
                    Nothing -> pure False
                    Just w -> do
                        r2 <- expectEqual "rooms.start" (ciPath w)
                        r3 <- expectEqual "'exitz' is not a known key - did you mean 'exits'?" (ciMessage w)
                        pure (r1 && r2 && r3)

testUnknownKeyNoSuggestion :: IO Bool
testUnknownKeyNoSuggestion = do
    let yaml = unlines
            [ "start_room: start"
            , "rooms:"
            , "  - id: start"
            , "    name: Start Room"
            , "    desc: Test."
            , "    foobar_unknown_field: 123"
            ]
    case decode1 (BLC.pack yaml) of
        Left err -> do
            putStrLn $ "  yaml parse failed: " ++ show err
            pure False
        Right (adv :: Adventure) -> case compileAdventure adv of
            Left errs -> do
                putStrLn $ "  compile failed unexpectedly: " ++ show errs
                pure False
            Right cr -> do
                let warns = filter (\i -> ciCode i == "UnknownYamlKey") (crWarnings cr)
                r1 <- expectEqual 1 (length warns)
                case listToMaybe warns of
                    Nothing -> pure False
                    Just w -> do
                        r2 <- expectEqual "rooms.start" (ciPath w)
                        r3 <- expectEqual "'foobar_unknown_field' is not a known key" (ciMessage w)
                        pure (r1 && r2 && r3)

testTopLevelTypoWarning :: IO Bool
testTopLevelTypoWarning = do
    let yaml = unlines
            [ "start_room: start"
            , "rooms:"
            , "  - id: start"
            , "    name: Start Room"
            , "    desc: Test."
            , "quest:"
            , "  - id: q1"
            , "    name: Q1"
            ]
    case decode1 (BLC.pack yaml) of
        Left err -> do
            putStrLn $ "  yaml parse failed: " ++ show err
            pure False
        Right (adv :: Adventure) -> case compileAdventure adv of
            Left errs -> do
                putStrLn $ "  compile failed unexpectedly: " ++ show errs
                pure False
            Right cr -> do
                let warns = filter (\i -> ciCode i == "UnknownYamlKey") (crWarnings cr)
                r1 <- expectEqual 1 (length warns)
                case listToMaybe warns of
                    Nothing -> pure False
                    Just w -> do
                        r2 <- expectEqual "quest" (ciPath w)
                        r3 <- expectEqual "'quest' is not a known key - did you mean 'quests'?" (ciMessage w)
                        pure (r1 && r2 && r3)

-- | Phase 0.3: Room dark message (dark_msg / dark_message) compiles cleanly with zero warnings
testRoomDarkMsgYamlParsing :: IO Bool
testRoomDarkMsgYamlParsing = do
    let yaml = unlines
            [ "start_room: room1"
            , "rooms:"
            , "  - id: room1"
            , "    name: Dark Room 1"
            , "    desc: Pitch dark."
            , "    tags: [dark]"
            , "    dark_msg: 'Pitch black darkness.'"
            , "    exits:"
            , "      north: room2"
            , "  - id: room2"
            , "    name: Dark Room 2"
            , "    desc: Pitch dark again."
            , "    tags: [dark]"
            , "    dark_message: 'Total gloom.'"
            , "    exits:"
            , "      south: room1"
            ]
    case decode1 (BLC.pack yaml) of
        Left err -> do
            putStrLn $ "  yaml parse failed: " ++ show err
            pure False
        Right (adv :: Adventure) -> case compileAdventure adv of
            Left errs -> do
                putStrLn $ "  compile failed: " ++ show errs
                pure False
            Right cr -> do
                let keyWarns = filter (\i -> ciCode i == "UnknownYamlKey") (crWarnings cr)
                r1 <- expectTrue ("expected 0 unknown key warnings, got " ++ show (length keyWarns)) (null keyWarns)
                let gw = crWorld cr
                    rm1 = Map.lookup "room1" (E.rooms gw)
                    rm2 = Map.lookup "room2" (E.rooms gw)
                r2 <- expectEqual (Just "Pitch black darkness.") (rm1 >>= E.roomDarkMsg)
                r3 <- expectEqual (Just "Total gloom.") (rm2 >>= E.roomDarkMsg)
                pure (r1 && r2 && r3)

-- ---------------------------------------------------------------------------
-- Phase 0.4: Validation warnings tests
-- ---------------------------------------------------------------------------

-- | Phase 0.4: Keyword collision between items/NPCs in the same room is a non-fatal warning
testWarningKeywordCollision :: IO Bool
testWarningKeywordCollision = do
    -- Two items in the same room with the same keyword
    let r0 = minRoom "loc_0"
        i1 = (minItem "key_gold") { aiKeywords = ["key", "gold"] }
        i2 = (minItem "key_silver") { aiKeywords = ["key", "silver"] }
        advItemsClash = (minAdventure r0) { advItems = [i1, i2] }
    r1 <- case compileAdventure advItemsClash of
        Left errs -> do
            putStrLn $ "  unexpected compile error: " ++ show errs
            pure False
        Right cr -> do
            let warns = filter (\w -> ciCode w == "KeywordCollision") (crWarnings cr)
            rA <- expectEqual 1 (length warns)
            rB <- expectEqual (Just SWarning) (ciSeverity <$> listToMaybe warns)
            rC <- expectEqual (Just "rooms.loc_0") (ciPath <$> listToMaybe warns)
            pure (rA && rB && rC)

    -- Item and NPC in the same room with the same keyword
    let n1 = (partySquire Nothing) { anId = "sentry", anKeywords = ["guard"] }
        iBadge = (minItem "badge") { aiKeywords = ["guard"] }
        advNpcItemClash = (minAdventure r0) { advItems = [iBadge], advNPCs = [n1] }
    r2 <- case compileAdventure advNpcItemClash of
        Left errs -> do
            putStrLn $ "  unexpected compile error: " ++ show errs
            pure False
        Right cr -> do
            let warns = filter (\w -> ciCode w == "KeywordCollision") (crWarnings cr)
            rA <- expectEqual 1 (length warns)
            rB <- expectEqual (Just "rooms.loc_0") (ciPath <$> listToMaybe warns)
            pure (rA && rB)

    -- Same keyword across different rooms produces zero warnings
    let r1Loc = minRoom "loc_1"
        iRoom0 = (minItem "key_gold") { aiLocation = "loc_0", aiKeywords = ["key"] }
        iRoom1 = (minItem "key_silver") { aiLocation = "loc_1", aiKeywords = ["key"] }
        advDiffRooms = (minAdventure r0) { advRooms = [r0, r1Loc], advItems = [iRoom0, iRoom1] }
    r3 <- case compileAdventure advDiffRooms of
        Left errs -> do
            putStrLn $ "  unexpected compile error: " ++ show errs
            pure False
        Right cr -> do
            let warns = filter (\w -> ciCode w == "KeywordCollision") (crWarnings cr)
            expectTrue "no keyword collision across different rooms" (null warns)

    -- One item in inventory does not collide with room item
    let iInv = (minItem "key_silver") { aiLocation = "inventory", aiKeywords = ["key"] }
        advInv = (minAdventure r0) { advItems = [iRoom0, iInv] }
    r4 <- case compileAdventure advInv of
        Left errs -> do
            putStrLn $ "  unexpected compile error: " ++ show errs
            pure False
        Right cr -> do
            let warns = filter (\w -> ciCode w == "KeywordCollision") (crWarnings cr)
            expectTrue "inventory item does not collide with room item" (null warns)

    pure (r1 && r2 && r3 && r4)

-- | Phase 0.4: Unknown variable placeholders in texts emit non-fatal warnings
testWarningUnknownPlaceholder :: IO Bool
testWarningUnknownPlaceholder = do
    -- Unknown placeholder in room description
    let r0 = (minRoom "loc_0") { arTexts = ACondText "Du hast {unknown_variable} Gold." [] }
        advDesc = minAdventure r0
    r1 <- case compileAdventure advDesc of
        Left errs -> do
            putStrLn $ "  unexpected compile error: " ++ show errs
            pure False
        Right cr -> do
            let warns = filter (\w -> ciCode w == "UnknownPlaceholder") (crWarnings cr)
            rA <- expectEqual 1 (length warns)
            rB <- expectEqual (Just "rooms.loc_0.desc") (ciPath <$> listToMaybe warns)
            rC <- expectEqual (Just SWarning) (ciSeverity <$> listToMaybe warns)
            pure (rA && rB && rC)

    -- Unknown placeholder in outcome message
    let r0Outcome = (minRoom "loc_0") { arOnEnter = Just [AOMessage "Willkommen {player_title}!"] }
        advOutcome = minAdventure r0Outcome
    r2 <- case compileAdventure advOutcome of
        Left errs -> do
            putStrLn $ "  unexpected compile error: " ++ show errs
            pure False
        Right cr -> do
            let warns = filter (\w -> ciCode w == "UnknownPlaceholder") (crWarnings cr)
            rA <- expectEqual 1 (length warns)
            rB <- expectEqual (Just "rooms.loc_0") (ciPath <$> listToMaybe warns)
            pure (rA && rB)

    -- Declared variables, modifiers ({gold:6}), and system variables emit zero warnings
    let r0Known = (minRoom "loc_0")
            { arTexts = ACondText "Gold: {gold:6}, HP: {player.hp}, Turns: {turn.count}, Arg: {cmd.arg1}." [] }
        vGold = AVariable "gold" "int" (Just (Aeson.Number 50)) Nothing Nothing
        advKnown = (minAdventure r0Known) { advVariables = [vGold] }
    r3 <- case compileAdventure advKnown of
        Left errs -> do
            putStrLn $ "  unexpected compile error: " ++ show errs
            pure False
        Right cr -> do
            let warns = filter (\w -> ciCode w == "UnknownPlaceholder") (crWarnings cr)
            expectTrue "known variables produce 0 placeholder warnings" (null warns)

    -- Escaped braces are not treated as placeholders
    let r0Escaped = (minRoom "loc_0")
            { arTexts = ACondText "Ascii pattern: {{foo}} and \\{bar\\}." [] }
        advEscaped = minAdventure r0Escaped
    r4 <- case compileAdventure advEscaped of
        Left errs -> do
            putStrLn $ "  unexpected compile error: " ++ show errs
            pure False
        Right cr -> do
            let warns = filter (\w -> ciCode w == "UnknownPlaceholder") (crWarnings cr)
            expectTrue "escaped braces produce 0 placeholder warnings" (null warns)

    pure (r1 && r2 && r3 && r4)

-- | Phase 0.4: Dark room with items but without feelable/light_flag/lightsource emits warning
testWarningDarkRoomDeadEnd :: IO Bool
testWarningDarkRoomDeadEnd = do
    -- Dark room with item, no light flag, no feelable tag, no lightsource anywhere
    let rDark = (minRoom "loc_0") { arTags = ["dark"] }
        iNormal = minItem "sword"
        advDeadEnd = (minAdventure rDark) { advItems = [iNormal] }
    r1 <- case compileAdventure advDeadEnd of
        Left errs -> do
            putStrLn $ "  unexpected compile error: " ++ show errs
            pure False
        Right cr -> do
            let warns = filter (\w -> ciCode w == "DarkRoomDeadEnd") (crWarnings cr)
            rA <- expectEqual 1 (length warns)
            rB <- expectEqual (Just "rooms.loc_0") (ciPath <$> listToMaybe warns)
            rC <- expectEqual (Just SWarning) (ciSeverity <$> listToMaybe warns)
            pure (rA && rB && rC)

    -- Feelable item in dark room prevents dead end
    let iFeelable = (minItem "sword") { aiTags = ["feelable"] }
        advFeelable = (minAdventure rDark) { advItems = [iFeelable] }
    r2 <- case compileAdventure advFeelable of
        Left errs -> do
            putStrLn $ "  unexpected compile error: " ++ show errs
            pure False
        Right cr -> do
            let warns = filter (\w -> ciCode w == "DarkRoomDeadEnd") (crWarnings cr)
            expectTrue "feelable item prevents dead-end warning" (null warns)

    -- light_flag on dark room prevents dead end
    let rWithFlag = rDark { arLightFlag = Just "cave_lit" }
        advFlag = (minAdventure rWithFlag) { advItems = [iNormal] }
    r3 <- case compileAdventure advFlag of
        Left errs -> do
            putStrLn $ "  unexpected compile error: " ++ show errs
            pure False
        Right cr -> do
            let warns = filter (\w -> ciCode w == "DarkRoomDeadEnd") (crWarnings cr)
            expectTrue "light_flag prevents dead-end warning" (null warns)

    -- Reachable lightsource item prevents dead end
    let rLitStart = minRoom "loc_0"
        rDarkEast = (minRoom "loc_1")
            { arTags = ["dark"]
            , arExits = Map.fromList [("west", AExitRef "loc_0" Nothing Nothing Nothing)] }
        rLitStart' = rLitStart
            { arExits = Map.fromList [("east", AExitRef "loc_1" Nothing Nothing Nothing)] }
        torch = (minItem "torch") { aiLocation = "loc_0", aiTags = ["lightsource"] }
        gem = (minItem "gem") { aiLocation = "loc_1" }
        advWithTorch = (minAdventure rLitStart')
            { advRooms = [rLitStart', rDarkEast]
            , advItems = [torch, gem] }
    r4 <- case compileAdventure advWithTorch of
        Left errs -> do
            putStrLn $ "  unexpected compile error: " ++ show errs
            pure False
        Right cr -> do
            let warns = filter (\w -> ciCode w == "DarkRoomDeadEnd") (crWarnings cr)
            expectTrue "reachable lightsource prevents dead-end warning" (null warns)

    -- Lightsource in player inventory prevents dead end
    let torchInv = (minItem "torch") { aiLocation = "inventory", aiTags = ["lightsource"] }
        advWithTorchInv = (minAdventure rDark) { advItems = [torchInv, iNormal] }
    r5 <- case compileAdventure advWithTorchInv of
        Left errs -> do
            putStrLn $ "  unexpected compile error: " ++ show errs
            pure False
        Right cr -> do
            let warns = filter (\w -> ciCode w == "DarkRoomDeadEnd") (crWarnings cr)
            expectTrue "carried lightsource prevents dead-end warning" (null warns)

    -- Dark room without any items does not produce dead-end warning
    let advEmptyDark = minAdventure rDark
    r6 <- case compileAdventure advEmptyDark of
        Left errs -> do
            putStrLn $ "  unexpected compile error: " ++ show errs
            pure False
        Right cr -> do
            let warns = filter (\w -> ciCode w == "DarkRoomDeadEnd") (crWarnings cr)
            expectTrue "empty dark room produces no dead-end warning" (null warns)

    pure (r1 && r2 && r3 && r4 && r5 && r6)

-- | Phase 2.2: Guarded exit with when-predicate and failure msg compiles to E.Guarded.
testGuardedExitCompilation :: IO Bool
testGuardedExitCompilation = do
    let yaml = unlines
            [ "name: Guarded Exit Test"
            , "start_room: room_a"
            , "rooms:"
            , "  - id: room_a"
            , "    name: Room A"
            , "    desc: First room."
            , "    exits:"
            , "      north:"
            , "        to: room_b"
            , "        when:"
            , "          has_flag: unlocked"
            , "        msg: 'The gate is barred.'"
            , "  - id: room_b"
            , "    name: Room B"
            , "    desc: Second room."
            , "    exits:"
            , "      south: room_a"
            ]
    case decode1 (BLC.pack yaml) of
        Left err -> do
            putStrLn $ "  yaml parse failed: " ++ show err
            pure False
        Right (adv :: Adventure) -> case compileAdventure adv of
            Left errs -> do
                putStrLn $ "  compile failed: " ++ show errs
                pure False
            Right cr -> do
                let rA = (E.rooms (crWorld cr)) Map.! "room_a"
                case Map.lookup E.North (E.roomConnections rA) of
                    Just (E.Guarded "room_b" (E.HasFlag "unlocked") (Just "The gate is barred.")) ->
                        pure True
                    other -> do
                        putStrLn $ "  unexpected exit: " ++ show other
                        pure False

-- | Phase 2.2: on: before <verb> and block: outcomes compile to E.OnBefore and E.Block.
testOnBeforeAndBlockCompilation :: IO Bool
testOnBeforeAndBlockCompilation = do
    let yaml = unlines
            [ "name: Before and Block Test"
            , "start_room: room_a"
            , "rooms:"
            , "  - id: room_a"
            , "    name: Room A"
            , "    desc: A room."
            , "rules:"
            , "  - id: r1"
            , "    on: before take"
            , "    effects:"
            , "      - block: 'Stop right there!'"
            , "  - id: r2"
            , "    on: before use"
            , "    effects:"
            , "      - block:"
            , "          msg: 'Jammed'"
            , "          turn: true"
            , "  - id: r3"
            , "    on: before drop"
            , "    effects:"
            , "      - block: true"
            , "  - id: r4"
            , "    on: before look"
            , "    effects:"
            , "      - block: false"
            ]
    case decode1 (BLC.pack yaml) of
        Left err -> do
            putStrLn $ "  yaml parse failed: " ++ show err
            pure False
        Right (adv :: Adventure) -> case compileAdventure adv of
            Left errs -> do
                putStrLn $ "  compile failed: " ++ show errs
                pure False
            Right cr -> do
                let trigs = E.triggerDefs (crWorld cr)
                    t1 = find (\t -> E.trId t == "r1") trigs
                    t2 = find (\t -> E.trId t == "r2") trigs
                    t3 = find (\t -> E.trId t == "r3") trigs
                    t4 = find (\t -> E.trId t == "r4") trigs
                r1 <- expectEqual (Just (E.OnBefore "take")) (E.trEvent <$> t1)
                r2 <- expectEqual (Just [E.Block (Just "Stop right there!") False]) (E.trEffects <$> t1)
                r3 <- expectEqual (Just (E.OnBefore "use")) (E.trEvent <$> t2)
                r4 <- expectEqual (Just [E.Block (Just "Jammed") True]) (E.trEffects <$> t2)
                r5 <- expectEqual (Just (E.OnBefore "drop")) (E.trEvent <$> t3)
                r6 <- expectEqual (Just [E.Block Nothing False]) (E.trEffects <$> t3)
                r7 <- expectEqual (Just (E.OnBefore "look")) (E.trEvent <$> t4)
                r8 <- expectEqual (Just [E.Block Nothing False]) (E.trEffects <$> t4)
                pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8)

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
    , aiCapacity = Nothing, aiPortable = Just True
    , aiTakeFailure = Nothing
    , aiInContainer = Nothing
    , aiCarriedBy = Nothing
    , aiGrammar = E.emptyGrammar
    }

-- | Minimal usable ANPC for pool entries. Map.empty
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
    , anParty = Nothing, anTopics = Map.empty, anBarks = [], anOnTalk = Nothing
    , anDropsOnDeath = False
    , anGrammar = E.emptyGrammar }

testSayNodeDialogEndSugar :: IO Bool
testSayNodeDialogEndSugar = do
    r1 <- expectEqual (E.SetValue (E.VRVariable "dialog_node") (E.EVString "knoten1"))
                      (compileAActionOutcome (AOSayNode "knoten1"))
    r2 <- expectEqual (E.SetValue (E.VRVariable "dialog_node") (E.EVString ""))
                      (compileAActionOutcome AODialogEnd)
    pure (r1 && r2)

testBarkTriggerSugar :: IO Bool
testBarkTriggerSugar = do
    let npc = (minNpcKey "waechter")
            { anBarks = [ABark "Es ist still..." Nothing Nothing, ABark "Wind weht." (Just (E.HasFlag "windig")) (Just 10)] }
        adv = (minAdventure (minRoom "loc_0")) { advNPCs = [npc] }
    case compileAdventure adv of
        Left errs -> expectTrue ("bark compile: " ++ issuesText errs) False
        Right cr -> do
            let trigs = E.triggerDefs (crWorld cr)
                bark1 = find (\t -> E.trId t == "bark.waechter.1") trigs
                bark2 = find (\t -> E.trId t == "bark.waechter.2") trigs
            r1 <- expectTrue "bark.1 exists" (isJust bark1)
            let b1 = case bark1 of Just x -> x; Nothing -> error "b1"
            r2 <- expectEqual E.OnTurn (E.trEvent b1)
            r3 <- expectEqual [E.SendMessage "Es ist still..."] (E.trEffects b1)
            r4 <- expectEqual 20 (E.trCooldown b1)
            r5 <- expectTrue "bark.2 exists" (isJust bark2)
            let b2 = case bark2 of Just x -> x; Nothing -> error "b2"
            r6 <- expectEqual (Just (E.HasFlag "windig")) (E.trCondition b2)
            r7 <- expectEqual 10 (E.trCooldown b2)
            pure (r1 && r2 && r3 && r4 && r5 && r6 && r7)

testOnTalkTriggerSugar :: IO Bool
testOnTalkTriggerSugar = do
    let npc = (minNpcKey "waechter") { anOnTalk = Just (AOMessage "Der Waechter grunzt.") }
        adv = (minAdventure (minRoom "loc_0")) { advNPCs = [npc] }
    case compileAdventure adv of
        Left errs -> expectTrue ("on_talk compile: " ++ issuesText errs) False
        Right cr -> do
            let trigs = E.triggerDefs (crWorld cr)
                talkTrig = find (\t -> E.trId t == "talk.waechter") trigs
            r1 <- expectTrue "talk.waechter exists" (isJust talkTrig)
            let tt = case talkTrig of Just x -> x; Nothing -> error "tt"
            r2 <- expectEqual (E.OnTalk "waechter" "") (E.trEvent tt)
            r3 <- expectEqual [E.SendMessage "Der Waechter grunzt."] (E.trEffects tt)
            r4 <- expectEqual 0 (E.trCooldown tt)
            pure (r1 && r2 && r3 && r4)

-- ===========================================================================
-- B7: NPC possession
-- ===========================================================================

-- | `carried_by:` compiles to CarriedBy (ActorNPC …) in the start item states;
--   "player" is the engine's actor-string alias for the player.
testCarriedByCompiles :: IO Bool
testCarriedByCompiles = do
    let npc = (minNpcKey "waechter") { anLocation = "loc_0" }
        item = (minItem "schluessel") { aiCarriedBy = Just "waechter" }
        brot = (minItem "brot") { aiCarriedBy = Just "player" }
        adv = (advWithItem item) { advNPCs = [npc], advItems = [item, brot] }
    case compileAdventure adv of
        Left errs -> expectTrue ("carried_by compile: " ++ issuesText errs) False
        Right cr -> do
            let locOf i = fmap E.itemLocation (Map.lookup i (E.itemStates (crSave cr)))
            r1 <- expectEqual (Just (E.CarriedBy (E.ActorNPC "waechter"))) (locOf "schluessel")
            r2 <- expectEqual (Just (E.CarriedBy E.ActorPlayer)) (locOf "brot")
            pure (r1 && r2)

-- | `carried_by:` naming an unknown npc is a hard error (UnknownNpc).
testCarriedByUnknownNpcFails :: IO Bool
testCarriedByUnknownNpcFails = do
    let item = (minItem "schluessel") { aiCarriedBy = Just "niemand" }
    case compileAdventure (advWithItem item) of
        Left errs -> expectTrue ("unknown npc error: " ++ issuesText errs)
                        (any (\i -> ciCode i == "UnknownNpc") errs)
        Right _ -> expectTrue "expected a compile error" False

-- | `carried_by:` together with `in_container:` is a hard error
--   (CarriedByConflict — one item can only start in one place).
testCarriedByConflictFails :: IO Bool
testCarriedByConflictFails = do
    let npc = (minNpcKey "waechter") { anLocation = "loc_0" }
        item = (minItem "schluessel") { aiCarriedBy = Just "waechter", aiInContainer = Just "kiste" }
        adv = (advWithItem item) { advNPCs = [npc] }
    case compileAdventure adv of
        Left errs -> expectTrue ("conflict error: " ++ issuesText errs)
                        (any (\i -> ciCode i == "CarriedByConflict") errs)
        Right _ -> expectTrue "expected a compile error" False

-- | The `give:` effect: the string form still targets the player (byte-compat),
--   the object form ({item, to}) can hand items to NPCs. Both YAML shapes.
testGiveToSugar :: IO Bool
testGiveToSugar = do
    r1 <- expectEqual (E.MoveEntity "schluessel" (E.CarriedBy E.ActorPlayer))
                      (compileAActionOutcome (AOGiveItem "schluessel"))
    r2 <- expectEqual (E.MoveEntity "schluessel" (E.CarriedBy (E.ActorNPC "waechter")))
                      (compileAActionOutcome (AOGiveTo "schluessel" "waechter"))
    r3 <- expectEqual (Just (AOGiveItem "schluessel"))
                      (Aeson.decode (BLC.pack "{\"give\": \"schluessel\"}"))
    r4 <- expectEqual (Just (AOGiveTo "schluessel" "waechter"))
                      (Aeson.decode (BLC.pack "{\"give\": {\"item\": \"schluessel\", \"to\": \"waechter\"}}"))
    pure (r1 && r2 && r3 && r4)

-- | B9: `drops_on_death:` is a known npc key that reaches the engine flag and
--   is omitted from the world json when it is not set (byte contract).
testDropsOnDeathFlag :: IO Bool
testDropsOnDeathFlag = do
    let npc = (minNpcKey "waechter") { anDropsOnDeath = True }
        adv = advWithNPC npc
        plain = advWithNPC (minNpcKey "waechter")
        compiles adv' f = case compileAdventure adv' of
            Right cr -> pure (f (E.npcDefs (crWorld cr)))
            Left errs -> expectTrue ("compile: " ++ issuesText errs) False
    r1 <- compiles adv (\defs -> maybe False npcDropsOnDeath (Map.lookup "waechter" defs))
    r2 <- compiles adv (\defs -> "npcDropsOnDeath" `isInfixOf` BLC.unpack (Aeson.encode defs))
    r3 <- compiles plain (\defs -> not ("npcDropsOnDeath" `isInfixOf` BLC.unpack (Aeson.encode defs)))
    let yaml = unlines
            [ "start_room: loc_0"
            , "rooms:"
            , "  - id: loc_0"
            , "    name: Halle"
            , "    desc: Eine Halle."
            , "npcs:"
            , "  - id: waechter"
            , "    name: Waechter"
            , "    location: loc_0"
            , "    max_hp: 10"
            , "    drops_on_death: true"
            ]
        yamlPlain = unlines (take (length (lines yaml) - 2) (lines yaml))
    -- the same key must survive the YAML front door without a UnknownYamlKey
    -- warning (the B7 lesson: a new key without knownKeys warns everywhere)
    r4 <- case decode1 (BLC.pack yaml) of
        Left err -> expectTrue ("yaml parse failed: " ++ show err) False
        Right (rawAdv :: Adventure) -> do
            r4a <- expectEqual True (anDropsOnDeath (head (advNPCs rawAdv)))
            case compileAdventure rawAdv of
                Left errs -> do r4b <- expectTrue ("compile: " ++ issuesText errs) False
                                pure (r4a && r4b)
                Right cr -> do
                    let keyWarns = filter (\i -> ciCode i == "UnknownYamlKey") (crWarnings cr)
                    r4b <- expectTrue ("unknown key warnings: " ++ show (length keyWarns)) (null keyWarns)
                    pure (r4a && r4b)
    r5 <- case decode1 (BLC.pack yamlPlain) of
        Left err -> expectTrue ("yaml parse failed: " ++ show err) False
        Right (rawAdv :: Adventure) -> expectEqual False (anDropsOnDeath (head (advNPCs rawAdv)))
    pure (r1 && r2 && r3 && r4 && r5)

-- | B9: `give: {item, to, equip: true}` compiles to `EquippedBy` (the B7
--   sugar, not a new player command), `equip: false` stays `CarriedBy`, and an
--   unknown target is a hard error on the new form too. The bool has to be
--   *used* — a discarded `equip` would accept `false` silently.
testGiveEquipSugar :: IO Bool
testGiveEquipSugar = do
    r1 <- expectEqual (E.MoveEntity "schwert" (E.EquippedBy (E.ActorNPC "waechter")))
                      (compileAActionOutcome (AOGiveEquipTo "schwert" "waechter"))
    r2 <- expectEqual (E.MoveEntity "schwert" (E.EquippedBy E.ActorPlayer))
                      (compileAActionOutcome (AOGiveEquipTo "schwert" "player"))
    r3 <- expectEqual (Just (AOGiveEquipTo "schwert" "waechter"))
                      (Aeson.decode (BLC.pack "{\"give\": {\"item\": \"schwert\", \"to\": \"waechter\", \"equip\": true}}"))
    r4 <- expectEqual (Just (AOGiveTo "schwert" "waechter"))
                      (Aeson.decode (BLC.pack "{\"give\": {\"item\": \"schwert\", \"to\": \"waechter\", \"equip\": false}}"))
    r5 <- expectEqual (Just (AOGiveTo "schwert" "waechter"))
                      (Aeson.decode (BLC.pack "{\"give\": {\"item\": \"schwert\", \"to\": \"waechter\"}}"))
    r6 <- expectEqual (Just (AOGiveEquipTo "schwert" "player"))
                      (Aeson.decode (BLC.pack "{\"give\": {\"item\": \"schwert\", \"equip\": true}}"))
    let bad = (minItem "schwert") { aiOnTake = Just [AOGiveEquipTo "schwert" "niemand"] }
    r7 <- expectTrue "unknown npc on the equip form"
            (case compileAdventure (advWithItem bad) of
                Left errs -> any (\i -> ciCode i == "UnknownNpc") errs
                Right _   -> False)
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7)

-- | `give: {item, to}` naming an unknown npc is a hard error (UnknownNpc).
testGiveToUnknownNpcFails :: IO Bool
testGiveToUnknownNpcFails = do
    let item = (minItem "schluessel") { aiOnTake = Just [AOGiveTo "schluessel" "niemand"] }
    case compileAdventure (advWithItem item) of
        Left errs -> expectTrue ("give.to unknown npc: " ++ issuesText errs)
                        (any (\i -> ciCode i == "UnknownNpc") errs)
        Right _ -> expectTrue "expected a compile error" False

-- | `carried_by:` and `capacity:` are known item keys — no UnknownYamlKey
--   (capacity was missing from knownKeys since 4.4, pinned here too).
testNpcPossessionKnownKeysClean :: IO Bool
testNpcPossessionKnownKeysClean = do
    let yaml = unlines
            [ "start_room: loc_0"
            , "rooms:"
            , "  - id: loc_0"
            , "    name: Halle"
            , "    desc: Eine Halle."
            , "npcs:"
            , "  - id: waechter"
            , "    name: Waechter"
            , "    location: loc_0"
            , "items:"
            , "  - id: schluessel"
            , "    name: Schluessel"
            , "    desc: Ein Schluessel."
            , "    location: loc_0"
            , "    carried_by: waechter"
            , "  - id: kiste"
            , "    name: Kiste"
            , "    desc: Eine Kiste."
            , "    location: loc_0"
            , "    capacity: 3"
            ]
    case decode1 (BLC.pack yaml) of
        Left err -> do
            putStrLn $ "  yaml parse failed: " ++ show err
            pure False
        Right (adv :: Adventure) -> case compileAdventure adv of
            Left errs -> do
                putStrLn $ "  compile failed: " ++ issuesText errs
                pure False
            Right cr -> do
                let keyWarns = filter (\i -> ciCode i == "UnknownYamlKey") (crWarnings cr)
                expectTrue ("expected 0 unknown key warnings, got " ++ show (length keyWarns)) (null keyWarns)

-- ===========================================================================
-- B9: item-on-NPC interactions
-- ===========================================================================

-- | `interactions: npc:` compiles into the (item, npc) -> outcome map; the
--   effect list is compiled like every other outcome surface.
testNpcInteractionCompiles :: IO Bool
testNpcInteractionCompiles = do
    let ix = AInteractions
            { aiEntity = []
            , aiItem = []
            , aiNpc = [ ANPCInteraction "verband" "waechter" [AOMessage "Du verbindest den Waechter."] ] }
        npc = (minNpcKey "waechter") { anLocation = "loc_0" }
        adv = (minAdventure (minRoom "loc_0"))
            { advNPCs = [npc]
            , advItems = [minItem "verband"]
            , advInteractions = Just ix }
    case compileAdventure adv of
        Left errs -> do
            putStrLn $ "  compile errors: " ++ issuesText errs
            pure False
        Right cr -> do
            let effs = Map.lookup ("verband", "waechter") (E.npcInteractions (crWorld cr))
            r1 <- expectTrue "npc interaction present" (Map.member ("verband", "waechter") (E.npcInteractions (crWorld cr)))
            r2 <- expectEqual (Just (E.SendMessage "Du verbindest den Waechter.")) effs
            pure (r1 && r2)

-- | Both halves of an `interactions: npc:` entry must resolve: a typo in the
--   item id would silently fall through to the attack fallback.
testNpcInteractionRefsFail :: IO Bool
testNpcInteractionRefsFail = do
    let npc = (minNpcKey "waechter") { anLocation = "loc_0" }
        withIx n adv = adv { advNPCs = [npc], advItems = [minItem "verband"]
                           , advInteractions = Just (AInteractions
                                { aiEntity = [], aiItem = [], aiNpc = [n] }) }
        base = minAdventure (minRoom "loc_0")
    r1 <- case compileAdventure (withIx (ANPCInteraction "verbandt" "waechter" []) base) of
        Left errs -> expectTrue ("unknown item: " ++ issuesText errs)
                        (any (\i -> ciCode i == "UnknownNpcInteractionItem") errs)
        Right _ -> expectTrue "expected a compile error (unknown item)" False
    r2 <- case compileAdventure (withIx (ANPCInteraction "verband" "niemand" []) base) of
        Left errs -> expectTrue ("unknown npc: " ++ issuesText errs)
                        (any (\i -> ciCode i == "UnknownNpc") errs)
        Right _ -> expectTrue "expected a compile error (unknown npc)" False
    pure (r1 && r2)

-- | `npc:` is a known key of the interactions section — no UnknownYamlKey.
testNpcInteractionKnownKeysClean :: IO Bool
testNpcInteractionKnownKeysClean = do
    let yaml = unlines
            [ "start_room: loc_0"
            , "rooms:"
            , "  - id: loc_0"
            , "    name: Halle"
            , "    desc: Eine Halle."
            , "npcs:"
            , "  - id: waechter"
            , "    name: Waechter"
            , "    location: loc_0"
            , "items:"
            , "  - id: verband"
            , "    name: Verband"
            , "    desc: Ein Verband."
            , "    location: loc_0"
            , "interactions:"
            , "  npc:"
            , "    - item: verband"
            , "      target: waechter"
            , "      effects:"
            , "        - msg: \"Du verbindest den Waechter.\""
            ]
    case decode1 (BLC.pack yaml) of
        Left err -> do
            putStrLn $ "  yaml parse failed: " ++ show err
            pure False
        Right (adv :: Adventure) -> case compileAdventure adv of
            Left errs -> do
                putStrLn $ "  compile failed: " ++ issuesText errs
                pure False
            Right cr -> do
                let keyWarns = filter (\i -> ciCode i == "UnknownYamlKey") (crWarnings cr)
                r1 <- expectTrue ("expected 0 unknown key warnings, got " ++ show (length keyWarns)) (null keyWarns)
                r2 <- expectTrue "npc interaction survived the yaml round trip"
                        (Map.member ("verband", "waechter") (E.npcInteractions (crWorld cr)))
                pure (r1 && r2)

-- | 4.6: `on_complete:` is parsed at last (it was documented in
--   `docs/adventure-schema.md` but dropped on the floor — the field reached no
--   parser at all), survives into the compiled world, and produces no
--   `UnknownYamlKey` warning.
testQuestOnCompleteParsed :: IO Bool
testQuestOnCompleteParsed = do
    let yaml = unlines
            [ "start_room: loc_0"
            , "rooms:"
            , "  - id: loc_0"
            , "    name: Halle"
            , "    desc: Eine Halle."
            , "quests:"
            , "  - id: q1"
            , "    name: Erste"
            , "    desc: \"Etwas tun.\""
            , "    stages:"
            , "      - id: s1"
            , "        desc: \"Ein Schritt.\""
            , "    on_complete: q2"
            , "  - id: q2"
            , "    name: Zweite"
            , "    stages:"
            , "      - id: s1"
            , "        desc: \"Der Folge-Schritt.\""
            ]
    case decode1 (BLC.pack yaml) of
        Left err -> do
            putStrLn $ "  yaml parse failed: " ++ show err
            pure False
        Right (adv :: Adventure) -> case compileAdventure adv of
            Left errs -> do
                putStrLn $ "  compile failed: " ++ issuesText errs
                pure False
            Right cr -> do
                let keyWarns = filter (\i -> ciCode i == "UnknownYamlKey") (crWarnings cr)
                    q1 = Map.lookup "q1" (E.questDefs (crWorld cr))
                r1 <- expectTrue ("expected 0 unknown key warnings, got " ++ show (length keyWarns)) (null keyWarns)
                r2 <- expectEqual (Just (Just "q2")) (fmap E.questOnComplete q1)
                r3 <- expectEqual (Just Nothing) (fmap E.questOnComplete (Map.lookup "q2" (E.questDefs (crWorld cr))))
                pure (r1 && r2 && r3)

-- | 4.6: quest references must resolve at compile time, with a path (the world
--   validator catches the same class as `MissingQuest`, but only after
--   compilation, without a Fundstelle, and `--force` writes anyway).
testQuestRefErrors :: IO Bool
testQuestRefErrors = do
    let base = minAdventure (minRoom "loc_0")
        questErrs adv = case compileAdventure adv of
            Left errs -> Just errs
            Right _ -> Nothing
        withRule o adv = adv { advTriggers = [ATrigger "r1" "enter loc_0" Nothing [o] False 0] }
        codeIn c m = maybe False (any (\i -> ciCode i == c)) m
    r1 <- expectTrue "start_quest to an unknown quest is a hard error"
            (codeIn "UnknownQuestEffect" (questErrs (withRule (AOStartQuest "nope") base)))
    r2 <- expectTrue "advance_quest to an unknown quest is a hard error"
            (codeIn "UnknownQuestEffect" (questErrs (withRule (AOAdvanceQuest "nope") base)))
    r3 <- expectTrue "complete_quest to an unknown quest is a hard error"
            (codeIn "UnknownQuestEffect" (questErrs (withRule (AOCompleteQuest "nope") base)))
    r4 <- expectTrue "on_complete to an unknown quest is a hard error"
            (codeIn "UnknownOnComplete" (questErrs base { advQuests = [AQuest "q1" "Q" "" [] [] Nothing (Just "nope")] }))
    r5 <- expectTrue "a known quest target compiles clean"
            (questErrs (withRule (AOStartQuest "q1") base { advQuests = [AQuest "q1" "Q" "" [] [] Nothing Nothing] }) == Nothing)
    r6 <- expectTrue "nested branches are covered too"
            (codeIn "UnknownQuestEffect"
                (questErrs (withRule (AOConditional (E.HasFlag "x") [] [AOStartQuest "nope"]) base)))
    pure (r1 && r2 && r3 && r4 && r5 && r6)

-- | 4.6: `initial_flags:` belongs to the start save, so the world validator
--   has to be told about it. Before the fix, this adventure was rejected with
--   `MissingSetFlag "started"` and `UnknownQuestPrereq "zugang" "started"` and
--   `compile` wrote nothing without `--force`.
testInitialFlagsSatisfyValidation :: IO Bool
testInitialFlagsSatisfyValidation = do
    let yaml = unlines
            [ "start_room: loc_0"
            , "initial_flags:"
            , "  started: \"true\""
            , "rooms:"
            , "  - id: loc_0"
            , "    name: Halle"
            , "    desc: Eine Halle."
            , "quests:"
            , "  - id: zugang"
            , "    name: Zugang"
            , "    prereqs: [started]"
            , "    stages:"
            , "      - id: s1"
            , "        desc: \"Ein Schritt.\""
            , "rules:"
            , "  - id: nur_wenn_started"
            , "    on: \"turn\""
            , "    when:"
            , "      has_flag: started"
            , "    effects:"
            , "      - msg: \"Passt.\""
            ]
    case decode1 (BLC.pack yaml) of
        Left err -> do
            putStrLn $ "  yaml parse failed: " ++ show err
            pure False
        Right (adv :: Adventure) -> case compileAdventure adv of
            Left errs -> do
                putStrLn $ "  compile failed: " ++ issuesText errs
                pure False
            Right cr -> do
                -- exactly the combination the CLI uses
                let errs = validateWorldWithFlags (crWorld cr) (Map.keysSet (E.flags (crSave cr)))
                          ++ validateGameState (crWorld cr) (crSave cr)
                r1 <- expectTrue ("expected no validation errors, got " ++ show errs) (null errs)
                r2 <- expectTrue "the flag really is in the start save"
                        (Map.member "started" (E.flags (crSave cr)))
                pure (r1 && r2)

-- ---------------------------------------------------------------------------
-- 4.6 S2: map positions, auto-layout, MapOverlap, map-set
-- ---------------------------------------------------------------------------

-- | `map: {x: n, y: m}` at a room: parsed, a known key, and written to the
--   world **only when set** (the byte contract of roomIntro/roomFloor).
testRoomMapPosRoundTrip :: IO Bool
testRoomMapPosRoundTrip = do
    let yaml = unlines
            [ "start_room: loc_0"
            , "rooms:"
            , "  - id: loc_0"
            , "    name: Halle"
            , "    desc: Eine Halle."
            , "    map: {x: 3, y: 4}"
            , "  - id: loc_1"
            , "    name: Gang"
            , "    desc: Ein Gang."
            ]
    case decode1 (BLC.pack yaml) of
        Left err -> do
            putStrLn $ "  yaml parse failed: " ++ show err
            pure False
        Right (adv :: Adventure) -> case compileAdventure adv of
            Left errs -> do
                putStrLn $ "  compile failed: " ++ issuesText errs
                pure False
            Right cr -> do
                let warns = filter (\i -> ciCode i == "UnknownYamlKey") (crWarnings cr)
                    wsRooms = E.rooms (crWorld cr)
                    enc = Aeson.encode
                r1 <- expectTrue ("expected 0 unknown key warnings, got " ++ show (length warns)) (null warns)
                r2 <- expectEqual (Just (E.MapPos 3 4)) (E.roomMapPos (wsRooms Map.! "loc_0"))
                r3 <- expectTrue "a room without map: has no position" (isNothing' (E.roomMapPos (wsRooms Map.! "loc_1")))
                r4 <- expectTrue "roomMapPos is omitted when unset"
                        (not (contains "\"roomMapPos\"" (enc (wsRooms Map.! "loc_1"))))
                r5 <- expectTrue "roomMapPos is written when set"
                        (contains "\"roomMapPos\"" (enc (wsRooms Map.! "loc_0")))
                r6 <- expectTrue "position survives a JSON round trip"
                        (fmap E.roomMapPos (Aeson.decode (Aeson.encode (wsRooms Map.! "loc_0")))
                            == Just (Just (E.MapPos 3 4)))
                pure (and [r1,r2,r3,r4,r5,r6])
  where
    isNothing' = maybe True (const False)

-- | The auto-layout: row = BFS depth from start_room, column = order inside the
--   layer (north before south, because the exit keys are canonical), an authored
--   `map:` is an anchor the walk must not touch, unreachable rooms get their own
--   row below, and `floor:` separates the grids.
testMapLayout :: IO Bool
testMapLayout = do
    let yaml = unlines
            [ "start_room: a"
            , "rooms:"
            , "  - id: a"
            , "    name: A"
            , "    desc: Start."
            , "    exits:"
            , "      east: { to: b }"
            , "      north: { to: c }"
            , "  - id: b"
            , "    name: B"
            , "    desc: B."
            , "    map: {x: 5, y: 1}"
            , "    exits:"
            , "      north: { to: d }"
            , "  - id: c"
            , "    name: C"
            , "    desc: C."
            , "  - id: d"
            , "    name: D"
            , "    desc: D."
            , "  - id: e"
            , "    name: E"
            , "    desc: Abgeschnitten."
            , "  - id: f"
            , "    name: F"
            , "    desc: Andere Ebene."
            , "    floor: 2"
            ]
    case decode1 (BLC.pack yaml) of
        Left err -> do
            putStrLn $ "  yaml parse failed: " ++ show err
            pure False
        Right (adv :: Adventure) -> do
            let cells = Map.fromList [ (mcRoom c, (mcX c, mcY c)) | c <- roomLayout adv ]
                flagged = Set.fromList [ mcRoom c | c <- roomLayout adv, mcSet c ]
                floors = Map.fromList [ (mcRoom c, mcFloor c) | c <- roomLayout adv ]
            r1 <- expectEqual (Just (0, 0)) (Map.lookup "a" cells)
            r2 <- expectEqual
                            (Just [(0, 1), (5, 1)]) (fmap id (sequence [Map.lookup "c" cells, Map.lookup "b" cells]))
            r3 <- expectEqual (Just (0, 2)) (Map.lookup "d" cells)
            r4 <- expectEqual (Just (0, 3)) (Map.lookup "e" cells)
            r5 <- expectEqual (Just 2) (Map.lookup "f" floors)
            r6 <- expectEqual (Set.fromList ["b"]) flagged
            r7 <- expectEqual
                            (["a","b","c","d","e","f"]) (map mcRoom (roomLayout adv))
            pure (and [r1,r2,r3,r4,r5,r6,r7])

-- | Two rooms on one cell of one floor is a hard error naming both rooms; the
--   same coordinates on different floors are fine, because `floor:` is the z axis.
testMapOverlapIsAnError :: IO Bool
testMapOverlapIsAnError = do
    let withPos a b = (minAdventure (minRoom "loc_0"))
            { advRooms = [ (minRoom "loc_0") { arMapPos = Just (E.MapPos a b) }
                         , (minRoom "loc_1") { arMapPos = Just (E.MapPos a b) } ] }
        codes adv = case compileAdventure adv of
            Left errs -> map ciCode errs
            Right _ -> []
        msgs adv = case compileAdventure adv of
            Left errs -> concatMap ciMessage errs
            Right _ -> []
        twoFloors = (minAdventure (minRoom "loc_0"))
            { advRooms = [ (minRoom "loc_0") { arMapPos = Just (E.MapPos 2 2) }
                         , (minRoom "loc_1") { arMapPos = Just (E.MapPos 2 2), arFloor = Just 2 } ] }
    r1 <- expectTrue "same cell, same floor -> MapOverlap"
            ("MapOverlap" `elem` codes (withPos 2 2))
    r2 <- expectTrue "the message names the other room"
            (contains "loc_1" (BLC.pack (msgs (withPos 2 2))))
    r3 <- expectTrue "the same cell on different floors is fine"
            (not ("MapOverlap" `elem` codes twoFloors))
    pure (and [r1,r2,r3])

-- | `map-set` is the first real user of the S1 writer: insert the key when the
--   room has none, replace both coordinates when it has one — and keep every
--   other byte, comments included.
testMapSetInsertsAndReplaces :: IO Bool
testMapSetInsertsAndReplaces = do
    let doc0 = BLC.pack (unlines
            [ "# Karte"
            , "name: M"
            , "start_room: a"
            , "rooms:"
            , "  - id: a"
            , "    name: A"
            , "    desc: \"Start.\"   # Kommentar"
            , "  - id: b"
            , "    name: B"
            , "    desc: Ende."
            ])
    case parseYamlDoc doc0 of
        Left err -> do
            putStrLn $ "  parse failed: " ++ err
            pure False
        Right doc -> do
            let inserted = insertKeys doc "rooms.a" [("map", "{x: 4, y: 1}")]
            r1 <- case inserted of
                Left e -> expectTrue ("insert failed: " ++ show e) False
                Right out -> do
                    r1a <- expectTrue "the key is inserted at the room's indentation"
                            (contains "    map: {x: 4, y: 1}" out)
                    r1b <- expectTrue "the comment on the last line survives"
                            (contains "# Kommentar\n" out)
                    r1c <- expectTrue "the other room is untouched"
                            (contains "  - id: b\n    name: B\n    desc: Ende." out)
                    -- the inserted document must still parse, with the position where we put it
                    r1d <- case parseYamlDoc out of
                        Left e -> expectTrue ("re-parse failed: " ++ e) False
                        Right doc2 -> expectTrue "the inserted map resolves"
                                (fmap posLine (ydResolveIssuePath doc2 "rooms.a.map.x") /= Nothing)
                    pure (r1a && r1b && r1c && r1d)
            -- The replace path needs a document that already has `map:` — take
            -- the inserted one. Applying both coordinates to the *original* doc
            -- is exactly the bug `setScalarsAt` exists to prevent.
            r2 <- case inserted of
                Left e -> expectTrue ("insert failed: " ++ show e) False
                Right out -> case parseYamlDoc out of
                    Left e -> expectTrue ("re-parse failed: " ++ e) False
                    Right doc2 -> case setScalarsAt doc2 "rooms.a.map" [("x", "9"), ("y", "3")] of
                        Left e -> expectTrue ("set failed: " ++ show e) False
                        Right out2 -> do
                            r2a <- expectTrue "both coordinates are replaced"
                                    (contains "map: {x: 9, y: 3}" out2)
                            r2b <- expectTrue "the rest of the file is unchanged"
                                    (contains "# Kommentar" out2 && contains "  - id: b" out2)
                            pure (r2a && r2b)
            r3 <- expectTrue "a flow mapping cannot take a new line"
                    (case insertKeys doc "rooms.a.map" [("x", "1")] of
                        Left _ -> True
                        Right _ -> False)
            pure (and [r1,r2,r3])

-- ---------------------------------------------------------------------------
-- 4.6 S3: quest diagnostics + project view
-- ---------------------------------------------------------------------------

-- | The two dead-quest findings, and just as important: the cases that must
--   stay silent. An @on_complete:@ chain is a start path, @advance_quest:@ is a
--   progress path, and a pure @a -> b / b -> a@ cycle is left alone (largest
--   fixpoint, conservative).
testQuestDiagnostics :: IO Bool
testQuestDiagnostics = do
    let base = (minAdventure (minRoom "loc_0"))
            { advRooms = [ minRoom "loc_0", minRoom "loc_1" ] }
        diagOf adv = questDiagnostics (allAOutcomes adv) adv
        codes adv = map qdCode (diagOf adv)
        q qid stages onComplete = AQuest
            { aqId = qid, aqName = qid, aqDesc = "", aqPrereqs = []
            , aqStages = [ AQuestStage { aqsId = st, aqsDesc = st, aqsHint = Nothing }
                         | st <- stages ]
            , aqReward = Nothing, aqOnComplete = onComplete }
        advWith quests outcomes = base
            { advQuests = quests
            , advTriggers = [ ATrigger { atId = "r", atOn = "enter loc_0", atWhen = Nothing
                                       , atEffects = outcomes, atOnce = False, atCooldown = 0 } ] }
    -- a: started directly, advances. b: only reachable through a's on_complete,
    -- advances. c: nothing starts it. d: started, but never advances.
    let allQuests = [ q "a" ["s1","s2"] (Just "b")
                    , q "b" ["s1"] Nothing
                    , q "c" ["s1"] Nothing
                    , q "d" ["s1"] Nothing ]
        outAll = [ AOStartQuest "a", AOAdvanceQuest "a", AOAdvanceQuest "b"
                 , AOStartQuest "d" ]
    r1 <- expectEqual
            (codes (advWith [q "x" ["s1"] Nothing] [AOStartQuest "x"]))
            ["QuestNeverProgressed"]
    r2 <- expectEqual
            (codes (advWith [q "x" ["s1"] Nothing] [AOAdvanceQuest "x"]))
            ["QuestNeverStarted"]
    r3 <- expectEqual
            (codes (advWith allQuests outAll))
            ["QuestNeverStarted", "QuestNeverProgressed"]
    r4 <- expectEqual (map qdQuest (diagOf (advWith allQuests outAll))) ["c", "d"]
    r5 <- expectTrue "the reason names the missing effect"
            (all (\d -> contains "start_quest" (BLC.pack (qdReason d))
                          || contains "advance_quest" (BLC.pack (qdReason d)))
                (diagOf (advWith allQuests outAll)))
    -- a chain counts as a start path: y starts z, z advances, both silent
    r6 <- expectEqual
            (codes (advWith [ q "y" ["s1"] (Just "z"), q "z" ["s1"] Nothing ]
                            [AOStartQuest "y", AOAdvanceQuest "y", AOAdvanceQuest "z"]))
            []
    -- a pure cycle is dead content, and saying so is true, not noisy
    r7 <- expectEqual
            (codes (advWith [ q "p" ["s1"] (Just "r"), q "r" ["s1"] (Just "p") ]
                            [AOAdvanceQuest "p"]))
            ["QuestNeverStarted", "QuestNeverStarted"]
    -- advance_quest alone is progress: it completes the last stage
    r8 <- expectEqual
            (codes (advWith [q "s" ["only"] Nothing] [AOStartQuest "s", AOAdvanceQuest "s"]))
            []
    pure (and [r1,r2,r3,r4,r5,r6,r7,r8])

-- | The project view: the machine-readable half an editor consumes. Keys sorted
--   at every depth, two runs byte-identical, room order = declaration order,
--   and the authored position marked as such.
testProjectView :: IO Bool
testProjectView = do
    let yaml = unlines
            [ "name: View"
            , "start_room: a"
            , "rooms:"
            , "  - id: a"
            , "    name: A"
            , "    desc: \"Start.\""
            , "    exits:"
            , "      north: { to: b }"
            , "    map: {x: 2, y: 2}"
            , "  - id: b"
            , "    name: B"
            , "    desc: Ende."
            , "    floor: 1"
            , "  - id: abseits"
            , "    name: Abseits"
            , "    desc: \"Nirgendwohin.\""
            , "rules:"
            , "  - id: start_it"
            , "    on: \"enter a\""
            , "    effects:"
            , "      - {start_quest: q1}"
            , "      - {advance_quest: q1}"
            , "quests:"
            , "  - id: q1"
            , "    name: Q1"
            , "    stages:"
            , "      - {id: s1, desc: S1}"
            , "    on_complete: q2"
            , "  - id: q2"
            , "    name: Q2"
            , "    stages:"
            , "      - {id: s1, desc: S1}"
            ]
    case decode1 (BLC.pack yaml) of
        Left err -> do
            putStrLn $ "  yaml parse failed: " ++ show err
            pure False
        Right (adv :: Adventure) -> case compileAdventure adv of
            Left errs -> do
                putStrLn $ "  compile failed: " ++ issuesText errs
                pure False
            Right cr -> do
                let view = buildProjectView adv (crWarnings cr)
                    enc = encodeProjectView view
                    j = BLC.unpack enc
                    rooms' = pvRooms view
                    roomOf :: String -> Maybe PVRoom
                    roomOf i = listToMaybe [ r | r <- rooms', pvrId r == i ]
                    indexOf needle hay = T.length (fst (T.breakOn (T.pack needle) (T.pack hay)))
                r1 <- expectTrue "version field" (contains "\"version\": 1" enc)
                r2 <- expectTrue "rooms in declaration order"
                        (map pvrId rooms' == ["a", "b", "abseits"])
                r3 <- expectEqual (fmap (\r -> (pvrX r, pvrY r)) (roomOf "a")) (Just (2, 2))
                r4 <- expectEqual (fmap pvrSource (roomOf "a")) (Just "authored")
                r5 <- expectEqual (fmap pvrSource (roomOf "b")) (Just "layout")
                r6 <- expectEqual (fmap pvrFloor (roomOf "b")) (Just 1)
                r7 <- expectEqual (pvrUnreachable (pvReachability view)) ["abseits"]
                r8 <- expectEqual (map (\e -> (pveFrom e, pveTo e, pveDirection e)) (pvEdges view))
                                   [("a","b","north")]
                r9 <- expectEqual
                        (map (\q -> (pvqId q, pvqOnComplete q, pvqStartable q, pvqProgressed q))
                            (pvQuests view))
                        ([("q1",Just "q2",True,True), ("q2",Nothing,True,False)]
                            :: [(String, Maybe String, Bool, Bool)])
                r10 <- expectTrue "the unreached quest shows up as an issue"
                        (contains "QuestNeverProgressed" enc)
                r11 <- expectEqual (encodeProjectView (buildProjectView adv (crWarnings cr))) enc
                r12 <- expectTrue "keys are sorted at every depth"
                        (indexOf "direction" j < indexOf "from" j
                         && indexOf "edges" j < indexOf "rooms" j)
                pure (and [r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12])

testMapGridTellsRoomsApart :: IO Bool
testMapGridTellsRoomsApart = do
    let mk i x y = PVRoom { pvrId = i, pvrName = i, pvrX = x, pvrY = y
                          , pvrFloor = 0, pvrSource = "layout" }
        pin i x y = (mk i x y) { pvrSource = "authored" }
        grid = unlines (renderMapGrid 4 [ mk "loc_1" 0 0, mk "loc_17" 1 0
                                       , mk "loc_3" 0 1, pin "wacht" 1 1
                                       , (mk "tief" 0 0) { pvrFloor = 1 } ])
        txt = BLC.pack grid
    r1 <- expectTrue "the long id is not cut" (contains "loc_17" txt)
    r2 <- expectTrue "no cut marker on an id that fits" (not (contains "~" txt))
    r3 <- expectTrue "rows carry their y" (contains "y=1" txt)
    r4 <- expectTrue "columns carry their x" (contains "x=1" txt)
    r5 <- expectTrue "an authored position is starred" (contains "*wacht" txt)
    r6 <- expectTrue "an id too long for any sane cell is marked, not silently cut"
            (contains "~" (BLC.pack (unlines
                (renderMapGrid 4 [ mk "raum_mit_einer_sehr_ausfuehrlichen_id" 0 0 ]))))
    r7 <- expectEqual
            2 (length [ () | l <- lines grid, "-- floor" `contains` BLC.pack l ])
    pure (and [r1,r2,r3,r4,r5,r6,r7])

-- ===========================================================================
-- B6: game export (bundle)
-- ===========================================================================

-- | Asset collection: sfx:/music: over every surface — nested `if:`/`random:`
--   branches included — plus the `assets:` manifest; sorted and deduplicated.
testCollectAssetRefs :: IO Bool
testCollectAssetRefs = do
    let npc = (minNpcKey "sprecher")
            { anLocation = "loc_0"
            , anTopics = Map.fromList [("geruecht", AOPlaySfx "audio/da.wav")]
            , anOnTalk = Just (AOPlaySfx "audio/hi.wav")
            , anDialogue = Map.singleton "alive" (ADialogueTree "e"
                (Map.singleton "e" (ADialogueNode "Hallo."
                    [ ADialogueChoice "Tschuess" Nothing Nothing [AOPlayMusic "audio/end.xm"] ])))
            }
        item = (minItem "radio")
            { aiOnTake = Just
                [ AOPlayMusic "audio/theme.xm"
                , AOConditional (E.HasFlag "x") [AOPlaySfx "audio/da.wav"] []
                , AORandomChoice "" [(1, [AOPlaySfx "audio/rand.wav"])]
                ] }
        adv = (advWithItem item) { advNPCs = [npc], advAssets = ["README.txt", "audio/theme.xm"] }
    expectEqual [ "README.txt", "audio/da.wav", "audio/end.xm"
                , "audio/hi.wav", "audio/rand.wav", "audio/theme.xm" ]
                (collectAssetRefs adv)

-- | The outcome traversal is recursive (B6 contract): a `give: {to:}` nested
--   in an `if:` branch is validated like a top-level one.
testAllAOutcomesDeep :: IO Bool
testAllAOutcomesDeep = do
    let item = (minItem "schluessel")
            { aiOnTake = Just [AOConditional (E.HasFlag "x") [AOGiveTo "schluessel" "niemand"] []] }
    case compileAdventure (advWithItem item) of
        Left errs -> expectTrue ("nested give.to caught: " ++ issuesText errs)
                        (any (\i -> ciCode i == "UnknownNpc") errs)
        Right _ -> expectTrue "expected a compile error" False

-- | exportBundle writes the compile bytes verbatim, both launchers and the
--   referenced assets in their relative layout; missing assets and paths that
--   escape the bundle root are warnings, not errors.
testExportBundle :: IO Bool
testExportBundle = do
    tmpRoot <- getTemporaryDirectory
    let srcDir = tmpRoot </> "wb-b6-src"
        outDir = tmpRoot </> "wb-b6-bundle"
        cleanDir d = do
            ex <- doesDirectoryExist d
            when ex (removeDirectoryRecursive d)
    mapM_ cleanDir [srcDir, outDir]
    createDirectoryIfMissing True (srcDir </> "audio")
    writeFile (srcDir </> "audio" </> "theme.xm") "placeholder"
    writeFile (srcDir </> "liesmich.txt") "hallo"
    let item = (minItem "radio")
            { aiOnTake = Just [AOPlayMusic "audio/theme.xm", AOPlaySfx "audio/fehlt.wav"] }
        adv = (advWithItem item) { advAssets = ["liesmich.txt", "../escape.bin"] }
    case compileAdventure adv of
        Left errs -> do
            putStrLn $ "  compile failed: " ++ issuesText errs
            pure False
        Right cr -> do
            warns <- exportBundle srcDir outDir (crWorld cr) (crSave cr) (collectAssetRefs adv) False
            worldBytes <- BLC.readFile (outDir </> "world.json")
            r1 <- expectEqual (Aeson.encode (crWorld cr)) worldBytes
            shExists <- doesFileExist (outDir </> "play.sh")
            batExists <- doesFileExist (outDir </> "play.bat")
            perms <- getPermissions (outDir </> "play.sh")
            shBody <- readFile (outDir </> "play.sh")
            batBody <- readFile (outDir </> "play.bat")
            -- Windows has no POSIX owner-execute bit: `setPermissions` is a no-op
            -- there and `getPermissions` always reports executable=False, while the
            -- Windows launcher is play.bat. So the bit is asserted on POSIX only.
            r2 <- expectTrue "launchers written (play.sh executable on POSIX)"
                    (shExists && batExists && (Info.os == "mingw32" || executable perms))
            r2b <- expectEqual launcherSh shBody
            r2c <- expectEqual launcherBat batBody
            copiedTheme <- doesFileExist (outDir </> "audio" </> "theme.xm")
            copiedReadme <- doesFileExist (outDir </> "liesmich.txt")
            r3 <- expectTrue "assets copied in relative layout" (copiedTheme && copiedReadme)
            r4 <- expectTrue ("missing asset warns: " ++ show warns)
                    (any ("not found" `isInfixOf`) warns)
            r5 <- expectTrue ("path escape warns: " ++ show warns)
                    (any ("not game-root-relative" `isInfixOf`) warns)
            r6 <- expectEqual [ "world.json", "save.json", "play.sh", "play.bat"
                              , "../escape.bin", "audio/fehlt.wav", "audio/theme.xm", "liesmich.txt" ]
                    (bundleFiles (collectAssetRefs adv))
            mapM_ cleanDir [srcDir, outDir]
            pure (r1 && r2 && r2b && r2c && r3 && r4 && r5 && r6)

-- | `assets:` is a known adventure key and feeds the collection.
testAssetsKnownKey :: IO Bool
testAssetsKnownKey = do
    let yaml = unlines
            [ "start_room: loc_0"
            , "rooms:"
            , "  - id: loc_0"
            , "    name: Halle"
            , "    desc: Eine Halle."
            , "assets:"
            , "  - README.txt"
            ]
    case decode1 (BLC.pack yaml) of
        Left err -> do
            putStrLn $ "  yaml parse failed: " ++ show err
            pure False
        Right (adv :: Adventure) -> case compileAdventure adv of
            Left errs -> do
                putStrLn $ "  compile failed: " ++ issuesText errs
                pure False
            Right cr -> do
                let keyWarns = filter (\i -> ciCode i == "UnknownYamlKey") (crWarnings cr)
                r1 <- expectTrue ("expected 0 unknown key warnings, got " ++ show (length keyWarns)) (null keyWarns)
                r2 <- expectEqual ["README.txt"] (collectAssetRefs adv)
                pure (r1 && r2)

-- ===========================================================================
-- B4: Regel-Diagnostik (dead content)
-- ===========================================================================

-- | Rules whose event can never fire: unknown room/item refs in `on:`,
--   `custom X` without any `raise: X`, unknown chapter, `levelup` without a
--   progression section — and the live counterparts stay silent.
testUnreachableTriggerEvents :: IO Bool
testUnreachableTriggerEvents = do
    let mk tId on = ATrigger tId on Nothing [AOMessage "x"] False 0
        deadRules = [ mk "t1" "enter nirwana", mk "t2" "take spiegel"
                    , mk "t3" "custom sturm", mk "t4" "chapter ende"
                    , mk "t5" "levelup 2" ]
        dead = (minAdventure (minRoom "loc_0")) { advTriggers = deadRules }
        live = (minAdventure (minRoom "loc_0"))
            { advRooms = [minRoom "loc_0", minRoom "nirwana"]
            , advItems = [(minItem "spiegel") { aiOnTake = Just [AORaiseEvent "sturm"] }]
            , advTriggers = [ mk "t1" "enter nirwana", mk "t2" "take spiegel"
                            , mk "t3" "custom sturm" ] }
        codesOf code cr = [ ciMessage i | i <- crWarnings cr, ciCode i == code ]
    case compileAdventure dead of
        Left errs -> expectTrue ("compile: " ++ issuesText errs) False
        Right cr -> do
            let msgs = codesOf "UnreachableTrigger" cr
            r1 <- expectEqual 5 (length msgs)
            r2 <- expectTrue ("room ref: " ++ show msgs) (any ("no room 'nirwana'" `isInfixOf`) msgs)
            r3 <- expectTrue ("item ref: " ++ show msgs) (any ("no item 'spiegel'" `isInfixOf`) msgs)
            r4 <- expectTrue ("raise ref: " ++ show msgs) (any ("raises 'sturm'" `isInfixOf`) msgs)
            r5 <- expectTrue ("chapter ref: " ++ show msgs) (any ("no chapter 'ende'" `isInfixOf`) msgs)
            r6 <- expectTrue ("progression: " ++ show msgs) (any ("progression" `isInfixOf`) msgs)
            case compileAdventure live of
                Left errs2 -> expectTrue ("compile live: " ++ issuesText errs2) False
                Right cr2 -> do
                    r7 <- expectEqual [] (codesOf "UnreachableTrigger" cr2)
                    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7)

-- | Conditions that can never hold: literal contradictions, disjoint numeric
--   bounds, conflicting text values, never-set flags — tautologies and live
--   alternatives stay silent.
testUnsatisfiableConditions :: IO Bool
testUnsatisfiableConditions = do
    let mk tId whenP = ATrigger tId "turn" (Just whenP) [AOMessage "x"] False 0
        flagA = E.HasFlag "a"
        rules =
            [ mk "clash" (E.PAll [flagA, E.PNot flagA])
            , mk "numeric" (E.PAll [ E.CompareVar "mana" E.CGte 5
                                   , E.CompareVar "mana" E.CLt 3 ])
            , mk "text" (E.PAll [E.VarIs "ort" "halle", E.VarIs "ort" "keller"])
            , mk "neverset" (E.HasFlag "nie")
            , mk "live" flagA
            , mk "any" (E.PAny [E.HasFlag "nie", flagA])
            ]
        adv = (minAdventure (minRoom "loc_0"))
            { advTriggers = rules
            , advInitialFlags = Map.singleton "a" "true" }
        msgsOf code cr = [ ciMessage i | i <- crWarnings cr, ciCode i == code ]
    case compileAdventure adv of
        Left errs -> expectTrue ("compile: " ++ issuesText errs) False
        Right cr -> do
            let msgs = msgsOf "UnsatisfiableCondition" cr
            r1 <- expectEqual 4 (length msgs)
            r2 <- expectTrue ("contradiction: " ++ show msgs) (any ("negation" `isInfixOf`) msgs)
            r3 <- expectTrue ("numeric: " ++ show msgs) (any ("'mana'" `isInfixOf`) msgs)
            r4 <- expectTrue ("text: " ++ show msgs) (any ("'ort'" `isInfixOf`) msgs)
            r5 <- expectTrue ("never set: " ++ show msgs) (any ("'nie' is never set" `isInfixOf`) msgs)
            pure (r1 && r2 && r3 && r4 && r5)

-- | Exits that can never be taken: guards that can never hold and locks that
--   can never be opened. NPC-locks (dying unlocks), container-locks (the
--   `unlock` verb) and explicit `set_state … unlocked` stay silent.
testDeadExits :: IO Bool
testDeadExits = do
    let roomB = minRoom "b"
        exitTo tgt lock whenP = AExitRef tgt lock whenP Nothing
        wolf = (minNpcKey "wolf") { anLocation = "a" }
        kiste = (minItem "kiste") { aiCapacity = Just 3, aiLocation = "a" }
        hebel = (minItem "hebel")
            { aiLocation = "a"
            , aiOnTake = Just [AOSetEntityState "freigeschaltet" "unlocked"] }
        roomA = (minRoom "a")
            { arExits = Map.fromList
                [ ("north", exitTo "b" Nothing (Just (E.PNot E.PTrue)))
                , ("east",  exitTo "b" (Just "tuer") Nothing)
                , ("south", exitTo "b" (Just "freigeschaltet") Nothing)
                , ("west",  exitTo "b" (Just "wolf") Nothing)
                , ("down",  exitTo "b" (Just "kiste") Nothing)
                ] }
        adv = (minAdventure roomA)
            { advRooms = [roomA, roomB]
            , advNPCs = [wolf]
            , advItems = [kiste, hebel] }
        msgsOf code cr = [ ciMessage i | i <- crWarnings cr, ciCode i == code ]
    case compileAdventure adv of
        Left errs -> expectTrue ("compile: " ++ issuesText errs) False
        Right cr -> do
            let msgs = msgsOf "DeadExit" cr
            r1 <- expectEqual 2 (length msgs)
            r2 <- expectTrue ("guard: " ++ show msgs) (any ("guard can never hold" `isInfixOf`) msgs)
            r3 <- expectTrue ("lock: " ++ show msgs) (any ("'tuer'" `isInfixOf`) msgs)
            r4 <- expectTrue ("set_state unlocks: " ++ show msgs)
                    (not (any ("freigeschaltet" `isInfixOf`) msgs))
            r5 <- expectTrue ("npc lock silent: " ++ show msgs)
                    (not (any ("'wolf'" `isInfixOf`) msgs))
            r6 <- expectTrue ("container lock silent: " ++ show msgs)
                    (not (any ("'kiste'" `isInfixOf`) msgs))
            pure (r1 && r2 && r3 && r4 && r5 && r6)

-- | Rooms the player can never reach from start_room — exits and dynamic
--   edges (set_exit, generate_room) count, explicit arrivals (`move:`) count
--   as reachable.
testUnreachableRooms :: IO Bool
testUnreachableRooms = do
    let roomC = minRoom "c"
        roomB = minRoom "b"
        roomBtoC = (minRoom "b") { arExits = Map.singleton "north" (AExitRef "c" Nothing Nothing Nothing) }
        isolatedA = (minRoom "a") { arExits = Map.singleton "north" (AExitRef "b" Nothing Nothing Nothing) }
        linkedA = (minRoom "a")
            { arExits = Map.fromList
                [ ("north", AExitRef "b" Nothing Nothing Nothing)
                , ("east", AExitRef "c" Nothing Nothing Nothing) ] }
        viaEffectA = isolatedA { arOnEnter = Just [AORoomTransition "c"] }
        viaSetExitA = isolatedA { arOnEnter = Just [AOSetExit "a" "up" "c" Nothing] }
        codesOf code cr = [ ciPath i | i <- crWarnings cr, ciCode i == code ]
        runCase rms = case compileAdventure ((minAdventure (head rms)) { advRooms = rms }) of
            Left errs  -> Left ("compile: " ++ issuesText errs)
            Right cr   -> Right (codesOf "UnreachableRoom" cr)
    r1 <- expectEqual (Right ["rooms.c"]) (runCase [isolatedA, roomB, roomC])
    r2 <- expectEqual (Right []) (runCase [linkedA, roomB, roomC])
    r3 <- expectEqual (Right []) (runCase [viaEffectA, roomB, roomC])
    r4 <- expectEqual (Right []) (runCase [viaSetExitA, roomB, roomC])
    r5 <- expectEqual (Right ["rooms.b", "rooms.c"]) (runCase [minRoom "a", roomBtoC, roomC])
    pure (r1 && r2 && r3 && r4 && r5)

-- ---------------------------------------------------------------------------
-- B5: content fuzzer
-- ---------------------------------------------------------------------------

-- | B5 helper: a tiny compiled world (one room, one item) for fuzzer runs.
fuzzFixtureWorld :: IO (Maybe (E.GameWorld, E.SaveState))
fuzzFixtureWorld = do
    let lampe = (minItem "lampe") { aiLocation = "a" }
        adv = (minAdventure (minRoom "a")) { advItems = [lampe] }
    case compileAdventure adv of
        Left errs -> do
            putStrLn ("  compile: " ++ issuesText errs)
            pure Nothing
        Right cr  -> pure (Just (crWorld cr, crSave cr))

-- | The generated stream is a pure function of the seed (B5): same seed, same
--   200 inputs; a different seed diverges.
testFuzzGenDeterministic :: IO Bool
testFuzzGenDeterministic = do
    let vocab = FuzzVocab
            { fvActions = ["look", "take", "use"]
            , fvNouns   = ["lampe", "wolf"]
            , fvDirs    = ["north", "south"] }
        sampleA = take 200 (genInputs 42 vocab)
        sampleB = take 200 (genInputs 42 vocab)
        sampleC = take 200 (genInputs 43 vocab)
    r1 <- expectEqual sampleA sampleB
    r2 <- expectTrue "different seeds produce different streams" (sampleA /= sampleC)
    pure (r1 && r2)

-- | The generator draws its target words from the compiled world (B5).
testFuzzVocabExtraction :: IO Bool
testFuzzVocabExtraction = do
    let lampe = (minItem "lampe") { aiLocation = "a" }
        wolf  = (minNpcKey "wolf") { anLocation = "a" }
        adv   = (minAdventure (minRoom "a")) { advItems = [lampe], advNPCs = [wolf] }
    case compileAdventure adv of
        Left errs -> expectTrue ("compile: " ++ issuesText errs) False
        Right cr -> do
            let vocab = fuzzVocab (crWorld cr)
                sample = take 300 (genInputs 7 vocab)
            r1 <- expectTrue "item name is in the vocabulary" ("lampe" `elem` fvNouns vocab)
            r2 <- expectTrue "npc name is in the vocabulary" ("wolf" `elem` fvNouns vocab)
            r3 <- expectTrue "stream uses world nouns"
                    (any ("lampe" `isInfixOf`) sample && any ("wolf" `isInfixOf`) sample)
            r4 <- expectTrue "stream uses directions" (any (elem "north" . words) sample)
            pure (r1 && r2 && r3 && r4)

-- | Run seeds derive from the base seed plus the run index (B5): independent
--   of how many runs ran before, reproducible per index.
testFuzzRunSeeds :: IO Bool
testFuzzRunSeeds = do
    r1 <- expectTrue "run seeds differ per index" (runSeedFor 42 1 /= runSeedFor 42 2)
    r2 <- expectEqual (runSeedFor 42 3) (runSeedFor 42 3)
    r3 <- expectTrue "base seed influences run seeds" (runSeedFor 42 1 /= runSeedFor 43 1)
    pure (r1 && r2 && r3)

-- | The frozen-window detector (B5): identical state key over k steps, at
--   least one turn-shaped command and at least three distinct inputs.
testFuzzFrozenWindow :: IO Bool
testFuzzFrozenWindow = do
    let quartet = [("take x", "S1", True), ("take y", "S1", True)
                  , ("take z", "S1", True), ("look", "S1", False)]
        frozen = take 25 (cycle quartet)
        noTurn = [ (i, k, False) | (i, k, _) <- frozen ]
        changing = take 25 (cycle [("take x", "S1", True), ("take y", "S2", True)])
        oneInput = replicate 25 ("take x", "S1", True)
    r1 <- expectTrue "frozen window with turn command fires" (frozenWindow 25 frozen)
    r2 <- expectTrue "no turn-shaped command: no window" (not (frozenWindow 25 noTurn))
    r3 <- expectTrue "changing keys: no window" (not (frozenWindow 25 changing))
    r4 <- expectTrue "window shorter than k: no window" (not (frozenWindow 25 (take 24 frozen)))
    r5 <- expectTrue "one spammed input: no window" (not (frozenWindow 25 oneInput))
    pure (r1 && r2 && r3 && r4 && r5)

-- | Crash detection (B5): an exception raised inside the step is reported as
--   a crash finding naming the exception.
testFuzzCrashDetection :: IO Bool
testFuzzCrashDetection = do
    m <- fuzzFixtureWorld
    case m of
        Nothing -> pure False
        Just (gw, sv) -> do
            let broken = sv { E.turnCount = error "kaboom" }
            found <- fuzzRun 1000000 5 25 1 99 gw broken ["take lampe", "go north"]
            case found of
                Just f -> do
                    r1 <- expectEqual FCrash (ffKind f)
                    r2 <- expectTrue ("detail names the exception: " ++ ffDetail f)
                            ("kaboom" `isInfixOf` ffDetail f)
                    pure (r1 && r2)
                Nothing -> expectTrue "expected a crash finding" False

-- | Hang detection (B5): a step that never returns is reported as a hang
--   finding instead of blocking the run.
testFuzzHangDetection :: IO Bool
testFuzzHangDetection = do
    m <- fuzzFixtureWorld
    case m of
        Nothing -> pure False
        Just (gw, sv) -> do
            let hangInt :: Int
                hangInt = hangInt
                hanging = sv { E.turnCount = hangInt }
            found <- fuzzRun 200000 5 25 1 99 gw hanging ["take lampe", "go north"]
            case found of
                Just f  -> expectEqual FHang (ffKind f)
                Nothing -> expectTrue "expected a hang finding" False

-- | End-to-end loop finding (B5): a `before take` veto that never consumes a
--   turn freezes the game — 25 steps without any state/turn progress must be
--   reported as a loop finding at exactly the window boundary.
testFuzzLoopEndToEnd :: IO Bool
testFuzzLoopEndToEnd = do
    let lampe = (minItem "lampe") { aiLocation = "a" }
        blockTakes = ATrigger
            { atId = "block_takes"
            , atOn = "before take"
            , atWhen = Nothing
            , atEffects = [AOBlock (Just "nope") False]
            , atOnce = False
            , atCooldown = 0 }
        adv = (minAdventure (minRoom "a")) { advItems = [lampe], advTriggers = [blockTakes] }
    case compileAdventure adv of
        Left errs -> expectTrue ("compile: " ++ issuesText errs) False
        Right cr -> do
            let blocked = take 30 (cycle ["take lampe", "take wolf", "take ding", "help"])
            found <- fuzzRun 1000000 40 25 1 99 (crWorld cr) (crSave cr) blocked
            case found of
                Just f -> do
                    r1 <- expectEqual FLoop (ffKind f)
                    r2 <- expectEqual 25 (ffStep f)
                    pure (r1 && r2)
                Nothing -> expectTrue "expected a frozen-loop finding" False

-- | A veto-free world can never freeze (turn-shaped commands always advance
--   the clock) — a generated run over a clean world must stay clean (B5).
testFuzzSmoke :: IO Bool
testFuzzSmoke = do
    m <- fuzzFixtureWorld
    case m of
        Nothing -> pure False
        Just (gw, sv) -> do
            let sample = take 80 (genInputs 42 (fuzzVocab gw))
            found <- fuzzRun 1000000 80 25 1 42 gw sv sample
            case found of
                Nothing -> expectTrue "clean run" True
                Just f  -> expectTrue ("unexpected finding: " ++ show f) False

-- ---------------------------------------------------------------------------
-- B8: named RNG streams (authoring side)
-- ---------------------------------------------------------------------------

-- | B8: the `rng.*` namespace is engine-internal — every authored write
--   (`set_var`/`set_text_var`/`add_var`/`compute_var`, `variables:`
--   declarations, `initial_variables:` entries, procedure params, also
--   nested inside `random:` branches) is a hard compile error. Stream
--   *reads* via `random: {stream: …}` are fine, and the object form carries
--   no unknown YAML keys.
testRngVarWriteGuard :: IO Bool
testRngVarWriteGuard = do
    let rngRule effs = ATrigger
            { atId = "t", atOn = "turn", atWhen = Nothing
            , atEffects = effs, atOnce = False, atCooldown = 0 }
        advWithEffects effs = (minAdventure (minRoom "a")) { advTriggers = [rngRule effs] }
        rngErrCount adv = case compileAdventure adv of
            Left errs -> length [ e | e <- errs, ciCode e == "RngVarWrite" ]
            Right _   -> 0
        textVarDecl = AVariable
            { avbVarName = "rng.d", avbVarType = "int", avbInitial = Nothing
            , avbMin = Nothing, avbMax = Nothing }
        procWithRngParam = AProcDef
            { apId = "p", apParams = ["rng.p"], apEffects = [] }
        rawRandomVal = maybe (Aeson.object []) id (Aeson.decode (BLC.pack
            ("{\"rules\": [{\"id\": \"t\", \"on\": \"turn\", \"effects\": ["
             ++ "{\"random\": {\"stream\": \"beute\", \"choices\": [[1, [{\"msg\": \"x\"}]]]}}]}]}")))
        cleanAdv = advWithEffects [AOSetVar "foo" 1, AORandomChoice "beute" [(1, [AOMessage "hi"])]]
    r1 <- expectEqual 1 (rngErrCount (advWithEffects [AOSetVar "rng.x" 1]))
    r2 <- expectEqual 1 (rngErrCount (advWithEffects [AOAddVar "rng.y" 3]))
    r3 <- expectEqual 1 (rngErrCount (advWithEffects [AOComputeVar "rng.z" (E.ELit 1)]))
    r4 <- expectEqual 1 (rngErrCount (advWithEffects [AOSetTextVar "rng.t" "s"]))
    r5 <- expectEqual 1 (rngErrCount ((minAdventure (minRoom "a"))
            { advInitialVariables = Map.singleton "rng.q" (Aeson.Number 1) }))
    r6 <- expectEqual 1 (rngErrCount ((minAdventure (minRoom "a"))
            { advVariables = [textVarDecl] }))
    r7 <- expectEqual 1 (rngErrCount ((minAdventure (minRoom "a"))
            { advProcedures = [procWithRngParam] }))
    r8 <- expectEqual 1 (rngErrCount
            (advWithEffects [AORandomChoice "" [(1, [AOSetVar "rng.n" 1])]]))
    r9 <- expectEqual 0 (rngErrCount cleanAdv)
    r10 <- expectRight (compileAdventure cleanAdv)
    r11 <- expectTrue "random object form has no UnknownYamlKey"
            (null [ () | e <- checkUnknownYamlKeys rawRandomVal, ciCode e == "UnknownYamlKey" ])
    pure (and [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11])

-- ---------------------------------------------------------------------------
-- K1: dice pool (roll_dice)
-- ---------------------------------------------------------------------------

-- | K1.1: roll_dice compile validation rejects invalid die, pool and keep values.
testRollDiceValidation :: IO Bool
testRollDiceValidation = do
    let rollRule effs = ATrigger
            { atId = "t", atOn = "turn", atWhen = Nothing
            , atEffects = effs, atOnce = False, atCooldown = 0 }
        advWithEffects effs = (minAdventure (minRoom "a")) { advTriggers = [rollRule effs] }
    -- die < 2 is rejected
    let badDieAdv = advWithEffects [AORollDice 1 1 "" 1]
    r1 <- expectTrue "die < 2 is a hard error"
            (case compileAdventure badDieAdv of
                Left errs -> any (\i -> ciCode i == "InvalidDiceSides") errs
                Right _   -> False)
    -- pool < 1 is rejected
    let badPoolAdv = advWithEffects [AORollDice 0 6 "" 0]
    r2 <- expectTrue "pool < 1 is a hard error"
            (case compileAdventure badPoolAdv of
                Left errs -> any (\i -> ciCode i == "InvalidDicePool") errs
                Right _   -> False)
    -- keep < 0 is rejected
    let badKeepNeg = advWithEffects [AORollDice 2 6 "" (-1)]
    r3 <- expectTrue "keep < 0 is a hard error"
            (case compileAdventure badKeepNeg of
                Left errs -> any (\i -> ciCode i == "InvalidDiceKeep") errs
                Right _   -> False)
    -- keep > pool is rejected
    let badKeepExceed = advWithEffects [AORollDice 2 6 "" 3]
    r4 <- expectTrue "keep > pool is a hard error"
            (case compileAdventure badKeepExceed of
                Left errs -> any (\i -> ciCode i == "InvalidDiceKeep") errs
                Right _   -> False)
    -- valid roll_dice compiles cleanly
    let validAdv = advWithEffects [AORollDice 3 6 "beute" 2]
    r5 <- expectRight (compileAdventure validAdv)
    -- writes to dice.* are rejected by RngVarWrite guard
    r6 <- expectTrue "authored write to dice.* is rejected"
            (case compileAdventure (advWithEffects [AOSetVar "dice.last_roll" 1]) of
                Left errs -> any (\i -> ciCode i == "RngVarWrite") errs
                Right _   -> False)
    -- YAML decoding of roll_dice object
    let rawYaml = "{\"rules\": [{\"id\": \"t\", \"on\": \"turn\", \"effects\": ["
                  ++ "{\"roll_dice\": {\"pool\": 2, \"die\": 6, \"stream\": \"beute\", \"keep\": 1}}]}]}"
    r7 <- expectTrue "roll_dice object parses from JSON"
            (case Aeson.decode (BLC.pack rawYaml) of
                Just (adv :: Adventure) ->
                    case advTriggers adv of
                        [tr] -> case atEffects tr of
                            [AORollDice 2 6 "beute" 1] -> True
                            _ -> False
                        _ -> False
                Nothing -> False)
    pure (and [r1, r2, r3, r4, r5, r6, r7])

-- | K1.1 (Variante A): {var: dice.last_roll}, {var: dice.count}, {var: dice.highest},
--   {var: dice.sum} in authored texts produce NO UnknownPlaceholder warnings,
--   even when not declared in variables: or initial_variables:.
testDicePlaceholderNoWarning :: IO Bool
testDicePlaceholderNoWarning = do
    let r0 = (minRoom "loc_0")
            { arTexts = ACondText "Du hast {var: dice.last_roll} gewuerfelt (Count: {var: dice.count}, Best: {var: dice.highest}, Sum: {var: dice.sum})." [] }
        adv = minAdventure r0
    case compileAdventure adv of
        Left errs -> do
            putStrLn $ "  unexpected compile error: " ++ show errs
            pure False
        Right cr -> do
            let warns = filter (\w -> ciCode w == "UnknownPlaceholder") (crWarnings cr)
            expectTrue "dice.* placeholders produce zero UnknownPlaceholder warnings" (null warns)

-- ---------------------------------------------------------------------------
-- Phase 4.2: verb_map phases (authoring side)
-- ---------------------------------------------------------------------------

-- | Phase 4.2: `verb_map` keys are `[before:|instead:]verb[,state]`. The
--   phase prefix reaches the compiled map, bare keys keep the historical
--   PhaseAfter behaviour, and the default state stays "intact".
testVerbMapPhaseKeys :: IO Bool
testVerbMapPhaseKeys = do
    let itemWith vm = (minItem "gem") { aiVerbMap = vm }
        compiled vm = case compileAdventure ((minAdventure (minRoom "a")) { advItems = [itemWith vm] }) of
            Left errs -> Left (length errs)
            Right cr  -> Right (E.itemVerbMap
                (Map.findWithDefault (error "missing gem") "gem" (E.itemDefs (crWorld cr))))
        one k v = Map.singleton k [v]
        keysOf vm = fmap Map.keys (compiled vm)
    r1 <- expectEqual (Right [(E.PhaseBefore, E.VTake, "intact")])
            (keysOf (one "before:take,intact" (AOMessage "a")))
    r2 <- expectEqual (Right [(E.PhaseInstead, E.VTake, "intact")])
            (keysOf (one "instead:take" (AOMessage "a")))
    r3 <- expectEqual (Right [(E.PhaseBefore, E.VUse, "primed")])
            (keysOf (one "before:use,primed" (AOMessage "a")))
    r4 <- expectEqual (Right [(E.PhaseAfter, E.VDrop, "intact")])
            (keysOf (one "drop" (AOMessage "a")))
    r5 <- expectEqual (Right [(E.PhaseAfter, E.VUse, "open")])
            (keysOf (one "use,open" (AOMessage "a")))
    pure (and [r1, r2, r3, r4, r5])

-- | Phase 4.2: one (verb, state) pair may be assigned in exactly one phase —
--   `VerbPhaseClash` is a hard error. Unknown verbs keep erroring with the
--   prefix in place.
testVerbMapPhaseClash :: IO Bool
testVerbMapPhaseClash = do
    let itemWith vm = (minItem "gem") { aiVerbMap = vm }
        codes vm = case compileAdventure ((minAdventure (minRoom "a")) { advItems = [itemWith vm] }) of
            Left errs -> [ ciCode e | e <- errs ]
            Right _   -> []
    r1 <- expectEqual 1 (length [ () | c <- codes (Map.fromList
            [ ("take,intact", [AOSetFlag "a" "true"])
            , ("instead:take,intact", [AOSetFlag "b" "true"]) ]), c == "VerbPhaseClash" ])
    r2 <- expectEqual 1 (length [ () | c <- codes (Map.fromList
            [ ("before:take", [AOSetFlag "a" "true"])
            , ("instead:take", [AOSetFlag "b" "true"]) ]), c == "VerbPhaseClash" ])
    r3 <- expectEqual 0 (length [ () | c <- codes (Map.fromList
            [ ("before:take", [AOSetFlag "a" "true"])
            , ("use,intact", [AOSetFlag "b" "true"]) ]), c == "VerbPhaseClash" ])
    r4 <- expectTrue "unknown verb errors with prefix"
            ("UnknownVerb" `elem` codes (Map.fromList [("before:frobnicate", [AOSetFlag "a" "true"])]))
    pure (and [r1, r2, r3, r4])

main :: IO ()
main = do
    hSetEncoding stdout utf8
    results <- mapM (\(name, test) -> runTest name test) tests
    let failed = length (filter not results)
    putStrLn ""
    putStrLn $ show (length results - failed) ++ " passed, " ++ show failed ++ " failed"
    when (failed > 0) exitFailure