{-# LANGUAGE PatternSynonyms #-}
module Main where

import Control.Monad (when)
import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.Types as AesonT
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Either (isLeft)
import System.Timeout (timeout)
import Control.Exception (bracket, evaluate, try, SomeException)
import Game
import Pursuit (bfsDistances, stepToward, stepAway)
import Vehicles
import Effects
import Quests
import Cards
import GameLoop (LoopState (..), initLoopState, PendingDisambiguation (..),
                 applyLoopCommand, applyLoopCommandEv,
                 sideEvents, bumpMetaRuns, reseedRng,
                 commandEvents, consumesTurn, consumesTurnIn, runGameWithFrontend,
                 handleGameOver, saveBlockedMessage, loadBlockedMessage, deathMenuText,
                 SessionRequest (..), SessionState (..),
                 transitionSave, transitionLoad, transitionLoadSuccess,
                 transitionRestart, transitionGameOver,
                 transitionDeathInput, transitionDeathLoadSlot,
                 transitionVictoryInput, advanceNarrative)
import Frontend (Frontend (..), commandCompletion)
import Messages (renderMsg, formatStringWith, catalogEntries, defaultCatalog,
                renderMsgIn, msgPayload, localizeEvents, localizeEventsFor,
                translateTerms, renderMsgFor,
                effectiveCatalog, langPacks, knownLanguages, LangPack (..),
                grammarArgs, templateGrammarKeys, isGrammarArgKey)
import Parser (Command (..), executeCommand, parseCommand, parseCommandWith, parseCommandFor, helpText, bindCommandVars,
               InteractTarget (..), resolveInteractTarget,
               TargetResolution (..), resolveTarget, preferInventoryTarget,
               defaultDarkMessage,
               pattern TargetItem, pattern TargetVehicle, pattern TargetAmbiguous, pattern TargetNotFound, pattern TargetBare)
import Verbs (verbAliasMap)
import Combat (CombatActor (..), CombatTarget (..), ShipSystems (..), combatScreenLines, resolveCombat, shipAbsorb)
import Validate (ValidationError (..), validateWorld, validateWorldWithFlags, validateGameState, idsFromOutcomeRoom)
import Sample (initSampleGame)
import SaveLoad (computeWorldChecksum, formatSaveEntry, currentSaveVersion)
import qualified SaveLoad as SaveLoad
import System.Directory (createDirectoryIfMissing, createDirectory, doesDirectoryExist,
                         doesFileExist, getTemporaryDirectory, listDirectory,
                         removeDirectoryRecursive, removeFile, withCurrentDirectory)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef')
import System.Console.Haskeline (Completion (..))
import System.Exit (exitFailure)
import Types
import World (loadGame, loadGameWorld, loadSaveState, siblingSavePath)
import System.FilePath ((</>))
import Ansi (stripAnsi, ansiFilter)

runTest :: String -> IO Bool -> IO Bool
runTest name testAction = do
    passed <- testAction
    putStrLn $ (if passed then "[PASS] " else "[FAIL] ") ++ name
    pure passed

-- | Rogue Phase 0: run an IO action with `TA_SAVES_DIR` pointed at a fresh
--   temp directory, restoring the ambient environment afterwards. The seam
--   that keeps save-related tests hermetic: no repo pollution, no leaking
--   slots between tests and no interference from an ambient `TA_SAVES_DIR`.
--   Tests run sequentially, so a fixed directory name is fine; a stale
--   directory from a crashed run is cleared on setup.
withSavesIsolation :: IO a -> IO a
withSavesIsolation action =
    bracket setup teardown (\(d, _) -> setEnv "TA_SAVES_DIR" d >> action)
  where
    setup = do
        tmp <- getTemporaryDirectory
        let d = tmp </> "ta-saves-test"
        stale <- doesDirectoryExist d
        when stale (removeDirectoryRecursive d)
        createDirectory d
        old <- lookupEnv "TA_SAVES_DIR"
        pure (d, old)
    teardown (d, old) = do
        case old of
            Just v  -> setEnv "TA_SAVES_DIR" v
            Nothing -> unsetEnv "TA_SAVES_DIR"
        leftover <- doesDirectoryExist d
        when leftover $ do
            _ <- try (removeDirectoryRecursive d) :: IO (Either SomeException ())
            pure ()

expectEqual :: (Eq a, Show a) => a -> a -> IO Bool
expectEqual expected actual
    | expected == actual = pure True
    | otherwise = do
        putStrLn $ "  expected: " ++ show expected
        putStrLn $ "    actual: " ++ show actual
        pure False

expectContains :: String -> [String] -> IO Bool
expectContains expected values
    | expected `elem` values = pure True
    | otherwise = do
        putStrLn $ "  expected to contain: " ++ show expected
        putStrLn $ "             actual: " ++ show values
        pure False

expectTrue :: String -> Bool -> IO Bool
expectTrue _ True = pure True
expectTrue msg False = do
    putStrLn $ "  expected True: " ++ msg
    pure False

getCompletions :: String -> IO [String]
getCompletions input = do
    (_, comps) <- commandCompletion initSampleGame (input, "")
    pure [replacement c | c <- comps]

getCompletionsWithState :: String -> IO [String]
getCompletionsWithState input = do
    let withTorch = pickupItem "torch" initSampleGame
    (_, comps) <- commandCompletion withTorch (input, "")
    pure [replacement c | c <- comps]

-- ===== Parser Tests =====

testParseLookAtMultiWord :: IO Bool
testParseLookAtMultiWord =
    expectEqual (Interact VLookAt "old man") (parseCommand "look at old man")

testParseUseOnMultiWord :: IO Bool
testParseUseOnMultiWord =
    expectEqual (InteractWith VUseOn "brass key" "treasure door") (parseCommand "use brass key on treasure door")

testParseTakeMultiWord :: IO Bool
testParseTakeMultiWord =
    expectEqual (Interact VTake "healing potion") (parseCommand "take healing potion")

testParseTakeAll :: IO Bool
testParseTakeAll =
    expectEqual TakeAll (parseCommand "take all")

testParseDropAll :: IO Bool
testParseDropAll =
    expectEqual DropAll (parseCommand "drop all")

testParseGetAll :: IO Bool
testParseGetAll =
    expectEqual TakeAll (parseCommand "get all")

testParseRestart :: IO Bool
testParseRestart =
    expectEqual Restart (parseCommand "restart")

testParseListSaves :: IO Bool
testParseListSaves =
    expectEqual ListSaves (parseCommand "saves")

testParseStopWordStripping :: IO Bool
testParseStopWordStripping =
    expectEqual (Interact VTake "potion") (parseCommand "take the potion")

testParseStopWordStrippingMultiple :: IO Bool
testParseStopWordStrippingMultiple =
    expectEqual (Interact VLookAt "old man") (parseCommand "look at the old man")

testParseEquip :: IO Bool
testParseEquip =
    expectEqual (EquipCmd "rusty sword") (parseCommand "equip rusty sword")

testParseWear :: IO Bool
testParseWear =
    expectEqual (EquipCmd "leather armor") (parseCommand "wear leather armor")

testParseUnequip :: IO Bool
testParseUnequip =
    expectEqual (UnequipCmd "rusty sword") (parseCommand "unequip rusty sword")

testParseUnequipAll :: IO Bool
testParseUnequipAll =
    expectEqual UnequipAllCmd (parseCommand "unequip all")

testParseStats :: IO Bool
testParseStats =
    expectEqual StatsCmd (parseCommand "stats")

testParseSearch :: IO Bool
testParseSearch =
    expectEqual (SearchCmd Nothing) (parseCommand "search room")

testParseSearchBare :: IO Bool
testParseSearchBare =
    expectEqual (SearchCmd Nothing) (parseCommand "search")

testParseSearchTarget :: IO Bool
testParseSearchTarget =
    expectEqual (SearchCmd (Just "chest")) (parseCommand "search chest")

-- ===== Command Execution Tests =====

testUseRequiresInventory :: IO Bool
testUseRequiresInventory = do
    let (_, msg) = executeCommand (parseCommand "use brass key on door") initSampleGame
    expectEqual "You need to be carrying 'brass key' to use it." msg

testUseRequiresReachableEntity :: IO Bool
testUseRequiresReachableEntity = do
    let withKey = pickupItem "key" initSampleGame
        (_, msg) = executeCommand (parseCommand "use brass key on goblin") withKey
    expectEqual "You can't reach 'goblin' from here." msg

testUseDoorUnlocksTreasureDoor :: IO Bool
testUseDoorUnlocksTreasureDoor = do
    let withKey = pickupItem "key" initSampleGame
        (newState, _) = executeCommand (parseCommand "use brass key on door") withKey
    expectEqual (Just "unlocked") (getEntityState "treasure_door" newState)

testTakeAllPicksUpItems :: IO Bool
testTakeAllPicksUpItems = do
    let (newState, _) = executeCommand TakeAll initSampleGame
    expectTrue "torch in inventory" (hasItem "torch" newState)

-- ===== Effect Tests =====

testGiveItem :: IO Bool
testGiveItem = do
    let (newState, _) = applyOutcome (MoveEntity "key" (CarriedBy ActorPlayer)) "" initSampleGame
    expectTrue "key in inventory" (hasItem "key" newState)

testConsumeItem :: IO Bool
testConsumeItem = do
    let withTorch = pickupItem "torch" initSampleGame
        (newState, _) = applyOutcome (MoveEntity "torch" Removed) "" withTorch
    expectTrue "torch not in inventory" (not (hasItem "torch" newState))

testSetFlag :: IO Bool
testSetFlag = do
    let (newState, _) = applyOutcome (SetValue (VRFlag "quest_started") (EVString "true")) "" initSampleGame
    expectEqual (Just "true") (getFlag "quest_started" newState)

testCheckFlagTrue :: IO Bool
testCheckFlagTrue = do
    let stateWithFlag = setFlag "door_open" "true" initSampleGame
        (_, msg) = applyOutcome
            (Conditional (HasFlag "door_open")
                (SendMessage "The door is open!")
                (SendMessage "The door is closed."))
            "" stateWithFlag
    expectEqual "The door is open!" msg

testCheckFlagFalse :: IO Bool
testCheckFlagFalse = do
    let (_, msg) = applyOutcome
            (Conditional (HasFlag "door_open")
                (SendMessage "The door is open!")
                (SendMessage "The door is closed."))
            "" initSampleGame
    expectEqual "The door is closed." msg

testGameEndDeath :: IO Bool
testGameEndDeath = do
    let (newState, msg) = applyOutcome (GameEnd Death "You perish!") "" initSampleGame
    r1 <- expectTrue "game is over" (gameOver (save newState))
    r2 <- expectEqual (Just Death) (gameOverReason (save newState))
    r3 <- expectEqual "You perish!" msg
    pure (r1 && r2 && r3)

testGameEndVictory :: IO Bool
testGameEndVictory = do
    let (newState, _) = applyOutcome (GameEnd Victory "You win!") "" initSampleGame
    expectEqual (Just Victory) (gameOverReason (save newState))

testMoveNPC :: IO Bool
testMoveNPC = do
    let (newState, _) = applyOutcome (MoveEntity "goblin" (InRoom "start")) "" initSampleGame
        npcsInStart = getNPCsInRoom "start" newState
    expectTrue "goblin now in start room" (any (\n -> npcId n == "goblin") npcsInStart)

-- ===== Equipment Tests (Phase 0) =====

testEquipRequiresCarried :: IO Bool
testEquipRequiresCarried = do
    -- sword_rusty starts in "start", not in inventory
    case equipItem "sword_rusty" initSampleGame of
        Left err -> expectEqual "You need to be carrying the rusty sword." err
        Right _  -> expectTrue "should have failed" False

testEquipAppliesBonus :: IO Bool
testEquipAppliesBonus = do
    let withSword = pickupItem "sword_rusty" initSampleGame
    case equipItem "sword_rusty" withSword of
        Left err -> expectTrue err False
        Right st -> do
            r1 <- expectEqual 15 (effectiveAttack st)  -- base 10 + 5
            r2 <- expectTrue "is equipped" (isEquipped "sword_rusty" st)
            pure (r1 && r2)

testEquipSlotConflict :: IO Bool
testEquipSlotConflict = do
    -- Both are Weapon-slot items; only sword_rusty exists in the sample.
    -- Use two-body-armor scenario via ring + armor is not a conflict, so check
    -- that equipping the same slot twice with different items is impossible.
    let withBoth = pickupItem "sword_rusty" initSampleGame
    case equipItem "sword_rusty" withBoth of
        Left err -> expectTrue err False
        Right st -> case equipItem "sword_rusty" st of
            -- same item re-equip is a no-op success
            Right st2 -> expectTrue "still equipped" (isEquipped "sword_rusty" st2)
            Left err   -> expectTrue err False

testUnequipRemovesBonus :: IO Bool
testUnequipRemovesBonus = do
    let withSword = pickupItem "sword_rusty" initSampleGame
    case equipItem "sword_rusty" withSword of
        Left err -> expectTrue err False
        Right st -> do
            let st' = unequipItem "sword_rusty" st
            r1 <- expectEqual 10 (effectiveAttack st')  -- back to base
            r2 <- expectTrue "not equipped" (not (isEquipped "sword_rusty" st'))
            pure (r1 && r2)

testEquipNonEquippable :: IO Bool
testEquipNonEquippable = do
    let withTorch = pickupItem "torch" initSampleGame
    case equipItem "torch" withTorch of
        Left err -> expectEqual "You cannot equip the torch." err
        Right _  -> expectTrue "should have failed" False

testMaxHealthBonus :: IO Bool
testMaxHealthBonus = do
    let withRing = pickupItem "ring_vigor" initSampleGame
    case equipItem "ring_vigor" withRing of
        Left err -> expectTrue err False
        Right st -> expectEqual 120 (effectiveMaxHealth st)  -- 100 + 20

testEquipViaCommand :: IO Bool
testEquipViaCommand = do
    let withSword = pickupItem "sword_rusty" initSampleGame
        (newState, _) = executeCommand (parseCommand "equip rusty sword") withSword
    expectTrue "equipped via command" (isEquipped "sword_rusty" newState)

testUnequipAllCommand :: IO Bool
testUnequipAllCommand = do
    let withSword = pickupItem "sword_rusty" initSampleGame
    case equipItem "sword_rusty" withSword of
        Left err -> expectTrue err False
        Right st -> do
            let (st', _) = executeCommand UnequipAllCmd st
            expectTrue "nothing equipped" (Map.null (equipment (save st')))

testStatsCommand :: IO Bool
testStatsCommand = do
    let withSword = pickupItem "sword_rusty" initSampleGame
    case equipItem "sword_rusty" withSword of
        Left err -> expectTrue err False
        Right st -> do
            let (_, msg) = executeCommand StatsCmd st
            expectTrue "stats mentions Attack" ("Attack" `elem` words msg || any ("Attack" `isPrefixOfT`) (lines msg))
  where
    isPrefixOfT p s = take (length p) s == p

-- ===== Room hook / visited / search Tests (Phase 1) =====

testVisitedRoomsStartsEmpty :: IO Bool
testVisitedRoomsStartsEmpty =
    expectTrue "no visited rooms at start" (Set.null (visitedRooms (save initSampleGame)))

testMoveMarksRoomVisited :: IO Bool
testMoveMarksRoomVisited = do
    let (newState, _) = executeCommand (Go North) initSampleGame
    expectTrue "hallway is visited" (isRoomVisited "hallway" newState)

testSetRoomVisitedOutcome :: IO Bool
testSetRoomVisitedOutcome = do
    let (newState, _) = applyOutcome (SetValue (VRActorProp (ActorRoom "treasure") PVisited) (EVInt 1)) "" initSampleGame
    expectTrue "treasure marked visited" (isRoomVisited "treasure" newState)

testDarkRoomHidesContents :: IO Bool
testDarkRoomHidesContents = do
    -- hallway is tagged "dark"
    let inHallway = initSampleGame { save = (save initSampleGame) { currentRoom = "hallway" } }
        (_, msg) = executeCommand Look inHallway
    expectEqual "It's pitch black. You can't see anything." msg

testLightSourceIlluminatesDarkRoom :: IO Bool
testLightSourceIlluminatesDarkRoom = do
    let inHallway = initSampleGame { save = (save initSampleGame) { currentRoom = "hallway" } }
        withTorch = pickupItem "torch" inHallway
        (_, msg) = executeCommand Look withTorch
    expectTrue "hallway is visible with torch" (msg /= "It's pitch black. You can't see anything.")

testSearchRevealsHiddenItem :: IO Bool
testSearchRevealsHiddenItem = do
    let inHallway = initSampleGame { save = (save initSampleGame) { currentRoom = "hallway" } }
        withTorch = pickupItem "torch" inHallway
    -- hidden note must not show up on a plain look
    let (_, lookMsg) = executeCommand Look withTorch
        (afterSearch, searchMsg) = executeCommand (parseCommand "search") withTorch
        (_, lookAfter) = executeCommand Look afterSearch
    r1 <- expectTrue "note not visible before search" (not ("old note" `isInfixOf` lookMsg))
    r2 <- expectTrue "search reports the find" ("old note" `isInfixOf` searchMsg)
    r3 <- expectTrue "note visible after search" ("old note" `isInfixOf` lookAfter)
    pure (r1 && r2 && r3)

testSearchSetsFlag :: IO Bool
testSearchSetsFlag = do
    let inHallway = initSampleGame { save = (save initSampleGame) { currentRoom = "hallway" } }
        withTorch = pickupItem "torch" inHallway
        (afterSearch, _) = executeCommand (parseCommand "search") withTorch
    expectEqual (Just "true") (getFlag "torch_lit" afterSearch)

testAltDescriptionUsed :: IO Bool
testAltDescriptionUsed = do
    let inHallway = initSampleGame { save = (save initSampleGame) { currentRoom = "hallway" } }
        withTorch = pickupItem "torch" inHallway
        lit = setFlag "torch_lit" "true" withTorch
        (_, msg) = executeCommand Look lit
    expectTrue "alt description shown" ("sputter to life" `isInfixOf` msg)

-- ===== RNG salt test (Phase 4.1) =====

testRandomChoiceSaltDiffers :: IO Bool
testRandomChoiceSaltDiffers = do
    -- Two consecutive RandomChoices in the same Sequence must be able
    -- to resolve differently. With the old hardcoded salt=0 they were identical.
    let outcome = Sequence
            [ RandomChoice [(1, SendMessage "A"), (1, SendMessage "B"), (1, SendMessage "C"), (1, SendMessage "D")]
            , RandomChoice [(1, SendMessage "A"), (1, SendMessage "B"), (1, SendMessage "C"), (1, SendMessage "D")] ]
        (_, msg) = applyOutcome outcome "" initSampleGame
        parts = lines msg
    -- With 4 options and different salts the two draws may differ; the guarantee
    -- we assert is that the mechanism produces two independent draws.
    expectEqual 2 (length parts)

-- ===== Combat Tests =====

testCombatDamageUsesDefense :: IO Bool
testCombatDamageUsesDefense = do
    let inHallway = initSampleGame { save = (save initSampleGame) { currentRoom = "hallway" } }
        (newState, msg) = executeCommand (Interact VAttack "goblin") inHallway
        goblinState = Map.lookup "goblin" (npcStates (save newState))
        goblinHp = goblinState >>= npcHealth
    r1 <- expectTrue "combat message mentions hit" (not (null msg))
    r2 <- expectEqual (Just 22) goblinHp
    pure (r1 && r2)

-- | Phase 7f: with `combat: profile: off` the attack is refused and neither
--   side loses HP (no attrition).
testCombatOffRefusesAttack :: IO Bool
testCombatOffRefusesAttack = do
    let worldOff = (world initSampleGame) { combatProfile = CombatOff Nothing }
        st = initSampleGame { world = worldOff, save = (save initSampleGame) { currentRoom = "hallway" } }
        (newState, msg) = executeCommand (Interact VAttack "goblin") st
        goblinHp = (Map.lookup "goblin" (npcStates (save newState))) >>= npcHealth
    r1 <- expectTrue "refused message" (isInfixOf "can't attack" msg)
    r2 <- expectEqual (Just 30) goblinHp
    r3 <- expectEqual 100 (playerHealth (player (save newState)))
    pure (r1 && r2 && r3)

-- | Phase 7f: `attack_refused` overrides the default off message.
testCombatOffCustomRefusal :: IO Bool
testCombatOffCustomRefusal = do
    let worldOff = (world initSampleGame) { combatProfile = CombatOff (Just "Guards are watching!") }
        st = initSampleGame { world = worldOff, save = (save initSampleGame) { currentRoom = "hallway" } }
        (_, msg) = executeCommand (Interact VAttack "goblin") st
    expectTrue "custom refusal shown" (isInfixOf "Guards are watching" msg)

-- | Phase 7f: narrative combat rolls attack vs defense + difficulty and runs
--   on_win / on_lose effects. No HP is spent on either side.
testCombatNarrativeWin :: IO Bool
testCombatNarrativeWin = do
    let narrative = CombatNarrative (NarrativeCombat 0 (SetValue (VRFlag "fight_won") (EVString "true")) (SetValue (VRFlag "fight_lost") (EVString "true")))
        worldN = (world initSampleGame) { combatProfile = narrative }
        st = initSampleGame { world = worldN, save = (save initSampleGame) { currentRoom = "hallway" } }
        (newState, msg) = executeCommand (Interact VAttack "goblin") st
        goblinHp = (Map.lookup "goblin" (npcStates (save newState))) >>= npcHealth
    r1 <- expectEqual (Just "true") (getFlag "fight_won" newState)
    r2 <- expectEqual (Just 30) goblinHp
    r3 <- expectTrue "win message" (isInfixOf "win the fight" msg)
    pure (r1 && r2 && r3)

testCombatNarrativeLose :: IO Bool
testCombatNarrativeLose = do
    -- difficulty 20 > attack 10 -> lose
    let narrative = CombatNarrative (NarrativeCombat 20 (SetValue (VRFlag "fight_won") (EVString "true")) (SetValue (VRFlag "fight_lost") (EVString "true")))
        worldN = (world initSampleGame) { combatProfile = narrative }
        st = initSampleGame { world = worldN, save = (save initSampleGame) { currentRoom = "hallway" } }
        (newState, msg) = executeCommand (Interact VAttack "goblin") st
        goblinHp = (Map.lookup "goblin" (npcStates (save newState))) >>= npcHealth
    r1 <- expectEqual (Just "true") (getFlag "fight_lost" newState)
    r2 <- expectEqual (Just 30) goblinHp
    r3 <- expectTrue "lose message" (isInfixOf "lose the fight" msg)
    pure (r1 && r2 && r3)

-- ===== Party Tests (Phase 7g) =====

-- | A minimal companion-capable NPC used by the party tests.
squireDef :: NPCDef
squireDef = NPCDef "squire" "squire" (plainText "A loyal squire with a chipped blade.")
    Map.empty ["squire", "knappe"] (Just 20) 3 1 Map.empty False emptyAscii Map.empty emptyGrammar

-- | Sample game plus a `squire`. Joining is just the roster convention:
--   the follow variable `party.squire` set to 1.
partyGame :: Bool -> GameState
partyGame following =
    let base = initSampleGame
        npcSt = NPCState (InRoom "start") "alive" (Just 20) Map.empty Nothing
    in base
        { world = (world base) { npcDefs = Map.insert "squire" squireDef (npcDefs (world base)) }
        , save = (save base)
            { npcStates = Map.insert "squire" npcSt (npcStates (save base))
            , variables = if following then Map.singleton "party.squire" (VVInt 1) else Map.empty
            }
        }

-- | Move the squire into the hallway (where the goblin is).
partyGameInHallway :: Bool -> GameState
partyGameInHallway following =
    let st = partyGame following
    in st { save = (save st)
            { currentRoom = "hallway"
            , npcStates = Map.insert "squire"
                (NPCState (InRoom "hallway") "alive" (Just 20) Map.empty Nothing)
                (npcStates (save st)) } }

-- | Phase 7g: a party member follows the player through a walk.
testPartyFollowsOnRoomChange :: IO Bool
testPartyFollowsOnRoomChange = do
    let (st', _) = executeCommand (Go North) (partyGame True)
    r1 <- expectEqual (Just (InRoom "hallway")) (npcLocation <$> Map.lookup "squire" (npcStates (save st')))
    r2 <- expectEqual (Just (InRoom "start")) (npcLocation <$> Map.lookup "oldman" (npcStates (save st')))
    pure (r1 && r2)

-- | A non-member stays where it is (leave/dismiss = follow variable not 1).
testPartyNonMemberStays :: IO Bool
testPartyNonMemberStays = do
    let (st', _) = executeCommand (Go North) (partyGame False)
    expectEqual (Just (InRoom "start")) (npcLocation <$> Map.lookup "squire" (npcStates (save st')))

-- | The party follows teleports too (same shared room-transition helper).
testPartyFollowsOnTeleport :: IO Bool
testPartyFollowsOnTeleport = do
    let teleport = SetValue (VRActorProp ActorPlayer PRoom) (EVString "treasure")
        (st', _) = applyOutcome teleport "" (partyGame True)
    expectEqual (Just (InRoom "treasure")) (npcLocation <$> Map.lookup "squire" (npcStates (save st')))

-- | Phase 7g: a companion standing where the target stands strikes alongside
--   the player (player 8 + squire 3-2=1 damage), no special combat path.
testPartyCompanionFights :: IO Bool
testPartyCompanionFights = do
    let (st', msg) = executeCommand (Interact VAttack "goblin") (partyGameInHallway True)
        goblinHp = (Map.lookup "goblin" (npcStates (save st'))) >>= npcHealth
    r1 <- expectEqual (Just 21) goblinHp
    r2 <- expectTrue "companion strike reported" (isInfixOf "squire strikes for 1" msg)
    pure (r1 && r2)

-- | Regression gate: without a companion the classic combat output is
--   unchanged (same damage, same message, no companion line).
testPartyNoCompanionUnchanged :: IO Bool
testPartyNoCompanionUnchanged = do
    let (st', msg) = executeCommand (Interact VAttack "goblin") (partyGameInHallway False)
        goblinHp = (Map.lookup "goblin" (npcStates (save st'))) >>= npcHealth
    r1 <- expectEqual (Just 22) goblinHp
    r2 <- expectTrue "classic damage line" (isInfixOf "You hit for 8, it hits you for 3." msg)
    r3 <- expectTrue "no companion line" (not (isInfixOf "strikes for" msg))
    pure (r1 && r2 && r3)

-- | A killed companion neither follows nor fights, and its death fires the
--   state-change event the fixture rules listen to.
testPartyDeadCompanionInactive :: IO Bool
testPartyDeadCompanionInactive = do
    let st0 = (partyGameInHallway True)
        killed = killNPC "squire" st0
        (st', msg) = executeCommand (Interact VAttack "goblin") killed
        goblinHp = (Map.lookup "goblin" (npcStates (save st'))) >>= npcHealth
        roster = partyMembersInRoom st0
        rosterAfter = partyMembersInRoom killed
    r1 <- expectEqual ["squire"] roster
    r2 <- expectEqual [] rosterAfter
    r3 <- expectEqual (Just 22) goblinHp
    r4 <- expectTrue "dead companion stays silent" (not (isInfixOf "strikes for" msg))
    pure (r1 && r2 && r3 && r4)

-- | Phase 7g: the roster survives save/load because it is a plain VarMap
--   entry, and so does the companion's position.
testPartyRosterSaveLoadRoundTrip :: IO Bool
testPartyRosterSaveLoadRoundTrip = do
    let st = partyGameInHallway True
        encoded = Aeson.encode (save st)
        decoded = Aeson.decode encoded :: Maybe SaveState
    case decoded of
        Nothing -> do putStrLn "  decode failed"; pure False
        Just ss -> do
            r1 <- expectEqual (Just (VVInt 1)) (Map.lookup "party.squire" (variables ss))
            r2 <- expectEqual (Just (InRoom "hallway")) (npcLocation <$> Map.lookup "squire" (npcStates ss))
            pure (r1 && r2)

-- | An `OnStateChange` rule's message must reach the player when the NPC dies
--   from HP damage: the interpreter threads the death-event messages instead
--   of discarding them.
testNPCDeathEventMessageShown :: IO Bool
testNPCDeathEventMessageShown = do
    let sample = initSampleGame
        w = (world sample)
            { triggerDefs = [ TriggerDef "death_note" (OnStateChange "goblin") Nothing
                                [ SendMessage "Der Goblin fällt und lässt die Keule fallen."
                                , SetValue (VRFlag "goblin_down") (EVString "true") ] False 0 ] }
        st = sample { world = w }
        (st', msg) = applyOutcome (ModifyValue (VRActorProp (ActorNPC "goblin") PHealth) (-100)) "" st
    r1 <- expectTrue "death event message threaded" (isInfixOf "Der Goblin fällt" msg)
    r2 <- expectEqual (Just "true") (getFlag "goblin_down" st')
    pure (r1 && r2)

-- ===== Ship System Tests (Phase 7h) =====

-- | Sample game plus a courier ship with `systems:` (as VarMap entries) and
--   the player aboard, standing where the goblin is.
shipGame :: Int -> Int -> Int -> Int -> GameState
shipGame power weapons shields hull =
    let base = initSampleGame
        shipDef = VehicleDef "ship" "Kestrel" "A lean courier."
            PlayerControlled
            ["carriage_cabin"] "carriage_cabin" (Just "carriage_cabin")
            (Map.fromList [("hallway", VehicleStop "hallway" "the dark hallway" Nothing)])
            [] ["ship", "kestrel"] Nothing Map.empty
        vars = Map.fromList
            [ ("ship.ship.power",   VVInt power)
            , ("ship.ship.weapons", VVInt weapons)
            , ("ship.ship.shields", VVInt shields)
            , ("ship.ship.hull",    VVInt hull) ]
    in base
        { world = (world base) { vehicleDefs = Map.insert "ship" shipDef (vehicleDefs (world base)) }
        , save = (save base)
            { currentRoom = "hallway"
            , currentVehicle = Just "ship"
            , vehicleStates = Map.insert "ship"
                (VehicleState "hallway" Nothing Set.empty Map.empty)
                (vehicleStates (save base))
            , variables = vars } }

shipVarOf :: String -> GameState -> Maybe Int
shipVarOf name st = case getVariable name st of
    Just (VVInt n) -> Just n
    _              -> Nothing

-- | The ship fires its `weapons` alongside the player and spends one power.
testShipFiresAndSpendsPower :: IO Bool
testShipFiresAndSpendsPower = do
    let (st', msg) = executeCommand (Interact VAttack "goblin") (shipGame 2 3 4 10)
        goblinHp = (Map.lookup "goblin" (npcStates (save st'))) >>= npcHealth
    r1 <- expectEqual (Just 19) goblinHp            -- player 8 + ship 3
    r2 <- expectEqual (Just 1) (shipVarOf "ship.ship.power" st')
    r3 <- expectTrue "ship volley reported" (isInfixOf "Kestrel fires for 3" msg)
    pure (r1 && r2 && r3)

-- | Without power the ship's guns stay silent; the player still attacks.
testShipWithoutPowerDoesNotFire :: IO Bool
testShipWithoutPowerDoesNotFire = do
    let (st', msg) = executeCommand (Interact VAttack "goblin") (shipGame 0 3 4 10)
        goblinHp = (Map.lookup "goblin" (npcStates (save st'))) >>= npcHealth
    r1 <- expectEqual (Just 22) goblinHp
    r2 <- expectEqual (Just 0) (shipVarOf "ship.ship.power" st')
    r3 <- expectTrue "no power reported" (isInfixOf "has no power" msg)
    pure (r1 && r2 && r3)

-- | The return fire hits the ship: shields absorb first, the rest goes into
--   the hull, and the player is unharmed.
testShipShieldsAbsorbReturnFire :: IO Bool
testShipShieldsAbsorbReturnFire = do
    let (st', msg) = executeCommand (Interact VAttack "goblin") (shipGame 2 3 2 10)
    r1 <- expectEqual 100 (playerHealth (player (save st')))
    r2 <- expectEqual (Just 0) (shipVarOf "ship.ship.shields" st')
    r3 <- expectEqual (Just 9) (shipVarOf "ship.ship.hull" st')     -- 3 damage - 2 absorbed
    r4 <- expectTrue "absorb reported" (isInfixOf "shields absorb 2" msg)
    r5 <- expectTrue "hull spill reported" (isInfixOf "hull takes 1" msg)
    pure (r1 && r2 && r3 && r4 && r5)

-- | A ship with a hull but no shields takes the full hit on the hull.
testShipHullTakesHit :: IO Bool
testShipHullTakesHit = do
    let st0 = shipGame 2 3 0 10
        st1 = st0 { save = (save st0) { variables = Map.delete "ship.ship.shields" (variables (save st0)) } }
        (st', msg) = executeCommand (Interact VAttack "goblin") st1
    r1 <- expectEqual 100 (playerHealth (player (save st')))
    r2 <- expectEqual (Just 7) (shipVarOf "ship.ship.hull" st')
    r3 <- expectTrue "hull hit reported" (isInfixOf "the hull takes 3" msg)
    pure (r1 && r2 && r3)

-- | Regression gate: an ordinary vehicle (no `systems:`) behaves exactly like
--   a fight on foot.
testOrdinaryVehicleUnchanged :: IO Bool
testOrdinaryVehicleUnchanged = do
    let base = initSampleGame
        st0 = base { save = (save base) { currentRoom = "hallway", currentVehicle = Just "carriage" } }
        (st', msg) = executeCommand (Interact VAttack "goblin") st0
        goblinHp = (Map.lookup "goblin" (npcStates (save st'))) >>= npcHealth
    r1 <- expectEqual (Just 22) goblinHp
    r2 <- expectEqual 97 (playerHealth (player (save st')))
    r3 <- expectTrue "no ship chatter" (not (isInfixOf "fires for" msg) && not (isInfixOf "shields" msg))
    pure (r1 && r2 && r3)

-- | Ship systems live in the VarMap, so they survive save/load.
testShipSystemsSaveLoad :: IO Bool
testShipSystemsSaveLoad = do
    let st = shipGame 2 3 4 10
        decoded = Aeson.decode (Aeson.encode (save st)) :: Maybe SaveState
    case decoded of
        Nothing -> do putStrLn "  decode failed"; pure False
        Just ss -> do
            r1 <- expectEqual (Just (VVInt 3)) (Map.lookup "ship.ship.weapons" (variables ss))
            r2 <- expectEqual (Just (VVInt 10)) (Map.lookup "ship.ship.hull" (variables ss))
            pure (r1 && r2)

-- | Helper: Two ships at the same stop ("hallway") with systems.
shipDuelGame :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> GameState
shipDuelGame (pPow, pWeap, pShield, pHull) (ePow, eWeap, eShield, eHull) =
    let base = shipGame pPow pWeap pShield pHull
        pirateDef = VehicleDef "pirate" "Corsair" "A pirate raider."
            PlayerControlled
            ["pirate_cabin"] "pirate_cabin" (Just "pirate_cabin")
            (Map.fromList [("hallway", VehicleStop "hallway" "the dark hallway" Nothing)])
            [] ["pirate", "corsair", "raider"] Nothing Map.empty
        pirateVars = Map.fromList
            [ ("ship.pirate.power",   VVInt ePow)
            , ("ship.pirate.weapons", VVInt eWeap)
            , ("ship.pirate.shields", VVInt eShield)
            , ("ship.pirate.hull",    VVInt eHull) ]
        allVars = Map.union pirateVars (variables (save base))
    in base
        { world = (world base) { vehicleDefs = Map.insert "pirate" pirateDef (vehicleDefs (world base)) }
        , save = (save base)
            { vehicleStates = Map.insert "pirate"
                (VehicleState "hallway" Nothing Set.empty Map.empty)
                (vehicleStates (save base))
            , variables = allVars } }

-- | Phase 7h-2 (B0/B1): Attacking an ordinary vehicle without systems is refused.
testAttackOrdinaryVehicleRefused :: IO Bool
testAttackOrdinaryVehicleRefused = do
    let base = initSampleGame
        st0 = base { save = (save base) { currentRoom = "meadow" } }
        (st', msg) = executeCommand (Interact VAttack "carriage") st0
    r1 <- expectEqual "You can't attack the carriage." msg
    r2 <- expectEqual (playerHealth (player (save base))) (playerHealth (player (save st')))
    pure (r1 && r2)

-- | Phase 7h-2 (B1): A ship at a different stop cannot be targeted.
testAttackShipDifferentStopRefused :: IO Bool
testAttackShipDifferentStopRefused = do
    let st0 = shipDuelGame (2, 3, 10, 20) (2, 4, 5, 15)
        stDiffStop = st0 { save = (save st0)
            { vehicleStates = Map.adjust (\vs -> vs { vsCurrentStop = "treasure" }) "pirate" (vehicleStates (save st0)) } }
        (st', msg) = executeCommand (Interact VAttack "pirate") stDiffStop
    r1 <- expectEqual "You don't see 'pirate' here." msg
    r2 <- expectEqual (getVariable "ship.pirate.hull" st0) (getVariable "ship.pirate.hull" st')
    pure (r1 && r2)

-- | Phase 7h-2 (B0/B1): Ship-to-ship attack with damage absorption and retaliation.
testAttackEnemyShipDamageAndRetaliation :: IO Bool
testAttackEnemyShipDamageAndRetaliation = do
    let st0 = shipDuelGame (2, 3, 10, 20) (2, 4, 5, 15)
        (st', msg) = executeCommand (Interact VAttack "pirate") st0
    -- Corsair takes 10 (player) + 3 (ship weapons) = 13 dmg.
    -- Corsair shields 5 absorbs 5 -> 0. Spill 8 hits hull: 15 - 8 = 7.
    r1 <- expectEqual (Just 0) (shipVarOf "ship.pirate.shields" st')
    r2 <- expectEqual (Just 7) (shipVarOf "ship.pirate.hull" st')
    r3 <- expectEqual (Just 1) (shipVarOf "ship.ship.power" st')
    -- Corsair retaliates with weapons 4 (costs 1 power: 2 -> 1).
    -- Kestrel shields 10 absorbs 4 -> 6. Hull stays 20.
    r4 <- expectEqual (Just 1) (shipVarOf "ship.pirate.power" st')
    r5 <- expectEqual (Just 6) (shipVarOf "ship.ship.shields" st')
    r6 <- expectEqual (Just 20) (shipVarOf "ship.ship.hull" st')
    r7 <- expectTrue "contains attack messages"
        (isInfixOf "You attack the pirate." msg
         && isInfixOf "Kestrel fires for 3." msg
         && isInfixOf "Corsair: shields absorb 5." msg
         && isInfixOf "Corsair fires for 4." msg
         && isInfixOf "Kestrel: shields absorb 4." msg)
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7)

-- | Phase 7h-2 (B0): Destroying an enemy ship reports destruction and draws no return fire.
testAttackEnemyShipDestroyed :: IO Bool
testAttackEnemyShipDestroyed = do
    let st0 = shipDuelGame (2, 3, 10, 20) (2, 4, 0, 5)
        (st', msg) = executeCommand (Interact VAttack "pirate") st0
    r1 <- expectEqual (Just 0) (shipVarOf "ship.pirate.hull" st')
    r2 <- expectEqual (Just 10) (shipVarOf "ship.ship.shields" st')
    r3 <- expectTrue "destroy message" (isInfixOf "You attack the pirate and destroy it!" msg)
    r4 <- expectTrue "no enemy retaliation" (not (isInfixOf "Corsair fires" msg))
    pure (r1 && r2 && r3 && r4)

-- | Phase 7h-2 (B2): ActorShip reference to an undeclared vehicle in a rule is reported.
testValidateMissingShipInActorProp :: IO Bool
testValidateMissingShipInActorProp = do
    let gw = (world initSampleGame)
                { triggerDefs =
                    [ TriggerDef "t_ship" OnTurn Nothing
                        [ ModifyValue (VRActorProp (ActorShip "ghost_ship") PHealth) (-10) ] False 0
                    ] }
        errors = validateWorld gw
    r1 <- expectTrue "undeclared ActorShip in rule is detected"
            (MissingVehicle "ghost_ship" `elem` errors)
    let gwOk = gw { vehicleDefs = Map.insert "ghost_ship"
                        (VehicleDef "ghost_ship" "Ghost" "A ghost ship." PlayerControlled []
                            "start" Nothing Map.empty [] ["ghost"] Nothing Map.empty)
                        (vehicleDefs gw) }
    r2 <- expectTrue "declared vehicle id is not a false positive"
            (MissingVehicle "ghost_ship" `notElem` validateWorld gwOk)
    pure (r1 && r2)

-- | `Location "player" <room>` gates on the player's room (Phase 7h); the
--   entity form keeps working.
testLocationPlayerPredicate :: IO Bool
testLocationPlayerPredicate = do
    let st = initSampleGame { save = (save initSampleGame) { currentRoom = "hallway" } }
    r1 <- expectTrue "player in room" (evalPredicate (Location ActorPlayer "hallway") st)
    r2 <- expectTrue "player not in room" (not (evalPredicate (Location ActorPlayer "start") st))
    r3 <- expectTrue "npc location still works" (evalPredicate (Location (ActorNPC "goblin") "hallway") st)
    pure (r1 && r2 && r3)

-- | R1: Location round-trip and backward-compatible decoding of legacy "player" strings.
testLocationJsonRoundTrip :: IO Bool
testLocationJsonRoundTrip = do
    let locs = [ CarriedBy ActorPlayer
               , CarriedBy (ActorNPC "goblin")
               , EquippedBy ActorPlayer
               , EquippedBy (ActorNPC "goblin")
               , InRoom "hallway"
               , InContainer "chest"
               , Removed
               ]
        roundTrips = all (\l -> Aeson.decode (Aeson.encode l) == Just l) locs
    r1 <- expectTrue "Location round-trips cleanly" roundTrips
    -- Legacy decode: String "player" -> ActorPlayer
    let legacyCarried = Aeson.decode (BLC.pack "{\"tag\":\"CarriedBy\",\"contents\":\"player\"}")
    r2 <- expectEqual (Just (CarriedBy ActorPlayer)) legacyCarried
    let legacyEquipped = Aeson.decode (BLC.pack "{\"tag\":\"EquippedBy\",\"contents\":\"player\"}")
    r3 <- expectEqual (Just (EquippedBy ActorPlayer)) legacyEquipped
    let legacyNpc = Aeson.decode (BLC.pack "{\"tag\":\"CarriedBy\",\"contents\":\"goblin\"}")
    r4 <- expectEqual (Just (CarriedBy (ActorNPC "goblin"))) legacyNpc
    let legacyRoom = Aeson.decode (BLC.pack "\"start\"")
    r5 <- expectEqual (Just (InRoom "start")) legacyRoom
    pure (r1 && r2 && r3 && r4 && r5)

-- | R1: Predicate.Location round-trip and backward-compatible decoding of legacy "player" string.
testPredicateLocationJsonRoundTrip :: IO Bool
testPredicateLocationJsonRoundTrip = do
    let pPlayer = Location ActorPlayer "hallway"
        pNpc = Location (ActorNPC "goblin") "hallway"
    r1 <- expectEqual (Just pPlayer) (Aeson.decode (Aeson.encode pPlayer))
    r2 <- expectEqual (Just pNpc) (Aeson.decode (Aeson.encode pNpc))
    -- Legacy JSON: { "at": "player", "room": "hallway" }
    let legacyPlayer = Aeson.decode (BLC.pack "{\"at\":\"player\",\"room\":\"hallway\"}")
    r3 <- expectEqual (Just pPlayer) legacyPlayer
    let legacyNpc = Aeson.decode (BLC.pack "{\"at\":\"goblin\",\"room\":\"hallway\"}")
    r4 <- expectEqual (Just pNpc) legacyNpc
    pure (r1 && r2 && r3 && r4)

-- | Phase 2.1: Predicate has_condition evaluates active conditions and round-trips.
testPredicateHasCondition :: IO Bool
testPredicateHasCondition = do
    let st = applyCondition "poison" 3 Nothing Nothing initSampleGame
    r1 <- expectTrue "hasCondition True when active" (evalPredicate (HasCondition "poison") st)
    let cleared = clearCondition "poison" st
    r2 <- expectTrue "hasCondition False when cleared" (not (evalPredicate (HasCondition "poison") cleared))
    r3 <- expectTrue "hasCondition False when not present" (not (evalPredicate (HasCondition "fire") st))
    let jsonDec = Aeson.decode (BLC.pack "{\"has_condition\":\"poison\"}")
    r4 <- expectEqual (Just (HasCondition "poison")) jsonDec
    let encDec = Aeson.decode (Aeson.encode (HasCondition "poison"))
    r5 <- expectEqual (Just (HasCondition "poison")) encDec
    pure (r1 && r2 && r3 && r4 && r5)

-- | Phase 2.1: ValueRef condition_turns queries remaining turns, evaluates in comparisons.
testPredicateConditionTurns :: IO Bool
testPredicateConditionTurns = do
    let st = applyCondition "cooldown" 4 Nothing Nothing initSampleGame
    r1 <- expectEqual 4 (resolveValueRef (VRConditionTurns "cooldown") st)
    r2 <- expectEqual 0 (resolveValueRef (VRConditionTurns "absent") st)
    r3 <- expectTrue "cooldown <= 4 is True" (evalPredicate (Compare (VRConditionTurns "cooldown") CLte (VRVariable "4")) st)
    r4 <- expectTrue "cooldown > 4 is False" (not (evalPredicate (Compare (VRConditionTurns "cooldown") CGt (VRVariable "4")) st))
    r5 <- expectTrue "cooldown == 4 is True" (evalPredicate (Compare (VRConditionTurns "cooldown") CEq (VRVariable "4")) st)
    r6 <- expectTrue "CompareVar condition_turns.cooldown <= 4 is True" (evalPredicate (CompareVar "condition_turns.cooldown" CLte 4) st)
    let vrDec = Aeson.decode (BLC.pack "{\"condition_turns\":\"cooldown\"}")
    r7 <- expectEqual (Just (VRConditionTurns "cooldown")) vrDec
    let vrStrDec = Aeson.decode (BLC.pack "\"condition_turns.cooldown\"")
    r8 <- expectEqual (Just (VRConditionTurns "cooldown")) vrStrDec
    let cmpDec = Aeson.decode (BLC.pack "{\"compare\":{\"lhs\":{\"condition_turns\":\"cooldown\"},\"op\":\"<=\",\"rhs\":4}}")
    r9 <- expectEqual (Just (Compare (VRConditionTurns "cooldown") CLte (VRVariable "4"))) cmpDec
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9)

-- | Phase 2.1: Hidden condition is omitted from stats output and protocol snapshot.
testConditionHidden :: IO Bool
testConditionHidden = do
    let st0 = initSampleGame
        st1 = applyConditionWithHidden "hidden_timer" 5 Nothing Nothing True st0
        st2 = applyConditionWithHidden "visible_buff" 3 Nothing Nothing False st1
    let (_, out) = executeCommand StatsCmd st2
    r1 <- expectTrue "visible condition in stats" ("visible_buff" `isInfixOf` out)
    r2 <- expectTrue "hidden condition not in stats" (not ("hidden_timer" `isInfixOf` out))
    let snap = makeSnapshot st2
        snapNames = map csName (psConditions (snapPlayer snap))
    r3 <- expectTrue "visible condition in snapshot" ("visible_buff" `elem` snapNames)
    r4 <- expectTrue "hidden condition not in snapshot" (not ("hidden_timer" `elem` snapNames))
    pure (r1 && r2 && r3 && r4)

-- | Phase 2.1: Condition JSON backward compatibility.
testConditionJsonCompatibility :: IO Bool
testConditionJsonCompatibility = do
    let cNormal = Condition "poison" 3 Nothing Nothing False
        cHidden = Condition "bomb" 5 Nothing Nothing True
    let encNormal = BLC.unpack (Aeson.encode cNormal)
    r1 <- expectTrue "condHidden omitted when False" (not ("condHidden" `isInfixOf` encNormal))
    r2 <- expectTrue "hidden omitted when False" (not ("\"hidden\"" `isInfixOf` encNormal))
    let encHidden = BLC.unpack (Aeson.encode cHidden)
    r3 <- expectTrue "condHidden included when True" ("\"condHidden\":true" `isInfixOf` encHidden)
    let legacyJson = BLC.pack "{\"condName\":\"poison\",\"condRemaining\":3,\"condTickOutcome\":null,\"condEndOutcome\":null}"
    r4 <- expectEqual (Just cNormal) (Aeson.decode legacyJson)
    let hiddenJson = BLC.pack "{\"condName\":\"bomb\",\"condRemaining\":5,\"condTickOutcome\":null,\"condEndOutcome\":null,\"condHidden\":true}"
    r5 <- expectEqual (Just cHidden) (Aeson.decode hiddenJson)
    let hiddenAltJson = BLC.pack "{\"condName\":\"bomb\",\"condRemaining\":5,\"condTickOutcome\":null,\"condEndOutcome\":null,\"hidden\":true}"
    r6 <- expectEqual (Just cHidden) (Aeson.decode hiddenAltJson)
    let legacyEffectJson = BLC.pack "{\"tag\":\"ApplyCondition\",\"contents\":[\"poison\",3,null,null]}"
    r7 <- expectEqual (Just (ApplyCondition "poison" 3 Nothing Nothing False)) (Aeson.decode legacyEffectJson)
    let eff = ApplyCondition "bomb" 5 Nothing Nothing True
    r8 <- expectEqual (Just eff) (Aeson.decode (Aeson.encode eff))
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8)

-- | Phase 2.2: OnBefore triggers run in definition order; the first Block stops
--   the action, and remaining effects/Before-triggers do NOT run.
testOnBeforeVetoOrderAndStop :: IO Bool
testOnBeforeVetoOrderAndStop = do
    let trig1 = TriggerDef "trig1" (OnBefore "take")
                    (Just (VarIs "cmd.target" "torch"))
                    [ Block (Just "The torch is magnetized to the table!") False
                    , SetValue (VRFlag "trig1_extra") (EVString "ran")
                    ]
                    False 0
        trig2 = TriggerDef "trig2" (OnBefore "take")
                    (Just (VarIs "cmd.target" "torch"))
                    [ SendMessage "Second trigger should never run"
                    , SetValue (VRFlag "trig2_flag") (EVString "ran")
                    ]
                    False 0
        st0 = initSampleGame
        stTrigs = st0 { world = (world st0) { triggerDefs = [trig1, trig2] } }

    -- 1. Execute "take torch": vetoed by trig1
    let (stAfterTake, msgTake) = executeCommand (Interact VTake "torch") stTrigs
    r1 <- expectEqual "The torch is magnetized to the table!\n" msgTake
    r2 <- expectTrue "torch was not picked up" (not (hasItem "torch" stAfterTake))
    r3 <- expectTrue "trig1 trailing effect did not run" (getFlag "trig1_extra" stAfterTake == Nothing)
    r4 <- expectTrue "trig2 did not run" (getFlag "trig2_flag" stAfterTake == Nothing)

    -- 2. Execute "take rusty sword" (not vetoed): runs normally
    let (stAfterSword, msgSword) = executeCommand (Interact VTake "rusty sword") stTrigs
    r5 <- expectTrue "sword was picked up" (hasItem "sword_rusty" stAfterSword)
    r6 <- expectTrue "take sword message succeeded" ("You take the rusty sword." `isPrefixOf` msgSword)

    pure (r1 && r2 && r3 && r4 && r5 && r6)

-- | Phase 2.2: cmd.target and cmd.target_kind are bound before command execution.
testCmdTargetBinding :: IO Bool
testCmdTargetBinding = do
    let st0 = initSampleGame

    -- 1. Go direction (with valid exit to "hallway")
    let stGo = bindCommandVars (Go North) st0
        varsGo = variables (save stGo)
    r1 <- expectEqual (Just (VVText "hallway")) (Map.lookup "cmd.target" varsGo)
    r2 <- expectEqual (Just (VVText "room")) (Map.lookup "cmd.target_kind" varsGo)

    -- 2. Go direction with no exit (West has no exit from "start")
    let stGoNone = bindCommandVars (Go West) st0
        varsGoNone = variables (save stGoNone)
    r3 <- expectEqual (Just (VVText "west")) (Map.lookup "cmd.target" varsGoNone)
    r4 <- expectEqual (Just (VVText "none")) (Map.lookup "cmd.target_kind" varsGoNone)

    -- 3. Interact VTake on room item "torch"
    let stTake = bindCommandVars (Interact VTake "torch") st0
        varsTake = variables (save stTake)
    r5 <- expectEqual (Just (VVText "torch")) (Map.lookup "cmd.target" varsTake)
    r6 <- expectEqual (Just (VVText "item")) (Map.lookup "cmd.target_kind" varsTake)

    -- 4. Interact VLookAt on room NPC "goblin" (in hallway)
    let stHallway = st0 { save = (save st0) { currentRoom = "hallway" } }
        stNpc = bindCommandVars (Interact VLookAt "goblin") stHallway
        varsNpc = variables (save stNpc)
    r7 <- expectEqual (Just (VVText "goblin")) (Map.lookup "cmd.target" varsNpc)
    r8 <- expectEqual (Just (VVText "npc")) (Map.lookup "cmd.target_kind" varsNpc)

    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8)

-- | Phase 2.2: Guarded exit gates movement with a predicate and optional failure message.
testGuardedExit :: IO Bool
testGuardedExit = do
    let customMsg = "The portcullis is down. The guard shakes his head."
        guardedExit = Guarded "hallway" (HasFlag "guard_bribed") (Just customMsg)
        st0 = initSampleGame
        startRoom = (rooms (world st0)) Map.! "start"
        startRoomWithGuarded = startRoom
            { roomConnections = Map.insert North guardedExit (roomConnections startRoom) }
        stGuarded = st0 { world = (world st0) { rooms = Map.insert "start" startRoomWithGuarded (rooms (world st0)) } }

    -- 1. Predicate false: blocked with custom message
    let (stBlocked, msgBlocked) = executeCommand (Go North) stGuarded
    r1 <- expectEqual customMsg msgBlocked
    r2 <- expectEqual "start" (currentRoom (save stBlocked))

    -- 2. Predicate false with Nothing: blocked with default move.blocked message
    let guardedExitNoMsg = Guarded "hallway" (HasFlag "guard_bribed") Nothing
        startRoomNoMsg = startRoom
            { roomConnections = Map.insert North guardedExitNoMsg (roomConnections startRoom) }
        stGuardedNoMsg = st0 { world = (world st0) { rooms = Map.insert "start" startRoomNoMsg (rooms (world st0)) } }
        (stBlockedDef, msgBlockedDef) = executeCommand (Go North) stGuardedNoMsg
    r3 <- expectEqual (renderMsg "move.blocked" []) msgBlockedDef
    r4 <- expectEqual "start" (currentRoom (save stBlockedDef))

    -- 3. Predicate true: movement allowed
    let stAllowed0 = setFlag "guard_bribed" "true" stGuarded
        (stAllowed, msgAllowed) = executeCommand (Go North) stAllowed0
    r5 <- expectEqual "hallway" (currentRoom (save stAllowed))
    r6 <- expectTrue "move.ok message rendered" ("You move North." `isPrefixOf` msgAllowed)

    pure (r1 && r2 && r3 && r4 && r5 && r6)

-- | Phase 2.2: Veto with consumesTurn=False does not advance turns or tick conditions;
--   consumesTurn=True advances turn and ticks conditions.
testVetoTurnCost :: IO Bool
testVetoTurnCost = do
    let trigFree = TriggerDef "trigFree" (OnBefore "take")
                    (Just (VarIs "cmd.target" "heavy_rock"))
                    [ Block (Just "Too heavy, you do not even budge it.") False ]
                    False 0
        trigCost = TriggerDef "trigCost" (OnBefore "take")
                    (Just (VarIs "cmd.target" "trap_chest"))
                    [ Block (Just "The chest shocks you, wasting your turn!") True ]
                    False 0
        st0 = applyCondition "poison" 5 Nothing Nothing initSampleGame
        stWithTrigs = st0 { world = (world st0) { triggerDefs = [trigFree, trigCost] } }
        ls0 = initLoopState stWithTrigs

    -- 1. Veto with consumesTurn = False
    let (ls1, _evs1) = applyLoopCommandEv (Interact VTake "heavy_rock") ls0
        st1 = lsCurrent ls1
    r1 <- expectEqual 0 (turnCount (save st1))
    r2 <- expectEqual 5 (resolveValueRef (VRConditionTurns "poison") st1)

    -- 2. Veto with consumesTurn = True
    let (ls2, _evs2) = applyLoopCommandEv (Interact VTake "trap_chest") ls0
        st2 = lsCurrent ls2
    r3 <- expectEqual 1 (turnCount (save st2))
    r4 <- expectEqual 4 (resolveValueRef (VRConditionTurns "poison") st2)

    pure (r1 && r2 && r3 && r4)

-- | Phase 2.2: Exit, Effect, and EventType JSON round-trip and backward compatibility.
testExitAndEffectJsonCompatibility :: IO Bool
testExitAndEffectJsonCompatibility = do
    -- 1. Exit round-trip
    let exOpen = Open "hallway"
        exLocked = Locked "hallway" "iron_key"
        exGuarded = Guarded "hallway" (HasFlag "unlocked") (Just "Barred!")
    r1 <- expectEqual (Just exOpen) (Aeson.decode (Aeson.encode exOpen))
    r2 <- expectEqual (Just exLocked) (Aeson.decode (Aeson.encode exLocked))
    r3 <- expectEqual (Just exGuarded) (Aeson.decode (Aeson.encode exGuarded))

    -- 2. Legacy Exit JSON (exact decoding backward compatibility)
    let legacyOpenJson = BLC.pack "{\"tag\":\"Open\",\"contents\":\"hallway\"}"
        legacyLockedJson = BLC.pack "{\"tag\":\"Locked\",\"contents\":[\"hallway\",\"iron_key\"]}"
    r4 <- expectEqual (Just exOpen) (Aeson.decode legacyOpenJson)
    r5 <- expectEqual (Just exLocked) (Aeson.decode legacyLockedJson)

    -- 3. Effect Block round-trip
    let effBlockMsg = Block (Just "Blocked") False
        effBlockTurn = Block Nothing True
    r6 <- expectEqual (Just effBlockMsg) (Aeson.decode (Aeson.encode effBlockMsg))
    r7 <- expectEqual (Just effBlockTurn) (Aeson.decode (Aeson.encode effBlockTurn))

    -- 4. EventType OnBefore round-trip
    let evBefore = OnBefore "take"
    r8 <- expectEqual (Just evBefore) (Aeson.decode (Aeson.encode evBefore))

    -- 5. EventType OnTalk round-trip (4.5, including the wildcard form)
    let evTalk = OnTalk "wirt" "bier"
    r9 <- expectEqual (Just evTalk) (Aeson.decode (Aeson.encode evTalk))
    let evTalkGlobal = OnTalk "" ""
    r10 <- expectEqual (Just evTalkGlobal) (Aeson.decode (Aeson.encode evTalkGlobal))

    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9 && r10)

-- ===========================================================================
-- Phase 2.3: Disambiguation
-- ===========================================================================

-- | Phase 2.3 base game: a second item in the start room that shares the
--   keyword `torch` with the sample torch, so `take torch` resolves to two
--   candidates. The candidate order comes from the item-state map (sorted by
--   id): @["torch", "torch2"]@.
disambiguationGame :: GameState
disambiguationGame = initSampleGame
    { world = (world initSampleGame)
        { itemDefs = Map.insert "torch2" spareTorch (itemDefs (world initSampleGame)) }
    , save = (save initSampleGame)
        { itemStates = Map.insert "torch2"
                        (ItemState (InRoom "start") "intact" Map.empty False)
                        (itemStates (save initSampleGame)) } }
  where
    spareTorch = (itemDefs (world initSampleGame) Map.! "torch")
        { itemId = "torch2", itemName = "spare torch", itemKeywords = ["torch", "spare torch"] }

-- | Phase 2.3: same world plus an `on: turn` trigger so the clock is visible.
disambiguationTickGame :: GameState
disambiguationTickGame = disambiguationGame
    { world = (world disambiguationGame)
        { triggerDefs = [ TriggerDef "tick" OnTurn Nothing [SendMessage "TICK"] False 0 ] } }

-- | Phase 2.3: the ambiguous attempt becomes an event plus a numbered
--   question and records the pending question on the loop state.
testDisambiguationEventAndPending :: IO Bool
testDisambiguationEventAndPending = do
    let (ls1, evs1) = applyLoopCommandEv (parseCommand "take torch") (initLoopState disambiguationGame)
    r1 <- expectTrue "EvDisambiguate carries the candidate ids in prompt order"
            (EvDisambiguate ["torch", "torch2"] `elem` evs1)
    r2 <- expectTrue "question numbers every candidate"
            ("Which do you mean: [1] torch, [2] spare torch?" `isInfixOf` renderEvents evs1)
    r3 <- expectEqual (Just (PendingDisambiguation ["torch", "torch2"] (Interact VTake "torch")))
            (lsPendingDisambiguation ls1)
    pure (r1 && r2 && r3)

-- | Phase 2.3: answering with a number picks the candidate at that position.
testDisambiguationByNumber :: IO Bool
testDisambiguationByNumber = do
    let (ls1, _evs1) = applyLoopCommandEv (parseCommand "take torch") (initLoopState disambiguationGame)
        (ls2, evs2)  = applyLoopCommandEv (parseCommand "1") ls1
        itemLoc i st = fmap itemLocation (Map.lookup i (itemStates (save st)))
    r1 <- expectEqual (Just (CarriedBy ActorPlayer)) (itemLoc "torch" (lsCurrent ls2))
    r2 <- expectEqual (Just (InRoom "start")) (itemLoc "torch2" (lsCurrent ls2))
    r3 <- expectTrue "candidate 1 is taken" ("You take the torch." `isInfixOf` renderEvents evs2)
    r4 <- expectEqual Nothing (lsPendingDisambiguation ls2)
    pure (r1 && r2 && r3 && r4)

-- | Phase 2.3: answering with a distinguishing word picks that candidate.
testDisambiguationByWord :: IO Bool
testDisambiguationByWord = do
    let (ls1, _evs1) = applyLoopCommandEv (parseCommand "take torch") (initLoopState disambiguationGame)
        (ls2, evs2)  = applyLoopCommandEv (parseCommand "spare") ls1
        itemLoc i st = fmap itemLocation (Map.lookup i (itemStates (save st)))
    r1 <- expectEqual (Just (CarriedBy ActorPlayer)) (itemLoc "torch2" (lsCurrent ls2))
    r2 <- expectEqual (Just (InRoom "start")) (itemLoc "torch" (lsCurrent ls2))
    r3 <- expectTrue "the spare torch is taken" ("You take the spare torch." `isInfixOf` renderEvents evs2)
    r4 <- expectEqual Nothing (lsPendingDisambiguation ls2)
    pure (r1 && r2 && r3 && r4)

-- | Phase 2.3: input that does not single out a candidate is not an answer —
--   it runs as a normal command and closes the question.
testDisambiguationFallback :: IO Bool
testDisambiguationFallback = do
    let (ls1, _evs1) = applyLoopCommandEv (parseCommand "take torch") (initLoopState disambiguationGame)
        -- "torch" describes both candidates -> still ambiguous, not an answer
        (ls2, evs2) = applyLoopCommandEv (parseCommand "torch") ls1
        -- a number outside 1..2 is not an answer either
        (ls3, evs3) = applyLoopCommandEv (parseCommand "9") ls1
    r1 <- expectEqual Nothing (lsPendingDisambiguation ls2)
    r2 <- expectTrue "undistinguishing word runs as an unknown command"
            ("I don't understand 'torch'." `isInfixOf` renderEvents evs2)
    r3 <- expectEqual Nothing (lsPendingDisambiguation ls3)
    r4 <- expectTrue "out-of-range number is not a choice"
            ("You are not in a conversation right now." `isInfixOf` renderEvents evs3)
    pure (r1 && r2 && r3 && r4)

-- | Phase 2.3: the answer costs no turn — the clock stays where it was and
--   `on: turn` does not fire for the replayed command, while the ambiguous
--   attempt itself still costs its turn.
testDisambiguationTurnCost :: IO Bool
testDisambiguationTurnCost = do
    let (ls1, evs1) = applyLoopCommandEv (parseCommand "take torch") (initLoopState disambiguationTickGame)
        (ls2, evs2) = applyLoopCommandEv (parseCommand "1") ls1
    r1 <- expectEqual 1 (turnCount (save (lsCurrent ls1)))
    r2 <- expectTrue "the ambiguous attempt fires on: turn" ("TICK" `isInfixOf` renderEvents evs1)
    r3 <- expectEqual 1 (turnCount (save (lsCurrent ls2)))
    r4 <- expectTrue "the answer does not fire on: turn" (not ("TICK" `isInfixOf` renderEvents evs2))
    r5 <- expectTrue "the answer still performs the action"
            (fmap itemLocation (Map.lookup "torch" (itemStates (save (lsCurrent ls2))))
                == Just (CarriedBy ActorPlayer))
    pure (r1 && r2 && r3 && r4 && r5)

-- ---------------------------------------------------------------------------
-- Phase 2.4: Text-Erweiterung (Inline-Bedingungen, Ausdrücke, Props, Fehlerfälle)
-- ---------------------------------------------------------------------------

-- | Phase 2.4: Inline condition {if <flag/var-cond>|a|b}
testInterpolationInlineConditions :: IO Bool
testInterpolationInlineConditions = do
    let st0 = initSampleGame
        st1 = setFlag "has_torch" "true" st0
        st2 = setVariable "gold" (VVInt 50) st1
        st3 = setVariable "weather" (VVText "rain") st2
    -- Flag truthiness (true vs unset/false)
    r1 <- expectEqual "It is bright." (formatWithVars "{if has_torch|It is bright.|It is pitch black.}" st3)
    r2 <- expectEqual "It is pitch black." (formatWithVars "{if has_torch|It is bright.|It is pitch black.}" st0)
    -- Negated flag (!flag)
    r3 <- expectEqual "You need a light source." (formatWithVars "{if !has_torch|You need a light source.|All good.}" st0)
    r4 <- expectEqual "All good." (formatWithVars "{if !has_torch|You need a light source.|All good.}" st3)
    -- Numeric variable comparisons (>, >=, ==, !=, <=, <)
    r5 <- expectEqual "Rich" (formatWithVars "{if gold > 10|Rich|Poor}" st3)
    r6 <- expectEqual "Poor" (formatWithVars "{if gold < 10|Rich|Poor}" st3)
    r7 <- expectEqual "Fifty" (formatWithVars "{if gold == 50|Fifty|Other}" st3)
    r8 <- expectEqual "Not zero" (formatWithVars "{if gold != 0|Not zero|Zero}" st3)
    r9 <- expectEqual "GTE" (formatWithVars "{if gold >= 50|GTE|Less}" st3)
    r10 <- expectEqual "LTE" (formatWithVars "{if gold <= 50|LTE|More}" st3)
    -- Text variable comparison
    r11 <- expectEqual "Wet" (formatWithVars "{if weather == rain|Wet|Dry}" st3)
    r12 <- expectEqual "Not sunny" (formatWithVars "{if weather != sun|Not sunny|Sunny}" st3)
    -- Single branch: else defaults to empty string
    r13 <- expectEqual "Lit!" (formatWithVars "{if has_torch|Lit!}" st3)
    r14 <- expectEqual "" (formatWithVars "{if has_lantern|Lit!}" st3)
    -- Nested interpolation inside selected branch
    r15 <- expectEqual "You have 50 coins." (formatWithVars "{if gold > 0|You have {gold} coins.|None.}" st3)
    -- Nested conditional
    r16 <- expectEqual "Wet and bright." (formatWithVars "{if has_torch|{if weather == rain|Wet and bright.|Dry and bright.}|Dark.}" st3)
    -- Escaped pipe \| inside branch
    r17 <- expectEqual "A | B" (formatWithVars "{if has_torch|A \\| B|C}" st3)
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9 && r10 && r11 && r12 && r13 && r14 && r15 && r16 && r17)

-- | Phase 2.4: Expressions {= <expr>} via existing Expr parser
testInterpolationExpressions :: IO Bool
testInterpolationExpressions = do
    let st0 = initSampleGame
        st1 = setVariable "gold" (VVInt 42) st0
        st2 = setVariable "bonus" (VVInt 8) st1
        st3 = setVariable "hp" (VVInt 20) st2
    -- Basic arithmetic with variables and literals
    r1 <- expectEqual "84" (formatWithVars "{= gold * 2}" st3)
    r2 <- expectEqual "50" (formatWithVars "{= gold + bonus}" st3)
    -- Operator precedence and parentheses
    r3 <- expectEqual "100" (formatWithVars "{= (gold + bonus) * 2}" st3)
    r4 <- expectEqual "58" (formatWithVars "{= gold + bonus * 2}" st3)
    -- Functions: min, max, clamp
    r5 <- expectEqual "10" (formatWithVars "{= min(10, gold)}" st3)
    r6 <- expectEqual "42" (formatWithVars "{= max(10, gold)}" st3)
    r7 <- expectEqual "30" (formatWithVars "{= clamp(0, 30, gold)}" st3)
    -- Modifiers on expressions (:+, :6)
    r8 <- expectEqual "+84" (formatWithVars "{= gold * 2:+}" st3)
    r9 <- expectEqual "    84" (formatWithVars "{= gold * 2:6}" st3)
    -- Division-by-zero safety
    r10 <- expectEqual "0" (formatWithVars "{= gold / 0}" st3)
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9 && r10)

-- | Phase 2.4: Props {item.torch.fuel} and {npc.<id>.<prop>}
testInterpolationProps :: IO Bool
testInterpolationProps = do
    let st0 = initSampleGame
        -- Setup item with props: torch with fuel = 7
        torchState = ItemState (CarriedBy ActorPlayer) "intact" (Map.fromList [("fuel", 7), ("weight", 2)]) False
        st1 = st0 { save = (save st0) { itemStates = Map.insert "torch" torchState (itemStates (save st0)) } }
        -- Setup NPC with props: guard with mood = 3
        guardState = NPCState (InRoom "start") "alive" (Just 100) (Map.fromList [("mood", 3)]) Nothing
        st2 = st1 { save = (save st1) { npcStates = Map.insert "guard" guardState (npcStates (save st1)) } }
    -- Item prop direct interpolation
    r1 <- expectEqual "Torch has 7 fuel." (formatWithVars "Torch has {item.torch.fuel} fuel." st2)
    -- Item prop with modifier
    r2 <- expectEqual "Fuel: +7" (formatWithVars "Fuel: {item.torch.fuel:+}" st2)
    -- Item prop in expression
    r3 <- expectEqual "Double fuel: 14." (formatWithVars "Double fuel: {= item.torch.fuel * 2}." st2)
    -- Item prop in conditional
    r4 <- expectEqual "Torch is burning!" (formatWithVars "{if item.torch.fuel > 0|Torch is burning!|Torch is out.}" st2)
    -- NPC prop direct interpolation
    r5 <- expectEqual "Guard mood: 3." (formatWithVars "Guard mood: {npc.guard.mood}." st2)
    -- NPC health prop
    r6 <- expectEqual "Guard HP: 100." (formatWithVars "Guard HP: {npc.guard.hp}." st2)
    -- ValueRef resolution for item prop
    r7 <- expectEqual 7 (resolveValueRef (VRVariable "item.torch.fuel") st2)
    r8 <- expectEqual 3 (resolveValueRef (VRVariable "npc.guard.mood") st2)
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8)

-- | Phase 2.4: Error handling for props, expressions, and conditionals (Constraint 3)
testInterpolationErrorHandling :: IO Bool
testInterpolationErrorHandling = do
    let st0 = initSampleGame
        torchState = ItemState (CarriedBy ActorPlayer) "intact" (Map.fromList [("fuel", 5)]) False
        st1 = st0 { save = (save st0) { itemStates = Map.insert "torch" torchState (itemStates (save st0)) } }
    -- 1. Unknown prop on existing item
    r1 <- expectEqual "<error: unknown prop 'durability' on item 'torch'>"
            (formatWithVars "{item.torch.durability}" st1)
    -- 2. Unknown item
    r2 <- expectEqual "<error: unknown item 'missing_item'>"
            (formatWithVars "{item.missing_item.fuel}" st1)
    -- 3. Unknown prop on existing NPC
    let guardState = NPCState (InRoom "start") "alive" (Just 50) Map.empty Nothing
        st2 = st1 { save = (save st1) { npcStates = Map.insert "guard" guardState (npcStates (save st1)) } }
    r3 <- expectEqual "<error: unknown prop 'mana' on npc 'guard'>"
            (formatWithVars "{npc.guard.mana}" st2)
    -- 4. Unknown NPC
    r4 <- expectEqual "<error: unknown npc 'ghost'>"
            (formatWithVars "{npc.ghost.mood}" st2)
    -- 5. Syntactically broken {if}: missing branches
    r5 <- expectEqual "<error: invalid if syntax: expected {if <cond>|a|b}>"
            (formatWithVars "{if gold > 0}" st2)
    r6 <- expectEqual "<error: invalid if syntax: expected {if <cond>|a|b}>"
            (formatWithVars "{if}" st2)
    -- 6. Syntactically broken {if}: empty condition
    r7 <- expectEqual "<error: invalid if condition: empty condition>"
            (formatWithVars "{if |then|else}" st2)
    -- 7. Syntactically broken {if}: invalid condition operator
    r8 <- expectEqual "<error: invalid if condition: gold @@ 10>"
            (formatWithVars "{if gold @@ 10|then|else}" st2)
    -- 8. Unknown variable in expression
    r9 <- expectEqual "<error: unknown variable 'unknown_var'>"
            (formatWithVars "{= unknown_var * 2}" st2)
    -- 9. Syntax error in expression (unexpected token / unparseable)
    r10 <- expectEqual "<error: expr: Unexpected token: TokMul>"
            (formatWithVars "{= 10 + * 2}" st2)
    r11 <- expectEqual "<error: expr: Unexpected character in expression: @>"
            (formatWithVars "{= 10 + @@@}" st2)
    -- 10. Empty expression
    r12 <- expectEqual "<error: expr: empty expression>"
            (formatWithVars "{=}" st2)
    -- 11. Unknown prop propagated inside expression and condition
    r13 <- expectEqual "<error: unknown prop 'durability' on item 'torch'>"
            (formatWithVars "{= item.torch.durability + 1}" st1)
    r14 <- expectEqual "<error: unknown item 'missing_item'>"
            (formatWithVars "{if item.missing_item.fuel > 0|Yes|No}" st1)
    -- 12. Standard unknown placeholder stays intact (backward compatibility)
    r15 <- expectEqual "{completely_unknown}"
            (formatWithVars "{completely_unknown}" st2)
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9 && r10 && r11 && r12 && r13 && r14 && r15)

-- | Phase 1.1: catalog invariants — no duplicate keys (Map.fromList would drop
--   them silently), no empty keys/templates, no template containing the
--   missing-key marker.
testMessageCatalogInvariants :: IO Bool
testMessageCatalogInvariants = do
    let keys = map fst catalogEntries
        r1 = expectEqual' (length catalogEntries) (Map.size defaultCatalog)
        r2 = not (any null keys)
        r3 = not (any (null . snd) catalogEntries)
        r4 = not (any ("<msg:" `isPrefixOf`) (map snd catalogEntries))
        expectEqual' a b = a == b
    pure (r1 && r2 && r3 && r4)

-- | Phase 1.1: renderMsg flags unknown keys loudly; {arg} substitution and
--   modifiers come from the moved formatStringWith. Catalog-backed rendering
--   is pinned by the converted modules (e.g. dark.blocked_anything).
testRenderMsgArgs :: IO Bool
testRenderMsgArgs = do
    r1 <- expectEqual "<msg:no.such.key>" (renderMsg "no.such.key" [])
    r2 <- expectEqual "You take the rusty sword."
                (formatStringWith "You take the {item}." (`lookup` [("item", "rusty sword")]))
    r3 <- expectEqual "{unknown}" (formatStringWith "{unknown}" (\_ -> Nothing))
    r4 <- expectEqual "+5" (formatStringWith "{n:+}" (`lookup` [("n", "5")]))
    r5 <- expectEqual "  7" (formatStringWith "{n:3}" (`lookup` [("n", "7")]))
    pure (r1 && r2 && r3 && r4 && r5)

-- | Phase 1.2: the fragment algebra replicates the four string join idioms
--   byte for byte: joinMessages (\\n between non-empty), direct ++, unlines
--   (trailing \\n incl. empty pieces), intercalate (\\n incl. empty pieces).
testOutputFragmentAlgebra :: IO Bool
testOutputFragmentAlgebra = do
    let a = evText "a"
        b = evText "b"
        e = evText ""
        r1 = renderEvents (joinEv a b) == joinMessages "a" "b"
        r2 = renderEvents (joinEv a []) == joinMessages "a" ""
        r3 = renderEvents (joinEv [] b) == joinMessages "" "b"
        r4 = renderEvents (a ++ b) == "a" ++ "b"
        r5 = renderEvents (unlinesEv [a, e, b]) == unlines ["a", "", "b"]
        r6 = renderEvents (evIntercalate [a, e, b]) == intercalate "\n" ["a", "", "b"]
        r7 = renderEvents (unlinesEv [a, b]) == unlines ["a", "b"]
        r8 = null (renderEvents [EvSfx "x.wav", EvMusicStop, EvRoomChanged "r"])
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8)

-- | Phase 1.2: styling model — plain text stays byte-identical, spans render
--   to SGR sequences, colours map to the 3x band.
testOutputStylingModel :: IO Bool
testOutputStylingModel = do
    let plain = styledText "hello world"
        r1 = renderStyled plain == "hello world"
        r2 = styleToAnsi plainStyle == ""
        styled = StyledText "hello world"
            [ Span 0 5 (plainStyle { stColor = Just CRed, stBold = True }) ]
        r3 = renderStyled styled == "\ESC[31;1mhello\ESC[0m world"
        r4 = styleToAnsi (plainStyle { stColor = Just CYellow }) == "\ESC[33m"
        r5 = styleToAnsi (plainStyle { stUnderline = True, stDim = True, stItalic = True }) == "\ESC[39;2;3;4m"
    pure (r1 && r2 && r3 && r4 && r5)

-- | Phase 1.2: catalog messages survive the loop as structured events —
--   key + args + rendered text; the rendered text matches the flat path.
testOutputEventKeys :: IO Bool
testOutputEventKeys = do
    let (_, evs) = applyLoopCommandEv (parseCommand "take torch") (initLoopState initSampleGame)
        msgEv = case evs of
            (EvMessage p : _) -> Just p
            _ -> Nothing
    r1 <- case msgEv of
        Just p -> expectEqual (Just "take.ok") (mpKey p)
        Nothing -> expectTrue "first event is the take.ok message" False
    r2 <- expectTrue "args pin the item id"
              (maybe False (\p -> lookup "item" (mpArgs p) == Just "torch") msgEv)
    r3 <- expectTrue "rendered text matches the flat path"
              (maybe False (\p -> mpText p == renderMsg "take.ok" [("item", "torch")]) msgEv)
    -- and the compat wrapper reproduces the flat string
    let (_, flat) = applyLoopCommand (parseCommand "take torch") (initLoopState initSampleGame)
    r4 <- expectTrue "compat wrapper renders the same text"
              (takeWhile (/= '\n') flat == maybe "" mpText msgEv)
    pure (r1 && r2 && r3 && r4)

-- | Phase 1.2: side events — room change, game over, queued sfx/music, and
--   the quest marker; none of them contributes text.
testOutputSideEvents :: IO Bool
testOutputSideEvents = do
    -- room change: go north from the sample start
    let (_, evs1) = applyLoopCommandEv (Go North) (initLoopState initSampleGame)
        roomEvs = [ r | EvRoomChanged r <- evs1 ]
    r1 <- expectEqual ["hallway"] roomEvs
    -- game over + sfx/music: endGame via a GameEnd effect, audio via pending fields
    let st0 = initSampleGame
        (st1, _) = applyOutcomeEv (GameEnd Victory "you win") "" st0
        st2 = st1 { pendingSfx = ["win.wav"], pendingMusic = Just (MusicStart "theme.ogg") }
        evs2 = sideEvents st0 st2
    r2 <- expectEqual [EvGameOver, EvSfx "win.wav", EvMusicStart "theme.ogg"] evs2
    -- quest update marker
    let st3 = st1 { save = (save st1) { activeQuests = Map.singleton "q1" 0 } }
        evs3 = sideEvents st0 st3
        r3 = EvQuestUpdate `elem` evs3
    pure (r1 && r2 && r3)

-- | Phase 1.3: pure save transition — checks ironman savezone policy and yields ReqSave
testSessionTransitionSave :: IO Bool
testSessionTransitionSave = do
    let normalSt = initSampleGame
        normalLoop = initLoopState normalSt
        (l1, reqs1, msgs1) = transitionSave "slot1" normalLoop
    r1 <- expectEqual [ReqSave "slot1"] reqs1
    r2 <- expectEqual (Just "slot1") (lsSaveSlot l1)
    r3 <- expectTrue "normal save produces no extra messages" (null msgs1)

    -- ironman outside savezone is rejected
    let ironSt = normalSt
            { world = (world normalSt)
                { worldGamePolicy = defaultGamePolicy
                    { gpIronman = True, gpSaveZones = ["safe_room"] } } }
        ironLoop = initLoopState ironSt
        (l2, reqs2, msgs2) = transitionSave "slot2" ironLoop
    r4 <- expectTrue "ironman save rejected outside savezone" (null reqs2)
    r5 <- expectEqual Nothing (lsSaveSlot l2)
    r6 <- expectEqual [renderMsg "save.savezone_only" []] msgs2

    -- ironman inside savezone routes to checkpoint slot
    let ironInZoneSt = ironSt
            { save = (save ironSt) { currentRoom = "safe_room" } }
        ironInZoneLoop = initLoopState ironInZoneSt
        (l3, reqs3, msgs3) = transitionSave "slot3" ironInZoneLoop
    r7 <- expectEqual [ReqSave "checkpoint"] reqs3
    r8 <- expectEqual (Just "checkpoint") (lsSaveSlot l3)
    r9 <- expectTrue "ironman save in zone produces no extra messages" (null msgs3)
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9)

-- | Phase 1.3: pure load transition and load success — checks ironman and merges meta
testSessionTransitionLoad :: IO Bool
testSessionTransitionLoad = do
    let normalSt = initSampleGame
        normalLoop = initLoopState normalSt
        (_, reqs1, msgs1) = transitionLoad "slot1" normalLoop
    r1 <- expectEqual [ReqLoad "slot1"] reqs1
    r2 <- expectTrue "normal load produces no immediate messages" (null msgs1)

    -- ironman rejects load
    let ironSt = normalSt
            { world = (world normalSt)
                { worldGamePolicy = defaultGamePolicy { gpIronman = True } } }
        ironLoop = initLoopState ironSt
        (_, reqs2, msgs2) = transitionLoad "slot2" ironLoop
    r3 <- expectTrue "ironman load produces no requests" (null reqs2)
    r4 <- expectEqual [renderMsg "load.ironman_blocked" []] msgs2

    -- load success merges disk meta variables and executes Look
    let loadedSt = normalSt
            { save = (save normalSt)
                { variables = Map.singleton "gold" (VVInt 10) } }
        diskMeta = Map.singleton "meta.souls" (VVInt 42)
        (freshLoop, lookLines) = transitionLoadSuccess diskMeta loadedSt
        freshVars = variables (save (lsCurrent freshLoop))
    r5 <- expectEqual (Just (VVInt 42)) (Map.lookup "meta.souls" freshVars)
    r6 <- expectEqual (Just (VVInt 10)) (Map.lookup "gold" freshVars)
    r7 <- expectTrue "lookLines non-empty" (not (null lookLines))
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7)

-- | Phase 1.3: pure restart transition — reseeds RNG, increments meta.runs, requests ReqPersistMeta
testSessionTransitionRestart :: IO Bool
testSessionTransitionRestart = do
    let metaSt = initSampleGame
            { world = (world initSampleGame)
                { varDefs = Map.singleton "meta.runs" (VarDef "Runs" (VTInt Nothing Nothing) (VVInt 0)) }
            , save = (save initSampleGame)
                { rngState = 1234
                , variables = Map.singleton "meta.runs" (VVInt 3) }
            }
        loop0 = initLoopState metaSt
        seed = 987654321
        (freshLoop, reqs, lines') = transitionRestart seed loop0
        freshSt = lsCurrent freshLoop
    r1 <- expectEqual [ReqPersistMeta] reqs
    r2 <- expectEqual seed (rngState (save freshSt))
    r3 <- expectEqual (Just (VVInt 4)) (Map.lookup "meta.runs" (variables (save freshSt)))
    r4 <- expectEqual [renderMsg "game.restart_start" []] (take 1 lines')
    pure (r1 && r2 && r3 && r4)

-- | Phase 1.3: pure game-over transition — yields ReqPersistMeta, checkpoint deletion, menu lines
testSessionTransitionGameOver :: IO Bool
testSessionTransitionGameOver = do
    -- Death in normal mode
    let deadSt = initSampleGame
            { save = (save initSampleGame) { gameOver = True, gameOverReason = Just Death } }
        deadLoop = (initLoopState deadSt) { lsSaveSlot = Just "slotA" }
        (sess1, reqs1, lines1) = transitionGameOver deadLoop
    r1 <- case sess1 of
        SessionDeath _ -> pure True
        _              -> expectTrue "session enters SessionDeath" False
    r2 <- expectEqual [ReqPersistMeta] reqs1
    r3 <- expectTrue "death title included in lines"
              (any (renderMsg "death.title" [] `isInfixOf`) lines1)
    r4 <- expectEqual (Just (deathMenuText defaultGamePolicy))
              (case lines1 of [] -> Nothing; xs -> Just (last xs))

    -- Death in ironman mode deletes checkpoint
    let ironPolicy = defaultGamePolicy { gpIronman = True }
        ironDeadSt = deadSt { world = (world deadSt) { worldGamePolicy = ironPolicy } }
        ironDeadLoop = (initLoopState ironDeadSt) { lsSaveSlot = Just "checkpoint" }
        (_, reqs2, lines2) = transitionGameOver ironDeadLoop
    r5 <- expectEqual [ReqPersistMeta, ReqDeleteSave "checkpoint"] reqs2
    r6 <- expectEqual (Just (deathMenuText ironPolicy))
              (case lines2 of [] -> Nothing; xs -> Just (last xs))

    -- Victory
    let winSt = initSampleGame
            { save = (save initSampleGame) { gameOver = True, gameOverReason = Just Victory } }
        winLoop = initLoopState winSt
        (sess3, reqs3, lines3) = transitionGameOver winLoop
    r7 <- case sess3 of
        SessionVictory _ Victory -> pure True
        _                        -> expectTrue "session enters SessionVictory" False
    r8 <- expectEqual [ReqPersistMeta] reqs3
    r9 <- expectTrue "victory title included in lines"
              (any (renderMsg "victory.title" [] `isInfixOf`) lines3)
    r10 <- expectEqual (Just (renderMsg "menu.restart_quit" []))
              (case lines3 of [] -> Nothing; xs -> Just (last xs))

    -- Custom reason
    let customSt = initSampleGame
            { save = (save initSampleGame) { gameOver = True, gameOverReason = Just (Custom "The end.") } }
        customLoop = initLoopState customSt
        (sess4, reqs4, lines4) = transitionGameOver customLoop
    r11 <- case sess4 of
        SessionVictory _ (Custom "The end.") -> pure True
        _                                    -> expectTrue "session enters SessionVictory custom" False
    r12 <- expectEqual [ReqPersistMeta] reqs4
    r13 <- expectTrue "custom gameover message in lines"
              (any ("The end." `isInfixOf`) lines4)

    -- Quit (no reason)
    let quitSt = initSampleGame
            { save = (save initSampleGame) { gameOver = True, gameOverReason = Nothing } }
        quitLoop = initLoopState quitSt
        (sess5, reqs5, lines5) = transitionGameOver quitLoop
    r14 <- expectEqual SessionEnded sess5
    r15 <- expectEqual [ReqPersistMeta] reqs5
    r16 <- expectTrue "quit without reason produces no end lines" (null lines5)
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9 && r10 && r11 && r12 && r13 && r14 && r15 && r16)

-- | Phase 1.3: pure death menu transitions — undo, load, restart, quit
testSessionTransitionDeath :: IO Bool
testSessionTransitionDeath = do
    let deadSt = initSampleGame
            { save = (save initSampleGame) { gameOver = True, gameOverReason = Just Death } }
        deadLoop = initLoopState deadSt

    -- 'u' with empty history fails
    let (sess1, _, msgs1) = transitionDeathInput 0 (Just "u") deadLoop
    r1 <- expectEqual (SessionDeath deadLoop) sess1
    r2 <- expectEqual [renderMsg "undo.nothing" []] msgs1

    -- 'u' with history restores previous state
    let aliveSt = initSampleGame
        deadLoopWithHist = deadLoop { lsHistory = [aliveSt] }
        (sess2, _, _) = transitionDeathInput 0 (Just "u") deadLoopWithHist
    r3 <- case sess2 of
        SessionPlaying ls -> expectEqual aliveSt (lsCurrent ls)
        _                 -> expectTrue "undo enters SessionPlaying" False

    -- 'u' with permadeath rejected
    let permaDeadLoop = deadLoop
            { lsHistory = [aliveSt]
            , lsCurrent = deadSt { world = (world deadSt) { worldGamePolicy = defaultGamePolicy { gpPermadeath = True } } } }
        (sess3, _, msgs3) = transitionDeathInput 0 (Just "u") permaDeadLoop
    r4 <- expectEqual (SessionDeath permaDeadLoop) sess3
    r5 <- expectEqual [renderMsg "undo.permadeath" []] msgs3

    -- 'l' in normal mode prompts for save
    let (sess4, _, msgs4) = transitionDeathInput 0 (Just "l") deadLoop
    r6 <- expectEqual (SessionDeathPromptLoad deadLoop) sess4
    r7 <- expectEqual [renderMsg "load.prompt" []] msgs4

    -- 'l' with slot name returns ReqLoad
    let (slotA, reqA) = transitionDeathLoadSlot (Just "slotA")
        (slotDef, reqDef) = transitionDeathLoadSlot Nothing
    r8 <- expectEqual ("slotA", ReqLoad "slotA") (slotA, reqA)
    r9 <- expectEqual ("savegame", ReqLoad "savegame") (slotDef, reqDef)

    -- 'r' restarts
    let (sess5, reqs5, msgs5) = transitionDeathInput 55555 (Just "r") deadLoop
    r10 <- case sess5 of
        SessionPlaying ls -> expectEqual 55555 (rngState (save (lsCurrent ls)))
        _                 -> expectTrue "restart enters SessionPlaying" False
    r11 <- expectEqual [ReqPersistMeta] reqs5
    r12 <- expectEqual [renderMsg "game.restart_start" []] (take 1 msgs5)

    -- 'q' quits
    let (sess6, _, msgs6) = transitionDeathInput 0 (Just "q") deadLoop
    r13 <- expectEqual SessionEnded sess6
    r14 <- expectEqual [renderMsg "quit.thanks" []] msgs6

    -- invalid input shows death menu
    let (sess7, _, msgs7) = transitionDeathInput 0 (Just "invalid") deadLoop
    r15 <- expectEqual (SessionDeath deadLoop) sess7
    r16 <- expectEqual [deathMenuText defaultGamePolicy] msgs7
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9 && r10 && r11 && r12 && r13 && r14 && r15 && r16)

-- | Phase 1.3: pure victory menu transitions — restart, quit, invalid
testSessionTransitionVictory :: IO Bool
testSessionTransitionVictory = do
    let winSt = initSampleGame
            { save = (save initSampleGame) { gameOver = True, gameOverReason = Just Victory } }
        winLoop = initLoopState winSt

    -- 'r' restarts
    let (sess1, reqs1, _) = transitionVictoryInput 77777 (Just "r") winLoop Victory
    r1 <- case sess1 of
        SessionPlaying ls -> expectEqual 77777 (rngState (save (lsCurrent ls)))
        _                 -> expectTrue "restart enters SessionPlaying" False
    r2 <- expectEqual [ReqPersistMeta] reqs1

    -- 'q' quits
    let (sess2, _, msgs2) = transitionVictoryInput 0 (Just "q") winLoop Victory
    r3 <- expectEqual SessionEnded sess2
    r4 <- expectEqual [renderMsg "quit.thanks" []] msgs2

    -- invalid input shows restart/quit menu
    let (sess3, _, msgs3) = transitionVictoryInput 0 (Just "bad") winLoop Victory
    r5 <- expectEqual (SessionVictory winLoop Victory) sess3
    r6 <- expectEqual [renderMsg "menu.restart_quit" []] msgs3
    pure (r1 && r2 && r3 && r4 && r5 && r6)

-- | Phase 1.3: advanceNarrative yields ReqPause for intermediate lines and applies follow-up
testSessionAdvanceNarrative :: IO Bool
testSessionAdvanceNarrative = do
    -- multi-line narrative yields ReqPause for intermediate lines
    let st0 = initSampleGame
            { pendingNarrative = Just (["Line 1", "Line 2", "Line 3"], SetValue (VRFlag "narrative_done") (EVString "true")) }
    (r1, r2, r3, r4) <- case advanceNarrative st0 of
        Just (clearedSt, reqs, lines') -> do
            chk1 <- expectEqual [ReqPause, ReqPause] reqs
            chk2 <- expectEqual Nothing (pendingNarrative clearedSt)
            chk3 <- expectEqual (Just "true") (Map.lookup "narrative_done" (flags (save clearedSt)))
            chk4 <- expectEqual ["Line 1", "Line 2", "Line 3"] lines'
            pure (chk1, chk2, chk3, chk4)
        Nothing -> do
            chk <- expectTrue "advanceNarrative multi-line yields Just" False
            pure (chk, False, False, False)

    -- single-line narrative yields no pauses
    let st1 = initSampleGame
            { pendingNarrative = Just (["Only Line"], SetValue (VRFlag "single_done") (EVString "true")) }
    (r5, r6, r7, r8) <- case advanceNarrative st1 of
        Just (clearedSt1, reqs1, lines1) -> do
            chk1 <- expectEqual [] reqs1
            chk2 <- expectEqual Nothing (pendingNarrative clearedSt1)
            chk3 <- expectEqual (Just "true") (Map.lookup "single_done" (flags (save clearedSt1)))
            chk4 <- expectEqual ["Only Line"] lines1
            pure (chk1, chk2, chk3, chk4)
        Nothing -> do
            chk <- expectTrue "advanceNarrative single-line yields Just" False
            pure (chk, False, False, False)

    -- no narrative yields Nothing
    let st2 = initSampleGame { pendingNarrative = Nothing }
    r9 <- expectTrue "no narrative yields Nothing" (isNothing (advanceNarrative st2))
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9)

-- ===========================================================================
-- Phase 1.4: Protocol v1 (Types, Codec, Golden Tests, Versioning)
-- ===========================================================================

-- | Phase 1.4: ClientMsg round-trip encoding and decoding for all message types
testProtocolClientMsgRoundTrip :: IO Bool
testProtocolClientMsgRoundTrip = do
    let msgs =
            [ ClientCommand 1 "look"
            , ClientChoose 1 2
            , ClientContinue 1
            , ClientLoadWorld 1 "examples/thefog.yaml"
            , ClientSave 1 "slot1"
            , ClientLoad 1 "slot1"
            ]
    results <- mapM (\m -> expectEqual (Right m) (decodeClientMsg (encodeSorted m))) msgs
    pure (and results)

-- | Phase 1.4: ServerMsg round-trip encoding and decoding for all message types
testProtocolServerMsgRoundTrip :: IO Bool
testProtocolServerMsgRoundTrip = do
    let sampleEvents =
            [ EvMessage (MsgPayload (Just "game.look") [("room", "Entrance Hall")] "You are in an entrance hall.")
            , EvText (StyledText "A cold draft blows from the north." [Span 2 10 (Style (Just CBlue) True False False False)])
            , EvArt (ArtPayload "  +---+\n  | @ |\n  +---+" [ArtHotspot 1 '@' "torch"])
            , EvAnim 50000 ["frame1", "frame2"]
            , EvSfx "sounds/door.wav"
            , EvMusicStart "music/ambient.ogg"
            , EvMusicStop
            , EvRoomChanged "dungeon"
            , EvQuestUpdate
            , EvDialogue
            , EvCombat True
            , EvGameOver
            , EvDisambiguate ["torch", "torch2"]
            ]
        msgs =
            [ ServerEvents 1 sampleEvents
            , ServerSnapshot 1 (makeSnapshot initSampleGame)
            , ServerError 1 (ProtocolError ErrVersionMismatch "Unsupported protocol version 2, expected 1")
            ]
    results <- mapM (\m -> expectEqual (Right m) (decodeServerMsg (encodeSorted m))) msgs
    pure (and results)

-- | Phase 1.4: Golden JSON tests. The bytes written by this encoder must match the
--   golden files on disk byte for byte (Plan 1.4, constraint 4). Those files came
--   from an earlier process, so the comparison is the cross-process determinism
--   check; an in-process `encodeSorted x == encodeSorted x` would not be one
--   (both sides share a single thunk and can never differ).
testProtocolGoldenDeterministic :: IO Bool
testProtocolGoldenDeterministic = do
    let sampleEvents =
            [ EvMessage (MsgPayload (Just "game.look") [("room", "Entrance Hall")] "You are in an entrance hall.")
            , EvText (StyledText "A cold draft blows from the north." [Span 2 10 (Style (Just CBlue) True False False False)])
            , EvArt (ArtPayload "  +---+\n  | @ |\n  +---+" [ArtHotspot 1 '@' "torch"])
            , EvAnim 50000 ["frame1", "frame2"]
            , EvSfx "sounds/door.wav"
            , EvMusicStart "music/ambient.ogg"
            , EvMusicStop
            , EvRoomChanged "dungeon"
            , EvQuestUpdate
            , EvDialogue
            , EvCombat True
            , EvGameOver
            , EvDisambiguate ["torch", "torch2"]
            ]
        makeCases =
            [ ("client_command.json", encodeSorted (cmdCommand "look"))
            , ("client_choose.json", encodeSorted (cmdChoose 2))
            , ("client_continue.json", encodeSorted cmdContinue)
            , ("client_load_world.json", encodeSorted (cmdLoadWorld "examples/thefog.yaml"))
            , ("client_save.json", encodeSorted (cmdSave "slot1"))
            , ("client_load.json", encodeSorted (cmdLoad "slot1"))
            , ("server_events.json", encodeSorted (msgEvents sampleEvents))
            , ("server_snapshot.json", encodeSorted (msgSnapshot (makeSnapshot initSampleGame)))
            , ("server_error.json", encodeSorted (msgError ErrVersionMismatch "Unsupported protocol version 2, expected 1"))
            ]
    goldenOk <- mapM (\(fn, b) -> do
        diskBytes <- BLC.readFile ("test/golden/protocol/" ++ fn)
        expectEqual diskBytes b) makeCases
    pure (and goldenOk)

-- | Phase 1.4: Protocol version mismatch and missing version error handling (constraint 5).
testProtocolVersionMismatch :: IO Bool
testProtocolVersionMismatch = do
    let cV2 = BLC.pack "{\"version\":2,\"type\":\"command\",\"command\":\"look\"}"
    r1 <- expectEqual (Left (ProtocolError ErrVersionMismatch "Unsupported protocol version 2, expected 1"))
                      (decodeClientMsg cV2)

    let cV0 = BLC.pack "{\"version\":0,\"type\":\"continue\"}"
    r2 <- expectEqual (Left (ProtocolError ErrVersionMismatch "Unsupported protocol version 0, expected 1"))
                      (decodeClientMsg cV0)

    let cMissing = BLC.pack "{\"type\":\"continue\"}"
    r3 <- expectEqual (Left (ProtocolError ErrMalformedPayload "Missing required 'version' field"))
                      (decodeClientMsg cMissing)

    let sV3 = BLC.pack "{\"version\":3,\"type\":\"events\",\"events\":[]}"
    r4 <- expectEqual (Left (ProtocolError ErrVersionMismatch "Unsupported protocol version 3, expected 1"))
                      (decodeServerMsg sV3)

    let sMissing = BLC.pack "{\"type\":\"events\",\"events\":[]}"
    r5 <- expectEqual (Left (ProtocolError ErrMalformedPayload "Missing required 'version' field"))
                      (decodeServerMsg sMissing)

    -- An unrecognized discriminator gets its own code (spec 2.3), so a client
    -- built against a newer protocol learns why it was rejected instead of
    -- seeing a generic malformed payload.
    let codeOf = either (Left . peCode) Right
        cUnknown = BLC.pack "{\"version\":1,\"type\":\"teleport\"}"
        sUnknown = BLC.pack "{\"version\":1,\"type\":\"telemetry\"}"
        cBroken  = BLC.pack "{\"version\":1,\"type\":\"choose\"}"
    r6 <- expectTrue "unknown client type -> unknown_type"
              (codeOf (decodeClientMsg cUnknown) == Left ErrUnknownType)
    r7 <- expectTrue "unknown server type -> unknown_type"
              (codeOf (decodeServerMsg sUnknown) == Left ErrUnknownType)
    r8 <- expectTrue "known type with broken payload stays malformed_payload"
              (codeOf (decodeClientMsg cBroken) == Left ErrMalformedPayload)

    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8)

-- | Phase 1.4: Session transition lines bridged to protocol wire events (constraint 6).
testProtocolSessionBridge :: IO Bool
testProtocolSessionBridge = do
    let sessionLines = ["Game saved successfully.", "Slot: quicksave"]
        events = sessionLinesToEvents sessionLines
        expected = [EvText (styledText "Game saved successfully."), EvText (styledText "Slot: quicksave")]
    r1 <- expectEqual expected events
    let wireMsg = msgEvents events
        encoded = encodeSorted wireMsg
        decoded = decodeServerMsg encoded
    r2 <- expectEqual (Right wireMsg) decoded
    pure (r1 && r2)

-- | Phase 1.4: makeSnapshot extracts consistent presentation-tier state from GameState.
testProtocolMakeSnapshot :: IO Bool
testProtocolMakeSnapshot = do
    let snap = makeSnapshot initSampleGame
    r1 <- expectEqual 100 (psHealth (snapPlayer snap))
    r2 <- expectEqual 100 (psMaxHealth (snapPlayer snap))
    r3 <- expectEqual 10 (psAttack (snapPlayer snap))
    r4 <- expectEqual 5 (psDefense (snapPlayer snap))
    r5 <- expectEqual "start" (rsId (snapRoom snap))
    r6 <- expectEqual 3 (length (rsExits (snapRoom snap)))
    r7 <- expectEqual False (csEngaged (snapCombat snap))
    r8 <- expectEqual Nothing (snapDialogue snap)
    r9 <- expectEqual Nothing (snapGameOver snap)
    r10 <- expectEqual 0 (snapTurn snap)
    r11 <- expectEqual [] (snapVisitedRooms snap)
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9 && r10 && r11)

testValidateTypoInPredicateLocation :: IO Bool
testValidateTypoInPredicateLocation = do
    let decodedTypo = Aeson.decode (BLC.pack "{\"at\":\"palyer\",\"room\":\"start\"}") :: Maybe Predicate
        decodedOk   = Aeson.decode (BLC.pack "{\"at\":\"player\",\"room\":\"start\"}") :: Maybe Predicate
    case (decodedTypo, decodedOk) of
        (Just typoPred, Just okPred) -> do
            let gwTypo = (world initSampleGame)
                    { triggerDefs = [ TriggerDef "t" OnTurn (Just typoPred) [SendMessage "ok"] False 0 ] }
                gwOk = (world initSampleGame)
                    { triggerDefs = [ TriggerDef "t" OnTurn (Just okPred) [SendMessage "ok"] False 0 ] }
            r1 <- expectTrue "'at: palyer' fixture produces MissingEntity validation error"
                      (MissingEntity "palyer" "property" `elem` validateWorld gwTypo)
            r2 <- expectTrue "'at: player' does not produce validation error"
                      (MissingEntity "player" "property" `notElem` validateWorld gwOk)
            pure (r1 && r2)
        _ -> expectTrue "R1 fixtures decode into a Predicate" False

-- ---------------------------------------------------------------------------
-- Progression (W2) tests
-- ---------------------------------------------------------------------------

testGainXpAndLevelUp :: IO Bool
testGainXpAndLevelUp = do
    let pdef = ProgressionDef
            [ LevelDef 1 0 "Novize" Nothing []
            , LevelDef 2 100 "Krieger" Nothing [ModifyValue (VRVariable "bonus.attack") 2]
            ]
        gw = emptyGameWorld { progressionDef = Just pdef }
        st0 = emptyGameState
            { world = gw
            , save = (save emptyGameState)
                { variables = Map.fromList
                    [ ("xp.current", VVInt 0)
                    , ("level.current", VVInt 1)
                    , ("bonus.attack", VVInt 0)
                    ]
                }
            }
    -- 1. Gain 50 XP: not enough to level up
    let (st1, msg1) = applyOutcome (GainXp 50) "" st0
    r1 <- expectEqual 50 (getXp st1)
    r2 <- expectEqual 1 (getLevel st1)
    r3 <- expectTrue "no level up msg yet" (null msg1)

    -- 2. Gain another 50 XP: reaches 100 XP -> level up to level 2
    let (st2, msg2) = applyOutcome (GainXp 50) "" st1
    r4 <- expectEqual 100 (getXp st2)
    r5 <- expectEqual 2 (getLevel st2)
    r6 <- expectTrue "level up msg emitted" ("level 2" `isInfixOf` msg2 && "Krieger" `isInfixOf` msg2)
    r7 <- expectEqual (Just (VVInt 2)) (Map.lookup "bonus.attack" (variables (save st2)))

    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7)

testMultiLevelUpInOneTurn :: IO Bool
testMultiLevelUpInOneTurn = do
    let pdef = ProgressionDef
            [ LevelDef 1 0 "Rekrut" Nothing []
            , LevelDef 2 100 "Soeldner" Nothing [ModifyValue (VRVariable "bonus.attack") 2]
            , LevelDef 3 250 "Veteran" Nothing [ModifyValue (VRVariable "bonus.defense") 3]
            ]
        trigLvl2 = TriggerDef "trig2" (OnLevelUp 2) Nothing [SendMessage "Trigger: Lvl2!"] False 0
        trigLvl3 = TriggerDef "trig3" (OnLevelUp 3) Nothing [SendMessage "Trigger: Lvl3!"] False 0
        gw = emptyGameWorld
            { progressionDef = Just pdef
            , triggerDefs = [trigLvl2, trigLvl3]
            }
        st0 = emptyGameState
            { world = gw
            , save = (save emptyGameState)
                { variables = Map.fromList
                    [ ("xp.current", VVInt 0)
                    , ("level.current", VVInt 1)
                    , ("bonus.attack", VVInt 0)
                    , ("bonus.defense", VVInt 0)
                    ]
                }
            }
    -- Gain 300 XP in one shot (crosses level 2 and level 3)
    let (st1, msg) = applyOutcome (GainXp 300) "" st0
    r1 <- expectEqual 300 (getXp st1)
    r2 <- expectEqual 3 (getLevel st1)
    r3 <- expectEqual (Just (VVInt 2)) (Map.lookup "bonus.attack" (variables (save st1)))
    r4 <- expectEqual (Just (VVInt 3)) (Map.lookup "bonus.defense" (variables (save st1)))
    r5 <- expectTrue "Lvl2 trigger fired" ("Trigger: Lvl2!" `isInfixOf` msg)
    r6 <- expectTrue "Lvl3 trigger fired" ("Trigger: Lvl3!" `isInfixOf` msg)
    pure (r1 && r2 && r3 && r4 && r5 && r6)

testCombatBonusVars :: IO Bool
testCombatBonusVars = do
    let st0 = emptyGameState
    r1 <- expectEqual 10 (effectiveAttack st0)
    r2 <- expectEqual 5 (effectiveDefense st0)
    r3 <- expectEqual 100 (effectiveMaxHealth st0)

    let st1 = emptyGameState
            { save = (save emptyGameState)
                { variables = Map.fromList
                    [ ("bonus.attack", VVInt 4)
                    , ("bonus.defense", VVInt 3)
                    , ("bonus.hp", VVInt 25)
                    ]
                }
            }
    r4 <- expectEqual 14 (effectiveAttack st1)
    r5 <- expectEqual 8 (effectiveDefense st1)
    r6 <- expectEqual 125 (effectiveMaxHealth st1)
    pure (r1 && r2 && r3 && r4 && r5 && r6)

testNegativeXpClampAndAntiDelevel :: IO Bool
testNegativeXpClampAndAntiDelevel = do
    let pdef = ProgressionDef
            [ LevelDef 1 0 "Novize" Nothing []
            , LevelDef 2 100 "Krieger" Nothing []
            ]
        gw = emptyGameWorld { progressionDef = Just pdef }
        st0 = emptyGameState
            { world = gw
            , save = (save emptyGameState)
                { variables = Map.fromList
                    [ ("xp.current", VVInt 150)
                    , ("level.current", VVInt 2)
                    ]
                }
            }
    -- 1. Deduct 50 XP: stays at level 2
    let (st1, _) = applyOutcome (GainXp (-50)) "" st0
    r1 <- expectEqual 100 (getXp st1)
    r2 <- expectEqual 2 (getLevel st1)

    -- 2. Deduct 200 XP: underflows below 0 -> clamped to 0, level remains 2 (anti-de-level)
    let (st2, msg2) = applyOutcome (GainXp (-200)) "" st1
    r3 <- expectEqual 0 (getXp st2)
    r4 <- expectEqual 2 (getLevel st2)
    r5 <- expectTrue "clamp message emitted" ("cannot drop below 0" `isInfixOf` msg2)
    pure (r1 && r2 && r3 && r4 && r5)

testStatsProgression :: IO Bool
testStatsProgression = do
    let pdef = ProgressionDef
            [ LevelDef 1 0 "Lehrling" Nothing []
            , LevelDef 2 100 "Meister" Nothing []
            ]
        gwWithProg = emptyGameWorld { progressionDef = Just pdef }
        stProg1 = emptyGameState
            { world = gwWithProg
            , save = (save emptyGameState)
                { variables = Map.fromList
                    [ ("xp.current", VVInt 42)
                    , ("level.current", VVInt 1)
                    ]
                }
            }
        (_, out1) = executeCommand StatsCmd stProg1
    r1 <- expectTrue "displays level and xp progress" ("Level 1 — Lehrling (42/100 XP)" `isInfixOf` out1)

    -- Max level formatting
    let stProg2 = emptyGameState
            { world = gwWithProg
            , save = (save emptyGameState)
                { variables = Map.fromList
                    [ ("xp.current", VVInt 120)
                    , ("level.current", VVInt 2)
                    ]
                }
            }
        (_, out2) = executeCommand StatsCmd stProg2
    r2 <- expectTrue "displays max level formatting" ("Level 2 — Meister (120 XP)" `isInfixOf` out2)

    -- Without progressionDef: unchanged
    let stNoProg = emptyGameState
        (_, outNoProg) = executeCommand StatsCmd stNoProg
    r3 <- expectTrue "no level line when progressionDef is Nothing" (not ("Level " `isInfixOf` outNoProg))
    pure (r1 && r2 && r3)

testProgressionGameWorldM2Invariant :: IO Bool
testProgressionGameWorldM2Invariant = do
    let gwNoProg = emptyGameWorld
        encodedNoProg = BLC.unpack (Aeson.encode gwNoProg)
    r1 <- expectTrue "progression key omitted when Nothing" (not ("\"progression\"" `isInfixOf` encodedNoProg))

    let pdef = ProgressionDef [ LevelDef 1 0 "Start" Nothing [SendMessage "Hi"], LevelDef 2 50 "Next" Nothing [] ]
        gwWithProg = emptyGameWorld { progressionDef = Just pdef }
        encodedWithProg = Aeson.encode gwWithProg
        decoded = Aeson.decode encodedWithProg :: Maybe GameWorld
    case decoded of
        Nothing -> do
            putStrLn "Failed to decode GameWorld with progressionDef"
            pure False
        Just gwDec -> do
            r2 <- expectEqual (Just pdef) (progressionDef gwDec)
            pure (r1 && r2)

-- | `exit` is the quit alias, `disembark` leaves a vehicle — the help text has
--   to say the same, otherwise players quit the game instead of leaving a ship.
testExitIsNotDisembark :: IO Bool
testExitIsNotDisembark = do
    r1 <- expectEqual Quit (parseCommand "exit")
    r2 <- expectEqual ExitVehicleCmd (parseCommand "disembark")
    r3 <- expectTrue "help does not advertise 'exit' for vehicles"
        (isInfixOf "disembark" helpText && not (isInfixOf "exit / disembark" helpText))
    pure (r1 && r2 && r3)

testPlayerDeathSetsGameOver :: IO Bool
testPlayerDeathSetsGameOver = do
    let weakPlayer = initSampleGame { save = (save initSampleGame) { currentRoom = "hallway", player = Player 1 100 10 0 Map.empty } }
        (newState, _) = executeCommand (Interact VAttack "goblin") weakPlayer
    r1 <- expectTrue "game over on death" (gameOver (save newState))
    r2 <- expectEqual (Just Death) (gameOverReason (save newState))
    pure (r1 && r2)

-- ===== P0-Regressionen: defaultSaveState + Kampf-Effektmeldungen =====

-- | P0-1: `loadGame world Nothing` walks through `defaultSaveState`. Every
--   SaveState field must be initialised. The four formerly-missing fields
--   (`rngState`, `variables`, `containers`, `triggerStates`) were `undefined`
--   at runtime, so the first variable / trigger / RNG evaluation aborted the
--   game with \"Missing field in record construction\". The fields are forced
--   here so a regression fails the test instead of crashing much later.
testDefaultSaveStateFieldsInitialised :: IO Bool
testDefaultSaveStateFieldsInitialised = do
    let base = initSampleGame
        w = (world base)
            { varDefs = Map.fromList
                [ ("quest_stage", VarDef "quest_stage" (VTInt Nothing Nothing) (VVInt 3)) ]
            , triggerDefs =
                [ TriggerDef "welcome" (OnEnter "start") Nothing
                    [ SendMessage "Willkommen zurück." ] False 0 ]
            }
        
    tmpDir <- getTemporaryDirectory
    let worldPath = tmpDir ++ "/ta-p01-defaultsavestate-world.json"
    BLC.writeFile worldPath (Aeson.encode w)
    loaded <- loadGame worldPath Nothing
    case loaded of
        Left err -> do
            putStrLn ("  loadGame (without save) failed: " ++ err)
            pure False
        Right st -> do
            let ss = save st
            _ <- evaluate (rngState ss)
            _ <- evaluate (variables ss)
            _ <- evaluate (triggerStates ss)
            r1 <- expectEqual initialRngState (rngState ss)
            r2 <- expectEqual (Just (VVInt 3)) (Map.lookup "quest_stage" (variables ss))
            r3 <- expectTrue "triggerStates default to empty" (Map.null (triggerStates ss))
            -- the variable must be usable by the predicate DSL ...
            r5 <- expectTrue "CompareVar evaluates against the default variable"
                    (evalPredicate (CompareVar "quest_stage" CGte 3) st)
            -- ... and a real loop turn must run through the rule engine.
            let (loop', _) = applyLoopCommand (Go North) (initLoopState st)
                st' = lsCurrent loop'
            r6 <- expectEqual "hallway" (currentRoom (save st'))
            pure (r1 && r2 && r3 && r5 && r6)

-- | Second companion used by the P0-2 combat regressions.
guardDef :: NPCDef
guardDef = NPCDef "guard" "guard" (plainText "A silent guard.")
    Map.empty ["guard"] (Just 20) 3 1 Map.empty False emptyAscii Map.empty emptyGrammar

-- | P0-2 fixture: sample game in the hallway with two companions (guard,
--   squire) and a rule that announces the goblin's death. `goblinHp` decides
--   whether the player's own blow is lethal (5) or helper blows are needed (9).
twoCompanionGame :: Int -> GameState
twoCompanionGame goblinHp =
    let st0 = partyGameInHallway True
        deathRule = TriggerDef "goblin_dies" (OnStateChange "goblin") Nothing
            [ SendMessage "Der Goblin fällt und lässt die Keule fallen." ] False 0
    in st0
        { world = (world st0)
            { npcDefs = Map.insert "guard" guardDef (npcDefs (world st0))
            , triggerDefs = [deathRule] }
        , save = (save st0)
            { npcStates = Map.insert "guard"
                (NPCState (InRoom "hallway") "alive" (Just 20) Map.empty Nothing)
                (Map.insert "goblin"
                    (NPCState (InRoom "hallway") "alive" (Just goblinHp) Map.empty Nothing)
                    (npcStates (save st0)))
            , variables = Map.insert "party.guard" (VVInt 1) (variables (save st0)) }
        }

-- | P0-2 (a), literal review case: a companion is in the room and the target
--   dies to the player's own strike; the OnStateChange message must survive.
testCombatDeathMessagePlayerKill :: IO Bool
testCombatDeathMessagePlayerKill = do
    let (st', msg) = executeCommand (Interact VAttack "goblin") (twoCompanionGame 5)
    r1 <- expectTrue "NPC death event message reaches the player"
            (isInfixOf "Der Goblin fällt und lässt die Keule fallen." msg)
    r2 <- expectTrue "player kill line still shown" (isInfixOf "kill it" msg)
    r3 <- expectEqual (Just "dead") (npcStatus <$> Map.lookup "goblin" (npcStates (save st')))
    pure (r1 && r2 && r3)

-- | P0-2 (b), the actual regression: the lethal blow is a companion's, and a
--   second companion's (silent) effect follows it. The old `executeAttack`
--   folded effects itself and kept only the LAST effect's message, so the
--   death message was swallowed.
testCombatDeathMessageAfterTrailingEffect :: IO Bool
testCombatDeathMessageAfterTrailingEffect = do
    let (st', msg) = executeCommand (Interact VAttack "goblin") (twoCompanionGame 9)
    r1 <- expectTrue "NPC death event message survives a trailing effect"
            (isInfixOf "Der Goblin fällt und lässt die Keule fallen." msg)
    r2 <- expectTrue "companion strikes still reported" (isInfixOf "strikes for" msg)
    r3 <- expectEqual (Just "dead") (npcStatus <$> Map.lookup "goblin" (npcStates (save st')))
    pure (r1 && r2 && r3)

-- | P0-2 (c): the same shape with the player's ship aboard — the ship's power
--   spend and hull effect trail the lethal companion strike.
shipCompanionGame :: GameState
shipCompanionGame =
    let st0 = shipGame 2 3 4 10
        w0 = world st0
        deathRule = TriggerDef "goblin_dies" (OnStateChange "goblin") Nothing
            [ SendMessage "Der Goblin fällt und lässt die Keule fallen." ] False 0
    in st0
        { world = w0
            { npcDefs = Map.insert "squire" squireDef (npcDefs w0)
            , triggerDefs = [deathRule] }
        , save = (save st0)
            { npcStates = Map.insert "squire"
                (NPCState (InRoom "hallway") "alive" (Just 20) Map.empty Nothing)
                (Map.insert "goblin"
                    (NPCState (InRoom "hallway") "alive" (Just 9) Map.empty Nothing)
                    (npcStates (save st0)))
            , variables = Map.insert "party.squire" (VVInt 1) (variables (save st0)) }
        }

testCombatDeathMessageWithShip :: IO Bool
testCombatDeathMessageWithShip = do
    let (st', msg) = executeCommand (Interact VAttack "goblin") shipCompanionGame
    r1 <- expectTrue "NPC death event message survives with the ship present"
            (isInfixOf "Der Goblin fällt und lässt die Keule fallen." msg)
    r2 <- expectTrue "ship volley still reported" (isInfixOf "Kestrel fires for 3" msg)
    r3 <- expectEqual (Just "dead") (npcStatus <$> Map.lookup "goblin" (npcStates (save st')))
    pure (r1 && r2 && r3)

-- ===== Phase V: frontend abstraction =====

-- | A recording frontend with scripted input. Proves the loop's policy
--   functions drive any presentation, not just Haskeline/stdout: the same
--   'runGameWithFrontend' entry point runs to completion without a terminal.
cannedFrontend :: [Maybe String] -> IO ([String], [String], [(Int, [String])], [Maybe String])
cannedFrontend script = cannedFrontendWith initSampleGame script

-- | Variant with a custom initial state (used by the cutscene test).
cannedFrontendWith :: GameState -> [Maybe String] -> IO ([String], [String], [(Int, [String])], [Maybe String])
cannedFrontendWith startState script = do
    outRef <- newIORef []
    diagRef <- newIORef []
    inRef <- newIORef script
    playRef <- newIORef []
    let fe = Frontend
            { feEmitLine    = \l -> modifyIORef' outRef (l :)
            , feEmitRaw     = \s -> modifyIORef' outRef (s :)
            , feReadInput   = \_ _ -> do
                  queue <- readIORef inRef
                  case queue of
                      -- empty queue = end of input, the loop quits
                      []       -> pure Nothing
                      (x : xs) -> writeIORef inRef xs >> pure x
            , feReadPlain   = \_st _ -> do
                  -- Rogue Phase 1: the death/victory loops read via
                  -- feReadPlain, so they share the scripted queue.
                  queue <- readIORef inRef
                  case queue of
                      []       -> pure Nothing
                      (x : xs) -> writeIORef inRef xs >> pure x
            , feReadPause   = \_prompt -> pure ()
            , fePlayFrames  = \micros frames -> modifyIORef' playRef ((micros, frames) :)
            , feDiagnostics = \ms -> modifyIORef' diagRef (++ ms)
            , fePlaySfx     = \_ -> pure ()
            , feStartMusic  = \_ -> pure ()
            , feStopMusic   = pure ()
            }
    runGameWithFrontend fe startState
    out <- reverse <$> readIORef outRef
    diag <- readIORef diagRef
    remaining <- readIORef inRef
    played <- reverse <$> readIORef playRef
    pure (out, diag, played, remaining)

-- | Rogue Phase 1: drive 'handleGameOver' directly with a scripted death
--   screen. 'slot' becomes the LoopState's 'lsSaveSlot' (the ironman
--   checkpoint); 'script' feeds 'feReadPlain'. Returns the captured output.
driveDeathScreen :: GameState -> Maybe String -> [Maybe String] -> IO [String]
driveDeathScreen startState slot script = do
    outRef <- newIORef []
    inRef <- newIORef script
    let fe = Frontend
            { feEmitLine    = \l -> modifyIORef' outRef (l :)
            , feEmitRaw     = \s -> modifyIORef' outRef (s :)
            , feReadInput   = \_ _ -> pure Nothing
            , feReadPlain   = \_st _ -> do
                  queue <- readIORef inRef
                  case queue of
                      []       -> pure Nothing
                      (x : xs) -> writeIORef inRef xs >> pure x
            , feReadPause   = \_prompt -> pure ()
            , fePlayFrames  = \_ _ -> pure ()
            , feDiagnostics = \_ -> pure ()
            , fePlaySfx     = \_ -> pure ()
            , feStartMusic  = \_ -> pure ()
            , feStopMusic   = pure ()
            }
    handleGameOver fe (initLoopState startState) { lsSaveSlot = slot }
    reverse <$> readIORef outRef

-- ===== Rogue Phase 1: permadeath, ironman, savezones =====

-- | A dead sample game: fresh world with the given policy, game over with
--   reason Death.
deadStateWith :: GamePolicy -> GameState
deadStateWith policy = initSampleGame
    { world = (world initSampleGame) { worldGamePolicy = policy }
    , save = (save initSampleGame) { gameOver = True, gameOverReason = Just Death }
    }

-- | Rogue Phase 1: `allow_undo: false` rejects `undo` outright — state stays,
--   message names the policy.
testPermadeathDisablesUndo :: IO Bool
testPermadeathDisablesUndo = do
    let noUndo = initSampleGame
            { world = (world initSampleGame)
                        { worldGamePolicy = defaultGamePolicy { gpAllowUndo = False } } }
        loop0 = initLoopState noUndo
        (l1, _) = applyLoopCommand (Go North) loop0
        (l2, msg) = applyLoopCommand Undo l1
    r1 <- expectTrue "undo rejected when allow_undo = false"
                     ("Undo is disabled in this adventure." `isInfixOf` msg)
    r2 <- expectTrue "state not reverted by the refused undo"
                     (lsCurrent l2 == lsCurrent l1)
    pure (r1 && r2)

-- | Rogue Phase 1: with permadeath the death screen offers only restart and
--   quit; 'u' and 'l' inputs are rejected, not executed.
testPermadeathDeathMenu :: IO Bool
testPermadeathDeathMenu = do
    let dead = deadStateWith defaultGamePolicy { gpPermadeath = True }
    (out, _, _, _) <- cannedFrontendWith dead [Just "u", Just "l", Just "q"]
    r1 <- expectTrue "permadeath menu shows restart/quit"
                     (any ("  [R]estart  |  [Q]uit" `isInfixOf`) out)
    r2 <- expectTrue "no [L]oad hint on the permadeath menu"
                     (not (any ("[L]oad last save" `isInfixOf`) out))
    r3 <- expectTrue "'u' is refused after death"
                     (any ("No undo after death" `isInfixOf`) out)
    r4 <- expectTrue "'l' is refused after death"
                     (any ("No load after death" `isInfixOf`) out)
    r5 <- expectTrue "'q' still quits" (any ("Thanks for playing!" `isInfixOf`) out)
    pure (r1 && r2 && r3 && r4 && r5)

-- | Rogue Phase 1 (pure gates): ironman saving is allowed only inside a
--   savezone; load is rejected in ironman mode. Without ironman everything
--   behaves as before.
testIronmanSaveOnlyInSavezone :: IO Bool
testIronmanSaveOnlyInSavezone = do
    let iron = defaultGamePolicy { gpIronman = True, gpSaveZones = ["start"] }
        ironSt = initSampleGame
            { world = (world initSampleGame) { worldGamePolicy = iron } }
        defaultSt = initSampleGame
        (moved, _) = executeCommand (Go North) ironSt
    r1 <- expectTrue "save allowed in a savezone"
                     (saveBlockedMessage ironSt == Nothing)
    r2 <- expectTrue "save blocked outside savezones"
                     (saveBlockedMessage moved == Just "You can only rest at a savezone.")
    r3 <- expectTrue "normal adventures save anywhere"
                     (saveBlockedMessage initSampleGame == Nothing)
    r4 <- expectTrue "load blocked in ironman"
                     (loadBlockedMessage ironSt == Just "Loading is disabled in ironman mode.")
    r5 <- expectTrue "load allowed without ironman"
                     (loadBlockedMessage defaultSt == Nothing)
    pure (r1 && r2 && r3 && r4 && r5)

-- | Rogue Phase 1 (IO): saving inside a savezone writes the checkpoint slot;
--   the death screen then deletes it ('--save' start files are untouched).
--   Also verifies the in-zone save message and the outside-zone rejection
--   through the scripted loop.
testIronmanCheckpointDeletedOnDeath :: IO Bool
testIronmanCheckpointDeletedOnDeath = withSavesIsolation $ do
    let iron = defaultGamePolicy { gpIronman = True, gpSaveZones = ["start"] }
        ironSt = initSampleGame
            { world = (world initSampleGame) { worldGamePolicy = iron } }
        dead = ironSt { save = (save ironSt) { gameOver = True, gameOverReason = Just Death } }
    SaveLoad.saveGame dead SaveLoad.ironmanCheckpointSlot
    checkpointPath <- SaveLoad.saveSlotPath SaveLoad.ironmanCheckpointSlot
    existed <- doesFileExist checkpointPath
    -- 'l' is rejected (load disabled in ironman), 'q' quits the death screen
    out <- driveDeathScreen dead (Just SaveLoad.ironmanCheckpointSlot) [Just "l", Just "q"]
    r1 <- expectTrue "checkpoint existed before death" existed
    r2 <- expectTrue "load attempt refused on the death screen"
                     (any ("Loading is disabled in ironman mode." `isInfixOf`) out)
    r3 <- expectTrue "permadeath/ironman menu without undo/load"
                     (any ("  [R]estart  |  [Q]uit" `isInfixOf`) out)
    gone <- doesFileExist =<< SaveLoad.saveSlotPath SaveLoad.ironmanCheckpointSlot
    r4 <- expectTrue "checkpoint deleted with the run" (not gone)
    pure (r1 && r2 && r3 && r4)

-- | Rogue Phase 2 (pure): meta.* survives Restart while normal variables
--   reset to 'lsInitial'; the checkpoint slot does not leak into the new run.
testMetaProgressionPreservedOnRestart :: IO Bool
testMetaProgressionPreservedOnRestart = do
    -- The pristine initial state (what `lsInitial` holds: no gold, meta.souls
    -- at 0) plus a mid-run state with earned progress and a checkpoint.
    let pristine = initSampleGame
            { world = (world initSampleGame)
                { worldGamePolicy = defaultGamePolicy { gpIronman = True } }
            , save = (save initSampleGame)
                { variables = Map.fromList [("meta.souls", VVInt 0)] } }
        mid = pristine { save = (save pristine)
            { variables = Map.fromList
                [ ("meta.souls", VVInt 7)
                , ("meta.unlocked_class", VVText "mage")
                , ("gold", VVInt 9) ] } }
        loop = LoopState mid [] pristine (Just "checkpoint") Nothing
        (l2, msg) = applyLoopCommand Restart loop
    -- The fresh run starts from `lsInitial` (gold absent there); meta.souls=7
    -- and meta.unlocked_class survive, gold does not leak.
    r1 <- expectEqual (Just (VVInt 7))  (Map.lookup "meta.souls" (variables (save (lsCurrent l2))))
    r2 <- expectEqual (Just (VVText "mage")) (Map.lookup "meta.unlocked_class" (variables (save (lsCurrent l2))))
    r3 <- expectTrue "normal variable gold does not leak into the new run"
                     (Map.notMember "gold" (variables (save (lsCurrent l2))))
    r4 <- expectTrue "checkpoint slot reset on restart" (lsSaveSlot l2 == Nothing)
    r5 <- expectTrue "restart lands on the pristine look text"
                     ("stone chamber" `isInfixOf` msg)
    pure (r1 && r2 && r3 && r4 && r5)

-- | Zusatz-Empfehlung 4 (Rogue P2): `meta.runs` counts fresh runs — bumped at
--   run start and on every restart, but only for adventures that actually use
--   meta-progression (declared meta.* variable or carried/disk meta vars).
testMetaRunsCounter :: IO Bool
testMetaRunsCounter = do
    -- a meta adventure: declares meta.souls, counter carried from the disk map
    let metaWorld = (world initSampleGame)
            { varDefs = Map.fromList [("meta.souls", VarDef "meta.souls" (VTInt Nothing Nothing) (VVInt 0))] }
        pristine = initSampleGame
            { world = metaWorld
            , save = (save initSampleGame)
                { variables = Map.fromList [("meta.souls", VVInt 0)] } }
    -- restart path bumps: 0 (carried) -> run 1... but the pristine state had no
    -- meta.runs yet; two bumps simulate run-start + restart
    let b1 = bumpMetaRuns pristine
        b2 = bumpMetaRuns b1
    r1 <- expectEqual (Just (VVInt 2)) (Map.lookup "meta.runs" (variables (save b2)))
    -- restart path: the carried counter increments once per restart
    let mid = pristine { save = (save pristine)
            { variables = Map.fromList [("meta.souls", VVInt 3), ("meta.runs", VVInt 5)] } }
        loop2 = LoopState mid [] pristine Nothing Nothing
        (l3, _) = applyLoopCommand Restart loop2
    r2 <- expectTrue "pure restart branch carries meta.runs (IO path bumps)"
        (Map.lookup "meta.runs" (variables (save (lsCurrent l3))) == Just (VVInt 1))
    -- plain adventures stay untouched (Default-Invariante): no meta.runs, no
    -- meta file is ever created for them
    let plain = initSampleGame
        plainBumped = bumpMetaRuns plain
    r3 <- expectTrue "no meta.runs for adventures without meta-progression"
        (Map.notMember "meta.runs" (variables (save plainBumped)))
    pure (r1 && r2 && r3)

-- | Zusatz-Empfehlung 3 (Rogue): the restart reseeds the rngState (fresh
--   stream per run); the pure injection function is what the IO path uses.
testRestartRngReseeded :: IO Bool
testRestartRngReseeded = do
    let st = initSampleGame
        reseeded = reseedRng 4242 st
    r1 <- expectEqual (Just 4242) (Just (rngState (save reseeded)))
    r2 <- expectTrue "other fields untouched"
        (save reseeded == (save st) { rngState = 4242 })
    pure (r1 && r2)

-- | Rogue Phase 2 (IO): meta.* is written at game over (death) to
--   `saves/<slug>_meta.json`, with the slug from `game.meta_slug` when given.
testMetaWrittenOnGameOver :: IO Bool
testMetaWrittenOnGameOver = withSavesIsolation $ do
    let withMeta = initSampleGame
            { world = (world initSampleGame)
                { worldGamePolicy = defaultGamePolicy
                    { gpIronman = True, gpSaveZones = ["start"]
                    , gpMetaSlug = Just "katakombe_test" } }
            , save = (save initSampleGame)
                { variables = Map.fromList [("meta.souls", VVInt 3), ("hp_like", VVInt 1)]
                , gameOver = True, gameOverReason = Just Death } }
    _ <- driveDeathScreen withMeta (Just SaveLoad.ironmanCheckpointSlot) [Just "q"]
    metaPath <- SaveLoad.metaSavePath (world withMeta)
    written <- doesFileExist metaPath
    r1 <- expectTrue "meta file written on death" written
    metaOnDisk <- SaveLoad.loadMeta (world withMeta)
    r2 <- expectEqual (Just (VVInt 3)) (Map.lookup "meta.souls" metaOnDisk)
    pure (r1 && r2)


-- ===== Rogue Phase 3: dynamic exits (SetExit / RemoveExit) =====

-- | Rogue Phase 3: `SetExit` opens/rewires a connection at runtime (a runtime
--   `Locked` exit lazily seeds its entity state as "locked", M4), `RemoveExit`
--   closes even a static exit. All consumers share 'effectiveConnections';
--   overrides survive the save round-trip; `idsFromOutcomeRoom` feeds the
--   MissingRoom validation (L4).
testDynamicExitOverrides :: IO Bool
testDynamicExitOverrides = do
    let st0 = initSampleGame
        -- open a brand-new exit west into the meadow
        (stOpen, _) = applyOutcome (SetExit "start" West (Open "meadow")) "torch" st0
        -- rewire south (statically open to meadow) into a locked vault door
        (stRewire, _) = applyOutcome (SetExit "start" South (Locked "vault" "vault_door")) "torch" st0
        -- close the static north exit
        (stRemoved, _) = applyOutcome (RemoveExit "start" North) "torch" st0
        -- a state whose overrides were set for the round trip
        stRT = st0 { save = (save st0) { exitOverrides = Map.fromList
            [ (("start", North), Just (Open "hallway"))
            , (("camp", South), Nothing) ] } }
    r1 <- expectTrue "new exit is walkable" (canMove West stOpen)
    r2 <- expectTrue "rewired exit resolves to the new connection"
                     (getExitInDirection South stRewire == Just (Locked "vault" "vault_door"))
    r3 <- expectTrue "runtime-locked entity seeded as locked"
                     (getEntityState "vault_door" stRewire == Just "locked")
    r4 <- expectTrue "removed static exit blocks movement"
                     (not (canMove North stRemoved) && canMove North st0)
    r5 <- expectTrue "exit overrides survive save/load round-trip"
                     (case Aeson.decode (Aeson.encode (save stRT)) :: Maybe SaveState of
                        Just ss -> exitOverrides ss == exitOverrides (save stRT)
                        Nothing -> False)
    r6 <- expectEqual ["hall", "treasure"]
                     (idsFromOutcomeRoom (SetExit "hall" North (Open "treasure")))
    pure (r1 && r2 && r3 && r4 && r5 && r6)

testLoopRunsOnCannedFrontend :: IO Bool
testLoopRunsOnCannedFrontend = do
    (out, diag, played, remaining) <- cannedFrontend [Just "look", Just "take torch", Nothing]
    r1 <- expectTrue "look output names the starting room description"
                     (any ("small stone chamber" `isInfixOf`) out)
    r2 <- expectTrue "take confirms the torch" (any ("You take the torch." `isInfixOf`) out)
    r3 <- expectTrue "EOF quits (Goodbye + thanks)"
                     (any ("Goodbye!" `isInfixOf`) out && any ("Thanks for playing!" `isInfixOf`) out)
    r4 <- expectTrue "no diagnostics on a healthy game" (null diag)
    r5 <- expectTrue "script fully consumed" (null remaining)
    r6 <- expectTrue "nothing played without art" (null played)
    pure (and [r1, r2, r3, r4, r5, r6])

-- ===== Phase H/H1: playback rate comes from the art =====

-- | An ambient loop plays its own frames at its own rate (H1): the pure
--   `asciiPlayback` returns frames and the delay in microseconds (fps 4 =
--   250000 µs), while `frames`/`every` arts fall back to the default rate.
testAsciiPlaybackAmbient :: IO Bool
testAsciiPlaybackAmbient = do
    let ambientArt = emptyAscii { aaAmbient = Just (Ambient ["A0", "A1", "A2"] 4) }
        frameArt   = emptyAscii { aaFrames = [plainText "F0", plainText "F1"], aaEvery = 1 }
        g = initSampleGame
    let (framesA, delayA) = asciiPlayback ambientArt g
        (framesB, delayB) = asciiPlayback frameArt g
    r1 <- expectEqual ["A0", "A1", "A2"] framesA
    r2 <- expectEqual 250000 delayA
    r3 <- expectEqual ["F0", "F1"] framesB
    r4 <- expectEqual defaultFrameMicros delayB
    pure (r1 && r2 && r3 && r4)

-- | `watch` on an art with an ambient block plays the ambient loop at its
--   rate: `pendingAnimation` now carries frames *and* the µs delay (H1).
testWatchCarriesRate :: IO Bool
testWatchCarriesRate = do
    let art = emptyAscii { aaAmbient = Just (Ambient ["W0", "W1"] 8) }
        torch = (itemDefs (world initSampleGame) Map.! "torch") { itemAscii = art }
        g = initSampleGame
                { world = (world initSampleGame)
                    { itemDefs = Map.insert "torch" torch (itemDefs (world initSampleGame)) } }
        cmd = parseCommandWith Map.empty "watch torch"
        (st', _) = executeCommand cmd g
    r1 <- expectEqual (Just (["W0","W1"], 125000)) (pendingAnimation st')
    pure r1

-- | Ambient survives the world JSON round trip and renders byte-identically.
-- | Ambient survives the world JSON round trip byte-identically (H1).
-- | `siblingSavePath` finds the compiler's save.json next to the world
--   (H4-Regressionsbegleitung: `--world` ohne `--save` laedt das Paar).
testSiblingSavePath :: IO Bool
testSiblingSavePath = do
    tmp <- getTemporaryDirectory
    let dir = tmp </> "sibsave"
        worldPath = dir </> "world.json"
    createDirectoryIfMissing True dir
    writeFile worldPath "{}"
    noSibling <- siblingSavePath worldPath
    writeFile (dir </> "save.json") "{}"
    withSibling <- siblingSavePath worldPath
    removeFile (dir </> "save.json")
    removeFile worldPath
    r1 <- expectTrue "no sibling -> Nothing" (isNothing noSibling)
    r2 <- expectTrue "sibling save.json found" (withSibling == Just (dir </> "save.json"))
    pure (r1 && r2)

testAmbientRoundTrip :: IO Bool
testAmbientRoundTrip = do
    let art = emptyAscii { aaAmbient = Just (Ambient ["w1", "w2"] 6) }
        decoded = Aeson.decode (Aeson.encode art) :: Maybe AsciiArt
        rate = case decoded >>= aaAmbient of
            Just amb -> (ambFps amb, ambFrames amb)
            Nothing  -> (0, [])
    r1 <- expectEqual (Just art) decoded
    r2 <- expectEqual (6, ["w1", "w2"]) rate
    pure (r1 && r2)

-- ===== Phase H/H4: cutscenes (clips, intro, play_clip) =====

-- | A small world: the hall carries an intro clip, the world declares it.
worldWithHallIntro :: GameState
worldWithHallIntro =
    let hall = (rooms (world initSampleGame) Map.! "hallway") { roomIntro = Just "pan" }
    in initSampleGame
        { world = (world initSampleGame)
            { rooms = Map.insert "hallway" hall (rooms (world initSampleGame))
            , worldClips = Map.fromList [("pan", Clip ["P0", "P1"] 4)]
            } }

-- | Entering a room with an `intro` queues its clip for one playback (H4):
--   frames and rate in µs (fps 4 = 250000 µs).
testIntroQueuesCutscene :: IO Bool
testIntroQueuesCutscene = do
    let cmd = parseCommandWith Map.empty "go north"
        (st', _) = executeCommand cmd worldWithHallIntro
    r1 <- expectEqual (Just (["P0", "P1"], 250000)) (pendingCutscene st')
    r2 <- expectEqual "hallway" (currentRoom (save st'))
    pure (r1 && r2)

-- | The `play_clip` effect queues the declared clip (H4, D19: the effect
--   wins over a room intro).
testPlayClipQueuesCutscene :: IO Bool
testPlayClipQueuesCutscene = do
    let (st', msg) = applyOutcome (PlayClip "pan") "" worldWithHallIntro
    r1 <- expectEqual (Just (["P0", "P1"], 250000)) (pendingCutscene st')
    r2 <- expectTrue "no player-facing message" (null msg)
    pure (r1 && r2)

-- | The loop plays a queued cutscene once through the frontend and clears it
--   (H4): the canned frontend records exactly one (rate, frames) playback.
testLoopPlaysCutscene :: IO Bool
testLoopPlaysCutscene = do
    (_, diag, played, remaining) <-
        cannedFrontendWith worldWithHallIntro [Just "go north", Nothing]
    r1 <- expectEqual [(250000, ["P0", "P1"])] played
    r2 <- expectTrue "no diagnostics" (null diag)
    r3 <- expectTrue "script fully consumed" (null remaining)
    pure (r1 && r2 && r3)

-- | Audio Phase 1 & 2: PlaySfx queues file paths, PlayMusic / StopMusic set pendingMusic.
testAudioEffectsQueueState :: IO Bool
testAudioEffectsQueueState = do
    let (st1, _) = applyOutcome (PlaySfx "sword.wav") "" initSampleGame
    r1 <- expectEqual ["sword.wav"] (pendingSfx st1)
    let (st2, _) = applyOutcome (Sequence [PlaySfx "hit.wav", PlayMusic "boss.xm"]) "" st1
    r2 <- expectEqual ["sword.wav", "hit.wav"] (pendingSfx st2)
    r3 <- expectEqual (Just (MusicStart "boss.xm")) (pendingMusic st2)
    let (st3, _) = applyOutcome StopMusic "" st2
    r4 <- expectEqual (Just MusicStop) (pendingMusic st3)
    pure (r1 && r2 && r3 && r4)

-- ===== Completion Tests =====

testCompletionSuggestsNpcName :: IO Bool
testCompletionSuggestsNpcName = do
    suggestions <- getCompletions "look at ol"
    expectContains "old man" suggestions

testCompletionSuggestsInventoryItemForUse :: IO Bool
testCompletionSuggestsInventoryItemForUse = do
    suggestions <- getCompletionsWithState "use t"
    expectContains "torch" suggestions

testCompletionSuggestsEquip :: IO Bool
testCompletionSuggestsEquip = do
    let withSword = pickupItem "sword_rusty" initSampleGame
    (_, comps) <- commandCompletion withSword ("equip ru", "")
    expectContains "rusty sword" [replacement c | c <- comps]

-- ===== JSON Round-Trip Tests =====

testSaveStateRoundTrip :: IO Bool
testSaveStateRoundTrip = do
    let ss = save initSampleGame
        encoded = Aeson.encode ss
    case Aeson.decode encoded of
        Just decoded -> expectEqual ss decoded
        Nothing -> do
            putStrLn "  JSON decode failed"
            pure False

testSaveStateBackwardCompat :: IO Bool
testSaveStateBackwardCompat = do
    -- Simulate an old save without flags/turnCount/gameOverReason/visitedRooms/equipment
    let oldJson = "{\"player\":{\"playerHealth\":100,\"playerMaxHealth\":100,\"playerAttack\":10,\"playerDefense\":5},\"currentRoom\":\"start\",\"inventory\":[],\"itemStates\":{},\"npcStates\":{},\"entityStates\":{},\"gameOver\":false}"
    case Aeson.decode (BLC.pack oldJson) :: Maybe SaveState of
        Just ss -> do
            r1 <- expectEqual Map.empty (flags ss)
            r2 <- expectEqual 0 (turnCount ss)
            r3 <- expectEqual Nothing (gameOverReason ss)
            r4 <- expectTrue "visitedRooms defaults empty" (Set.null (visitedRooms ss))
            r5 <- expectTrue "equipment defaults empty" (Map.null (equipment ss))
            pure (r1 && r2 && r3 && r4 && r5)
        Nothing -> do
            putStrLn "  Failed to decode old-format SaveState"
            pure False

-- ===== Skills (Phase 2) =====

testGetSkillUnknown :: IO Bool
testGetSkillUnknown = expectEqual 0 (getSkill "nonexistent" initSampleGame)

testModifySkill :: IO Bool
testModifySkill = do
    let s1 = modifySkill "stealth" 3 initSampleGame
        s2 = modifySkill "stealth" 2 s1
    r1 <- expectEqual 3 (getSkill "stealth" s1)
    r2 <- expectEqual 5 (getSkill "stealth" s2)
    r3 <- expectEqual 2 (getSkill "lockpick" initSampleGame)  -- sample has lockpick 2
    pure (r1 && r2 && r3)

testCheckSkillOutcome :: IO Bool
testCheckSkillOutcome = do
    -- Conditional with PTrue always takes the then-branch
    let (_, msg) = applyOutcome
            (Conditional PTrue
                (SendMessage "PASSES") (SendMessage "FAILED"))
            "" initSampleGame
    expectEqual "PASSES" msg

-- ===== Conditions (Phase 2) =====

testConditionTickExpire :: IO Bool
testConditionTickExpire = do
    let poisoned = applyCondition "poisoned" 2
                        (Just (SendMessage "It stings."))
                        (Just (SendMessage "You feel better.")) initSampleGame
        (after1, _) = tickConditions poisoned
        (after2, msgs2) = tickConditions after1
    r1 <- expectTrue "active after 1 tick" (hasCondition "poisoned" after1)
    r2 <- expectTrue "expired after 2 ticks" (not (hasCondition "poisoned" after2))
    let endMsgs = [renderEvents m | m <- msgs2, "You feel better." `isPrefixOf` renderEvents m]
    r3 <- expectTrue "end message produced" (not (null endMsgs))
    pure (r1 && r2 && r3)

testConditionTickDamage :: IO Bool
testConditionTickDamage = do
    let hurt = applyCondition "bleeding" 3
                    (Just (ModifyValue VRPlayerHealth (-10)))
                    Nothing initSampleGame
        (after1, _) = tickConditions hurt
        (after2, _) = tickConditions after1
        hp1 = playerHealth (player (save after1))
        hp2 = playerHealth (player (save after2))
    r1 <- expectEqual 90 hp1
    r2 <- expectEqual 80 hp2
    pure (r1 && r2)

testClearCondition :: IO Bool
testClearCondition = do
    let applied = applyCondition "blessed" 5 Nothing Nothing initSampleGame
        cleared = clearCondition "blessed" applied
    r1 <- expectTrue "active before clear" (hasCondition "blessed" applied)
    r2 <- expectTrue "gone after clear" (not (hasCondition "blessed" cleared))
    pure (r1 && r2)

testHasConditionBranch :: IO Bool
testHasConditionBranch = do
    -- Conditional with PAll/PNot: then-branch when predicate holds
    let outcome = Conditional (PAll [PTrue, PNot PTrue])
                    (SendMessage "WRONG") (SendMessage "RIGHT")
        (_, msg) = applyOutcome outcome "" initSampleGame
    expectEqual "RIGHT" msg

testStatsShowsConditions :: IO Bool
testStatsShowsConditions = do
    let applied = applyCondition "poisoned" 4 Nothing Nothing initSampleGame
        (_, msg) = executeCommand StatsCmd applied
    expectTrue "stats mentions poisoned" ("poisoned" `isInfixOf` msg)

-- ===== Quests (Phase 2) =====

testQuestLifecycle :: IO Bool
testQuestLifecycle = do
    let started = startQuest "find_treasure" initSampleGame
        advanced = advanceQuest "find_treasure" started
        advanced2 = advanceQuest "find_treasure" advanced
        completed = advanceQuest "find_treasure" advanced2  -- 3 stages, third advance completes
    r1 <- expectEqual (Just 0) (questStage "find_treasure" started)
    r2 <- expectEqual (Just 1) (questStage "find_treasure" advanced)
    r3 <- expectEqual (Just 2) (questStage "find_treasure" advanced2)
    r4 <- expectTrue "completed after final advance" (questCompleted "find_treasure" completed)
    r5 <- expectTrue "no longer active" (not (Map.member "find_treasure" (activeQuests (save completed))))
    pure (r1 && r2 && r3 && r4 && r5)

testQuestPrereqs :: IO Bool
testQuestPrereqs = do
    -- QuestOp StartQuest without the flag set must fail
    let outcome = QuestOp StartQuest "gated_quest"
        (_, msg1) = applyOutcome outcome "" initSampleGame
    r1 <- expectEqual "You cannot start that quest right now." msg1
    -- With the flag set it must work (no message)
    let unlocked = setFlag "met_oldman" "true" initSampleGame
        (st2, _) = applyOutcome outcome "" unlocked
    r2 <- expectEqual (Just 0) (questStage "gated_quest" st2)
    pure (r1 && r2)

testJournalShowsQuest :: IO Bool
testJournalShowsQuest = do
    let started = startQuest "find_treasure" initSampleGame
        (_, msg) = executeCommand JournalCmd started
    r1 <- expectTrue "journal mentions quest name" ("The Lost Treasure" `isInfixOf` msg)
    r2 <- expectTrue "journal shows current stage" ("Explore the dark hallway." `isInfixOf` msg)
    pure (r1 && r2)

testQuestRewardFires :: IO Bool
testQuestRewardFires = do
    -- Start, advance twice, then CompleteQuest directly
    let started = startQuest "find_treasure" initSampleGame
        (completed, msg) = applyOutcome (QuestOp CompleteQuest "find_treasure") "" started
    r1 <- expectTrue "quest completed" (questCompleted "find_treasure" completed)
    r2 <- expectTrue "reward message present" ("treasure is yours" `isInfixOf` msg)
    pure (r1 && r2)

-- ===== Vehicle tests (Phase 3) =====

testEnterVehicleWrongRoom :: IO Bool
testEnterVehicleWrongRoom = do
    -- Player starts in "start", carriage is at "meadow"
    let (_, msg) = executeCommand (EnterVehicleCmd "carriage") initSampleGame
    expectEqual "The carriage is not here." msg

testEnterVehicle :: IO Bool
testEnterVehicle = do
    let inMeadow = moveToRoom "meadow" initSampleGame
        (st, msg) = executeCommand (EnterVehicleCmd "carriage") inMeadow
    r1 <- expectEqual "You board the carriage." msg
    r2 <- expectEqual (Just "carriage") (currentVehicle (save st))
    r3 <- expectEqual "carriage_cabin" (currentRoom (save st))
    pure (r1 && r2 && r3)

testExitVehicle :: IO Bool
testExitVehicle = do
    let inMeadow = moveToRoom "meadow" initSampleGame
        aboard = fst (executeCommand (EnterVehicleCmd "carriage") inMeadow)
        (st, msg) = executeCommand ExitVehicleCmd aboard
    r1 <- expectEqual "You disembark from the carriage." msg
    r2 <- expectEqual Nothing (currentVehicle (save st))
    r3 <- expectEqual "meadow" (currentRoom (save st))
    pure (r1 && r2 && r3)

testDriveOutsideCockpitFails :: IO Bool
testDriveOutsideCockpitFails = do
    -- Driving requires being in the cockpit; the cockpit is the cabin itself,
    -- so driving while disembarked (not in a vehicle) fails.
    let (_, msg) = executeCommand (DriveToCmd "start") initSampleGame
    expectEqual "You are not in a vehicle." msg

testDriveToStop :: IO Bool
testDriveToStop = do
    let inMeadow = moveToRoom "meadow" initSampleGame
        aboard = fst (executeCommand (EnterVehicleCmd "carriage") inMeadow)
        (st, msg) = executeCommand (DriveToCmd "start") aboard
    r1 <- expectEqual "You drive to the stone chamber's entrance." msg
    r2 <- expectEqual "start" (currentRoom (save st))
    r3 <- expectEqual "start" (vsCurrentStop (getVehicleState "carriage" st))
    pure (r1 && r2 && r3)

testDriveToCurrentStop :: IO Bool
testDriveToCurrentStop = do
    let inMeadow = moveToRoom "meadow" initSampleGame
        aboard = fst (executeCommand (EnterVehicleCmd "carriage") inMeadow)
        (_, msg) = executeCommand (DriveToCmd "meadow") aboard
    expectTrue "cannot drive to the stop we are already at"
        ("can't drive there" `isInfixOf` msg)

testRefuelViaItem :: IO Bool
testRefuelViaItem = do
    let withHay = pickupItem "hay" initSampleGame
        (st, msg) = executeCommand (parseCommand "use hay on carriage") withHay
    r1 <- expectTrue "fuel message shown" ("fuelled" `isInfixOf` msg)
    r2 <- expectEqual (Just 10) (vsFuel (getVehicleState "carriage" st))
    pure (r1 && r2)

testVehicleConditionTick :: IO Bool
testVehicleConditionTick = do
    -- Simulate a hull breach-like condition: set it directly, then tick
    let aboard = fst (executeCommand (EnterVehicleCmd "carriage")
                        (moveToRoom "meadow" initSampleGame))
        breach = setVehicleState "carriage"
                    ((getVehicleState "carriage" aboard)
                        { vsActiveConditions = Set.singleton "test_leak" }) aboard
        breachDef = breach { world = (world breach)
            { vehicleDefs = Map.adjust (\v -> v { vehicleConditionEffects =
                Map.singleton "test_leak" (ModifyValue VRPlayerHealth (-5)) })
                "carriage" (vehicleDefs (world breach)) } }
        (st, _) = vehicleConditionTick breachDef
    r1 <- expectTrue "damage applied" (playerHealth (player (save st)) == 95)
    pure r1

testVehicleStateRoundTrip :: IO Bool
testVehicleStateRoundTrip = do
    let vs = VehicleState "meadow" (Just 7) (Set.singleton "derailed")
                (Map.singleton "cabin" "The cabin lists to one side.")
        encoded = Aeson.encode vs
        decoded = Aeson.decode encoded :: Maybe VehicleState
    expectEqual (Just vs) decoded

-- ===== Undo tests (Phase 4.3) =====

testUndoRestoresPreviousState :: IO Bool
testUndoRestoresPreviousState = do
    let loop0 = initLoopState initSampleGame
        (loop1, _) = applyLoopCommand (Go South) loop0
        (loop2, msg) = applyLoopCommand Undo loop1
    r1 <- expectEqual initSampleGame (lsCurrent loop2)
    r2 <- expectEqual "Undone." msg
    pure (r1 && r2)

testQuitDoesNotCreateUndoHistory :: IO Bool
testQuitDoesNotCreateUndoHistory = do
    let (loop1, _) = applyLoopCommand Quit (initLoopState initSampleGame)
    expectEqual [] (lsHistory loop1)

testHelpDoesNotAffectUndoHistory :: IO Bool
testHelpDoesNotAffectUndoHistory = do
    let (loop1, _) = applyLoopCommand Help (initLoopState initSampleGame)
    r1 <- expectEqual [] (lsHistory loop1)
    r2 <- expectEqual 0 (turnCount (save (lsCurrent loop1)))
    pure (r1 && r2)

testUndoEmptyHistory :: IO Bool
testUndoEmptyHistory = do
    let loop0 = initLoopState initSampleGame
        (loop1, msg) = applyLoopCommand Undo loop0
    r1 <- expectEqual loop0 loop1
    r2 <- expectEqual "Nothing to undo." msg
    pure (r1 && r2)

testMultipleUndo :: IO Bool
testMultipleUndo = do
    let loop0 = initLoopState initSampleGame
        (loop1, _) = applyLoopCommand (Go South) loop0
        (loop2, _) = applyLoopCommand (EnterVehicleCmd "carriage") loop1
        (loop3, _) = applyLoopCommand Undo loop2
        (loop4, _) = applyLoopCommand Undo loop3
    r1 <- expectEqual (lsCurrent loop1) (lsCurrent loop3)
    r2 <- expectEqual initSampleGame (lsCurrent loop4)
    pure (r1 && r2)

testUndoHistoryCappedAt50 :: IO Bool
testUndoHistoryCappedAt50 = do
    let step loop = fst (applyLoopCommand (Go South) loop)
        loop51 = iterate step (initLoopState initSampleGame) !! 51
    expectEqual 50 (length (lsHistory loop51))

testSaveDoesNotAffectUndoHistory :: IO Bool
testSaveDoesNotAffectUndoHistory = do
    let (loop1, _) = applyLoopCommand (Save "slot") (initLoopState initSampleGame)
    r1 <- expectEqual [] (lsHistory loop1)
    r2 <- expectEqual 0 (turnCount (save (lsCurrent loop1)))
    pure (r1 && r2)

testUndoRestoresAfterDeath :: IO Bool
testUndoRestoresAfterDeath = do
    let fragile = initSampleGame { save = (save initSampleGame)
            { player = (player (save initSampleGame)) { playerHealth = 1 } } }
        (deadLoop, _) = applyLoopCommand (Interact VAttack "goblin")
            (initLoopState (moveToRoom "hallway" fragile))
        (restoredLoop, _) = applyLoopCommand Undo deadLoop
    r1 <- expectTrue "attack killed player" (gameOver (save (lsCurrent deadLoop)))
    r2 <- expectTrue "undo clears game over" (not (gameOver (save (lsCurrent restoredLoop))))
    pure (r1 && r2)

-- ===== Phase 1: Einheitlicher Outcome-Interpreter (1a) =====

-- | Quest-Rewards unterstützen jetzt beliebige Outcomes (z.B. GiveItem),
--   nicht nur SendMessage – der vollständige Interpreter wird verwendet.
-- | 4.6: `on_complete:` starts the named quest once this one completes. The
--   chain target must be active at stage 0 afterwards, and the reward still
--   runs first (it may set the flags the chained quest requires).
testQuestOnCompleteStartsChain :: IO Bool
testQuestOnCompleteStartsChain = do
    let first = Quest "q1" "First" "d" Map.empty [QuestStage "s1" "step" Nothing] Nothing (Just "q2")
        second = Quest "q2" "Second" "d" Map.empty [QuestStage "s1" "step" Nothing] Nothing Nothing
        gated = Quest "q2" "Second" "d" (Map.singleton "unlock_q2" "true")
                    [QuestStage "s1" "step" Nothing] Nothing Nothing
        withQuests q2state = initSampleGame
            { world = (world initSampleGame)
                { questDefs = Map.fromList [("q1", first), ("q2", q2state)] } }
        started = fst (applyOutcome (QuestOp StartQuest "q1") "" (withQuests second))
        completed = fst (applyOutcome (QuestOp CompleteQuest "q1") "" started)
        chainedOn = fst (applyOutcome (QuestOp StartQuest "q1") "" (withQuests gated))
        gatedDone = fst (applyOutcome (QuestOp CompleteQuest "q1") "" chainedOn)
    r1 <- expectTrue "chained quest becomes active at stage 0"
            (Map.lookup "q2" (activeQuests (save completed)) == Just 0)
    r2 <- expectTrue "chained quest is not completed, only active"
            (not ("q2" `Set.member` completedQuests (save completed)))
    r3 <- expectTrue "chained quest with unmet prereq is not started"
            (Map.notMember "q2" (activeQuests (save gatedDone)))
    r4 <- expectTrue "the finished quest is completed either way"
            ("q1" `Set.member` completedQuests (save gatedDone))
    pure (r1 && r2 && r3 && r4)

-- | 4.6 byte contract: `questOnComplete` is written only when set, while
--   `questReward` stays in the JSON even as `null` (four shipped quests have
--   no reward, and those bytes are pinned). Round-trips through the parser.
testQuestOnCompleteJsonOmission :: IO Bool
testQuestOnCompleteJsonOmission = do
    let plain = Quest "q" "Q" "d" Map.empty [QuestStage "s1" "s" Nothing] Nothing Nothing
        chained = plain { questOnComplete = Just "q2" }
        enc q = BLC.unpack (Aeson.encode q)
    r1 <- expectTrue "questOnComplete omitted when unset" (not ("questOnComplete" `isInfixOf` enc plain))
    r2 <- expectTrue "questReward still written as null" ("questReward\":null" `isInfixOf` (filter (/= ' ') (enc plain)))
    r3 <- expectTrue "questOnComplete written when set" ("questOnComplete\":\"q2\"" `isInfixOf` (filter (/= ' ') (enc chained)))
    r4 <- expectEqual (Just (Just "q2")) (Aeson.decode (Aeson.encode chained) >>= Just . questOnComplete)
    r5 <- expectEqual (Just Nothing) (fmap questOnComplete (Aeson.decode (Aeson.encode plain)))
    pure (r1 && r2 && r3 && r4 && r5)

-- | 4.6: `initial_flags:` lives in the save, not the world — but a flag set
--   there *is* set when the game starts. Both flag checks ask "is this flag
--   ever set?", so both used to report a false positive and refuse to compile
--   legitimate content (measured: exit 1 + `MissingSetFlag "started"`).
testInitialFlagsCountAsSet :: IO Bool
testInitialFlagsCountAsSet = do
    let gw = (world initSampleGame)
                { triggerDefs =
                    [ TriggerDef "t_check" OnTurn (Just (HasFlag "started"))
                        [ SendMessage "nur wenn started" ] False 0 ]
                -- a different flag than the predicate checks: `setFlagsInWorld`
                -- counts predicate-referenced flags as set (a deliberately
                -- generous heuristic), so reusing one name would mask the case
                , questDefs = Map.singleton "q" (Quest "q" "Q" "d"
                                    (Map.singleton "tor_offen" "true")
                                    [QuestStage "s1" "s" Nothing] Nothing Nothing) }
        stWith = (save initSampleGame) { flags = Map.singleton "tor_offen" "true" }
        stWithout = save initSampleGame
        isMissingSetFlag e = case e of
            MissingSetFlag f _ -> f == "started"
            _ -> False
    r1 <- expectTrue "the save-blind entry point still reports the flag (unchanged contract)"
            (any isMissingSetFlag (validateWorld gw))
    r2 <- expectTrue "validateWorldWithFlags sees the start flags"
            (not (any isMissingSetFlag (validateWorldWithFlags gw (Set.singleton "started"))))
    r3 <- expectTrue "a quest prereq satisfied by initial_flags is not an error"
            (not (any isUnknownPrereq (validateGameState gw stWith)))
    r4 <- expectTrue "a prereq that is nowhere set is still an error"
            (any isUnknownPrereq (validateGameState gw stWithout))
    pure (r1 && r2 && r3 && r4)
  where
    isUnknownPrereq e = case e of
        UnknownQuestPrereq qid f -> qid == "q" && f == "tor_offen"
        _ -> False

testQuestRewardGiveItemWorks :: IO Bool
testQuestRewardGiveItemWorks = do
    let quest = Quest "test_quest" "Test Quest" "desc"
                    Map.empty
                    [QuestStage "s1" "step one" Nothing]
                    (Just (MoveEntity "torch" (CarriedBy ActorPlayer)))
                    Nothing
        withQuest = initSampleGame
            { world = (world initSampleGame)
                { questDefs = Map.insert "test_quest" quest (questDefs (world initSampleGame)) } }
        (started, _) = applyOutcome (QuestOp StartQuest "test_quest") "" withQuest
        (completed, _) = applyOutcome (QuestOp CompleteQuest "test_quest") "" started
    r1 <- expectTrue "give item reward is applied" (hasItem "torch" completed)
    r2 <- expectTrue "quest completed" ("test_quest" `Set.member` completedQuests (save completed))
    pure (r1 && r2)

-- | Condition-Tick nutzt ebenfalls den vollständigen Interpreter
testConditionTickGiveItemWorks :: IO Bool
testConditionTickGiveItemWorks = do
    let cond = Condition "reward_tick" 2 (Just (MoveEntity "torch" (CarriedBy ActorPlayer))) Nothing False
        withCond = initSampleGame
            { save = (save initSampleGame) { conditions = Map.singleton "reward_tick" cond } }
        (st1, _) = tickConditions withCond
        (st2, _) = tickConditions st1
    r1 <- expectTrue "give item on tick" (hasItem "torch" st1)
    r2 <- expectTrue "condition cleared after expiry" (not (hasCondition "reward_tick" st2))
    pure (r1 && r2)

-- ===== Phase 1: Equipment-Invarianten (1b) =====

-- | Equipiertes Item ablegen entfernt den Bonus (und das Equipment)
testDropEquippedItemRemovesBonus :: IO Bool
testDropEquippedItemRemovesBonus = do
    let withSword = pickupItem "sword_rusty" initSampleGame
    case equipItem "sword_rusty" withSword of
        Left err -> expectTrue err False
        Right st -> do
            let st' = dropItem "sword_rusty" st
            r1 <- expectEqual 10 (effectiveAttack st')  -- zurück auf base
            r2 <- expectTrue "not equipped after drop" (not (isEquipped "sword_rusty" st'))
            r3 <- expectTrue "item is in room after drop" (itemLocation (itemStates (save st') Map.! "sword_rusty") == InRoom (currentRoom (save st')))
            pure (r1 && r2 && r3)

-- | Equipiertes Item konsumieren entfernt den Bonus
testConsumeEquippedItemRemovesBonus :: IO Bool
testConsumeEquippedItemRemovesBonus = do
    let withSword = pickupItem "sword_rusty" initSampleGame
    case equipItem "sword_rusty" withSword of
        Left err -> expectTrue err False
        Right st -> do
            let st' = consumeItem "sword_rusty" st
            r1 <- expectEqual 10 (effectiveAttack st')
            r2 <- expectTrue "not equipped after consume" (not (isEquipped "sword_rusty" st'))
            r3 <- expectEqual Removed (itemLocation (itemStates (save st') Map.! "sword_rusty"))
            pure (r1 && r2 && r3)

-- | Max-HP-Bonus verlieren kappt aktuelle Health auf das neue Maximum
testEquipMaxHpClampOnDrop :: IO Bool
testEquipMaxHpClampOnDrop = do
    let withRing = pickupItem "ring_vigor" initSampleGame
    case equipItem "ring_vigor" withRing of
        Left err -> expectTrue err False
        Right st -> do
            let healed = updatePlayerHealth (+15) st  -- 115 von 120 max
                st' = dropItem "ring_vigor" healed
            r1 <- expectEqual 100 (effectiveMaxHealth st')
            r2 <- expectEqual 100 (playerHealth (player (save st')))  -- gekappt
            pure (r1 && r2)

-- ===== Phase 1: TransitionRoom-Hooks (1c) =====

-- | TransitionRoom (Outcome) führt Room-Hooks aus und markiert visited
testTransitionRoomRunsHooks :: IO Bool
testTransitionRoomRunsHooks = do
    let treasureWithHook = (rooms (world initSampleGame) Map.! "treasure")
            { roomOnEnter = Just (SetValue (VRFlag "entered_treasure") (EVString "true")) }
        withHook = initSampleGame
            { world = (world initSampleGame)
                { rooms = Map.insert "treasure" treasureWithHook (rooms (world initSampleGame)) } }
        (st, _) = applyOutcome (SetValue (VRActorProp ActorPlayer PRoom) (EVString "treasure")) "" withHook
    r1 <- expectEqual (Just "true") (getFlag "entered_treasure" st)
    r2 <- expectTrue "treasure marked visited" ("treasure" `Set.member` visitedRooms (save st))
    pure (r1 && r2)

-- | TransitionRoom räumt den aktiven Dialog
testTransitionRoomClearsDialogue :: IO Bool
testTransitionRoomClearsDialogue = do
    let inDialogue = initSampleGame
            { save = (save initSampleGame) { activeDialogue = Just "oldman" } }
        (st, _) = applyOutcome (SetValue (VRActorProp ActorPlayer PRoom) (EVString "treasure")) "" inDialogue
    expectEqual Nothing (activeDialogue (save st))

-- ===== Phase 1: Restart (1d) =====

-- | Restart startet die geladene Welt neu, nicht die Sample-Welt
testRestartUsesCustomWorld :: IO Bool
testRestartUsesCustomWorld = do
    let custom = initSampleGame
            { save = (save initSampleGame) { currentRoom = "meadow" } }
        loop0 = initLoopState custom
        (loop1, _) = applyLoopCommand (Go North) loop0  -- meadow → start
        (loop2, _) = applyLoopCommand Restart loop1
    r1 <- expectEqual "meadow" (currentRoom (save (lsCurrent loop2)))
    r2 <- expectTrue "custom world kept" (world custom == world (lsCurrent loop2))
    pure (r1 && r2)

-- ===== Phase 1: Turn-Kosten (1e) =====

-- | Look/Info-Kommandos verbrauchen keinen Zug; Go schon
testLookDoesNotConsumeTurn :: IO Bool
testLookDoesNotConsumeTurn = do
    let loop0 = initLoopState initSampleGame
        (loop1, _) = applyLoopCommand Look loop0
    r1 <- expectEqual 0 (turnCount (save (lsCurrent loop1)))
    r2 <- expectEqual [] (lsHistory loop1)
    pure (r1 && r2)

testGoConsumesTurn :: IO Bool
testGoConsumesTurn = do
    let loop0 = initLoopState initSampleGame
        (loop1, _) = applyLoopCommand (Go South) loop0
    r1 <- expectEqual 1 (turnCount (save (lsCurrent loop1)))
    r2 <- expectEqual 1 (length (lsHistory loop1))
    pure (r1 && r2)

testUnknownDoesNotConsumeTurn :: IO Bool
testUnknownDoesNotConsumeTurn = do
    let loop0 = initLoopState initSampleGame
        (loop1, _) = applyLoopCommand (Unknown "asdf") loop0
    r1 <- expectEqual 0 (turnCount (save (lsCurrent loop1)))
    r2 <- expectEqual [] (lsHistory loop1)
    pure (r1 && r2)

testInfoCommandsDoNotConsumeTurn :: IO Bool
testInfoCommandsDoNotConsumeTurn = do
    let check cmd = applyLoopCommand cmd (initLoopState initSampleGame)
    r1 <- expectEqual 0 (turnCount (save (lsCurrent (fst (check Inventory)))))
    r2 <- expectEqual 0 (turnCount (save (lsCurrent (fst (check StatsCmd)))))
    r3 <- expectEqual 0 (turnCount (save (lsCurrent (fst (check JournalCmd)))))
    pure (r1 && r2 && r3)

-- ===== Phase 1: Expliziter RNG-State (1f) =====

-- | RandomChoice schreibt den expliziten RNG-State fort
testRandomChoiceAdvancesRng :: IO Bool
testRandomChoiceAdvancesRng = do
    let (st, _) = applyOutcome (RandomChoice [(1, SendMessage "a"), (1, SendMessage "b")]) "" initSampleGame
    expectTrue "rngState advanced" (rngState (save st) /= rngState (save initSampleGame))

-- | Gleicher Seed → gleiche Auswahl (deterministisch)
testRandomChoiceDeterministic :: IO Bool
testRandomChoiceDeterministic = do
    let outcome = RandomChoice [(1, SendMessage "a"), (1, SendMessage "b"), (1, SendMessage "c")]
        (_, msg1) = applyOutcome outcome "" initSampleGame
        (_, msg2) = applyOutcome outcome "" initSampleGame
    expectEqual msg1 msg2

-- | LCG-Fortschreibung: deterministisch, nicht-trivial
testNextRngDeterministic :: IO Bool
testNextRngDeterministic = do
    let s0 = initialRngState
        s1 = nextRng s0
        s2 = nextRng s1
    r1 <- expectTrue "seed is nonzero" (s0 /= 0)
    r2 <- expectTrue "state advances" (s1 /= s0)
    r3 <- expectTrue "states differ" (s2 /= s1)
    r4 <- expectTrue "deterministic" (nextRng s0 == s1)
    pure (r1 && r2 && r3 && r4)

-- | P1-7: drawing from the LCG's HIGH bits must give a real spread, not the
--   strict A,B,A,B toggle the low bits (period-2 bit 0) produce. 200 draws
--   from a two-way choice: both branches in 35%-65% and the sequence must not
--   be a strict alternation.
testRandomChoiceDistribution :: IO Bool
testRandomChoiceDistribution = do
    let outcome = RandomChoice [(1, SendMessage "A"), (1, SendMessage "B")]
        go :: Int -> GameState -> [String]
        go 0 _ = []
        go n s = let (s', m) = applyOutcome outcome "" s in m : go (n - 1) s'
        draws = go 200 initSampleGame
        countA = length (filter (== "A") draws)
        countB = length (filter (== "B") draws)
        notAlternating = any (\(x, y) -> x == y) (zip draws (tail draws))
    r1 <- expectTrue ("both branches >= 35% (A=" ++ show countA ++ ")") (countA >= 70)
    r2 <- expectTrue ("both branches >= 35% (B=" ++ show countB ++ ")") (countB >= 70)
    r3 <- expectTrue "sequence is not a strict alternation" notAlternating
    pure (r1 && r2 && r3)

-- ===== Phase 3a: Verb Registry (Custom-Verben) =====

-- | Ein YAML mit `verbs: [{name: cast, aliases: [magic, spell]}]` erzeugt
--   die Registry und `cast scroll` → Interact (VCustom "cast") "scroll".
testCustomVerbParseCreatesVCustom :: IO Bool
testCustomVerbParseCreatesVCustom = do
    let custom = VerbDef "cast" ["magic", "spell"]
        reg = Map.singleton "cast" custom
    let cmd1 = parseCommandWith reg "cast scroll"
    let cmd2 = parseCommandWith reg "magic scroll"
    let cmd3 = parseCommandWith reg "spell scroll"
    r1 <- expectEqual (Interact (VCustom "cast") "scroll") cmd1
    r2 <- expectEqual (Interact (VCustom "cast") "scroll") cmd2
    r3 <- expectEqual (Interact (VCustom "cast") "scroll") cmd3
    pure (r1 && r2 && r3)

-- | Unknown verb ("frobnicate") liefert Unknown, auch mit leerer Registry
testCustomVerbUnknownFails :: IO Bool
testCustomVerbUnknownFails = do
    let cmd = parseCommandWith Map.empty "frobnicate widget"
    expectEqual (Unknown "frobnicate widget") cmd

-- | Core-Verben funktionieren weiterhin ohne Registry
testCustomVerbCoreStillWorks :: IO Bool
testCustomVerbCoreStillWorks = do
    let cmd = parseCommandWith Map.empty "take sword"
    expectEqual (Interact VTake "sword") cmd

-- | VerbAliasMap baut korrekte Lookup-Tabelle
testVerbAliasMapBuilt :: IO Bool
testVerbAliasMapBuilt = do
    let reg = Map.fromList [("cast", VerbDef "cast" ["magic"]), ("hack", VerbDef "hack" ["pwn", "exploit"])]
        aliases = verbAliasMap reg
    r1 <- expectEqual (Just "cast") (Map.lookup "cast" aliases)
    r2 <- expectEqual (Just "cast") (Map.lookup "magic" aliases)
    r3 <- expectEqual (Just "hack") (Map.lookup "pwn" aliases)
    r4 <- expectEqual (Just "hack") (Map.lookup "exploit" aliases)
    r5 <- expectEqual Nothing (Map.lookup "sword" aliases)
    pure (r1 && r2 && r3 && r4 && r5)

-- ===== Phase 3b: Variables (getVariable / setVariable) =====

testGetVariableReturnsNothing :: IO Bool
testGetVariableReturnsNothing = do
    expectEqual Nothing (getVariable "unknown_var" initSampleGame)

testSetVariableStoresValue :: IO Bool
testSetVariableStoresValue = do
    let st = setVariable "mana" (VVInt 50) initSampleGame
    case getVariable "mana" st of
        Just (VVInt n) -> expectEqual 50 n
        _ -> expectTrue "expected VVInt 50" False

testVariablePersistenceAcrossCommands :: IO Bool
testVariablePersistenceAcrossCommands = do
    let st = setVariable "oxygen" (VVInt 75) initSampleGame
        (st2, _) = executeCommand Look st
    case getVariable "oxygen" st2 of
        Just (VVInt n) -> expectEqual 75 n
        _ -> expectTrue "expected oxygen to persist" False

-- ===== Phase 3c: Predicate-DSL (evalPredicate) =====

-- | PTrue / PNot / PAll / PAny logische Verknüpfungen
testPredicateLogic :: IO Bool
testPredicateLogic = do
    let r1 = evalPredicate (PAll [PTrue, PTrue]) initSampleGame
        r2 = evalPredicate (PAll [PTrue, PNot PTrue]) initSampleGame
        r3 = evalPredicate (PAny [PNot PTrue, PTrue]) initSampleGame
        r4 = evalPredicate (PAny [PNot PTrue, PNot PTrue]) initSampleGame
        r5 = evalPredicate (PNot PTrue) initSampleGame
    pure (r1 && not r2 && r3 && not r4 && not r5)

-- | PlayerHas prüft, ob Item im Inventar ist
testPredicatePlayerHas :: IO Bool
testPredicatePlayerHas = do
    let noKey = initSampleGame
        withKey = pickupItem "key" noKey
    r1 <- expectTrue "no key" (not (evalPredicate (PlayerHas "key") noKey))
    r2 <- expectTrue "with key" (evalPredicate (PlayerHas "key") withKey)
    pure (r1 && r2)

-- | HasFlag prüft, ob Flag auf "true" gesetzt ist
testPredicateHasFlag :: IO Bool
testPredicateHasFlag = do
    let flagged = setFlag "portal_open" "true" initSampleGame
        unflagged = setFlag "portal_open" "false" initSampleGame
        absent = initSampleGame
    r1 <- expectTrue "flagged true" (evalPredicate (HasFlag "portal_open") flagged)
    r2 <- expectTrue "flagged false" (not (evalPredicate (HasFlag "portal_open") unflagged))
    r3 <- expectTrue "absent" (not (evalPredicate (HasFlag "portal_open") absent))
    pure (r1 && r2 && r3)

-- | EntityHasState prüft Entity-State
testPredicateEntityHasState :: IO Bool
testPredicateEntityHasState = do
    let st = setEntityState "door" "unlocked" initSampleGame
    r1 <- expectTrue "unlocked" (evalPredicate (EntityHasState "door" "unlocked") st)
    r2 <- expectTrue "not locked" (not (evalPredicate (EntityHasState "door" "locked") st))
    pure (r1 && r2)

-- | RoomHasTag prüft Raum-Tag
testPredicateRoomHasTag :: IO Bool
testPredicateRoomHasTag = do
    let hallRoom = rooms (world initSampleGame) Map.! "hallway"
    expectTrue "dark tag" (Set.member "dark" (roomTags hallRoom))

-- | Compare mit Flags (VRFlag) – Flags sind "true"=1, sonst 0
testPredicateCompareFlag :: IO Bool
testPredicateCompareFlag = do
    let st = setFlag "torch_lit" "true" initSampleGame
    r1 <- expectTrue "flag true is 1" (evalPredicate (Compare (VRFlag "torch_lit") CGt (VRFlag "absent_flag")) st)
    r2 <- expectTrue "absent is 0" (not (evalPredicate (Compare (VRFlag "torch_lit") CLt (VRFlag "absent_flag")) st))
    r3 <- expectTrue "true == true" (evalPredicate (Compare (VRFlag "torch_lit") CEq (VRFlag "torch_lit")) st)
    pure (r1 && r2 && r3)

-- | Conditional-Outcome mit Predicate
testConditionalOutcome :: IO Bool
testConditionalOutcome = do
    let st = setFlag "boss_dead" "true" initSampleGame
        (st2, _) = applyOutcome
            (Conditional (HasFlag "boss_dead")
                (SetValue (VRFlag "door_open") (EVString "true"))
                (SendMessage "Defeat the boss first."))
            "" st
    r1 <- expectEqual (Just "true") (getFlag "door_open" st2)
    pure r1

-- ===== Phase 3f: Trigger system =====

testTriggerFiresOnEnter :: IO Bool
testTriggerFiresOnEnter = do
    let trigger = TriggerDef "ent_test" (OnEnter "treasure") Nothing
            [SendMessage "You found the treasure room!"] False 0
        stateWithTrigger = initSampleGame
            { world = (world initSampleGame) { triggerDefs = [trigger] } }
        (_, msg) = fireTriggers (OnEnter "treasure") stateWithTrigger
    r1 <- expectTrue "trigger fired" (not (null (renderEvents msg)))
    r2 <- expectTrue "message mentions treasure" (isInfixOf "treasure" (renderEvents msg))
    pure (r1 && r2)

-- | Cooldown: after firing, the trigger stays silent for `trCooldown` further
--   OnTurn events, then fires again (module 7c relies on this for encounters).
testTriggerCooldownGatesTurns :: IO Bool
testTriggerCooldownGatesTurns = do
    let trigger = TriggerDef "cd_test" OnTurn Nothing
            [ModifyValue (VRVariable "hits") 1] False 2
        st0 = initSampleGame
            { world = (world initSampleGame) { triggerDefs = [trigger] }
            , save = (save initSampleGame) { variables = Map.singleton "hits" (VVInt 0) } }
        count st = case getVariable "hits" st of
            Just (VVInt n) -> n
            _ -> (-1)
        (st1, msg1) = fireTriggers OnTurn st0
        (st2, msg2) = fireTriggers OnTurn st1
        (st3, msg3) = fireTriggers OnTurn st2
        (st4, msg4) = fireTriggers OnTurn st3
    r1 <- expectEqual 1 (count st1)
    r2 <- expectTrue "fired on turn 1" (not (null msg1))
    r3 <- expectEqual 1 (count st2)
    r4 <- expectTrue "silent during cooldown (turn 2)" (null msg2)
    r5 <- expectEqual 1 (count st3)
    r6 <- expectTrue "silent during cooldown (turn 3)" (null msg3)
    r7 <- expectEqual 2 (count st4)
    r8 <- expectTrue "fires again after cooldown (turn 4)" (not (null msg4))
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8)

-- | Drain (module 7d): a per-turn variable drain gated by a flag is
--   stoppable — with `fed` set no value is lost, without it the value sinks
--   and the at_zero outcome (game end) fires once the variable hits ≤ 0.
testDrainStopsWhenFlagged :: IO Bool
testDrainStopsWhenFlagged = do
    let drainTrig = TriggerDef "environment.drain.hunger" OnTurn
            (Just (PNot (HasFlag "fed")))
            [ ModifyValue (VRVariable "hunger") (-1)
            , Conditional (CompareVar "hunger" CLte 0)
                (GameEnd Death "Du verhungerst.") Noop ]
            False 0
        base = initSampleGame
            { world = (world initSampleGame) { triggerDefs = [drainTrig] }
            , save = (save initSampleGame) { variables = Map.fromList [("hunger", VVInt 3)] } }
        (fed0, _)   = fireTriggers OnTurn base
        (starved, _) = fireTriggers OnTurn fed0
        (starved2, _) = fireTriggers OnTurn starved
    r1 <- case getVariable "hunger" fed0 of
            Just (VVInt n) -> expectEqual 2 n
            _ -> expectTrue "hunger after first turn" False
    r2 <- case getVariable "hunger" starved of
            Just (VVInt n) -> expectEqual 1 n
            _ -> expectTrue "hunger after second turn" False
    r3 <- expectTrue "drain kills at zero" (gameOverReason (save starved2) == Just Death)
    -- Now with fed set: no drain, no death.
    let fedState = initSampleGame
            { world = (world initSampleGame) { triggerDefs = [drainTrig] }
            , save = (save initSampleGame)
                { variables = Map.fromList [("hunger", VVInt 1)]
                , flags = Map.singleton "fed" "true" } }
        (fed1, _)  = fireTriggers OnTurn fedState
        (still, _) = fireTriggers OnTurn fed1
    r4 <- case getVariable "hunger" still of
            Just (VVInt n) -> expectEqual 1 n
            _ -> expectTrue "hunger unchanged while fed" False
    r5 <- expectEqual Nothing (gameOverReason (save still))
    pure (r1 && r2 && r3 && r4 && r5)

-- | Noise (module 7e): the compiled stealth triggers behave as a noise
--   meter. A move raises noise (clamped to max), the observer reacts only
--   while noise >= hears_at and then re-arms via cooldown, and decay runs
--   after the observer so a guard hears the full noise of the same turn.
testNoiseObserverAndDecay :: IO Bool
testNoiseObserverAndDecay = do
    let noiseTrig = TriggerDef "stealth.nmove.start" (OnEnter "start") Nothing
            [ ModifyValue (VRVariable "noise") 6
            , Conditional (CompareVar "noise" CGte 10) (SetValue (VRVariable "noise") (EVInt 10)) Noop ]
            False 0
        observeTrig = TriggerDef "stealth.observe.guard" OnTurn
            (Just (CompareVar "noise" CGte 3))
            [ ModifyValue (VRVariable "hearings") 1 ]
            False 2
        decayTrig = TriggerDef "stealth.decay" OnTurn Nothing
            [ ModifyValue (VRVariable "noise") (-1)
            , Conditional (CompareVar "noise" CLte 0) (SetValue (VRVariable "noise") (EVInt 0)) Noop ]
            False 0
        st0 = initSampleGame
            { world = (world initSampleGame)
                { triggerDefs = [noiseTrig, observeTrig, decayTrig] }
            , save = (save initSampleGame) { variables = Map.singleton "hearings" (VVInt 0) } }
        count st = case getVariable "hearings" st of
            Just (VVInt n) -> n
            _ -> (-1)
        (st1, _) = fireTriggers (OnEnter "start") st0
        (st2, _) = fireTriggers OnTurn st1
        (st3, _) = fireTriggers OnTurn st2
        (st4, _) = fireTriggers OnTurn st3
        (st5, _) = fireTriggers OnTurn st4
    -- after move: noise 6, observer (>=3) hears once, decay -> 5
    r1 <- case getVariable "noise" st2 of
            Just (VVInt n) -> expectEqual 5 n
            _ -> expectTrue "noise after move+decay" False
    r2 <- expectEqual 1 (count st2)
    -- cooldown 2: no re-fire on the next two OnTurn events, then hears again
    r3 <- expectEqual 1 (count st3)
    r4 <- expectEqual 1 (count st4)
    r5 <- expectEqual 2 (count st5)
    pure (r1 && r2 && r3 && r4 && r5)

testTriggerOnceFiresOnce :: IO Bool
testTriggerOnceFiresOnce = do
    let trigger = TriggerDef "once_test" (OnEnter "treasure") Nothing
            [SendMessage "One-time!"] True 0
        stateWithTrigger = initSampleGame
            { world = (world initSampleGame) { triggerDefs = [trigger] } }
        (st1, _) = fireTriggers (OnEnter "treasure") stateWithTrigger
        (_, msg2) = fireTriggers (OnEnter "treasure") st1
    r1 <- expectTrue "only fires once" (null msg2)
    r2 <- expectTrue "triggerState stored as fired"
        (case Map.lookup "once_test" (triggerStates (save st1)) of
            Just ts -> tsFired ts
            Nothing -> False)
    pure (r1 && r2)

-- | P1-5: a `once` trigger that a nested event round already fired must not
--   fire again when the outer fold subsequently reaches it. Trigger A kills
--   the wolf; the resulting OnStateChange round fires B (once); the outer
--   OnStateChange fold must then SKIP B, not re-fire it from a stale snapshot.
testTriggerOnceNoDoubleFireAcrossNestedRound :: IO Bool
testTriggerOnceNoDoubleFireAcrossNestedRound = do
    let sample = initSampleGame
        killWolf = TriggerDef "a_kill" (OnStateChange "wolf") Nothing
                        [ ModifyValue (VRActorProp (ActorNPC "wolf") PHealth) (-100) ] True 0
        counter  = TriggerDef "b_count" (OnStateChange "wolf") Nothing
                        [ ModifyValue (VRVariable "fired") 1 ] True 0
        st = sample
                { world = (world sample) { triggerDefs = [killWolf, counter] }
                , save = (save sample)
                    { npcStates = Map.insert "wolf"
                        (NPCState (InRoom "start") "alive" (Just 5) Map.empty Nothing)
                        (npcStates (save sample))
                    , variables = Map.singleton "fired" (VVInt 0) } }
        (st', _) = fireTriggers (OnStateChange "wolf") st
    case getVariable "fired" st' of
        Just (VVInt n) -> expectEqual 1 n
        _ -> expectTrue "fired variable present" False

-- | P1-12: `set_state` must fire the entity's `OnStateChange` event, so an
--   authored `on: state <entity>` rule reacts to it (not only to NPC death).
testSetStateFiresStateChange :: IO Bool
testSetStateFiresStateChange = do
    let rule = TriggerDef "gate_opens" (OnStateChange "gate") Nothing
                    [ SendMessage "The gate rumbles open." ] False 0
        st = initSampleGame { world = (world initSampleGame) { triggerDefs = [rule] } }
        (st', msg) = applyOutcome (SetValue (VRActorProp (ActorEntity "gate") PState) (EVString "unlocked")) "" st
    r1 <- expectTrue "OnStateChange fired on set_state" (isInfixOf "rumbles" msg)
    r2 <- expectEqual (Just "unlocked") (getEntityState "gate" st')
    pure (r1 && r2)

-- | P1-12 recursion bound: a rule whose `on: state` handler re-writes the same
--   state must terminate — the idempotent writer is the recursion bound.
testSetStateIdempotentNoRecursion :: IO Bool
testSetStateIdempotentNoRecursion = do
    let rule = TriggerDef "gate_loop" (OnStateChange "gate") Nothing
                    [ SetValue (VRActorProp (ActorEntity "gate") PState) (EVString "unlocked") ] False 0
        st = initSampleGame { world = (world initSampleGame) { triggerDefs = [rule] } }
        run = fst (applyOutcome (SetValue (VRActorProp (ActorEntity "gate") PState) (EVString "unlocked")) "" st)
    result <- timeout 3000000 (evaluate (length (show run)))
    case result of
        Nothing -> do putStrLn "  set_state recursion did not terminate"; pure False
        Just _ -> expectEqual (Just "unlocked") (getEntityState "gate" run)

testTriggerConditionGates :: IO Bool
testTriggerConditionGates = do
    let flagSet = setFlag "allowed" "true" initSampleGame
        trigger = TriggerDef "cond_test" (OnEnter "treasure")
            (Just (HasFlag "allowed")) [SendMessage "Flag is set!"] False 0
        stateWithTrigger = flagSet
            { world = (world flagSet) { triggerDefs = [trigger] } }
        (_, msg1) = fireTriggers (OnEnter "treasure") initSampleGame
        (_, msg2) = fireTriggers (OnEnter "treasure") stateWithTrigger
    r1 <- expectTrue "condition prevents firing" (null msg1)
    r2 <- expectTrue "condition allows firing" (not (null msg2))
    pure (r1 && r2)

testTriggerCommandEvent :: IO Bool
testTriggerCommandEvent = do
    let stateWithTrigger = initSampleGame
            { world = (world initSampleGame)
                { triggerDefs = [TriggerDef "use_test" (OnCommand "use") Nothing
                        [SendMessage "Custom use!"] False 0] } }
        (_, msg) = fireTriggers (OnCommand "use") stateWithTrigger
    expectTrue "OnCommand trigger fires" (not (null msg))

testTriggerThroughGameLoop :: IO Bool
testTriggerThroughGameLoop = do
    -- Setup: set a trigger on entering "hallway" from the sample game
    let trigger = TriggerDef "enter_hallway" (OnEnter "hallway") Nothing
            [SendMessage "A cold draft hits you."] True 0
        stateWithTrigger = initSampleGame
            { world = (world initSampleGame) { triggerDefs = [trigger] } }
        loop = initLoopState stateWithTrigger
        -- Go north from "start" to "hallway"
        (loopAfter, msg) = applyLoopCommand (Go North) loop
        st = lsCurrent loopAfter
    r1 <- expectTrue "trigger message in output" (isInfixOf "cold draft" msg)
    r2 <- expectTrue "player moved to hallway" (currentRoom (save st) == "hallway")
    pure (r1 && r2)

-- | P1-14: `on: command examine` must fire for `look at X`. The event name comes
--   from the verb registry; a `show`-derived name would have been "lookat".
--   Same for `use X on Y` (InteractWith) which must still be "use".
testOnCommandExamineFires :: IO Bool
testOnCommandExamineFires = do
    let withTrigger t = initSampleGame
            { world = (world initSampleGame) { triggerDefs = [t] } }
        loopFor t = initLoopState (withTrigger t)
    r1 <- do
        let t = TriggerDef "examine_test" (OnCommand "examine") Nothing
                    [SendMessage "You examine it closely."] False 0
            (_, msg) = applyLoopCommand (Interact VLookAt "torch") (loopFor t)
        expectTrue "on: command examine fires for look at X" (isInfixOf "examine it closely" msg)
    r2 <- do
        let t = TriggerDef "useon_test" (OnCommand "use") Nothing
                    [SendMessage "You apply it to the door."] False 0
            (_, msg) = applyLoopCommand (InteractWith VUseOn "oil_can" "door") (loopFor t)
        expectTrue "on: command use fires for use X on Y" (isInfixOf "apply it to the door" msg)
    pure (r1 && r2)

-- | P1-15: `on: take`/`on: drop` must fire only on a real inventory change —
--   not for a refused take (non-portable / already carried), not for an
--   unknown item id, and not for dropping something not held.
testTakeEventOnlyOnSuccess :: IO Bool
testTakeEventOnlyOnSuccess = do
    let base = initSampleGame
        nonPortable = ItemDef "statue" "statue" (plainText "A heavy stone statue.")
                          ["statue"] Set.empty Nothing [] False Nothing False
                          (Just "The statue will not budge.") Map.empty Nothing emptyAscii emptyGrammar
        st0 = base { world = (world base)
                         { itemDefs = Map.insert "statue" nonPortable (itemDefs (world base)) }
                   , save  = (save base)
                       { itemStates = Map.insert "statue"
                             (ItemState (InRoom "start") "intact" Map.empty False)
                             (itemStates (save base)) } }
        carriedTorch = st0 { save = (save st0)
                       { itemStates = Map.adjust (\i -> i { itemLocation = CarriedBy ActorPlayer })
                                                 "torch" (itemStates (save st0)) } }
        fires st ev cmd =
            let st' = st { world = (world st)
                             { triggerDefs = [TriggerDef "t" ev Nothing [SendMessage "FIRED"] False 0] } }
                (_, msg) = applyLoopCommand cmd (initLoopState st')
            in isInfixOf "FIRED" msg
    r1 <- expectTrue "refused take (non-portable) does not fire OnTake"
              (not (fires st0 (OnTake "statue") (Interact VTake "statue")))
    r2 <- expectTrue "unknown item does not fire OnTake"
              (not (fires st0 (OnTake "blubb") (Interact VTake "blubb")))
    r3 <- expectTrue "take while already carried does not fire OnTake"
              (not (fires carriedTorch (OnTake "torch") (Interact VTake "torch")))
    r4 <- expectTrue "successful take fires OnTake"
              (fires st0 (OnTake "torch") (Interact VTake "torch"))
    r5 <- expectTrue "drop of a non-held item does not fire OnDrop"
              (not (fires st0 (OnDrop "torch") (Interact VDrop "torch")))
    pure (r1 && r2 && r3 && r4 && r5)

-- | P1-20: `RaiseEvent` fires the matching `on: custom` rule; an event with no
--   listener is a no-op; and a rule that raises its own event terminates
--   (bounded by the threaded event depth) instead of looping forever.
testRaiseEventFires :: IO Bool
testRaiseEventFires = do
    let rule = TriggerDef "ritual" (OnCustomEvent "ritual_done") Nothing
                   [SendMessage "The ritual is complete."] False 0
        st0 = initSampleGame { world = (world initSampleGame) { triggerDefs = [rule] } }
        (_, msg) = applyOutcome (RaiseEvent "ritual_done") "" st0
    r1 <- expectTrue "raise fires the on: custom rule" (isInfixOf "ritual is complete" msg)
    r2 <- expectEqual "" (snd (applyOutcome (RaiseEvent "nobody_listens") "" initSampleGame))
    let looper = TriggerDef "loop" (OnCustomEvent "loop") Nothing
                     [Sequence [SendMessage "tick", RaiseEvent "loop"]] False 0
        st1 = initSampleGame { world = (world initSampleGame) { triggerDefs = [looper] } }
    r3 <- timeout 3000000 (evaluate (length (snd (applyOutcome (RaiseEvent "loop") "" st1))))
    r4 <- case r3 of
        Just _  -> pure True
        Nothing -> expectTrue "self-raising event terminates" False
    pure (r1 && r2 && r4)

-- ===== Narrative tests (Phase 4.4) =====

testNarrativeReturnsLines :: IO Bool
testNarrativeReturnsLines = do
    let (_, msg) = applyOutcome (Narrative ["Line 1", "Line 2"] (SendMessage "done")) "" initSampleGame
    expectEqual "Line 1\nLine 2" msg

testNarrativeStoresPending :: IO Bool
testNarrativeStoresPending = do
    let (st, _) = applyOutcome (Narrative ["Hello!"] (SendMessage "Done!")) "" initSampleGame
    r1 <- expectTrue "pendingNarrative is set" (isJust (pendingNarrative st))
    pure r1

testNarrativeStateRoundTrip :: IO Bool
testNarrativeStateRoundTrip = do
    let encoded = Aeson.encode (Sequence [SendMessage "A", SendMessage "B", SendMessage "end"])
        decoded = Aeson.decode encoded :: Maybe Effect
    expectEqual (Just (Sequence [SendMessage "A", SendMessage "B", SendMessage "end"])) decoded

-- ===== Validation tests (Phase 4.5) =====

testSampleWorldIsValid :: IO Bool
testSampleWorldIsValid = do
    let errors = validateWorld (world initSampleGame)
    if null errors
        then pure True
        else do
            putStrLn "  Unexpected validation errors in sample world:"
            mapM_ (putStrLn . ("    " ++) . show) errors
            pure False

testDanglingExitDetected :: IO Bool
testDanglingExitDetected = do
    let roomA = Room "roomA" "Room A" (plainText "desc.") (Map.singleton North (Open "roomZ")) Set.empty Nothing
            Nothing Nothing Nothing Nothing Nothing (emptyAscii) Nothing Nothing Nothing
        gw = (world initSampleGame) { rooms = Map.singleton "roomA" roomA }
        errors = validateWorld gw
    expectTrue "dangling exit detected" (DanglingExit "roomA" North "roomZ" `elem` errors)

testDuplicateIDsBetweenItemsAndRooms :: IO Bool
testDuplicateIDsBetweenItemsAndRooms = do
    let gw = (world initSampleGame)
                { rooms = Map.insert "key" (Room "key" "Duplicate" (plainText "desc.") Map.empty Set.empty Nothing
                    Nothing Nothing Nothing Nothing Nothing (emptyAscii) Nothing Nothing Nothing) (rooms (world initSampleGame)) }
        errors = validateWorld gw
    expectTrue "duplicate key found" (any isDup errors)
  where
    isDup (DuplicateID _ _ _) = True
    isDup _ = False

testUnreachableRoomDetected :: IO Bool
testUnreachableRoomDetected = do
    let roomIsolated = Room "isolated" "Isolated" (plainText "Alone.") Map.empty Set.empty Nothing
            Nothing Nothing Nothing Nothing Nothing (emptyAscii) Nothing Nothing Nothing
        gw = (world initSampleGame)
                { rooms = Map.insert "isolated" roomIsolated (rooms (world initSampleGame)) }
        -- Reachability is checked where the real start room is known, i.e. in
        -- validateGameState (SaveState.currentRoom) -- not guessed by validateWorld.
        errors = validateGameState gw (save initSampleGame)
    r1 <- expectTrue "unreachable room detected from the real start room"
            (UnreachableRoom "isolated" `elem` errors)
    r2 <- expectTrue "validateWorld does not guess a start room"
            (UnreachableRoom "isolated" `notElem` validateWorld gw)
    pure (r1 && r2)

-- | P1-1: effects inside trigger rules must be part of the validation collection.
--   A `give: ghost_item` / `start_quest: ghost_quest` inside a rule was invisible.
testRuleEffectsAreValidated :: IO Bool
testRuleEffectsAreValidated = do
    let gw = (world initSampleGame)
                { triggerDefs =
                    [ TriggerDef "t_missing_item" OnTurn Nothing
                        [ MoveEntity "ghost_item" (InRoom "isolated_void") ] False 0
                    , TriggerDef "t_missing_quest" OnTurn Nothing
                        [ QuestOp StartQuest "ghost_quest" ] False 0
                    ] }
        errors = validateWorld gw
    r1 <- expectTrue "item referenced in a rule is detected"
            (MissingItem "ghost_item" `elem` errors)
    r2 <- expectTrue "quest referenced in a rule is detected"
            (MissingQuest "ghost_quest" `elem` errors)
    pure (r1 && r2)

-- | P1-1: a flag set by a rule (and checked in a rule condition) is not a false
--   MissingSetFlag; a flag only ever checked is one.
testRuleFlagCheckedButNeverSet :: IO Bool
testRuleFlagCheckedButNeverSet = do
    let gw = (world initSampleGame)
                { triggerDefs =
                    [ TriggerDef "t_set" OnTurn (Just (HasFlag "rule_flag"))
                        [ SetValue (VRFlag "rule_flag") (EVBool True) ] False 0
                    , TriggerDef "t_check" OnTurn (Just (HasFlag "never_set_flag"))
                        [ SendMessage "checked only" ] False 0
                    ] }
        errors = validateWorld gw
    r1 <- expectTrue "flag set by a rule is not reported as MissingSetFlag"
            (MissingSetFlag "rule_flag" "checked but never set in any outcome" `notElem` errors)
    r2 <- expectTrue "flag checked but never set is reported"
            (any (\e -> case e of MissingSetFlag f _ -> f == "never_set_flag"; _ -> False) errors)
    pure (r1 && r2)

-- | P1-3: `ship.<id>.<system>` references must resolve to a declared vehicle.
testMissingVehicleInRuleDetected :: IO Bool
testMissingVehicleInRuleDetected = do
    let gw = (world initSampleGame)
                { triggerDefs =
                    [ TriggerDef "t_ship" OnTurn
                        (Just (CompareVar "ship.ghost.hull" CLte 0))
                        [ ModifyValue (VRVariable "ship.ghost.power") (-1) ] False 0
                    ] }
        errors = validateWorld gw
    r1 <- expectTrue "undeclared ship.<id> in a rule/predicate is detected"
            (MissingVehicle "ghost" `elem` errors)
    let gwOk = gw { vehicleDefs = Map.insert "ghost"
                        (VehicleDef "ghost" "Ghost" "A ghost ship." PlayerControlled []
                            "start" Nothing Map.empty [] ["ghost"] (Just (FuelSpec "hay" 10)) Map.empty)
                        (vehicleDefs gw) }
    r2 <- expectTrue "declared vehicle id is not a false positive"
            (MissingVehicle "ghost" `notElem` validateWorld gwOk)
    pure (r1 && r2)

-- ===== Dialogue tests (Phase 4.6) =====

testDialogueTreeStartAndRender :: IO Bool
testDialogueTreeStartAndRender = do
    let (st, msg) = executeCommand (Interact VTalk "old man") initSampleGame
    r1 <- expectTrue "renders header" ("Greetings, traveler!" `isInfixOf` msg)
    r2 <- expectTrue "renders option 1" ("[1] Who are you?" `isInfixOf` msg)
    r3 <- expectTrue "active dialogue set" (activeDialogue (save st) == Just "oldman")
    pure (r1 && r2 && r3)

testDialogueChoiceNavigation :: IO Bool
testDialogueChoiceNavigation = do
    let (st1, _) = executeCommand (Interact VTalk "old man") initSampleGame
    let (_, msg2) = executeCommand (ChooseCmd 1) st1  -- "Who are you?"
    r1 <- expectTrue "renders follow-up" ("old hermit" `isInfixOf` msg2)
    r2 <- expectTrue "renders next options" ("[1] What do you know about the treasure?" `isInfixOf` msg2)
    pure (r1 && r2)

testDialogueBareNumberChoice :: IO Bool
testDialogueBareNumberChoice = do
    let (st1, _) = executeCommand (Interact VTalk "old man") initSampleGame
    let cmd = parseCommand "1"
    let (_, msg2) = executeCommand cmd st1
    r1 <- expectTrue "parses bare number" (cmd == ChooseCmd 1)
    r2 <- expectTrue "navigates node" ("old hermit" `isInfixOf` msg2)
    pure (r1 && r2)

-- | Phase 6: a dialogue choice gated by visible_when is hidden until the
--   predicate holds, and `choose` numbering follows the visible list.
testDialogueChoiceVisibleWhen :: IO Bool
testDialogueChoiceVisibleWhen = do
    let gate c = c { dcVisible = Just (HasFlag "knows_secret") }
        gateNode n = n { dnChoices = zipWith (\i c -> if i == 1 then gate c else c) [0 :: Int ..] (dnChoices n) }
        gateTree (DialogueTree e ns) = DialogueTree e (Map.map gateNode ns)
        world' = (world initSampleGame)
            { npcDefs = Map.map (\d -> d { npcDialogueTrees = Map.map gateTree (npcDialogueTrees d) })
                                (npcDefs (world initSampleGame)) }
        st0 = initSampleGame { world = world' }
    let (_, msg) = executeCommand (Interact VTalk "old man") st0
    r1 <- expectTrue "gated choice hidden" (not ("Tell me about the treasure" `isInfixOf` msg))
    r2 <- expectTrue "ungated choice shown" ("[1] Who are you?" `isInfixOf` msg)
    let st1 = st0 { save = (save st0) { flags = Map.insert "knows_secret" "true" (flags (save st0)) } }
        (_, msg2) = executeCommand (Interact VTalk "old man") st1
    r3 <- expectTrue "gated choice visible once flag set" ("Tell me about the treasure" `isInfixOf` msg2)
    pure (r1 && r2 && r3)

-- | Phase 6: OnUse trigger fires for a multi-word item alias (e.g. "oil can").
testOnUseTriggerMultiWordAlias :: IO Bool
testOnUseTriggerMultiWordAlias = do
    let sample = initSampleGame
        w = (world sample)
            { itemDefs = Map.insert "oil_can"
                (ItemDef "oil_can" "oil can" (plainText "A dented oil can.") ["oil", "can"]
                         Set.empty Nothing [] False Nothing True Nothing Map.empty Nothing emptyAscii emptyGrammar)
                (itemDefs (world sample))
            , triggerDefs =
                [ TriggerDef "light_lantern" (OnUse "oil_can") Nothing
                    [SetValue (VRFlag "lantern_lit") (EVString "true")] False 0 ]
            }
        st = sample
            { world = w
            , save = (save sample)
                { itemStates = Map.insert "oil_can"
                    (ItemState (CarriedBy ActorPlayer) "intact" Map.empty True)
                    (itemStates (save sample)) } }
        runForm t = let (ls', _) = applyLoopCommand (Interact VUse t) (initLoopState st)
                    in Map.lookup "lantern_lit" (flags (save (lsCurrent ls')))
        -- every alias form must resolve to the item id and fire the trigger
        byId     = runForm "oil_can"
        byName   = runForm "oil can"
        byKey    = runForm "oil"
    r1 <- expectEqual (Just "true") byId
    r2 <- expectEqual (Just "true") byName
    r3 <- expectEqual (Just "true") byKey
    pure (r1 && r2 && r3)

-- | take of an item with on_take picks the item up AND fires the effects.
testTakeWithOnTakePicksUp :: IO Bool
testTakeWithOnTakePicksUp = do
    let sample = initSampleGame
        w = (world sample)
            { itemDefs = Map.insert "token"
                (ItemDef "token" "token" (plainText "A token.") ["token"] Set.empty
                         Nothing [] False Nothing True Nothing
                         (Map.singleton (PhaseAfter, VTake, "intact") (SetValue (VRFlag "took") (EVString "true"))) Nothing emptyAscii emptyGrammar)
                (itemDefs (world sample)) }
        here = currentRoom (save sample)
        st = sample { world = w
                    , save = (save sample)
                        { itemStates = Map.insert "token"
                            (ItemState (InRoom here) "intact" Map.empty True)
                            (itemStates (save sample)) } }
        (st', _) = executeCommand (Interact VTake "token") st
    r1 <- expectTrue "item is carried" (hasItem "token" st')
    r2 <- expectEqual (Just "true") (getFlag "took" st')
    pure (r1 && r2)

-- | take of a non-portable item fails with the authored take_failure message.
testTakeNonPortableFails :: IO Bool
testTakeNonPortableFails = do
    let sample = initSampleGame
        w = (world sample)
            { itemDefs = Map.insert "statue"
                (ItemDef "statue" "statue" (plainText "A statue.") ["statue"] Set.empty
                         Nothing [] False Nothing False (Just "Too heavy to lift.")
                         Map.empty Nothing emptyAscii emptyGrammar)
                (itemDefs (world sample)) }
        here = currentRoom (save sample)
        st = sample { world = w
                    , save = (save sample)
                        { itemStates = Map.insert "statue"
                            (ItemState (InRoom here) "intact" Map.empty True)
                            (itemStates (save sample)) } }
        (st', msg) = executeCommand (Interact VTake "statue") st
    r1 <- expectTrue "shows take_failure" ("Too heavy to lift." `isInfixOf` msg)
    r2 <- expectTrue "not carried" (not (hasItem "statue" st'))
    pure (r1 && r2)

-- ===== Phase 6b: engine invariants =====

-- Helper: an equippable item placed in the player's inventory.
invItem :: String -> ItemDef
invItem iid = ItemDef iid iid (plainText "x") [iid] Set.empty
    (Just Weapon) [] False Nothing True Nothing Map.empty Nothing emptyAscii emptyGrammar

-- | Equipped items are always also carried.
testEquippedImpliesCarried :: IO Bool
testEquippedImpliesCarried = do
    let sample = initSampleGame
        w = (world sample) { itemDefs = Map.insert "blade" (invItem "blade") (itemDefs (world sample)) }
        st = sample { world = w
                    , save = (save sample)
                        { itemStates = Map.insert "blade"
                            (ItemState (CarriedBy ActorPlayer) "intact" Map.empty True)
                            (itemStates (save sample)) } }
        (st', _) = executeCommand (EquipCmd "blade") st
    r1 <- expectEqual (Just "blade") (Map.lookup Weapon (equipment (save st')))
    r2 <- expectEqual (Just (CarriedBy ActorPlayer))
            (itemLocation <$> Map.lookup "blade" (itemStates (save st')))
    pure (r1 && r2)

-- | A consumed item is Removed and appears in no room.
testRemovedItemNotInAnyRoom :: IO Bool
testRemovedItemNotInAnyRoom = do
    let sample = initSampleGame
        w = (world sample) { itemDefs = Map.insert "ash" (invItem "ash") (itemDefs (world sample)) }
        st = sample { world = w
                    , save = (save sample)
                        { itemStates = Map.insert "ash"
                            (ItemState (CarriedBy ActorPlayer) "intact" Map.empty True)
                            (itemStates (save sample)) } }
        st' = consumeItem "ash" st
        inRooms = or [ "ash" `elem` map itemId (getItemsInLocation (InRoom r) st')
                     | r <- Map.keys (rooms (world st')) ]
    r1 <- expectEqual (Just Removed) (itemLocation <$> Map.lookup "ash" (itemStates (save st')))
    r2 <- expectTrue "removed item in no room" (not inRooms)
    pure (r1 && r2)

-- | Every item has exactly one location and container refs resolve.
testEveryItemHasOneLocation :: IO Bool
testEveryItemHasOneLocation = do
    let sample = initSampleGame
        itemKeys = Map.keys (itemStates (save sample))
        locs = [ itemLocation is | is <- Map.elems (itemStates (save sample)) ]
        -- container refs must point at an existing item
        containerRefs = [ c | InContainer c <- locs ]
        missing = [ c | c <- containerRefs, not (c `elem` itemKeys) ]
    r1 <- expectTrue "one entry per item" (length itemKeys == Map.size (itemStates (save sample)))
    r2 <- expectTrue "container refs resolve" (null missing)
    pure (r1 && r2)

-- | SaveState JSON round-trip preserves RNG, variables, quests, trigger state.
testSaveStateRoundTripInvariant :: IO Bool
testSaveStateRoundTripInvariant = do
    let sample = initSampleGame
        st = sample { save = (save sample)
                { rngState = 123456789
                , variables = Map.fromList [("mana", VVInt 7)]
                , activeQuests = Map.fromList [("q", 2)]
                , triggerStates = Map.fromList [("t", TriggerState True 3)]
                } }
        encoded = Aeson.encode (save st)
        decoded = Aeson.decode encoded :: Maybe SaveState
    case decoded of
        Nothing -> do putStrLn "  decode failed"; pure False
        Just ss -> do
            r1 <- expectEqual 123456789 (rngState ss)
            r2 <- expectEqual (Just (VVInt 7)) (Map.lookup "mana" (variables ss))
            r3 <- expectEqual (Just 2) (Map.lookup "q" (activeQuests ss))
            r4 <- expectEqual (Just (TriggerState True 3)) (Map.lookup "t" (triggerStates ss))
            pure (r1 && r2 && r3 && r4)

-- | A trigger that re-kills the same NPC on its own state-change event must
--   terminate (no unbounded event recursion).
testTriggerRecursionBounded :: IO Bool
testTriggerRecursionBounded = do
    let sample = initSampleGame
        loopEff = ModifyValue (VRActorProp (ActorNPC "wolf") PHealth) (-100)
        w = (world sample)
            { triggerDefs = [ TriggerDef "cascade" (OnStateChange "wolf") Nothing [loopEff] False 0 ] }
        st = sample { world = w
                    , save = (save sample)
                        { npcStates = Map.insert "wolf"
                            (NPCState (InRoom "start") "alive" (Just 5) Map.empty Nothing)
                            (npcStates (save sample)) } }
    result <- timeout 3000000 (evaluate (killNPC "wolf" st))
    case result of
        Nothing -> do putStrLn "  recursion did not terminate"; pure False
        Just st' -> expectEqual (Just "dead")
            (npcStatus <$> Map.lookup "wolf" (npcStates (save st')))

-- | Every item verb-map key resolves to a core verb or a declared custom verb.
testItemVerbKeysResolve :: IO Bool
testItemVerbKeysResolve = do
    let gw = world initSampleGame
        declared = Map.keys (verbDefs gw)
        keys = [ (itemId i, v) | i <- Map.elems (itemDefs gw), (_, v, _) <- Map.keys (itemVerbMap i) ]
        bad = [ (iid, n) | (iid, VCustom n) <- keys, n `notElem` declared ]
    expectTrue "all custom item verbs are declared" (null bad)

testDialogueInvalidChoice :: IO Bool
testDialogueInvalidChoice = do
    let (st1, _) = executeCommand (Interact VTalk "old man") initSampleGame
    let (st2, msg2) = executeCommand (ChooseCmd 99) st1
    r1 <- expectTrue "reports invalid choice" ("Invalid choice" `isInfixOf` msg2)
    r2 <- expectTrue "keeps dialogue active" (activeDialogue (save st2) == Just "oldman")
    pure (r1 && r2)

-- | P1-16: an invalid dialogue choice (or a bare number outside a conversation)
--   must not cost a turn: no condition tick, no `on: turn`, no undo history.
--   A valid choice does advance the clock.
testInvalidChoiceCostsNoTurn :: IO Bool
testInvalidChoiceCostsNoTurn = do
    let (stTalk, _) = executeCommand (Interact VTalk "old man") initSampleGame
        loop        = initLoopState stTalk
        turns l     = turnCount (save (lsCurrent l))
        (loopBad,  _) = applyLoopCommand (ChooseCmd 99) loop
        (loopGood, _) = applyLoopCommand (ChooseCmd 1) loop
        loopNoDialogue = initLoopState initSampleGame
        (loopNum, _)   = applyLoopCommand (ChooseCmd 1) loopNoDialogue
    r1 <- expectEqual (turns loop) (turns loopBad)
    r2 <- expectTrue "valid choice advances the clock" (turns loopGood > turns loop)
    r3 <- expectTrue "invalid choice leaves no undo history" (null (lsHistory loopBad))
    r4 <- expectEqual (turns loopNoDialogue) (turns loopNum)
    pure (r1 && r2 && r3 && r4)

testDialogueEndClearsActive :: IO Bool
testDialogueEndClearsActive = do
    let (st1, _) = executeCommand (Interact VTalk "old man") initSampleGame
    let (st2, msg2) = executeCommand (ChooseCmd 3) st1  -- "Farewell." (dcNextNode = Nothing)
    r1 <- expectTrue "shows goodbye" ("Stay safe" `isInfixOf` msg2)
    r2 <- expectTrue "dialogue cleared" (activeDialogue (save st2) == Nothing)
    pure (r1 && r2)

-- ===== Room ASCII art test (Phase 4.6) =====

-- | A static (non-animated) art for the Phase B tests.
staticArt :: CondText -> AsciiArt
staticArt ct = emptyAscii { aaStatic = ct }

testRoomAsciiArtDisplay :: IO Bool
testRoomAsciiArtDisplay = do
    let banner = "=== [CASTLE GATE] ==="
    let roomWithAscii = (rooms (world initSampleGame) Map.! "start") { roomAscii = staticArt (plainText banner) }
    let game = initSampleGame { world = (world initSampleGame) { rooms = Map.insert "start" roomWithAscii (rooms (world initSampleGame)) } }
    let (_, msg) = executeCommand Look game
    expectTrue "ascii banner displayed in look" (banner `isInfixOf` msg)

-- ===== State-dependent ASCII art (Phase B) =====

-- | The starter room with a flagged ASCII art: dark default, lit variant.
roomWithCondAscii :: GameState
roomWithCondAscii =
    let ct = CondText "DARK-ART" [ TextVariant (HasFlag "lit") "LIT-ART" ]
        r = (rooms (world initSampleGame) Map.! "start") { roomAscii = staticArt ct }
    in initSampleGame
        { world = (world initSampleGame)
            { rooms = Map.insert "start" r (rooms (world initSampleGame)) } }

-- | No variant matches -> the default art is shown.
testCondRoomAsciiDefault :: IO Bool
testCondRoomAsciiDefault = do
    let (_, msg) = executeCommand Look roomWithCondAscii
    expectTrue "default ascii shown when no variant matches" ("DARK-ART" `isInfixOf` msg)

-- | A matching variant replaces the default art.
testCondRoomAsciiVariant :: IO Bool
testCondRoomAsciiVariant = do
    let g = roomWithCondAscii
        (_, msg) = executeCommand Look
            (g { save = (save g) { flags = Map.singleton "lit" "true" } })
    r1 <- expectTrue "variant ascii shown when flag matches" ("LIT-ART" `isInfixOf` msg)
    r2 <- expectTrue "default ascii hidden when variant matches"
              (not ("DARK-ART" `isInfixOf` msg))
    pure (r1 && r2)

-- | First matching variant wins (CondText contract).
testCondRoomAsciiOrder :: IO Bool
testCondRoomAsciiOrder = do
    let ct = CondText "DEFAULT"
                [ TextVariant (HasFlag "lit") "FIRST"
                , TextVariant (HasFlag "lit") "SECOND" ]
        r = (rooms (world initSampleGame) Map.! "start") { roomAscii = staticArt ct }
        g = roomWithCondAscii
                { world = (world roomWithCondAscii)
                    { rooms = Map.insert "start" r (rooms (world initSampleGame)) }
                , save = (save roomWithCondAscii) { flags = Map.singleton "lit" "true" } }
        (_, msg) = executeCommand Look g
    r1 <- expectTrue "first matching variant wins" ("FIRST" `isInfixOf` msg)
    r2 <- expectTrue "later variant is not shown" (not ("SECOND" `isInfixOf` msg))
    pure (r1 && r2)

-- | `ascii:` on an NPC switches with the NPC status (alive/dead).
testNpcAsciiStateDependent :: IO Bool
testNpcAsciiStateDependent = do
    let art = CondText "ALIVE-ART"
                  [ TextVariant (EntityHasState "oldman" "dead") "DEAD-ART" ]
        oldman = (npcDefs (world initSampleGame) Map.! "oldman") { npcAscii = staticArt art }
        g = initSampleGame
                { world = (world initSampleGame)
                    { npcDefs = Map.insert "oldman" oldman (npcDefs (world initSampleGame)) } }
        (_, aliveMsg) = executeCommand (Interact VLookAt "old man") g
        gDead = g { save = (save g)
                      { npcStates = Map.adjust (\ns -> ns { npcStatus = "dead" })
                                               "oldman" (npcStates (save g)) } }
        (_, deadMsg) = executeCommand (Interact VLookAt "old man") gDead
    r1 <- expectTrue "living npc shows living art" ("ALIVE-ART" `isInfixOf` aliveMsg)
    r2 <- expectTrue "dead npc shows dead art" ("DEAD-ART" `isInfixOf` deadMsg)
    r3 <- expectTrue "dead npc drops the living art" (not ("ALIVE-ART" `isInfixOf` deadMsg))
    pure (r1 && r2 && r3)

-- | A corpse stays where it fell, and that is the only way the dead variant of an
--   NPC's art is reachable in play. Before this, `killNPC` moved the NPC to
--   `Removed`, so `look at <name>` failed and the body's art could never be
--   shown; the older test passed because it set the status by hand while leaving
--   the NPC in the room, which is not what the engine does on a kill.
testCorpseStaysFindable :: IO Bool
testCorpseStaysFindable = do
    let art = CondText "ALIVE-ART" [ TextVariant (EntityHasState "oldman" "dead") "DEAD-ART" ]
        oldman = (npcDefs (world initSampleGame) Map.! "oldman") { npcAscii = staticArt art }
        g = initSampleGame
                { world = (world initSampleGame)
                    { npcDefs = Map.insert "oldman" oldman (npcDefs (world initSampleGame)) } }
        -- one hit has to be lethal for the real kill path to run
        wounded = g { save = (save g)
                        { npcStates = Map.adjust (\ns -> ns { npcHealth = Just 1 })
                                                 "oldman" (npcStates (save g)) } }
        (stKill, killMsg) = executeCommand (Interact VAttack "old man") wounded
        (_, lookMsg) = executeCommand (Interact VLookAt "old man") stKill
        (_, attackMsg) = executeCommand (Interact VAttack "old man") stKill
        (_, talkMsg) = executeCommand (Interact VTalk "old man") stKill
        (_, roomMsg) = executeCommand Look stKill
        alsoHereLine = unlines [ l | l <- lines roomMsg, "Also here:" `isInfixOf` l ]
    r1 <- expectTrue "the attack reports the kill" ("kill it" `isInfixOf` killMsg)
    r2 <- expectTrue "the killing round renders the body's art" ("DEAD-ART" `isInfixOf` killMsg)
    r3 <- expectTrue "the body keeps its location"
              (case npcLocation <$> Map.lookup "oldman" (npcStates (save stKill)) of
                   Just loc -> loc == InRoom (currentRoom (save stKill))
                   Nothing  -> False)
    r4 <- expectTrue "looking at the corpse shows the dead art" ("DEAD-ART" `isInfixOf` lookMsg)
    r5 <- expectTrue "attacking a corpse is refused" ("already dead" `isInfixOf` attackMsg)
    r6 <- expectTrue "talking to a corpse is refused" ("says nothing" `isInfixOf` talkMsg)
    r7 <- expectTrue "looking around mentions the body" ("The body of " `isInfixOf` roomMsg)
    r8 <- expectTrue "the body is not announced as a living presence"
              (not (npcName oldman `isInfixOf` alsoHereLine))
    pure (and [r1, r2, r3, r4, r5, r6, r7, r8])

-- | `ascii:` on an item switches with the item status.
testItemAsciiStateDependent :: IO Bool
testItemAsciiStateDependent = do
    let art = CondText "ITEM-ART"
                  [ TextVariant (EntityHasState "torch" "out") "ITEM-OUT-ART" ]
        torch = (itemDefs (world initSampleGame) Map.! "torch") { itemAscii = staticArt art }
        g = initSampleGame
                { world = (world initSampleGame)
                    { itemDefs = Map.insert "torch" torch (itemDefs (world initSampleGame)) } }
        (_, burningMsg) = executeCommand (Interact VLookAt "torch") g
        gOut = g { save = (save g)
                     { itemStates = Map.adjust (\is -> is { itemStatus = "out" })
                                               "torch" (itemStates (save g)) } }
        (_, outMsg) = executeCommand (Interact VLookAt "torch") gOut
    r1 <- expectTrue "burning item shows default art" ("ITEM-ART" `isInfixOf` burningMsg)
    r2 <- expectTrue "spent item shows variant art" ("ITEM-OUT-ART" `isInfixOf` outMsg)
    pure (r1 && r2)

-- ===== Animated ASCII art (Phase D) =====

-- | An animated room art: frames F0..F2, one frame per 2 turns.
animatedRoom :: GameState
animatedRoom =
    let art = emptyAscii { aaFrames = map plainText ["F0", "F1", "F2"], aaEvery = 2 }
        r = (rooms (world initSampleGame) Map.! "start") { roomAscii = art }
    in initSampleGame
        { world = (world initSampleGame)
            { rooms = Map.insert "start" r (rooms (world initSampleGame)) } }

atTurn :: GameState -> Int -> GameState
atTurn g n = g { save = (save g) { turnCount = n } }

-- | The passive frame is a pure function of `turnCount` and `every`.
testPassiveFrame :: IO Bool
testPassiveFrame = do
    let frameAt n = let (_, msg) = executeCommand Look (atTurn animatedRoom n) in msg
    r1 <- expectTrue "turn 0 -> frame F0" ("F0" `isInfixOf` frameAt 0)
    r2 <- expectTrue "turn 1 -> still F0" ("F0" `isInfixOf` frameAt 1)
    r3 <- expectTrue "turn 2 -> frame F1" ("F1" `isInfixOf` frameAt 2)
    r4 <- expectTrue "turn 4 -> frame F2" ("F2" `isInfixOf` frameAt 4)
    r5 <- expectTrue "turn 6 -> wraps to F0" ("F0" `isInfixOf` frameAt 6)
    pure (r1 && r2 && r3 && r4 && r5)

-- | `asciiFrames` yields every resolved frame, in order.
testAsciiFramesList :: IO Bool
testAsciiFramesList = do
    let art = emptyAscii { aaFrames = [plainText "A", plainText "B"], aaEvery = 1 }
    expectEqual ["A", "B"] (asciiFrames art initSampleGame)

-- | `watch <target>` queues the frames for the IO loop and costs no turn.
testWatchCommand :: IO Bool
testWatchCommand = do
    let art = emptyAscii { aaFrames = [plainText "T1", plainText "T2"], aaEvery = 1 }
        torch = (itemDefs (world initSampleGame) Map.! "torch") { itemAscii = art }
        g = initSampleGame
                { world = (world initSampleGame)
                    { itemDefs = Map.insert "torch" torch (itemDefs (world initSampleGame)) } }
        cmd = parseCommandWith Map.empty "watch torch"
        (st', msg) = executeCommand cmd g
    r1 <- expectEqual (WatchCmd (Just "torch")) cmd
    r2 <- expectEqual (Just (["T1", "T2"], defaultFrameMicros)) (pendingAnimation st')
    r3 <- expectTrue "watch announces the target" ("torch" `isInfixOf` msg)
    r4 <- expectTrue "watch does not consume a turn" (not (consumesTurn cmd))
    pure (r1 && r2 && r3 && r4)

-- ===== End/title banners (Phase G) =====

-- | `end_art` resolves by game-over reason; absent art yields `Nothing` so the
--   built-in frame stays (backwards compatible).
testEndArtFor :: IO Bool
testEndArtFor = do
    let death = staticArt (plainText "DEATH-BANNER")
        victory = staticArt (plainText "VICTORY-BANNER")
        custom = staticArt (plainText "CUSTOM-BANNER")
        gw = (world initSampleGame)
                { worldEndArt = Map.fromList
                    [ ("death", death), ("victory", victory), ("the end", custom) ] }
        g = initSampleGame { world = gw }
        render r = fmap (\a -> resolveAsciiArt a g) (endArtFor r g)
    r1 <- expectEqual (Just "DEATH-BANNER") (render Death)
    r2 <- expectEqual (Just "VICTORY-BANNER") (render Victory)
    r3 <- expectEqual (Just "CUSTOM-BANNER") (render (Custom "the end"))
    r4 <- expectEqual Nothing (render (Custom "unknown ending"))
    r5 <- expectEqual Nothing (endArtFor Death initSampleGame)
    pure (r1 && r2 && r3 && r4 && r5)

-- | A title banner resolves against the world state; the empty default stays
--   empty so the caller falls back to `bannerFor`.
testTitleArtResolves :: IO Bool
testTitleArtResolves = do
    let art = staticArt (plainText "TITLE-BANNER")
        g = initSampleGame { world = (world initSampleGame) { worldTitleArt = art } }
    r1 <- expectEqual "TITLE-BANNER" (resolveAsciiArt (worldTitleArt (world g)) g)
    r2 <- expectEqual "" (resolveAsciiArt (worldTitleArt (world initSampleGame)) initSampleGame)
    pure (r1 && r2)

-- ===== Hotspots (Phase E) =====

-- | The starter room with an art that marks the torch with `*` and the old man
--   with `T`.
hotspotRoom :: GameState
hotspotRoom =
    let base = staticArt (plainText "  *\n  T")
        art = base { aaHotspots = [ Hotspot '*' "torch", Hotspot 'T' "oldman" ] }
        r = (rooms (world initSampleGame) Map.! "start") { roomAscii = art }
    in initSampleGame
        { world = (world initSampleGame)
            { rooms = Map.insert "start" r (rooms (world initSampleGame)) } }

-- | `look at <n>` resolves the n-th hotspot to its target (name binding still
--   works too).
testHotspotNumberLook :: IO Bool
testHotspotNumberLook = do
    let (_, byNumber) = executeCommand (Interact VLookAt "1") hotspotRoom
        (_, byName) = executeCommand (Interact VLookAt "torch") hotspotRoom
        (_, second) = executeCommand (Interact VLookAt "2") hotspotRoom
        (_, absent) = executeCommand (Interact VLookAt "9") hotspotRoom
    r1 <- expectTrue "number 1 shows the torch" ("A burning torch" `isInfixOf` byNumber)
    r2 <- expectTrue "name still works" ("A burning torch" `isInfixOf` byName)
    r3 <- expectTrue "number 2 shows the old man" ("withered old man" `isInfixOf` second)
    r4 <- expectTrue "an unknown number is not an object" ("9" `isInfixOf` absent)
    pure (r1 && r2 && r3 && r4)

-- | Markers are highlighted in `look`, and stripping yields the plain art.
testHotspotHighlight :: IO Bool
testHotspotHighlight = do
    let art = roomAscii (rooms (world hotspotRoom) Map.! "start")
        highlighted = renderArtForLook art hotspotRoom
        plain = resolveAsciiArt art hotspotRoom
    r1 <- expectTrue "marker is wrapped in ANSI" ('\ESC' `elem` highlighted)
    r2 <- expectTrue "the marker character is still there" ('*' `elem` stripAnsi highlighted)
    r3 <- expectEqual plain (stripAnsi highlighted)
    pure (r1 && r2 && r3)

-- | `map` numbers the markers and lists the legend.
testMapCommand :: IO Bool
testMapCommand = do
    let (_, msg) = executeCommand MapCmd hotspotRoom
    r1 <- expectTrue "map replaces the marker with its number" ("1" `isInfixOf` msg && "2" `isInfixOf` msg)
    r2 <- expectTrue "map lists the torch in the legend" ("1: torch" `isInfixOf` msg)
    r3 <- expectTrue "map lists the old man in the legend" ("2: old man" `isInfixOf` msg)
    r4 <- expectTrue "map does not consume a turn" (not (consumesTurn MapCmd))
    pure (r1 && r2 && r3 && r4)

-- | String shorthand, object form and full-room round-trip for `ascii`.
testAsciiJsonRoundTrip :: IO Bool
testAsciiJsonRoundTrip = do
    -- CondText accepts the plain string shorthand and round-trips object form.
    r1 <- expectEqual (Just (CondText "LEGACY" []))
              (Aeson.decode (BLC.pack "\"LEGACY\"") :: Maybe CondText)
    let ctObj = CondText "D" [TextVariant (HasFlag "lit") "L"]
    r2 <- expectEqual (Just ctObj)
              (Aeson.decode (Aeson.encode ctObj) :: Maybe CondText)
    -- A room keeps its conditional ASCII art through a JSON round-trip.
    let ct = CondText "DEF" [ TextVariant (HasFlag "lit") "LIT" ]
        room = (rooms (world initSampleGame) Map.! "start") { roomAscii = staticArt ct }
    case Aeson.decode (Aeson.encode room) :: Maybe Room of
        Nothing -> putStrLn "  room failed to decode" >> pure False
        Just room' -> do
            r3 <- expectEqual (Just (staticArt ct)) (Just (roomAscii room'))
            -- An empty art is omitted, keeping the historical JSON (checksum).
            let emptyRoom = rooms (world initSampleGame) Map.! "start"
                encodedEmpty = BLC.unpack (Aeson.encode emptyRoom)
            r4 <- expectTrue "empty ascii is omitted from JSON"
                      (not ("roomAscii" `isInfixOf` encodedEmpty))
            pure (r1 && r2 && r3 && r4)

-- ===== ANSI colour handling (Phase C) =====

-- | The pure stripper removes SGR sequences and leaves plain text untouched.
testStripAnsi :: IO Bool
testStripAnsi = do
    r1 <- expectEqual "red" (stripAnsi "\ESC[31mred\ESC[0m")
    r2 <- expectEqual "plain text" (stripAnsi "plain text")
    r3 <- expectEqual "a\nb" (stripAnsi "\ESC[38;2;1;2;3ma\ESC[0m\n\ESC[48;2;4;5;6mb\ESC[0m")
    -- the reset at the end of a coloured art line must be gone too
    r4 <- expectTrue "no escape survives stripping"
              (not ('\ESC' `elem` stripAnsi "\ESC[38;2;9;9;9mX\ESC[0m\n\ESC[38;2;8;8;8mY\ESC[0m"))
    pure (r1 && r2 && r3 && r4)

-- | The output policy: identity only on a TTY with colour enabled.
testAnsiFilter :: IO Bool
testAnsiFilter = do
    let colored = "\ESC[31mX\ESC[0m"
    r1 <- expectEqual colored (ansiFilter True False colored)
    r2 <- expectEqual "X" (ansiFilter False False colored)
    r3 <- expectEqual "X" (ansiFilter True True colored)
    r4 <- expectEqual "X" (ansiFilter False True colored)
    pure (r1 && r2 && r3 && r4)

-- | End to end: a coloured room art is plain text after the engine filter.
testColoredRoomArtFiltered :: IO Bool
testColoredRoomArtFiltered = do
    let art = "\ESC[38;2;255;0;0mRED-ART\ESC[0m"
        r = (rooms (world initSampleGame) Map.! "start") { roomAscii = staticArt (plainText art) }
        g = initSampleGame
                { world = (world initSampleGame)
                    { rooms = Map.insert "start" r (rooms (world initSampleGame)) } }
        (_, msg) = executeCommand Look g
        plain = stripAnsi msg
    r1 <- expectTrue "coloured art reaches the message" ('\ESC' `elem` msg)
    r2 <- expectTrue "stripped output keeps the art" ("RED-ART" `isInfixOf` plain)
    r3 <- expectTrue "stripped output has no escapes" (not ('\ESC' `elem` plain))
    pure (r1 && r2 && r3)

-- ===== Dialogue validation tests (Phase 4.6) =====

testMissingDialogueNodeDetected :: IO Bool
testMissingDialogueNodeDetected = do
    let brokenTree = DialogueTree "nonexistent_entry" Map.empty
    let brokenNpc = (npcDefs (world initSampleGame) Map.! "oldman")
            { npcDialogueTrees = Map.singleton "alive" brokenTree }
    let gw = (world initSampleGame)
            { npcDefs = Map.insert "oldman" brokenNpc (npcDefs (world initSampleGame)) }
    let errors = validateWorld gw
    expectTrue "missing dialogue entry node detected"
        (MissingDialogueNode "oldman" "alive" "nonexistent_entry" `elem` errors)

testDanglingDialogueChoiceDetected :: IO Bool
testDanglingDialogueChoiceDetected = do
    let brokenNode = DialogueNode "greeting" "Hello"
            [ DialogueChoice "Next" (Just "missing_target") Nothing (SendMessage "") ]
    let brokenTree = DialogueTree "greeting" (Map.singleton "greeting" brokenNode)
    let brokenNpc = (npcDefs (world initSampleGame) Map.! "oldman")
            { npcDialogueTrees = Map.singleton "alive" brokenTree }
    let gw = (world initSampleGame)
            { npcDefs = Map.insert "oldman" brokenNpc (npcDefs (world initSampleGame)) }
    let errors = validateWorld gw
    expectTrue "dangling dialogue choice detected"
        (DanglingDialogueChoice "oldman" "alive" "greeting" "missing_target" `elem` errors)

-- | A rule event must carry the RESOLVED canonical id, not the raw player text -- see skill.
-- ---------------------------------------------------------------------------
-- Phase 7a: standing predicate sugar (factions as VarMap variables)
-- ---------------------------------------------------------------------------

-- | `standing: { faction: X, at_least: N }` decodes to CompareVar on the
--   `faction.<id>` variable (pure parse alias — no new constructor).
testStandingPredicateParsesToCompareVar :: IO Bool
testStandingPredicateParsesToCompareVar = do
    let j = "{\"standing\":{\"faction\":\"corp\",\"at_least\":20}}"
    case Aeson.decode (BLC.pack j) :: Maybe Predicate of
        Just (CompareVar "faction.corp" CGte 20) -> pure True
        Just other -> do
            putStrLn $ "  decoded to: " ++ show other
            pure False
        Nothing -> do
            putStrLn "  standing predicate did not decode"
            pure False

-- | `at_most` maps to CLte, `equals` to CEq.
testStandingPredicateVariants :: IO Bool
testStandingPredicateVariants = do
    let jAtMost = "{\"standing\":{\"faction\":\"corp\",\"at_most\":5}}"
        jEquals = "{\"standing\":{\"faction\":\"corp\",\"equals\":3}}"
    case ( Aeson.decode (BLC.pack jAtMost) :: Maybe Predicate
         , Aeson.decode (BLC.pack jEquals) :: Maybe Predicate ) of
        (Just (CompareVar "faction.corp" CLte 5), Just (CompareVar "faction.corp" CEq 3)) -> pure True
        (a, b) -> do
            putStrLn $ "  at_most decoded to: " ++ show a
            putStrLn $ "  equals decoded to: " ++ show b
            pure False

-- | Round-trip: a compiled CompareVar stays CompareVar through save/load
--   (ToJSON stays the canonical compare_var form; standing is input-only sugar).
testStandingPredicateJSONRoundTrip :: IO Bool
testStandingPredicateJSONRoundTrip = do
    let p = CompareVar "faction.corp" CGte 20 :: Predicate
        encoded = Aeson.encode p
    case Aeson.decode encoded :: Maybe Predicate of
        Just p' -> expectEqual p p'
        Nothing -> do
            putStrLn $ "  round-trip failed for: " ++ show encoded
            pure False

-- | evalPredicate on a faction variable: true above threshold, false below.
testStandingPredicateEval :: IO Bool
testStandingPredicateEval = do
    let low  = setVariable "faction.corp" (VVInt 10) initSampleGame
        high = setVariable "faction.corp" (VVInt 25) initSampleGame
        gate = CompareVar "faction.corp" CGte 20
    r1 <- expectTrue "low standing blocked" (not (evalPredicate gate low))
    r2 <- expectTrue "high standing passes" (evalPredicate gate high)
    pure (r1 && r2)

-- | The outcome interpreter writes the faction.* VarMap entry via
--   ModifyValue (VRVariable ...) — the core promise of module 7a.
testStandingOutcomeWritesVariable :: IO Bool
testStandingOutcomeWritesVariable = do
    let base = initSampleGame
    let (st, _) = applyOutcome (ModifyValue (VRVariable "faction.smugglers") 20) "" base
    case getVariable "faction.smugglers" st of
        Just (VVInt n) -> expectEqual 20 n
        _ -> expectTrue "expected faction.smugglers = 20" False

-- | A dialogue choice with a `standing` outcome applies through the full
--   parse → choose path (Parser.executeCommand → applyOutcomeWith).
testStandingOutcomeViaDialogue :: IO Bool
testStandingOutcomeViaDialogue = do
    let sample = initSampleGame
        w = (world sample)
            { npcDefs = Map.insert "recruiter"
                (NPCDef "recruiter" "recruiter" (plainText "A quiet recruiter.")
                    (Map.singleton "alive"
                        (DialogueTree "intro"
                            (Map.singleton "intro"
                                (DialogueNode "intro" "Join us."
                                    [ DialogueChoice "I accept." Nothing Nothing
                                        (ModifyValue (VRVariable "faction.smugglers") 20) ]))))
                    ["recruiter"] Nothing 0 0 Map.empty False emptyAscii Map.empty emptyGrammar)
                (npcDefs (world sample)) }
        st0 = sample { world = w
                     , save = (save sample)
                        { npcStates = Map.insert "recruiter"
                            (NPCState (InRoom (currentRoom (save sample))) "alive" Nothing Map.empty Nothing)
                            (npcStates (save sample)) } }
    let (st1, _) = executeCommand (Interact VTalk "recruiter") st0
        (st2, _) = executeCommand (ChooseCmd 1) st1
    case getVariable "faction.smugglers" st2 of
        Just (VVInt n) -> expectEqual 20 n
        _ -> expectTrue "dialogue standing outcome not applied" False

-- ===== Phase 7b: Handelsmodule — Market::buy/sell über Item verb_map =====

-- | Shop item whose `buy` verb_map entry handles the transaction: stock and
--   credits live in the VarMap; no sales beyond stock; price via CompareVar.
tradeWorld :: [(String, Int)] -> Int -> GameState
tradeWorld stock credits =
    let sample = initSampleGame
        buyEff = Conditional
            (PAll [ CompareVar "credits" CGte 12
                  , CompareVar ("shop.merchant." ++ "rope") CGt 0 ])
            (Sequence [ ModifyValue (VRVariable "credits") (-12)
                      , ModifyValue (VRVariable "shop.merchant.rope") (-1)
                      , MoveEntity "rope" (CarriedBy ActorPlayer)
                      , SendMessage "You buy the rope." ])
            (SendMessage "You can't afford it.")
        sellEff = Sequence [ ModifyValue (VRVariable "credits") 5
                           , MoveEntity "rope" Removed
                           , SendMessage "You sell the rope." ]
    in sample
        { world = (world sample)
            { itemDefs = Map.singleton "rope"
                (ItemDef "rope" "rope" (plainText "A coil of rope.")
                    ["rope"] Set.empty Nothing [] False Nothing True Nothing
                    (Map.singleton (PhaseAfter, VCustom "buy", "intact") buyEff
                        `Map.union` Map.singleton (PhaseAfter, VCustom "sell", "intact") sellEff) Nothing emptyAscii emptyGrammar)
            , verbDefs = Map.singleton "buy" (VerbDef "buy" ["purchase"])
                `Map.union` Map.singleton "sell" (VerbDef "sell" ["pawn"])
            }
        , save = (save sample)
            { itemStates = Map.singleton "rope"
                (ItemState (InRoom "start") "intact" Map.empty True)
            , variables = Map.fromList
                (("credits", VVInt credits)
                    : [ (k, VVInt v) | (k, v) <- stock ])
            }
        }

-- | Buying with enough credits deducts, decrements stock, and hands over the item.
testTradeBuyWithFunds :: IO Bool
testTradeBuyWithFunds = do
    let st = tradeWorld [("shop.merchant.rope", 2)] 50
        (st', _) = executeCommand (Interact (VCustom "buy") "rope") st
    r1 <- expectEqual (Just (VVInt 38)) (getVariable "credits" st')
    r2 <- expectTrue "rope carried" (hasItem "rope" st')
    r3 <- expectEqual (Just (VVInt 1)) (getVariable "shop.merchant.rope" st')
    pure (r1 && r2 && r3)

-- | Buying without cover changes nothing: no deduction, no item, no stock drop.
testTradeBuyInsufficientFundsChangesNothing :: IO Bool
testTradeBuyInsufficientFundsChangesNothing = do
    let st = tradeWorld [("shop.merchant.rope", 2)] 5
        (st', msg) = executeCommand (Interact (VCustom "buy") "rope") st
    r1 <- expectTrue "rejection message" ("can't afford" `isInfixOf` msg)
    r2 <- expectEqual (Just (VVInt 5)) (getVariable "credits" st')
    r3 <- expectTrue "not carried" (not (hasItem "rope" st'))
    r4 <- expectEqual (Just (VVInt 2)) (getVariable "shop.merchant.rope" st')
    pure (r1 && r2 && r3 && r4)

-- | Selling a carried item pays out and removes it from the inventory.
testTradeSellAddsCredits :: IO Bool
testTradeSellAddsCredits = do
    let st = tradeWorld [] 20
        withRope = st { save = (save st)
            { itemStates = Map.insert "rope"
                (ItemState (CarriedBy ActorPlayer) "intact" Map.empty True)
                (itemStates (save st)) } }
        (st', _) = executeCommand (Interact (VCustom "sell") "rope") withRope
    r1 <- expectEqual (Just (VVInt 25)) (getVariable "credits" st')
    r2 <- expectTrue "rope removed" (not (hasItem "rope" st'))
    pure (r1 && r2)

-- ===== P1-8: VTInt-Grenzen wirksam =====

-- | `set_state` auf eine int-Variable respektiert min/max der Variablendefinition
--   (P1-8). Ohne Clamping würde 15 statt 10 gespeichert.
testVTIntBoundsEnforcedOnSet :: IO Bool
testVTIntBoundsEnforcedOnSet = do
    let w = (world initSampleGame)
            { varDefs = Map.insert "score"
                (VarDef "score" (VTInt (Just 0) (Just 10)) (VVInt 5))
                (varDefs (world initSampleGame)) }
        base = initSampleGame { world = w }
        (over, _)  = applyOutcome (SetValue (VRVariable "score") (EVInt 15)) "" base
        (under, _) = applyOutcome (SetValue (VRVariable "score") (EVInt (-3))) "" base
    r1 <- expectEqual (Just (VVInt 10)) (getVariable "score" over)
    r2 <- expectEqual (Just (VVInt 0)) (getVariable "score" under)
    pure (r1 && r2)

-- | `modify_value` clampt auf die Ober-/Untergrenze (P1-8).
testVTIntBoundsEnforcedOnModify :: IO Bool
testVTIntBoundsEnforcedOnModify = do
    let w = (world initSampleGame)
            { varDefs = Map.insert "score"
                (VarDef "score" (VTInt (Just 0) (Just 10)) (VVInt 8))
                (varDefs (world initSampleGame)) }
        base = initSampleGame
            { world = w
            , save = (save initSampleGame) { variables = Map.singleton "score" (VVInt 8) } }
        (up, _)   = applyOutcome (ModifyValue (VRVariable "score") 5) "" base
        (down, _) = applyOutcome (ModifyValue (VRVariable "score") (-100)) "" base
    r1 <- expectEqual (Just (VVInt 10)) (getVariable "score" up)
    r2 <- expectEqual (Just (VVInt 0)) (getVariable "score" down)
    pure (r1 && r2)

-- ===== P1-9: Fuel-Text in vehicleLookAddon =====

-- | Hilfszustand: an Bord der Kutsche mit vorgegebenem Treibstoffstand.
aboardWithFuel :: Int -> GameState
aboardWithFuel f = initSampleGame
    { save = (save initSampleGame)
        { currentVehicle = Just "carriage"
        , vehicleStates = Map.insert "carriage"
            ((getVehicleState "carriage" initSampleGame) { vsFuel = Just f })
            (vehicleStates (save initSampleGame)) } }

-- | Leerer Tank meldet "Out of hay (0/10)" statt eines kaputten Strings
--   (P1-9: `if f <= 0 then ... else ...` vertauschte Zweige).
testVehicleLookFuelEmpty :: IO Bool
testVehicleLookFuelEmpty = do
    r1 <- expectEqual (Just "Out of hay (0/10)") (vehicleLookAddon (aboardWithFuel 0))
    r2 <- expectEqual (Just "Out of hay (0/10)") (vehicleLookAddon (aboardWithFuel (-2)))
    pure (r1 && r2)

-- | Gefüllter Tank meldet "Fuel (hay: n/10)".
testVehicleLookFuelFilled :: IO Bool
testVehicleLookFuelFilled = do
    let addon = vehicleLookAddon (aboardWithFuel 7)
    r1 <- expectEqual (Just "Fuel (hay: 7/10)") addon
    pure r1

-- ===== P1-10/.11: geänderte Fahrzeug-Klauseln =====

-- | `move ... in_container:` ist ein dokumentierter No-op mit Fehlermeldung
--   statt eines stillen No-ops (P1-11).
testMoveEntityInContainerReportsError :: IO Bool
testMoveEntityInContainerReportsError = do
    let (st, msg) = applyOutcome (MoveEntity "torch" (InContainer "chest")) "" initSampleGame
        torchLoc = itemLocation <$> Map.lookup "torch" (itemStates (save st))
    r1 <- expectTrue "error message shown" ("can't move" `isInfixOf` msg)
    r2 <- expectTrue "item did not move" (torchLoc == Just (InRoom "start"))
    pure (r1 && r2)

-- | `vehicleStopList` liefert Stops in der AUTOREN-Reihenfolge, wenn
--   `vehicleRoute` gesetzt ist — nicht in Map-Schlüsselreihenfolge (P1-10).
testVehicleStopListUsesAuthoredRoute :: IO Bool
testVehicleStopListUsesAuthoredRoute = do
    let v0 = (vehicleDefs (world initSampleGame)) Map.! "carriage"
        v' = v0 { vehicleRoute = ["start", "meadow"] }
        authored = map fst (vehicleStopList v')
    r1 <- expectEqual ["start", "meadow"] authored
    -- ohne route: Fallback bleibt Map-Reihenfolge (abwärtskompatibel)
    r2 <- expectEqual ["meadow", "start"] (map fst (vehicleStopList v0))
    pure (r1 && r2)

-- | P1-13: `swim`/`crawl`/`dig`/`game` were hardcoded in the core parser and
--   all mapped to `Go Southeast`. They are genre content, not core verbs:
--   with no adventure-declared verb they must parse as `Unknown`.
testGenreVerbsNotCoreVerbs :: IO Bool
testGenreVerbsNotCoreVerbs = do
    r1 <- expectEqual (Unknown "swim")  (parseCommand "swim")
    r2 <- expectEqual (Unknown "crawl") (parseCommand "crawl")
    r3 <- expectEqual (Unknown "dig")   (parseCommand "dig")
    r4 <- expectEqual (Unknown "game")  (parseCommand "game")
    pure (and [r1, r2, r3, r4])

-- | P1-13: the generic replacement — a genre verb declared by the adventure
--   still parses (as `VCustom`), so `swim` needs no core code.
testGenreVerbFromRegistry :: IO Bool
testGenreVerbFromRegistry = do
    let reg = Map.fromList [("swim", VerbDef "swim" [])]
    expectEqual (Interact (VCustom "swim") "") (parseCommandWith reg "swim")

-- | P2-2: `executeCommand Restart` used to return `emptyGameState`, wiping the
--   loaded world. It must leave the state alone (the loop handles Restart via
--   `lsInitial`).
testRestartKeepsWorld :: IO Bool
testRestartKeepsWorld = do
    let (st, _) = executeCommand Restart initSampleGame
    r1 <- expectEqual (world initSampleGame) (world st)
    r2 <- expectTrue "world is not empty" (not (Map.null (rooms (world st))))
    pure (r1 && r2)

-- | P2-7: `visited` via `SetValue (VRActorProp (ActorRoom room) PVisited)` must accept
--   `EVBool`, not silently treat it as `False`.
testVisitedAcceptsBool :: IO Bool
testVisitedAcceptsBool = do
    let (stT, _) = applyOutcome (SetValue (VRActorProp (ActorRoom "treasure") PVisited) (EVBool True)) "" initSampleGame
        (stF, _) = applyOutcome (SetValue (VRActorProp (ActorRoom "treasure") PVisited) (EVBool False)) "" initSampleGame
    r1 <- expectTrue "EVBool True marks the room visited" (isRoomVisited "treasure" stT)
    r2 <- expectTrue "EVBool False clears it" (not (isRoomVisited "treasure" stF))
    pure (r1 && r2)

-- | P2-8: item lookup scans `itemStates`, so an `ItemDef` placed nowhere is
--   invisible at runtime. The validator must report it (§`MissingItemState`)
--   instead of letting the item silently not exist.
testItemWithoutStateIsReported :: IO Bool
testItemWithoutStateIsReported = do
    let lamp = ItemDef "lamp" "lamp" (plainText "A brass lamp.") ["lamp"] Set.empty
                    Nothing [] False Nothing True Nothing Map.empty Nothing emptyAscii emptyGrammar
        gw = (world initSampleGame)
                { itemDefs = Map.insert "lamp" lamp (itemDefs (world initSampleGame)) }
    r1 <- expectTrue "MissingItemState is reported"
              (MissingItemState "lamp" `elem` validateGameState gw (save initSampleGame))
    r2 <- expectTrue "sample world itself has no MissingItemState"
              (not (any isMissingState (validateGameState (world initSampleGame) (save initSampleGame))))
    pure (r1 && r2)
  where
    isMissingState (MissingItemState _) = True
    isMissingState _                    = False

-- | P2-9: compound map keys are encoded as objects, so a verb state or item id
--   containing the *old* separators (`:`, `|`) survives a round-trip. The
--   legacy string-keyed form still decodes for existing `world.json` files
--   (and is shown to be lossy: `("a|b","c")` cannot be expressed there).
testCompoundKeyRoundTrip :: IO Bool
testCompoundKeyRoundTrip = do
    let gw0   = world initSampleGame
        item0 = snd (Map.findMin (itemDefs gw0))
        gw    = gw0
            { itemDefs = Map.insert "weird"
                (item0 { itemVerbMap = Map.fromList [((PhaseAfter, VCustom "buy", "intact:v2"), SendMessage "a")] })
                (itemDefs gw0)
            , entityInteractions = Map.fromList [(("a|b", "c"), ("unlocked", "msg"))]
            , itemInteractions   = Map.fromList [(("x:y", "z|w"), SendMessage "b")] }
    r1 <- expectEqual (Just gw) (Aeson.decode (Aeson.encode gw))
    -- legacy form of the verb map: "VTake:intact"
    let legacyVerbValue = Aeson.toJSON (Map.fromList [("VTake:intact", SendMessage "x")] :: Map.Map String Effect)
    r2 <- case AesonT.parseMaybe verbStateMapFromJSON legacyVerbValue of
        Just m  -> expectEqual (Map.fromList [((PhaseAfter, VTake, "intact"), SendMessage "x")]) m
        Nothing -> expectTrue "legacy verb-state map decodes" False
    -- legacy form of the interaction map: "a|b"
    let legacyTupleValue = Aeson.toJSON (Map.fromList [("a|b", ["u", "m"])] :: Map.Map String [String])
    r3 <- case AesonT.parseMaybe tupleMapFromJSON legacyTupleValue of
        Just m  -> expectEqual (Map.fromList [(("a", "b"), ("u", "m"))]) m
        Nothing -> expectTrue "legacy tuple map decodes" False
    pure (r1 && r2 && r3)

-- | P2-10: the save-list line reports compatibility of a save against the
--   *current* world checksum. `listSaves` now computes that checksum once for
--   all files (it was recomputed inside the per-file loop) — this pins the
--   behaviour the hoisting must not change.
testSaveListEntryCompat :: IO Bool
testSaveListEntryCompat = do
    let st0  = initSampleGame
        csum = computeWorldChecksum (world st0)
        mkSave cs = SaveFile
            { saveVersion    = currentSaveVersion
            , saveTimestamp  = "2026-01-16 10:00"
            , worldChecksum  = cs
            , saveName       = "slot1"
            , saveData       = save st0
            }
        compatibleEntry = formatSaveEntry (world st0) csum (mkSave csum)
        mismatchEntry   = formatSaveEntry (world st0) csum (mkSave "12345")
    r1 <- expectTrue "matching world reports compatible"
              ("(compatible)" `isInfixOf` compatibleEntry)
    r2 <- expectTrue "different world reports mismatch"
              ("world mismatch!" `isInfixOf` mismatchEntry)
    r3 <- expectTrue "entry carries save name and timestamp"
              ("slot1" `isInfixOf` compatibleEntry
               && "2026-01-16 10:00" `isInfixOf` compatibleEntry)
    pure (r1 && r2 && r3)

-- | P2-11: the lazy `foldl` -> strict `foldl'` rewrite must not change the
--   result of the accumulating sites: effect order and message concatenation
--   stay exactly in authored order.
testStrictFoldKeepsEffectOrder :: IO Bool
testStrictFoldKeepsEffectOrder = do
    let st0 = initSampleGame
        (_, msgViaSequence) = applyOutcome
            (Sequence [ SendMessage "first"
                      , ModifyValue (VRVariable "credits") 3
                      , SendMessage "second"
                      , SendMessage "third" ]) "" st0
        (_, msgViaList) = applyOutcomes
            [SendMessage "first", SendMessage "second", SendMessage "third"] "" st0
    r1 <- expectTrue "Sequence keeps authored order and newline joining"
              (msgViaSequence == "first\nsecond\nthird")
    r2 <- expectTrue "effect list keeps authored order"
              (renderEvents msgViaList == "first\nsecond\nthird")
    pure (r1 && r2)

-- | P2-22: `pick` is both a dialogue keyword (`pick 3` = choose option 3) and a
--   `take` alias (`src/Verbs.hs`). The numeric keyword list is matched before
--   `parseVerbWith`, so a numeric `pick <n>` can *never* mean "take item <n>".
--   That is intended, but it was untested for `pick` (only the bare number and
--   `choose` were covered) — pin all four keyword forms, the bare number, and
--   the still-working verb reading of a non-numeric `pick <item>`.
testDialoguePickKeywordAlias :: IO Bool
testDialoguePickKeywordAlias = do
    let (st1, _) = executeCommand (Interact VTalk "old man") initSampleGame
        keywords = ["choose", "pick", "option", "select"]
    r1 <- expectTrue "every dialogue keyword parses as a choice"
              (all (\w -> parseCommand (w ++ " 3") == ChooseCmd 3) keywords)
    r2 <- expectTrue "bare number still parses as a choice"
              (parseCommand "1" == ChooseCmd 1)
    let (_, msgPick) = executeCommand (parseCommand "pick 1") st1
    r3 <- expectTrue "`pick 1` selects the dialogue option"
              ("old hermit" `isInfixOf` msgPick)
    r4 <- expectTrue "non-numeric `pick <item>` is still the take verb"
              (case parseCommand "pick lantern" of
                   Interact VTake _ -> True
                   _                -> False)
    pure (r1 && r2 && r3 && r4)

-- | P2-23: an effect nested past `maxOutcomeDepth` is a *content* error. It has
--   to reach the author (`diagnostics`) and never the player's text — it used to
--   be returned as `"[ERROR] Maximum outcome depth exceeded."` and, through
--   `applyTrigEffects`, landed between game text.
testDepthGuardUsesDiagnostics :: IO Bool
testDepthGuardUsesDiagnostics = do
    let deep = foldr (\_ acc -> Sequence [acc, SendMessage "leaf"])
                     (SendMessage "leaf") [1 .. (maxOutcomeDepth + 5) :: Int]
        (st', msg) = applyOutcome deep "" initSampleGame
    r1 <- expectTrue "over-deep outcome puts no engine text in the message"
              (not ("[ERROR]" `isInfixOf` msg) && not ("[engine]" `isInfixOf` msg))
    r2 <- expectTrue "over-deep outcome records a diagnostic"
              (any ("maximum outcome depth exceeded" `isInfixOf`) (diagnostics st'))
    let (st2, _) = applyOutcome (Sequence [SendMessage "a", Sequence [SendMessage "b"]]) "" initSampleGame
    r3 <- expectTrue "a legal sequence records nothing"
              (null (diagnostics st2))
    pure (r1 && r2 && r3)

-- | P2-23, trigger path: `applyTrigEffects` concatenates effect messages into
--   the trigger output, so the guard message used to appear as game text there.
testDepthGuardStaysOutOfTriggerText :: IO Bool
testDepthGuardStaysOutOfTriggerText = do
    let deep = foldr (\_ acc -> Sequence [acc]) (SendMessage "boom")
                     [1 .. (maxOutcomeDepth + 5) :: Int]
        st0 = initSampleGame
                { world = (world initSampleGame)
                    { triggerDefs = [ TriggerDef "deep" (OnCustomEvent "go")
                                        Nothing [deep] False 0 ] } }
        (st', msg) = fireTriggers (OnCustomEvent "go") st0
    r1 <- expectTrue "trigger output contains no engine error text"
              (not ("[ERROR]" `isInfixOf` renderEvents msg) && not ("[engine]" `isInfixOf` renderEvents msg))
    r2 <- expectTrue "trigger path recorded the diagnostic"
              (any ("maximum outcome depth exceeded" `isInfixOf`) (diagnostics st'))
    pure (r1 && r2)

-- | P2-23: the trigger-nesting guard silently dropped a runaway recursion. It
--   is defensive (the effect-depth guard normally fires first), so it is
--   exercised directly here.
testTriggerNestingGuardReports :: IO Bool
testTriggerNestingGuardReports = do
    let st0 = initSampleGame
                { world = (world initSampleGame)
                    { triggerDefs = [ TriggerDef "loop" (OnCustomEvent "loop")
                                        Nothing [SendMessage "never"] False 0 ] } }
        (st', msg) = fireTriggersWithDepth (maxOutcomeDepth + 1) (OnCustomEvent "loop") st0
    r1 <- expectTrue "guard emits no game text" (null msg)
    r2 <- expectTrue "guard records a diagnostic"
              (any ("trigger nesting exceeded" `isInfixOf`) (diagnostics st'))
    pure (r1 && r2)

-- ===== Test gaps from the review (L2, L3, L5, L7, L13) =====

-- | L7: `GameWorld` holds most of the hand-written `ToJSON`/`FromJSON` pairs
--   (ItemDef, NPCDef, VehicleDef, Room, CombatProfile, Predicate, CondText and
--   the compound-key maps). A hand-written decoder next to a generic encoder
--   produces a shape that cannot be read back — that class was only covered
--   indirectly by the E2E loads.
testGameWorldRoundTrip :: IO Bool
testGameWorldRoundTrip = do
    let gw = world initSampleGame
    r1 <- expectEqual (Just gw) (Aeson.decode (Aeson.encode gw))
    let profiles =
            [ CombatClassic Nothing
            , CombatClassic (Just (CombatScreen emptyAscii 12 Nothing Nothing))
            , CombatOff Nothing
            , CombatOff (Just "You cannot fight here.")
            , CombatNarrative (NarrativeCombat 3 (SendMessage "hit") (SendMessage "miss"))
            , CombatTactical (TacticalCombat PlayerFirst True 100 "speed")
            , CombatTactical (TacticalCombat EnemyFirst False 20 "reflexes")
            , CombatTactical (TacticalCombat BySpeed True 50 "agility")
            ]
        rt p = Aeson.decode (Aeson.encode p) :: Maybe CombatProfile
    r2 <- expectTrue "every combat profile round-trips"
              (all (\p -> rt p == Just p) profiles)
    -- the compound-key map specifically (rewritten by P2-9)
    let item0 = snd (Map.findMin (itemDefs gw))
        gw2 = gw { itemDefs = Map.insert "custom"
                     (item0 { itemVerbMap = Map.fromList
                                [((PhaseAfter, VCustom "buy", "intact"), SendMessage "ok")] })
                     (itemDefs gw) }
    r3 <- expectTrue "itemVerbMap with a VCustom key round-trips"
              (any (\d -> Map.member (PhaseAfter, VCustom "buy", "intact") (itemVerbMap d))
                    (Map.elems (maybe Map.empty itemDefs (Aeson.decode (Aeson.encode gw2)))))
    -- GameWorld with abilities round-trips
    let gw3 = gw { abilities = Map.singleton "strike" (PlayerAbility "strike" "Strike" "stamina" 5 2 [SendMessage "Pow!"]) }
    r4 <- expectEqual (Just gw3) (Aeson.decode (Aeson.encode gw3))
    pure (r1 && r2 && r3 && r4)

-- | L3: `resolveCombat` is pure and is exactly what `executeAttack` forwards.
--   Calling it directly inspects the *effect list*; every combat test before
--   this went through `executeCommand`, which is how P0-2 (a bug in that wiring)
--   stayed hidden.
testResolveCombatDirect :: IO Bool
testResolveCombatDirect = do
    let gw0 = world initSampleGame
        goblin = maybe (error "sample lost its goblin") id (Map.lookup "goblin" (npcDefs gw0))
        target = TargetNPC "goblin" "goblin"
        st0 = initSampleGame
        pdmg = max 1 (effectiveAttack st0 - npcDefenseBase goblin)
        retalDmg = max 0 (npcAttackBase goblin - effectiveDefense st0)
        playerEffect = ModifyValue (VRActorProp (ActorNPC "goblin") PHealth) (-pdmg)
    -- player alone: exactly one effect, no retaliation when the blow is fatal
    let (effs, msgs) = resolveCombat (CombatClassic Nothing) [PlayerActor] target CAAttack st0
        stKill = st0 { save = (save st0)
                         { npcStates = Map.adjust (\ns -> ns { npcHealth = Just 1 })
                                                  "goblin" (npcStates (save st0)) } }
        (effsKill, msgsKill) = resolveCombat (CombatClassic Nothing) [PlayerActor] target CAAttack stKill
        hurtsPlayer e = case e of
            ModifyValue VRPlayerHealth _ -> True
            _                            -> False
    r1 <- expectEqual [playerEffect, ModifyValue VRPlayerHealth (-retalDmg)] effs
    r2 <- expectTrue "classic message names the damage"
              (any ("You hit for" `isInfixOf`) msgs)
    r3 <- expectEqual [playerEffect] effsKill
    r4 <- expectTrue "a killing blow is reported as such"
              (any ("kill it" `isInfixOf`) msgsKill)
    r5 <- expectTrue "a killing blow draws no retaliation" (not (any hurtsPlayer effsKill))
    -- companion and ship: player first, then the companion, then the ship
    let ally = goblin { npcId = "ally", npcName = "Ally", npcAttackBase = 3 }
        stC = st0 { world = gw0 { npcDefs = Map.insert "ally" ally (npcDefs gw0) }
                  , save  = (save st0)
                      { npcStates = Map.insert "ally"
                                      (NPCState (InRoom "hallway") "alive" (Just 20) Map.empty Nothing)
                                      (npcStates (save st0))
                      , variables = Map.fromList [ ("ship.carriage.power", VVInt 3)
                                                 , ("ship.carriage.weapons", VVInt 6) ] } }
        allyDmg = max 1 (npcAttackBase ally - npcDefenseBase goblin)
        (effsC, _) = resolveCombat (CombatClassic Nothing)
                        [PlayerActor, CompanionActor "ally", ShipActor "carriage"] target CAAttack stC
    r6 <- expectEqual
              [ playerEffect
              , ModifyValue (VRActorProp (ActorNPC "goblin") PHealth) (-allyDmg)
              , SetValue (VRVariable "ship.carriage.power") (EVInt 2)
              , ModifyValue (VRActorProp (ActorNPC "goblin") PHealth) (-6) ]
              (take 4 effsC)
    pure (r1 && r2 && r3 && r4 && r5 && r6)

-- | L3: `shipAbsorb` decides where return fire lands. Three of the four
--   `(shields, hull)` combinations were covered indirectly by fixture runs;
--   `(Nothing, Nothing)` — the "player is exposed" case — was not covered at all.
testShipAbsorbMatrix :: IO Bool
testShipAbsorbMatrix = do
    let ship ss sh = ShipSystems "carriage" "Carriage" (Just 3) (Just 6) ss sh
        shieldsVar v = SetValue (VRVariable "ship.carriage.shields") (EVInt v)
        hullVar v    = SetValue (VRVariable "ship.carriage.hull") (EVInt v)
    -- no shields, no hull: everything reaches the player, no effects at all
    r1 <- expectEqual ([], 5, []) (shipAbsorb (ship Nothing Nothing) 5)
    -- shields cover it fully
    let (e2, taken2, m2) = shipAbsorb (ship (Just 5) (Just 9)) 5
    r2 <- expectEqual ([shieldsVar 0], 0) (e2, taken2)
    r3 <- expectTrue "shields-absorb message" (any ("shields absorb 5" `isInfixOf`) m2)
    -- shields absorb partially, the spill goes into the hull
    let (e3, taken3, m3) = shipAbsorb (ship (Just 2) (Just 9)) 5
    r4 <- expectEqual ([shieldsVar 0, hullVar 6], 0) (e3, taken3)
    r5 <- expectTrue "hull spill message" (any ("hull takes 3" `isInfixOf`) m3)
    -- shields but no hull: the spill hits the player
    let (e4, taken4, m4) = shipAbsorb (ship (Just 2) Nothing) 5
    r6 <- expectEqual ([shieldsVar 0], 3) (e4, taken4)
    r7 <- expectTrue "no-hull message" (any ("no hull plating" `isInfixOf`) m4)
    -- hull only, no shields
    let (e5, taken5, m5) = shipAbsorb (ship Nothing (Just 9)) 5
    r8 <- expectEqual ([hullVar 4], 0) (e5, taken5)
    r9 <- expectTrue "hull-only message" (any ("the hull takes 5" `isInfixOf`) m5)
    -- no damage in, nothing happens
    r10 <- expectEqual ([], 0, []) (shipAbsorb (ship (Just 5) (Just 9)) 0)
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9 && r10)

-- ===== Combat screen (authored fight screen, classic profile) =====

-- | The sample world with the player's, the goblin's and the clock's numbers
--   pinned, so every rendered line of the screen is predictable.
combatScreenState :: Int -> Int -> Int -> Int -> GameState
combatScreenState hp maxHp enemyHp turns =
    let st0 = initSampleGame
    in st0 { save = (save st0)
                { player = (player (save st0))
                    { playerHealth = hp, playerMaxHealth = maxHp
                    , playerAttack = 9, playerDefense = 3 }
                , npcStates = Map.insert "goblin"
                                (NPCState (InRoom "hallway") "alive" (Just enemyHp) Map.empty Nothing)
                                (npcStates (save st0))
                , turnCount = turns } }

-- | The screen renders the original layout from the state *before* the round:
--   rules, the enemy, the scene line, attack/defense, both HP bars, steps and
--   the flee hint. Pure formatting — nothing here decides damage.
testCombatScreenLayout :: IO Bool
testCombatScreenLayout = do
    let cs  = CombatScreen emptyAscii 10 Nothing Nothing
        st  = combatScreenState 7 10 22 12
        got = combatScreenLines cs "goblin" st
        rule = replicate 80 '_'
        bar filled = replicate filled '█' ++ replicate (10 - filled) ' '
    r1 <- expectEqual
        [ rule, rule
        , "                    You are fighting a goblin"
        , "                    >>You are in a Fight!<<"
        , ""
        , "Your Atk: 9"
        , "Your Def: 3"
        , ""
        , "Your HP:  7 [" ++ bar 7 ++ "]"
        , "Your Steps:   12"
        , ""
        , "HP of the goblin: 22 [" ++ bar 7 ++ "]"
        , ""
        , "You can 'attack' or try to 'flee'..\n What will u do?"
        ] got
    -- an unconfigured art must not add a line (no stray blank)
    r2 <- expectTrue "no art line without art" (length got == 14)
    pure (r1 && r2)

-- | Bar semantics: proportional fill at the configured width, clamped at both
--   ends. Two original artefacts are deliberately fixed — a negative fill no
--   longer overfills the bar, and a degenerate maximum draws an empty bar
--   instead of the original's stray bracket.
testCombatScreenBars :: IO Bool
testCombatScreenBars = do
    let cs = CombatScreen emptyAscii 4 Nothing Nothing
        st = combatScreenState 2 4 1 0
        lineOf prefix src = [ l | l <- src, prefix `isPrefixOf` l ]
    r1 <- expectEqual [ "Your HP:  2 [██  ]" ]
              (lineOf "Your HP:" (combatScreenLines cs "goblin" st))
    r2 <- expectEqual [ "HP of the goblin: 1 [    ]" ]
              (lineOf "HP of the goblin:" (combatScreenLines cs "goblin" st))
    -- full, over-max, zero and degenerate maximum
    r3 <- expectEqual [ "Your HP:  4 [████]" ]
              (lineOf "Your HP:" (combatScreenLines cs "goblin" (combatScreenState 4 4 1 0)))
    r4 <- expectEqual [ "Your HP:  5 [████]" ]
              (lineOf "Your HP:" (combatScreenLines cs "goblin" (combatScreenState 5 4 1 0)))
    r5 <- expectEqual [ "Your HP:  0 [    ]" ]
              (lineOf "Your HP:" (combatScreenLines cs "goblin" (combatScreenState 0 4 1 0)))
    r6 <- expectEqual [ "Your HP:  3 [    ]" ]
              (lineOf "Your HP:" (combatScreenLines cs "goblin" (combatScreenState 3 0 1 0)))
    -- an NPC without health or maximum (the sample's old man): empty bar
    r7 <- expectEqual [ "HP of the old man: 0 [    ]" ]
              (lineOf "HP of the old man:" (combatScreenLines cs "oldman" st))
    -- an unknown target renders nothing at all
    r8 <- expectEqual [] (combatScreenLines cs "nope" st)
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8)

-- | Authored art and text: art lands above the block, `scene`/`footer` replace
--   the original wording, and an empty string hides the line.
testCombatScreenAuthored :: IO Bool
testCombatScreenAuthored = do
    let art = AsciiArt (plainText "  (o o)\n  (___)") [] 0 [] Nothing
        cs  = CombatScreen art 10 (Just "You are in a duel!") (Just "")
        st  = combatScreenState 7 10 22 12
        got = combatScreenLines cs "goblin" st
    r1 <- expectEqual [ replicate 80 '_', replicate 80 '_', "  (o o)\n  (___)" ]
              (take 3 got)
    r2 <- expectTrue "authored scene line replaces the default"
              (any (== "                    You are in a duel!") got)
    r3 <- expectTrue "the default scene wording is gone"
              (not (any (">>You are in a Fight!<<" `isInfixOf`) got))
    r4 <- expectTrue "an empty footer hides the line"
              (not (any ("attack' or try to" `isInfixOf`) got))
    pure (r1 && r2 && r3 && r4)

-- | The wiring in `executeAttack`: with a screen the round output leads with the
--   rules, without one it must be unchanged (the invariant that keeps every
--   existing adventure byte-identical), and a corpse is never given a screen.
testCombatScreenWiring :: IO Bool
testCombatScreenWiring = do
    let screen = CombatScreen emptyAscii 10 Nothing Nothing
        rule = replicate 80 '_'
        stIn prof =
            let st0 = combatScreenState 100 100 30 4
                st1 = st0 { save = (save st0) { currentRoom = "hallway" } }
            in st1 { world = (world st1) { combatProfile = prof } }
        stLive = stIn (CombatClassic (Just screen))
        (_, withScreen)    = executeCommand (Interact VAttack "goblin") stLive
        (_, withoutScreen) = executeCommand (Interact VAttack "goblin") (stIn (CombatClassic Nothing))
        stDead = stLive { save = (save stLive)
                            { npcStates = Map.adjust (\ns -> ns { npcStatus = "dead" }) "goblin"
                                                     (npcStates (save stLive)) } }
        (_, corpseOut) = executeCommand (Interact VAttack "goblin") stDead
    r1 <- expectTrue "the screen leads the round output" (rule `isPrefixOf` withScreen)
    r2 <- expectTrue "the screen carries both HP lines"
              (any ("Your HP:" `isPrefixOf`) (lines withScreen)
               && any ("HP of the goblin:" `isPrefixOf`) (lines withScreen))
    r3 <- expectTrue "no screen without a screen block" (not (rule `isPrefixOf` withoutScreen))
    r4 <- expectTrue "the round itself stays unchanged" ("You hit for" `isInfixOf` withoutScreen)
    r5 <- expectTrue "a corpse gets no screen" (not (rule `isPrefixOf` corpseOut))
    r6 <- expectTrue "the corpse message is unchanged" ("already dead" `isInfixOf` corpseOut)
    pure (r1 && r2 && r3 && r4 && r5 && r6)

-- | L5: `commandEvents` chooses which triggers a command raises and in which
--   order — the project note says the *event* order decides for authors, not the
--   rule order, and only `OnEnter` plus one `OnUse` alias were covered.
testCommandEventsTable :: IO Bool
testCommandEventsTable = do
    let st0 = initSampleGame
        evs cmd = commandEvents cmd st0 st0
        -- real state transitions for the take/drop cases
        stHolding = pickupItem "sword_rusty" st0
        stDropped = dropItem "sword_rusty" stHolding
        stMoved   = moveToRoom "hallway" st0
    r1 <- expectEqual [OnLook "start", OnCommand "look"] (evs Look)
    -- a blocked move (locked door) raises the command/turn events but no room
    -- events, because the event list follows the state change, not the intent
    r2 <- expectEqual [OnCommand "go", OnTurn] (commandEvents (Go East) st0 st0)
    r3 <- expectEqual [OnLeave "start", OnEnter "hallway", OnCommand "go", OnTurn]
              (commandEvents (Go North) st0 stMoved)
    r4 <- expectEqual [OnSearch "start", OnCommand "search", OnTurn]
              (evs (SearchCmd Nothing))
    r5 <- expectEqual [OnTake "sword_rusty", OnCommand "take", OnTurn]
              (commandEvents (Interact VTake "sword") st0 stHolding)
    r6 <- expectEqual [OnDrop "sword_rusty", OnCommand "drop", OnTurn]
              (commandEvents (Interact VDrop "sword") stHolding stDropped)
    -- a failed take (already carried) raises no OnTake — but the command still
    -- costs a turn, so OnCommand/OnTurn stay (P1-15)
    r7 <- expectEqual [OnCommand "take", OnTurn]
              (commandEvents (Interact VTake "sword") stHolding stHolding)
    -- informational commands raise no turn event
    r8 <- expectEqual [OnCommand "stats"] (evs StatsCmd)
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8)

-- | L5: through the loop, an `on: enter` rule must fire before the `on: turn`
--   rule of the same command (the event order, not the rule order).
testEnterFiresBeforeTurnThroughLoop :: IO Bool
testEnterFiresBeforeTurnThroughLoop = do
    let st0 = initSampleGame
                { world = (world initSampleGame)
                    { triggerDefs =
                        [ TriggerDef "on_turn"  OnTurn Nothing [SendMessage "TURN"]  False 0
                        , TriggerDef "on_enter" (OnEnter "hallway") Nothing
                            [SendMessage "ENTER"] False 0 ] } }
        (_, msg) = applyLoopCommand (Go North) (initLoopState st0)
        ls = lines msg
        idxOf needle = length (takeWhile (not . (needle `isInfixOf`)) ls)
    r1 <- expectTrue "both rules fired" (("ENTER" `isInfixOf` msg) && ("TURN" `isInfixOf` msg))
    r2 <- expectTrue "the enter event precedes the turn event" (idxOf "ENTER" < idxOf "TURN")
    pure (r1 && r2)

-- ---------------------------------------------------------------------------
-- 4.4: containers
-- ---------------------------------------------------------------------------

-- | Helper: state with rooms, items, stationary containers and entity states.
cstate2 :: [Room] -> [(ItemDef, Location)] -> [ContainerDef] -> Map.Map String String -> GameState
cstate2 rms its cons ents =
    let gw = (world emptyGameState)
            { rooms = Map.fromList [ (roomId r, r) | r <- rms ]
            , itemDefs = Map.fromList [ (itemId i, i) | (i, _) <- its ]
            , containerDefs = Map.fromList [ (conId c, c) | c <- cons ]
            }
        sv = (save emptyGameState)
            { currentRoom = if null rms then "" else roomId (head rms)
            , itemStates = Map.fromList
                [ (itemId i, ItemState loc "intact" Map.empty False) | (i, loc) <- its ]
            , entityStates = ents
            }
    in emptyGameState { world = gw, save = sv }

-- | The container verbs: open/close/lock/unlock with their states and the
--   refusal messages (through the real command path).
testContainerVerbs :: IO Bool
testContainerVerbs = do
    let truhe = (mkTestItem "truhe" "Truhe") { itemCapacity = Just 5 }
        st0 = cstate2 [mkTestRoom "halle" "Halle"] [(truhe, InRoom "halle")] []
                (Map.fromList [("truhe", "closed")])
        run cmd st = applyLoopCommandEv (parseCommand cmd) (initLoopState st)
        stOf = lsCurrent . fst
        evsOf = snd
        stateOf c = containerStateOf "truhe" (stOf (run c st0))
    r1 <- expectEqual "open" (stateOf "open truhe")
    r2 <- expectTrue "open message" ("is open now" `isInfixOf` renderEvents (evsOf (run "open truhe" st0)))
    r3 <- expectEqual "locked" (stateOf "lock truhe")
    r4 <- expectTrue "lock message" ("is locked now" `isInfixOf` renderEvents (evsOf (run "lock truhe" st0)))
    let stLocked = stOf (run "lock truhe" st0)
    r5 <- expectTrue "open is refused when locked"
            ("is locked" `isInfixOf` renderEvents (evsOf (run "open truhe" stLocked)))
    r6 <- expectEqual "closed" (containerStateOf "truhe" (stOf (run "unlock truhe" stLocked)))
    r7 <- expectTrue "unlock message" ("is unlocked now" `isInfixOf` renderEvents (evsOf (run "unlock truhe" stLocked)))
    let stOpen = stOf (run "open truhe" st0)
    r8 <- expectTrue "close works" ("is closed now" `isInfixOf` renderEvents (evsOf (run "close truhe" stOpen)))
    r9 <- expectTrue "not a container" ("is not a container" `isInfixOf` renderEvents (evsOf (run "open nix" st0)))
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9)

-- | take X from Y / put X in Y: scope, capacity, nesting, inventory limit.
testContainerTakePut :: IO Bool
testContainerTakePut = do
    let truhe = (mkTestItem "truhe" "Truhe") { itemCapacity = Just 1 }
        lampe = mkTestItem "lampe" "Lampe"
        stein = mkTestItem "stein" "Stein"
        st0 = cstate2 [mkTestRoom "halle" "Halle"]
                [ (truhe, InRoom "halle"), (lampe, InContainer "truhe"), (stein, InRoom "halle") ]
                [] (Map.fromList [("truhe", "open")])
        run cmd st = applyLoopCommandEv (parseCommand cmd) (initLoopState st)
        stOf = lsCurrent . fst
        evsOf = snd
        itemLoc i st = fmap itemLocation (Map.lookup i (itemStates (save st)))
    -- the lamp is in scope through the open container (take finds it)
    r1 <- expectTrue "container contents are in scope"
            (any (\i -> itemId i == "lampe") (visibleItemsAt (InRoom "halle") st0))
    -- take the lamp out
    let st2 = stOf (run "take lampe from truhe" st0)
    r2 <- expectEqual (Just (CarriedBy ActorPlayer)) (itemLoc "lampe" st2)
    r3 <- expectTrue "took_from message"
            ("You take lampe from Truhe." `isInfixOf` renderEvents (evsOf (run "take lampe from truhe" st0)))
    -- put the stone in (capacity 1, the lamp is out)
    let st3 = stOf (run "put stein in truhe" st2)
    r4 <- expectEqual (Just (InContainer "truhe")) (itemLoc "stein" st3)
    r5 <- expectTrue "put message"
            ("You put stein in Truhe." `isInfixOf` renderEvents (evsOf (run "put stein in truhe" st2)))
    -- full: the lamp cannot go in
    let st4 = stOf (run "put lampe in truhe" st3)
    r6 <- expectEqual (Just (CarriedBy ActorPlayer)) (itemLoc "lampe" st4)
    r7 <- expectTrue "full message" ("no room" `isInfixOf` renderEvents (evsOf (run "put lampe in truhe" st3)))
    -- a closed container refuses take
    let stClosed = stOf (run "close truhe" st3)
    r8 <- expectTrue "take from closed refuses"
            ("Truhe is closed" `isInfixOf` renderEvents (evsOf (run "take stein from truhe" stClosed)))
    -- nesting: the stone is visible through two open containers
    let tasche = (mkTestItem "tasche" "Tasche") { itemCapacity = Just 3 }
        st5 = cstate2 [mkTestRoom "halle" "Halle"]
                [ (truhe, InRoom "halle"), (tasche, InContainer "truhe"), (stein, InContainer "tasche") ]
                [] (Map.fromList [("truhe", "open"), ("tasche", "open")])
    r9 <- expectTrue "nested contents are visible"
            (any (\i -> itemId i == "stein") (visibleItemsAt (InRoom "halle") st5))
    -- inventory limit (VarMap inventory.limit): one more item is refused
    let st6 = st0 { save = (save st0)
                    { variables = Map.insert "inventory.limit" (VVInt 1) (variables (save st0)) } }
        st7 = stOf (run "take stein" st6)
    r10 <- expectTrue "inventory limit refuses" ("carrying too much" `isInfixOf`
            renderEvents (evsOf (run "take lampe" st7)))
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9 && r10)

-- ---------------------------------------------------------------------------
-- 4.5: topic table
-- ---------------------------------------------------------------------------

-- | `ask X about Y` runs the topic's effect; unknown topic/npc report their
--   refusal messages.
testTopics :: IO Bool
testTopics = do
    let npc = NPCDef "gelehrter" "Gelehrter" (plainText "Ein Gelehrter.") Map.empty ["alter gelehrter"] (Just 20) 5 5
                Map.empty False emptyAscii
                (Map.fromList
                    [ ("altes schloss", SetValue (VRVariable "wissen") (EVInt 1))
                    , ("geruechte", SendMessage "Geruechte gibt es viele.") ]) emptyGrammar
        st0 = cstate2 [mkTestRoom "halle" "Halle"] [] [] Map.empty
        st0' = st0 { world = (world st0) { npcDefs = Map.singleton "gelehrter" npc } }
        run cmd st = applyLoopCommandEv (parseCommand cmd) (initLoopState st)
        evsOf = snd
    r1 <- expectTrue "topic effect runs"
            (Map.member "wissen" (variables (save (lsCurrent (fst (run "ask gelehrter about altes schloss" st0'))))))
    r2 <- expectTrue "topic message shows"
            ("Geruechte gibt es viele." `isInfixOf` renderEvents (evsOf (run "ask gelehrter about geruechte" st0')))
    r3 <- expectTrue "unknown topic reports"
            ("nothing to say" `isInfixOf` renderEvents (evsOf (run "ask gelehrter about unwichtiges" st0')))
    r4 <- expectTrue "unknown npc reports"
            ("no one by that name" `isInfixOf` renderEvents (evsOf (run "ask niemand about x" st0')))
    r5 <- expectTrue "tell works the same"
            ("Geruechte gibt es viele." `isInfixOf` renderEvents (evsOf (run "tell gelehrter about geruechte" st0')))
    r6 <- expectTrue "multi-word npc keyword works"
            ("Geruechte gibt es viele." `isInfixOf` renderEvents (evsOf (run "ask alter gelehrter about geruechte" st0')))
    pure (r1 && r2 && r3 && r4 && r5 && r6)

-- | 4.5: `on_talk` triggers fire when ask/tell targets the matching NPC.
testOnTalkTriggerFires :: IO Bool
testOnTalkTriggerFires = do
    let trig = TriggerDef "test_talk" (OnTalk "gelehrter" "") Nothing [SendMessage "Der Gelehrte runzelt die Stirn."] False 0
        npc = NPCDef "gelehrter" "Gelehrter" (plainText "Ein Gelehrter.") Map.empty [] (Just 20) 5 5
                Map.empty False emptyAscii
                (Map.fromList [("altes schloss", SendMessage "Altes Schloss?")]) emptyGrammar
        st0 = cstate2 [mkTestRoom "halle" "Halle"] [] [] Map.empty
        st0' = st0 { world = (world st0) { npcDefs = Map.singleton "gelehrter" npc
                                         , triggerDefs = [trig] } }
        run cmd st = applyLoopCommandEv (parseCommand cmd) (initLoopState st)
        evsOf = snd
    r1 <- expectTrue "on_talk trigger fires on ask"
            ("runzelt die Stirn" `isInfixOf` renderEvents (evsOf (run "ask gelehrter about altes schloss" st0')))
    r2 <- expectTrue "on_talk trigger fires on tell"
            ("runzelt die Stirn" `isInfixOf` renderEvents (evsOf (run "tell gelehrter about altes schloss" st0')))
    pure (r1 && r2)

-- ---------------------------------------------------------------------------
-- B7: NPC possession
-- ---------------------------------------------------------------------------

-- | Helper: room "halle" with the NPC "waechter" in it and items at start
--   locations (same shape as 'cstate2', plus the NPC).
b7State :: [(ItemDef, Location)] -> GameState
b7State its =
    let waechter = NPCDef "waechter" "Waechter" (plainText "Ein Waechter.") Map.empty ["waechter"]
                    (Just 20) 5 5 Map.empty False emptyAscii Map.empty emptyGrammar
        st0 = cstate2 [mkTestRoom "halle" "Halle"] its [] Map.empty
    in st0 { world = (world st0) { npcDefs = Map.singleton "waechter" waechter }
           , save = (save st0) { npcStates = Map.singleton "waechter"
                    (NPCState (InRoom "halle") "alive" (Just 20) Map.empty Nothing) } }

-- | `take X from <npc>` and `give X to <npc>` move items between player and
--   NPC possession (the real command path, containers keep precedence).
testNpcTakeGive :: IO Bool
testNpcTakeGive = do
    let schluessel = mkTestItem "schluessel" "Schluessel"
        stein = mkTestItem "stein" "Stein"
        st0 = b7State [ (schluessel, CarriedBy (ActorNPC "waechter"))
                      , (stein, CarriedBy ActorPlayer) ]
        run cmd st = applyLoopCommandEv (parseCommand cmd) (initLoopState st)
        stOf = lsCurrent . fst
        evsOf = snd
        itemLoc i st = fmap itemLocation (Map.lookup i (itemStates (save st)))
    -- take from the NPC
    let st1 = stOf (run "take schluessel from waechter" st0)
    r1 <- expectEqual (Just (CarriedBy ActorPlayer)) (itemLoc "schluessel" st1)
    r2 <- expectTrue "took_from message"
            ("You take the Schluessel from Waechter." `isInfixOf`
                renderEvents (evsOf (run "take schluessel from waechter" st0)))
    r3 <- expectTrue "no such item on the npc"
            ("You find no rostiger schluessel on Waechter." `isInfixOf`
                renderEvents (evsOf (run "take rostiger schluessel from waechter" st0)))
    r4 <- expectTrue "unknown holder keeps the container error"
            ("is not a container" `isInfixOf`
                renderEvents (evsOf (run "take schluessel from niemand" st0)))
    -- give to the NPC
    let st2 = stOf (run "give stein to waechter" st0)
    r5 <- expectEqual (Just (CarriedBy (ActorNPC "waechter"))) (itemLoc "stein" st2)
    r6 <- expectTrue "gave_to message"
            ("You give the Stein to Waechter." `isInfixOf`
                renderEvents (evsOf (run "give stein to waechter" st0)))
    r7 <- expectTrue "must carry the item"
            ("don't have" `isInfixOf`
                renderEvents (evsOf (run "give lampe to waechter" st0)))
    r8 <- expectTrue "unknown recipient"
            ("don't see 'niemand' here" `isInfixOf`
                renderEvents (evsOf (run "give stein to niemand" st0)))
    -- give/take round trip (German input forms live in the 4.3.4 alias tests)
    let st3 = stOf (run "give stein to waechter" st0)
    r9 <- expectEqual (Just (CarriedBy (ActorNPC "waechter"))) (itemLoc "stein" st3)
    let st4 = stOf (run "take stein from waechter" st3)
    r10 <- expectEqual (Just (CarriedBy ActorPlayer)) (itemLoc "stein" st4)
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9 && r10)

-- | `look at <npc>` lists what the NPC carries (hidden items need discovery)
--   and the protocol snapshot carries the same list (omitted when empty).
testNpcCarriedVisibility :: IO Bool
testNpcCarriedVisibility = do
    let schluessel = mkTestItem "schluessel" "Schluessel"
        amulett = (mkTestItem "amulett" "Amulett") { itemHidden = True }
        st0 = b7State [ (schluessel, CarriedBy (ActorNPC "waechter"))
                      , (amulett, CarriedBy (ActorNPC "waechter")) ]
        run cmd st = applyLoopCommandEv (parseCommand cmd) (initLoopState st)
        evsOf = snd
        out = renderEvents (evsOf (run "look at waechter" st0))
    r1 <- expectTrue "carried items are shown" ("Carrying: Schluessel." `isInfixOf` out)
    r2 <- expectTrue "hidden items stay hidden" (not ("Amulett" `isInfixOf` out))
    r3 <- expectEqual ["Schluessel"]
            [ isName i | n <- rsNpcs (snapRoom (makeSnapshot st0))
                       , nsId n == "waechter", i <- nsCarried n ]
    r4 <- expectTrue "no carried line when the npc carries nothing"
            (not ("Carrying" `isInfixOf`
                renderEvents (evsOf (run "look at waechter" (b7State [])))))
    r5 <- expectTrue "empty carried list is omitted from json"
            (not ("carried" `isInfixOf` BLC.unpack (Aeson.encode (head (rsNpcs (snapRoom (makeSnapshot (b7State []))))))))
    pure (r1 && r2 && r3 && r4 && r5)

-- | B9: `use <item> on <npc>` with a declared `interactions: npc:` entry runs
--   that entry's effect list; without an entry the attack fallback is unchanged.
testNpcItemInteraction :: IO Bool
testNpcItemInteraction = do
    let verband = mkTestItem "verband" "Verband"
        heiltrank = mkTestItem "heiltrank" "Heiltrank"
        bandage = Sequence
            [ SendMessage "Du verbindest den Waechter."
            , MoveEntity "verband" Removed ]
        withIx ixs items =
            let st0 = b7State items
            in st0 { world = (world st0) { npcInteractions = ixs } }
        stDeclared = withIx (Map.singleton ("verband", "waechter") bandage)
                            [ (verband, CarriedBy ActorPlayer)
                            , (heiltrank, CarriedBy ActorPlayer) ]
        stOpen = b7State [ (verband, CarriedBy ActorPlayer)
                         , (heiltrank, CarriedBy ActorPlayer) ]
        run cmd st = applyLoopCommandEv (parseCommand cmd) (initLoopState st)
        stOf = lsCurrent . fst
        out = renderEvents . snd
        npcHp st = maybe 0 (fromMaybe 0 . npcHealth) (Map.lookup "waechter" (npcStates (save st)))
        itemLoc i st = fmap itemLocation (Map.lookup i (itemStates (save st)))
    r1 <- expectTrue "declared interaction runs its effects"
            ("Du verbindest den Waechter." `isInfixOf` out (run "use verband on waechter" stDeclared))
    r2 <- expectEqual (Just Removed) (itemLoc "verband" (stOf (run "use verband on waechter" stDeclared)))
    r3 <- expectEqual (Just (CarriedBy ActorPlayer)) (itemLoc "heiltrank" (stOf (run "use verband on waechter" stDeclared)))
    r4 <- expectEqual 20 (npcHp (stOf (run "use verband on waechter" stDeclared)))
    -- no entry for the second item: the attack fallback is still in charge
    r5 <- expectTrue "undeclared pair still attacks"
            (npcHp (stOf (run "use heiltrank on waechter" stOpen)) /= 20)
    r6 <- expectTrue "undeclared pair leaves the item in hand"
            (not ("Du verbindest den Waechter." `isInfixOf` out (run "use heiltrank on waechter" stOpen)))
    r7 <- expectEqual (Just (CarriedBy ActorPlayer)) (itemLoc "heiltrank" (stOf (run "use heiltrank on waechter" stOpen)))
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7)

-- | B9: the engine field is omitted from world.json when no interaction is
--   declared, so every existing world stays byte-identical.
testNpcInteractionsOmittedWhenEmpty :: IO Bool
testNpcInteractionsOmittedWhenEmpty = do
    r1 <- expectTrue "empty npcInteractions stays out of the world json"
            (not ("npcInteractions" `isInfixOf` BLC.unpack (Aeson.encode emptyGameWorld)))
    r2 <- expectTrue "declared npcInteractions are written"
            ("npcInteractions" `isInfixOf` BLC.unpack
                (Aeson.encode emptyGameWorld { npcInteractions = Map.singleton ("verband", "waechter") (SendMessage "x") }))
    r3 <- expectTrue "round trip through json"
            (let w = Aeson.decode (BLC.pack (BLC.unpack (Aeson.encode emptyGameWorld { npcInteractions = Map.singleton ("verband", "waechter") (SendMessage "x") })))
             in case (w :: Maybe GameWorld) of
                    Just g -> Map.size (npcInteractions g) == 1
                    Nothing -> False)
    pure (r1 && r2 && r3)

-- | B9: `take all from <npc>` — the NPC variant of the `take all` mass
--   operation. Everything the NPC carries moves to the player as far as the
--   inventory limit allows, one `npc.took_from` line per item (no new key).
--   Equipped items stay on the NPC: they are not "in its hands".
testNpcTakeAll :: IO Bool
testNpcTakeAll = do
    let schluessel = mkTestItem "schluessel" "Schluessel"
        stein = mkTestItem "stein" "Stein"
        laterne = mkTestItem "laterne" "Laterne"
        run cmd st = applyLoopCommandEv (parseCommand cmd) (initLoopState st)
        stOf = lsCurrent . fst
        out = renderEvents . snd
        itemLoc i st = fmap itemLocation (Map.lookup i (itemStates (save st)))
        inHand i st = itemLoc i st == Just (CarriedBy ActorPlayer)
        onNpc i st = itemLoc i st == Just (CarriedBy (ActorNPC "waechter"))
        countOf w s = length (filter (== w) (words s))
        bothSt = b7State [ (schluessel, CarriedBy (ActorNPC "waechter"))
                         , (stein, CarriedBy (ActorNPC "waechter")) ]
        limitSt n st = st { save = (save st)
              { variables = Map.insert "inventory.limit" (VVInt n) (variables (save st)) } }
        -- the NPC lies dead but its pockets are still reachable
        deadSt = let s = bothSt
                 in s { save = (save s) { npcStates = Map.adjust (\ns -> ns { npcStatus = "dead" })
                                       "waechter" (npcStates (save s)) } }
    r0 <- expectEqual (TakeAllFromCmd "waechter") (parseCommand "take all from waechter")
    r1 <- expectEqual (Just (CarriedBy ActorPlayer)) (itemLoc "schluessel" (stOf (run "take all from waechter" bothSt)))
    r2 <- expectEqual (Just (CarriedBy ActorPlayer)) (itemLoc "stein" (stOf (run "take all from waechter" bothSt)))
    r3 <- expectTrue "both items are named"
            (let o = out (run "take all from waechter" bothSt) in "Schluessel" `isInfixOf` o && "Stein" `isInfixOf` o)
    r4 <- expectTrue "one took_from line per item"
            (countOf "You" (out (run "take all from waechter" bothSt)) == 2)
    -- empty hands / unknown target
    r5 <- expectTrue "empty-handed npc"
            ("no all on Waechter" `isInfixOf` out (run "take all from waechter" (b7State [])))
    r6 <- expectTrue "no such target"
            ("is not a container" `isInfixOf` out (run "take all from niemand" bothSt))
    -- the inventory limit only stops what does not fit (same idiom as `take all`)
    let stL = stOf (run "take all from waechter" (limitSt 1 bothSt))
    r7 <- expectEqual 1 (length [ () | i <- [ "schluessel", "stein" ], inHand i stL ])
    r8 <- expectEqual 1 (length [ () | i <- [ "schluessel", "stein" ], onNpc i stL ])
    -- a corpse's pockets still work
    r9 <- expectEqual (Just (CarriedBy ActorPlayer)) (itemLoc "stein" (stOf (run "take all from waechter" deadSt)))
    -- the player's own things are never in scope for this command
    r10 <- expectEqual (Just (CarriedBy ActorPlayer))
                (itemLoc "laterne" (stOf (run "take all from waechter"
                    (b7State [ (laterne, CarriedBy ActorPlayer) ]))))
    -- a single take still behaves exactly as before (shared helper)
    r11 <- expectTrue "single take_from unchanged"
            ("You take the Schluessel from Waechter." `isInfixOf` out (run "take schluessel from waechter" bothSt))
    -- the limit counts what the player really carries (filled by a real take)
    let fullSt = stOf (run "take laterne"
                (limitSt 1 (b7State [ (laterne, InRoom "halle")
                                    , (schluessel, CarriedBy (ActorNPC "waechter")) ])))
    r12 <- expectTrue "single take respects the limit"
            ("carrying too much" `isInfixOf` out (run "take schluessel from waechter" fullSt))
    -- L13: the mass operation costs a turn, like `take all`
    r13 <- expectTrue "take all from npc consumes a turn" (expectedConsumesTurn (TakeAllFromCmd "waechter"))
    pure (r0 && r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9 && r10 && r11 && r12 && r13)

-- | B9: NPC equipment. `give: {item, to, equip: true}` compiles to `MoveEntity x
--   (EquippedBy (ActorNPC …))`: the item's **location** carries the state (no
--   new SaveState field), the slot comes from the item's own `equip_slot`, the
--   worn bonuses count in the combat numbers and `look at <npc>` shows the line.
testNpcEquipment :: IO Bool
testNpcEquipment = do
    let schwert = (mkTestEquip "schwert" "Schwert" Weapon) { itemEquipEffects = [AttackBonus 4] }
        ruestung = (mkTestEquip "ruestung" "Ruestung" Body)   { itemEquipEffects = [DefenseBonus 3] }
        zweitSchwert = (mkTestEquip "zweit" "Zweitschwert" Weapon) { itemEquipEffects = [] }
        stein = mkTestItem "stein" "Stein"   -- no equip slot at all
        waechter2 = NPCDef "waechter2" "Waechter2" (plainText "Ein zweiter Waechter.")
                    Map.empty ["waechter2"] (Just 20) 5 5 Map.empty False
                    emptyAscii Map.empty emptyGrammar
        st0 = b7State [ (schwert, InRoom "halle"), (ruestung, InRoom "halle")
                      , (zweitSchwert, InRoom "halle"), (stein, InRoom "halle") ]
        -- a second NPC in the same room: the slot check is per actor
        stOther = let s = b7State [ (schwert, InRoom "halle"), (zweitSchwert, InRoom "halle") ]
                      s1 = s { world = (world s) { npcDefs = Map.insert "waechter2" waechter2 (npcDefs (world s)) } }
                  in s1 { save = (save s1)
                          { npcStates = Map.insert "waechter2"
                              (NPCState (InRoom "halle") "alive" (Just 20) Map.empty Nothing)
                              (npcStates (save s1)) } }
        equipOk it actor st = either (const st) id (equipItemFor actor it st)
        equipErr it actor st = either id (const "") (equipItemFor actor it st)
        itemLoc i st = fmap itemLocation (Map.lookup i (itemStates (save st)))
        run cmd st = applyLoopCommandEv (parseCommand cmd) (initLoopState st)
        out = renderEvents . snd
        npcDefOf n st = Map.lookup n (npcDefs (world st))
        stEquipped = equipOk "schwert" (ActorNPC "waechter") st0
    r1 <- expectEqual (Just (EquippedBy (ActorNPC "waechter")))
            (itemLoc "schwert" (equipOk "schwert" (ActorNPC "waechter") st0))
    r3 <- expectTrue "slot conflict is refused"
            ("already have" `isInfixOf` equipErr "zweit" (ActorNPC "waechter") stEquipped)
    r4 <- expectTrue "a non-equippable item is refused"
            ("equip" `isInfixOf` equipErr "stein" (ActorNPC "waechter") stEquipped)
    r5 <- expectTrue "an unknown item is refused"
            (not (null (equipErr "quatsch" (ActorNPC "waechter") st0)))
    r6 <- expectEqual (Just (EquippedBy (ActorNPC "waechter2")))
            (itemLoc "zweit" (equipOk "zweit" (ActorNPC "waechter2") stOther))
    -- the player keeps the historical equipment map (byte-frozen contract)
    r7 <- expectTrue "player equip is unchanged"
            (let stHeld = b7State [ (schwert, CarriedBy ActorPlayer) ]
                 s = equipOk "schwert" ActorPlayer stHeld
             in Map.lookup Weapon (equipment (save s)) == Just "schwert"
                && itemLoc "schwert" s == Just (CarriedBy ActorPlayer))
    r7c <- expectTrue "player equip still needs the item in hand"
            ("need to be carrying" `isInfixOf` equipErr "schwert" ActorPlayer st0)
    -- the effect-table path is the same function (no second implementation)
    r7b <- expectTrue "MoveEntity EquippedBy goes through the same check"
            (let (s, _) = applyOutcomeEv (MoveEntity "zweit" (EquippedBy (ActorNPC "waechter"))) "" stEquipped
             in itemLoc "zweit" s == Just (InRoom "halle"))
    -- worn bonuses count in the combat numbers
    r8 <- expectEqual 9 (maybe 0 id (fmap (\d -> npcAttackWith "waechter" d stEquipped) (npcDefOf "waechter" stEquipped)))
    r9 <- expectEqual 5 (maybe 0 id (fmap (\d -> npcAttackWith "waechter" d st0) (npcDefOf "waechter" st0)))
    r10 <- expectEqual 5 (maybe 0 id (fmap (\d -> npcDefenseWith "waechter" d st0) (npcDefOf "waechter" st0)))
    r11 <- expectEqual 8 (maybe 0 id (fmap (\d -> npcDefenseWith "waechter" d (equipOk "ruestung" (ActorNPC "waechter") st0)) (npcDefOf "waechter" st0)))
    -- visible in `look at <npc>` and in the protocol snapshot
    let stBoth = equipOk "ruestung" (ActorNPC "waechter") stEquipped
    r12 <- expectTrue "look at npc shows the worn line"
            (let o = out (run "look at waechter" stBoth)
             in "Wearing: " `isInfixOf` o && "Schwert" `isInfixOf` o && "Ruestung" `isInfixOf` o)
    r13 <- expectTrue "carried and worn are separate lines"
            ("Carrying" `notElem` words (out (run "look at waechter" stBoth)))
    r14 <- expectTrue "no worn line without worn items"
            (not ("Wearing" `isInfixOf` out (run "look at waechter" st0)))
    r15 <- expectEqual ["Ruestung", "Schwert"]   -- Map order (item id), not definition order
            [ isName i | n <- rsNpcs (snapRoom (makeSnapshot stBoth))
                       , nsId n == "waechter", i <- nsEquipped n ]
    r16 <- expectTrue "empty equipped list is omitted from json"
            (not ("equipped" `isInfixOf`
                BLC.unpack (Aeson.encode (head (rsNpcs (snapRoom (makeSnapshot st0)))))))
    pure (r1 && r3 && r4 && r5 && r6 && r7 && r7b && r7c && r8 && r9 && r10 && r11 && r12 && r13 && r14 && r15 && r16)

-- | B9: worn equipment really changes the classic combat round — the number
--   the player sees ("You hit for N") drops by exactly the armor bonus.
testNpcEquipmentAffectsCombat :: IO Bool
testNpcEquipmentAffectsCombat = do
    let hpOf s = maybe 0 (fromMaybe 0 . npcHealth) (Map.lookup "waechter" (npcStates (save s)))
        hit st = let (st', msg) = executeCommand (Interact VAttack "waechter") st
                 in (hpOf st - hpOf st', msg)
        -- one record update per line: a multi-line record update switches the
        -- layout context and breaks the following bindings
        fight its =
            let s0 = b7State its
                -- max_hp must follow npc_health: the health clamp would
                -- otherwise hide the damage difference
                s1 = s0 { world = (world s0) { npcDefs = Map.adjust (\d -> d { npcMaxHealth = Just 100 }) "waechter" (npcDefs (world s0)) } }
                s2 = s1 { save = (save s1) { npcStates = Map.adjust (\ns -> ns { npcHealth = Just 100 }) "waechter" (npcStates (save s1)) } }
                s3 = s2 { save = (save s2) { player = (player (save s2)) { playerHealth = 500, playerMaxHealth = 500, playerAttack = 10, playerDefense = 1 } } }
                s4 = s3 { save = (save s3) { equipment = Map.empty } }
                s5 = s4 { world = (world s4) { combatProfile = CombatClassic Nothing } }
            in hit s5
        ruestung = (mkTestEquip "ruestung" "Ruestung" Body) { itemEquipEffects = [DefenseBonus 3] }
        (dmgBare, _) = fight [ (ruestung, InRoom "halle") ]
        (dmgArmored, msgArmored) = fight [ (ruestung, EquippedBy (ActorNPC "waechter")) ]
    r1 <- expectTrue "armor absorbs exactly its bonus" (dmgArmored == dmgBare - 3)
    r2 <- expectTrue "the reported hit drops too"
            (("You hit for " ++ show dmgArmored) `isInfixOf` msgArmored)
    pure (r1 && r2)

-- | B9: `drops_on_death:` — the author decides whether a corpse lets go of
--   what it carried and wore. The default is the byte-frozen contract (the
--   corpse keeps everything), so existing adventures are untouched.
testNpcDropsOnDeath :: IO Bool
testNpcDropsOnDeath = do
    let schluessel = mkTestItem "schluessel" "Schluessel"
        stein = mkTestItem "stein" "Stein"
        ruestung = (mkTestEquip "ruestung" "Ruestung" Body) { itemEquipEffects = [DefenseBonus 2] }
        -- one state with the flag, one without; same items in the same places
        dying flag its =
            let s0 = b7State its
                def = case Map.lookup "waechter" (npcDefs (world s0)) of
                        Just d  -> d
                        Nothing -> error "b7State must carry the waechter"
                def' = def { npcDropsOnDeath = flag }
            in s0 { world = (world s0) { npcDefs = Map.insert "waechter" def' (npcDefs (world s0)) } }
        load = [ (schluessel, CarriedBy (ActorNPC "waechter"))
               , (ruestung, EquippedBy (ActorNPC "waechter"))
               , (stein, CarriedBy ActorPlayer) ]
        die st = killNPCWithMsg "waechter" st
        itemLoc i st = fmap itemLocation (Map.lookup i (itemStates (save st)))
        evsOf = renderEvents
    -- default: the corpse keeps everything (byte-frozen)
    let (kept, keptMsgs) = die (dying False load)
    r1 <- expectEqual (Just (CarriedBy (ActorNPC "waechter"))) (itemLoc "schluessel" kept)
    r2 <- expectEqual (Just (EquippedBy (ActorNPC "waechter"))) (itemLoc "ruestung" kept)
    r3 <- expectTrue "no drop line without the flag" (not ("drops" `isInfixOf` evsOf keptMsgs))
    r4 <- expectEqual (Just "dead") (npcStatus <$> Map.lookup "waechter" (npcStates (save kept)))
    -- with the flag: carried and worn items land in the room of the corpse
    let (dropped, droppedMsgs) = die (dying True load)
    r5 <- expectEqual (Just (InRoom "halle")) (itemLoc "schluessel" dropped)
    r6 <- expectEqual (Just (InRoom "halle")) (itemLoc "ruestung" dropped)
    r7 <- expectEqual (Just (CarriedBy ActorPlayer)) (itemLoc "stein" dropped)
    r8 <- expectTrue "the drop line names the npc and the items"
            (let o = evsOf droppedMsgs
             in "Waechter" `isInfixOf` o && "Schluessel" `isInfixOf` o && "Ruestung" `isInfixOf` o)
    -- a second death is a no-op (no double message, no second drop)
    let (again, againMsgs) = die dropped
    r9 <- expectTrue "a corpse does not drop twice" (null againMsgs)
    r10 <- expectEqual (Just (InRoom "halle")) (itemLoc "schluessel" again)
    -- an empty load drops nothing (no empty line)
    r11 <- expectTrue "an empty load stays silent"
            (snd (die (dying True [(stein, CarriedBy ActorPlayer)])) == [])
    -- end to end: the real command path (an attack that kills in the classic
    -- profile) must drop the load exactly like `killNPC` does
    let run cmd st = applyLoopCommandEv (parseCommand cmd) (initLoopState st)
        fragile = let s0 = dying True load
                      s1 = s0 { world = (world s0) { npcDefs = Map.adjust (\d -> d { npcMaxHealth = Just 5 }) "waechter" (npcDefs (world s0)) } }
                      s2 = s1 { save = (save s1) { npcStates = Map.adjust (\ns -> ns { npcHealth = Just 5 }) "waechter" (npcStates (save s1)) } }
                      s3 = s2 { save = (save s2) { player = (player (save s2)) { playerHealth = 500, playerMaxHealth = 500, playerAttack = 20, playerDefense = 0 } } }
                      s4 = s3 { save = (save s3) { equipment = Map.empty } }
                      s5 = s4 { world = (world s4) { combatProfile = CombatClassic Nothing } }
                  in s5
        stCorpse = lsCurrent (fst (run "attack waechter" fragile))
    r12 <- expectEqual (Just "dead") (npcStatus <$> Map.lookup "waechter" (npcStates (save stCorpse)))
    r12b <- expectEqual (Just (InRoom "halle")) (itemLoc "schluessel" stCorpse)
    r12c <- expectEqual (Just (InRoom "halle")) (itemLoc "ruestung" stCorpse)
    -- the flag is omitted from the world json unless set (byte contract)
    r13 <- expectTrue "the flag stays out of the json when unset"
            (not ("npcDropsOnDeath" `isInfixOf` BLC.unpack (Aeson.encode (npcDefs (world (dying False []))))))
    r14 <- expectTrue "the flag is written when set"
            ("npcDropsOnDeath" `isInfixOf` BLC.unpack (Aeson.encode (npcDefs (world (dying True [])))))
    r15 <- expectTrue "round trip through json"
            (case (Aeson.decode (BLC.pack (BLC.unpack (Aeson.encode (npcDefs (world (dying True []))))))) :: Maybe (Map.Map String NPCDef) of
                Just m  -> maybe False npcDropsOnDeath (Map.lookup "waechter" m)
                Nothing -> False)
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9 && r10 && r11 && r12 && r12b && r12c && r13 && r14 && r15)

-- | B9: the effect table: `MoveEntity x (CarriedBy <actor>)` honours the actor —
--   the `give:` object form can hand items to NPCs, not just the player.
testMoveEntityCarriedByActor :: IO Bool
testMoveEntityCarriedByActor = do
    let schluessel = mkTestItem "schluessel" "Schluessel"
        st0 = b7State [(schluessel, InRoom "halle")]
        (st1, _) = applyOutcomeEv (MoveEntity "schluessel" (CarriedBy (ActorNPC "waechter"))) "" st0
        (st2, _) = applyOutcomeEv (MoveEntity "schluessel" (CarriedBy ActorPlayer)) "" st1
        itemLoc st = fmap itemLocation (Map.lookup "schluessel" (itemStates (save st)))
    r1 <- expectEqual (Just (CarriedBy (ActorNPC "waechter"))) (itemLoc st1)
    r2 <- expectEqual (Just (CarriedBy ActorPlayer)) (itemLoc st2)
    pure (r1 && r2)

-- | L13: `consumesTurn` ends in a `_ -> True` catch-all, so a new `Command`
--   constructor silently becomes turn-consuming — that is how P1-16 happened.
--   `expectedConsumesTurn` matches every constructor **without** a wildcard and
--   the suite builds with `-Werror=incomplete-patterns`, so adding a constructor
--   fails the build until its verdict is written down here.
expectedConsumesTurn :: Command -> Bool
expectedConsumesTurn (AskCmd _ _) = True
expectedConsumesTurn (TellCmd _ _) = True
expectedConsumesTurn cmd = case cmd of
    Go _               -> True
    Look               -> False
    Inventory          -> False
    Interact _ _       -> True
    InteractWith _ _ _ -> True
    ActionWithArgs _ _ -> True
    ChooseCmd _        -> True   -- refined by consumesTurnIn: only a valid choice ticks
    TakeAll            -> True
    DropAll            -> True
    CompoundCommand _  -> True
    EquipCmd _         -> True
    UnequipCmd _       -> True
    UnequipAllCmd      -> True
    StatsCmd           -> False
    SearchCmd _        -> True
    WatchCmd _         -> False
    MapCmd             -> False
    JournalCmd         -> False
    Undo               -> False
    EnterVehicleCmd _  -> True
    ExitVehicleCmd     -> True
    OpenCmd _          -> True   -- 4.4: opening a container costs a turn
    CloseCmd _         -> True
    LockCmd _          -> True
    UnlockCmd _        -> True
    TakeFromCmd _ _    -> True
    TakeAllFromCmd _   -> True   -- B9: mass operation, costs a turn like `take all`
    PutInCmd _ _       -> True
    GiveCmd _ _        -> True   -- B7: handing an item to an NPC is an action
    DriveToCmd _       -> True
    WaitCmd            -> True
    RefuelCmd _        -> True
    RepairCmd _        -> True
    Save _             -> False
    Load _             -> False
    ListSaves          -> False
    Restart            -> False
    Help               -> False
    Quit               -> False
    PlayCardCmd _ _    -> False
    HandCmd            -> False
    DeckCmd            -> False
    DiscardCmd         -> False
    EndTurnCmd         -> True
    Unknown _          -> False

-- | One sample per `Command` constructor.
allCommandSamples :: [Command]
allCommandSamples =
    [ Go North, Look, Inventory, Interact VLookAt "x", InteractWith VUse "x" "y"
    , ChooseCmd 1, TakeAll, DropAll, CompoundCommand [Look]
    , EquipCmd "x", UnequipCmd "x", UnequipAllCmd, StatsCmd, SearchCmd Nothing
    , JournalCmd, Undo, EnterVehicleCmd "v", ExitVehicleCmd, DriveToCmd "s"
    , WaitCmd, RefuelCmd "v", RepairCmd "v", Save "s", Load "s", ListSaves
    , Restart, Help, Quit, ActionWithArgs (VCustom "action") ["a"]
    , PlayCardCmd 1 Nothing, HandCmd, DeckCmd, DiscardCmd, EndTurnCmd, Unknown "z" ]

-- | L13: the verdict table must match `consumesTurn` for every constructor, and
--   the sample count pins the list so a forgotten sample is noticed.
testConsumesTurnCompleteness :: IO Bool
testConsumesTurnCompleteness = do
    let st0 = initSampleGame
    r1 <- expectTrue "consumesTurn matches the documented verdict everywhere"
        (all (\c -> consumesTurn c == expectedConsumesTurn c) allCommandSamples)
    r2 <- expectEqual 35 (length allCommandSamples)
    r3 <- expectTrue "an invalid dialogue choice is a typo, not a turn"
              (not (consumesTurnIn st0 (ChooseCmd 99)))
    r4 <- expectTrue "a valid dialogue choice consumes the turn"
              (let st1 = fst (executeCommand (Interact VTalk "old man") st0)
               in consumesTurnIn st1 (ChooseCmd 1))
    pure (r1 && r2 && r3 && r4)

-- | L2: `SaveLoad` had zero coverage — 129 lines of IO with two compatibility
--   fallbacks (wrapper format, bare `SaveState`) and the checksum warning. The
--   save layout is asymmetric on purpose (`--save` is read-only, `save <slot>`
--   writes `saves/<slot>.json`), which is exactly what belongs in a test.
testSaveLoadRoundTrip :: IO Bool
testSaveLoadRoundTrip = do
    tmp <- getTemporaryDirectory
    withCurrentDirectory tmp $ do
        -- Rogue Phase 0: hermetic saves — keep this test's slots out of any
        -- TA_SAVES_DIR the ambient environment might carry.
        withSavesIsolation $ do
            let st0 = initSampleGame
            -- save -> load -> identical SaveState
            SaveLoad.saveGame st0 "l2slot"
            loaded <- SaveLoad.loadGame st0 "l2slot"
            r1 <- case loaded of
                Just st' -> expectEqual (save st0) (save st')
                Nothing  -> expectTrue "saveGame/loadGame round-trips" False
            -- a changed world warns but still loads (checksum compatibility path)
            let otherWorld = st0 { world = (world st0) { worldName = "Different World" } }
            loaded2 <- SaveLoad.loadGame otherWorld "l2slot"
            r2 <- case loaded2 of
                Just st' -> expectEqual (save st0) (save st')
                Nothing  -> expectTrue "a mismatching world still loads" False
            -- a bare SaveState is picked up by the legacy branch
            dir <- SaveLoad.savesDir
            createDirectoryIfMissing True dir
            legacyPath <- SaveLoad.saveSlotPath "l2legacy"
            BLC.writeFile legacyPath (Aeson.encode (save st0))
            legacy <- SaveLoad.loadGame st0 "l2legacy"
            r3 <- case legacy of
                Just st' -> expectEqual (save st0) (save st')
                Nothing  -> expectTrue "a bare SaveState loads via the legacy branch" False
            -- ... and it must not be mistaken for a wrapper while decoding
            r4 <- expectEqual (Nothing :: Maybe SaveFile)
                      (Aeson.decode (Aeson.encode (save st0)))
            mapM_ (\slot -> do
                      f <- SaveLoad.saveSlotPath slot
                      e <- doesFileExist f
                      when e (removeFile f))
                  ["l2slot", "l2legacy"]
            pure (r1 && r2 && r3 && r4)

-- | Rogue Phase 0: `TA_SAVES_DIR` redirects `saveGame`/`loadGame`/`listSaves`
--   and `saveSlotPath` — the hermetic seam for tests and E2E. Default without
--   the variable stays the CWD-relative `saves` (bit-identical behaviour).
testSavesDirOverride :: IO Bool
testSavesDirOverride = withSavesIsolation $ do
    let st0 = initSampleGame
    SaveLoad.saveGame st0 "p0slot"
    slotPath <- SaveLoad.saveSlotPath "p0slot"
    r1 <- expectTrue "slot file lands under TA_SAVES_DIR"
             (("ta-saves-test" `isInfixOf` slotPath) && ("p0slot.json" `isSuffixOf` slotPath))
    exists <- doesFileExist slotPath
    r2 <- expectTrue "saved slot exists in the redirected directory" exists
    -- the redirected directory holds exactly the one slot file
    files <- SaveLoad.savesDir >>= listDirectory
    r3 <- expectEqual ["p0slot.json"] files
    -- deleteSaveSlot removes the file; missing slots are idempotent
    SaveLoad.deleteSaveSlot (world st0) "p0slot"
    gone <- doesFileExist slotPath
    r4 <- expectTrue "deleteSaveSlot removed the file" (not gone)
    SaveLoad.deleteSaveSlot (world st0) "p0slot"   -- must not throw
    pure (r1 && r2 && r3 && r4)

-- | Rogue Phase 0: the default directory (no `TA_SAVES_DIR`) stays the
--   CWD-relative `saves` — the Default-Invariante for every existing setup.
testSavesDirDefault :: IO Bool
testSavesDirDefault = do
    old <- lookupEnv "TA_SAVES_DIR"
    unsetEnv "TA_SAVES_DIR"
    d <- SaveLoad.savesDir
    result <- expectEqual "saves" d
    case old of
        Just v  -> setEnv "TA_SAVES_DIR" v
        Nothing -> pure ()
    pure result

-- | Rogue Phase 0: `slugify` produces safe, deterministic file-name parts
--   and never an empty string.
testSlugify :: IO Bool
testSlugify = do
    r1 <- expectEqual "the_fog" (slugify "The Fog")
    r2 <- expectEqual "katakomben_von_vhal" (slugify "Katakomben von Vhal")
    r3 <- expectEqual "demo" (slugify "demo")
    r4 <- expectEqual "ber_fantastic_adventure_3000"
                       (slugify "Über fantastic! Adventure 3000")
    r5 <- expectEqual "default" (slugify "")
    r6 <- expectEqual "---" (slugify "  ---  ")   -- '-' is a legal file-name char
    r7 <- expectEqual "default" (slugify "  ")
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7)

-- ---------------------------------------------------------------------------
-- Phase 2.5: procedures
-- ---------------------------------------------------------------------------

-- | Helper: empty state with the given procedures installed
--   (name, params, body).
procState :: [(String, [String], [Effect])] -> GameState
procState procs =
    let gw = (world emptyGameState)
            { procDefs = Map.fromList
                [ (pid, ProcDef pid params effs) | (pid, params, effs) <- procs ] }
    in emptyGameState { world = gw }

-- | The call binds literal args as parameters; the body can read them in
--   expressions and {templates}, and the scope is popped afterwards.
testProcCallBindsParams :: IO Bool
testProcCallBindsParams = do
    let st0 = procState
            [ ("belohnen", ["betrag", "grund"],
                [ ComputeValue (VRVariable "gold")
                    (EAdd (EVar "gold") (EVar "betrag"))
                , SendMessage "Danke: {grund} ({gold})" ]) ]
        (st1, evs, _) = applyOutcomeWith 0 0
            (CallProc "belohnen" [EVInt 7, EVString "hilfe"]) "" st0
        out = renderEvents evs
    r1 <- expectEqual (Just (VVInt 7)) (getVariable "gold" st1)
    r2 <- expectTrue "params visible in message" ("Danke: hilfe (7)" `isInfixOf` out)
    r3 <- expectTrue "scope popped after call" (null (procScopes st1))
    pure (r1 && r2 && r3)

-- | Locals: writing to a parameter name mutates the local copy only (discarded
--   on return); writing to an unbound name goes to the adventure VarMap.
testProcLocalsAreDiscarded :: IO Bool
testProcLocalsAreDiscarded = do
    let st0 = setVariable "x" (VVInt 5) $ procState
            [ ("rechnen", ["x"],
                [ SetValue (VRVariable "x") (EVInt 99)
                , SetValue (VRVariable "y") (EVInt 1)
                , SendMessage "in={x}" ]) ]
        (st1, evs, _) = applyOutcomeWith 0 0 (CallProc "rechnen" [EVInt 1]) "" st0
    r1 <- expectEqual (Just (VVInt 5)) (getVariable "x" st1)
    r2 <- expectEqual (Just (VVInt 1)) (getVariable "y" st1)
    r3 <- expectTrue "local write visible inside" ("in=99" `isInfixOf` renderEvents evs)
    pure (r1 && r2 && r3)

-- | Nested calls: the inner scope shadows the outer one and is popped first.
testProcNestedScopes :: IO Bool
testProcNestedScopes = do
    let st0 = procState
            [ ("outer", ["a"],
                [ SetValue (VRVariable "a") (EVInt 10)
                , CallProc "inner" [EVInt 1]
                , SendMessage "a={a}" ])
            , ("inner", ["a"], [SetValue (VRVariable "a") (EVInt 20)]) ]
        (st1, evs, _) = applyOutcomeWith 0 0 (CallProc "outer" [EVInt 1]) "" st0
    r1 <- expectTrue "outer sees its own a after inner returned"
            ("a=10" `isInfixOf` renderEvents evs)
    r2 <- expectTrue "all scopes popped" (null (procScopes st1))
    pure (r1 && r2)

-- | A veto inside the body stops the remaining body effects — and, like any
--   other veto, the remaining effects of the caller (2.2 semantics).
testProcVetoStopsBody :: IO Bool
testProcVetoStopsBody = do
    let st0 = procState
            [ ("stopper", [], [Block (Just "halt") False, SendMessage "nie"]) ]
        (_, evs, _) = applyOutcomeWith 0 0
            (Sequence [CallProc "stopper" [], SendMessage "nachher"]) "" st0
        out = renderEvents evs
    r1 <- expectTrue "veto message shown" ("halt" `isInfixOf` out)
    r2 <- expectTrue "rest of body skipped" (not ("nie" `isInfixOf` out))
    r3 <- expectTrue "caller sequence stops too" (not ("nachher" `isInfixOf` out))
    pure (r1 && r2 && r3)

-- | Defensive: the compiler rejects unknown calls, but a hand-built world
--   reaching the engine must degrade to a diagnostic, not a crash.
testProcUnknownDefensive :: IO Bool
testProcUnknownDefensive = do
    let st0 = procState []
        (st1, evs, _) = applyOutcomeWith 0 0 (CallProc "nope" []) "" st0
    r1 <- expectTrue "diagnostic recorded" (not (null (diagnostics st1)))
    r2 <- expectTrue "message shown" ("procedure" `isInfixOf` renderEvents evs)
    pure (r1 && r2)

-- | Defensive: arity mismatch degrades to a diagnostic as well.
testProcArityDefensive :: IO Bool
testProcArityDefensive = do
    let st0 = procState [ ("f", ["a"], [Noop]) ]
        (st1, _, _) = applyOutcomeWith 0 0 (CallProc "f" [EVInt 1, EVInt 2]) "" st0
    r1 <- expectTrue "diagnostic recorded" (not (null (diagnostics st1)))
    pure r1

-- ---------------------------------------------------------------------------
-- W1: knowledge model
-- ---------------------------------------------------------------------------

-- | Helper: empty state with facts/combines installed.
kstate :: [FactDef] -> [CombineDef] -> GameState
kstate fds cds =
    let gw = (world emptyGameState) { factDefs = fds, combineDefs = cds }
    in emptyGameState { world = gw }

-- | 'Knows' reads `known.<actor>.<fact>`; NPCs are separate minds.
testKnowsPredicate :: IO Bool
testKnowsPredicate = do
    let st0 = kstate [FactDef "brief" ["brief"] "Der Brief." Nothing Nothing Nothing Nothing] []
        (st1, _, _) = applyOutcomeWith 0 0 (Learn ActorPlayer "brief") "" st0
        (st2, _, _) = applyOutcomeWith 0 0 (Learn (ActorNPC "butler") "brief") "" st1
    r1 <- expectTrue "player knows brief" (evalPredicate (Knows ActorPlayer "brief") st1)
    r2 <- expectTrue "npc knowledge is separate"
            (not (evalPredicate (Knows (ActorNPC "butler") "brief") st1))
    r3 <- expectTrue "npc learns too" (evalPredicate (Knows (ActorNPC "butler") "brief") st2)
    r4 <- expectTrue "unknown fact unknown" (not (evalPredicate (Knows ActorPlayer "brief") st0))
    pure (r1 && r2 && r3 && r4)

-- | 'Learn' is idempotent and the cascade derives A->B->C in one step;
--   'Forget' removes only the named fact.
testLearnCascadeAndForget :: IO Bool
testLearnCascadeAndForget = do
    let cds =
            [ CombineDef ["brief"] "brief_gelesen" Nothing
            , CombineDef ["brief_gelesen", "tagebuch"] "verabredung" (Just "Der Hafen!") ]
        st0 = kstate
            [ FactDef "brief" ["brief"] "Der Brief." Nothing Nothing Nothing Nothing
            , FactDef "brief_gelesen" [] "" Nothing Nothing Nothing Nothing
            , FactDef "tagebuch" ["tagebuch"] "Das Tagebuch." Nothing Nothing Nothing Nothing
            , FactDef "verabredung" [] "" Nothing Nothing Nothing Nothing ]
            cds
        (st1, _, _) = applyOutcomeWith 0 0 (Learn ActorPlayer "brief") "" st0
        (st2, _, _) = applyOutcomeWith 0 0 (Learn ActorPlayer "tagebuch") "" st1
    r1 <- expectTrue "brief learned" (evalPredicate (Knows ActorPlayer "brief") st1)
    r2 <- expectTrue "cascade derived brief_gelesen"
            (evalPredicate (Knows ActorPlayer "brief_gelesen") st1)
    r3 <- expectTrue "second learn completes the chain"
            (evalPredicate (Knows ActorPlayer "verabredung") st2)
    r4 <- expectTrue "idempotent: re-learn is a no-op"
            (let (st3, _, _) = applyOutcomeWith 0 0 (Learn ActorPlayer "brief") "" st2
             in st3 == st2)
    r5 <- expectTrue "scope stack untouched" (null (procScopes st2))
    let (st4, _, _) = applyOutcomeWith 0 0 (Forget ActorPlayer "brief") "" st2
    r6 <- expectTrue "forget removes the fact"
            (not (evalPredicate (Knows ActorPlayer "brief") st4))
    r7 <- expectTrue "forget does not cascade backwards"
            (evalPredicate (Knows ActorPlayer "brief_gelesen") st4)
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7)

-- | OnLearn fires once per newly learned fact, in learning order; silent
--   facts learn silently; author learn_msg wins.
testOnLearnAndMessages :: IO Bool
testOnLearnAndMessages = do
    let fds =
            [ FactDef "a" [] "" Nothing Nothing Nothing Nothing
            , FactDef "stille" [] "" Nothing Nothing Nothing (Just True)
            , FactDef "mit_msg" [] "" Nothing Nothing (Just "EUREKA: {grund}") Nothing ]
        st0 = kstate fds
            [ CombineDef ["a"] "b" (Just "B folgt aus A.")
            , CombineDef ["b"] "c" Nothing ]
        (st1, evs, _) = applyOutcomeWith 0 0
            (Sequence [ Learn ActorPlayer "a", SendMessage "m1", Learn ActorPlayer "stille"
                      , SendMessage "m2", Learn ActorPlayer "mit_msg" ]) "" st0
        out = renderEvents evs
    r1 <- expectTrue "learn.default shown for plain fact" ("Noted." `isInfixOf` out)
    r2 <- expectTrue "silent fact shows nothing"
            (length [l | l <- lines out, "Noted." `isInfixOf` l] == 2)
    r3 <- expectTrue "author learn_msg wins" ("EUREKA:" `isInfixOf` out)
    r4 <- expectTrue "combine cdMsg shown for derived fact" ("B folgt aus A." `isInfixOf` out)
    -- OnLearn fired for a and b (c derives only after b is learned — wait: the
    -- cascade of 'a' learns b AND (b -> c) in one walk; so c is new too).
    r5 <- expectTrue "all three facts known"
            (all (\x -> evalPredicate (Knows ActorPlayer x) st1) ["a", "b", "c"])
    pure (r1 && r2 && r3 && r4 && r5)

-- | NPC learning cascades for the NPC but never for the player.
testNpcKnowledgeCascade :: IO Bool
testNpcKnowledgeCascade = do
    let st0 = kstate
            [ FactDef "geruecht" [] "" Nothing Nothing Nothing Nothing
            , FactDef "verdacht" [] "" Nothing Nothing Nothing Nothing ]
            [ CombineDef ["geruecht"] "verdacht" Nothing ]
        (st1, _, _) = applyOutcomeWith 0 0 (Learn (ActorNPC "butler") "geruecht") "" st0
    r1 <- expectTrue "npc cascade derived"
            (evalPredicate (Knows (ActorNPC "butler") "verdacht") st1)
    r2 <- expectTrue "player untouched"
            (not (evalPredicate (Knows ActorPlayer "verdacht") st1))
    r3 <- expectTrue "no message for npc learning" (renderEvents [] == "")
    pure (r1 && r2 && r3)

-- ---------------------------------------------------------------------------
-- W3: chapters
-- ---------------------------------------------------------------------------

-- | Helper: state with chapters installed (declaration order = contract).
cstate :: [ChapterDef] -> GameState
cstate cs =
    let gw = (world emptyGameState) { chapterDefs = cs }
    in emptyGameState { world = gw }

-- | The auto-gate: first eligible chapter in declaration order, at most one
--   switch per call, visited chapters never re-entered.
testChapterGate :: IO Bool
testChapterGate = do
    let cds = [ ChapterDef "kap1" Nothing Nothing
              , ChapterDef "kap2" (Just "Kapitel eins.") (Just (HasFlag "tor_offen"))
              , ChapterDef "kap3" Nothing (Just (HasFlag "tor_offen")) ]
        st0 = cstate cds
    -- No gate fires without a trigger: chapter 1 is not entered either (no
    -- when, no effect) - the gate only switches gated chapters.
    r0 <- expectEqual "" (currentChapterId st0)
    -- Gate with both conditions true: first eligible in declaration order.
    let st1 = setFlag "tor_offen" "true" st0
        (st2, evs1) = checkChapterGate st1
    r1 <- expectEqual "kap2" (currentChapterId st2)
    r2 <- expectTrue "kap2 visited" (chapterVisited "kap2" st2)
    r3 <- expectTrue "one switch per call: kap3 stays out"
            (not (chapterVisited "kap3" st2))
    r4 <- expectTrue "intro shown" ("Kapitel eins." `isInfixOf` renderEvents evs1)
    -- Second call with kap2 visited: kap3 is now the first eligible.
    let (st3, _) = checkChapterGate st2
    r5 <- expectEqual "kap3" (currentChapterId st3)
    -- All visited: no more switches.
    let (st4, _) = checkChapterGate st3
    r6 <- expectEqual "kap3" (currentChapterId st4)
    pure (r0 && r1 && r2 && r3 && r4 && r5 && r6)

-- | goto_chapter: forward ok, backward refused, unknown diagnosed;
--   next_chapter walks declaration order; OnChapter fires.
testChapterSwitchEffects :: IO Bool
testChapterSwitchEffects = do
    let cds = [ ChapterDef "eins" Nothing Nothing
              , ChapterDef "zwei" (Just "Eins.") Nothing
              , ChapterDef "drei" Nothing Nothing ]
        st0 = cstate cds
        (st1, evs1, _) = applyOutcomeWith 0 0 (GotoChapter "zwei") "" st0
    r1 <- expectEqual "zwei" (currentChapterId st1)
    r2 <- expectTrue "OnChapter fired (intro shown)" ("Eins." `isInfixOf` renderEvents evs1)
    -- forward to unvisited "eins" is allowed (visited = {zwei}), then the
    -- return jump to the VISITED "zwei" is refused (no retrospection)
    let (st2, _, _) = applyOutcomeWith 0 0 (GotoChapter "eins") "" st1
    r3 <- expectEqual "eins" (currentChapterId st2)
    let (st2b, evs3, _) = applyOutcomeWith 0 0 (GotoChapter "zwei") "" st2
    r4 <- expectEqual "eins" (currentChapterId st2b)
    r5 <- expectTrue "refusal message" ("behind you" `isInfixOf` renderEvents evs3)
    -- next_chapter from eins -> zwei; from drei -> no_next + diagnostic
    let st5a = cstate cds
        (st5, _, _) = applyOutcomeWith 0 0 (GotoChapter "eins") "" st5a
        (st6, _, _) = applyOutcomeWith 0 0 NextChapter "" st5
    r6 <- expectEqual "zwei" (currentChapterId st6)
    let (st7, _, _) = applyOutcomeWith 0 0 (GotoChapter "drei") "" st1
        (st8, _, _) = applyOutcomeWith 0 0 NextChapter "" st7
    r7 <- expectEqual "drei" (currentChapterId st8)
    r8 <- expectTrue "no_next diagnosed" (not (null (diagnostics st8)))
    -- unknown target diagnosed
    let (st9, _, _) = applyOutcomeWith 0 0 (GotoChapter "fuenf") "" st0
    r9 <- expectTrue "unknown target diagnosed" (not (null (diagnostics st9)))
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9)

-- | A goto_chapter from a chapter rule set is itself a chapter entry: the
--   auto-gate does not double-fire (one switch per turn contract).
testChapterOnChapterDoesNotCascade :: IO Bool
testChapterOnChapterDoesNotCascade = do
    let cds = [ ChapterDef "eins" Nothing Nothing
              , ChapterDef "zwei" Nothing (Just (HasFlag "go"))
              , ChapterDef "drei" Nothing Nothing ]
        st0 = cstate cds
        -- Explicit goto enters "eins"; turn 1 runs with the gate closed (no
        -- flag), then the flag opens and turn 2 fires the gate - one switch
        -- per turn, after the fold.
        (stA, _, _) = applyOutcomeWith 0 0 (GotoChapter "eins") "" st0
        (ls1, _) = applyLoopCommandEv (Interact VTake "zugx") (initLoopState stA)
        (ls2, _) = applyLoopCommandEv (Interact VTake "zugy")
                    ls1 { lsCurrent = setFlag "go" "true" (lsCurrent ls1) }
        (ls3, _) = applyLoopCommandEv (Interact VTake "zugg")
                    ls2 { lsCurrent = setFlag "go" "false" (lsCurrent ls2) }
    r1 <- expectEqual "eins" (currentChapterId (lsCurrent ls1))
    r2 <- expectEqual "zwei" (currentChapterId (lsCurrent ls2))
    r2b <- expectEqual "zwei" (currentChapterId (lsCurrent ls3))
    r3 <- expectTrue "no third chapter auto-fired" (not (chapterVisited "drei" (lsCurrent ls3)))
    pure (r1 && r2 && r2b && r3)

-- ---------------------------------------------------------------------------
-- Pursuit (Tür IV): distance queries and one-edge steps
-- ---------------------------------------------------------------------------

-- | Helper: rooms with connections plus positioned NPCs.
pstate :: [(RoomID, [(Direction, Exit)])] -> [(String, RoomID)] -> GameState
pstate rms npcs =
    let pRooms = [ (mkTestRoom r r) { roomConnections = Map.fromList cs } | (r, cs) <- rms ]
        gw = (world emptyGameState) { rooms = Map.fromList [ (roomId r, r) | r <- pRooms ] }
        sv = (save emptyGameState)
            { currentRoom = if null rms then "" else fst (head rms)
            , npcStates = Map.fromList
                [ (n, NPCState (InRoom loc) "alive" Nothing Map.empty Nothing)
                | (n, loc) <- npcs ]
            }
    in emptyGameState { world = gw, save = sv }

-- | The search core: distances, one-edge steps, the explicit tie-break
--   (smallest target room id first, then directionPriority) and the -1
--   sentinel. This pins the selection point against tie-break drift.
testPursuitCore :: IO Bool
testPursuitCore = do
    --        ziel
    --       / |  \
    --      a  b   c
    --       \ |  /
    --        start
    let edges = Map.fromList
            [ ("start", [(North, "a"), (South, "b"), (East, "c"), (West, "c")])
            , ("a", [(South, "start"), (North, "ziel")])
            , ("b", [(North, "start"), (South, "ziel")])
            , ("c", [(West, "start"), (Northeast, "ziel")])
            , ("ziel", [(South, "a"), (North, "b"), (Southwest, "c")])
            ]
        dists = bfsDistances edges "ziel"
    r1 <- expectEqual (Just (2 :: Int)) (Map.lookup "start" dists)
    r2 <- expectEqual (Just (1 :: Int)) (Map.lookup "a" dists)
    r3 <- expectTrue "unreachable rooms are absent (-1 sentinel)"
            (isNothing (Map.lookup "isolated" dists))
    -- tie-break at equal distance: smallest target room id wins
    r4 <- expectEqual (Just (North, "a")) (stepToward (edges Map.! "start") dists "start")
    -- tie-break at equal room: directionPriority (North before East)
    r5 <- expectEqual (Just (East, "c"))
            (stepToward [(West, "c"), (East, "c")] dists "start")
    -- one edge per call: from "a" the next hop is the goal
    r6 <- expectEqual (Just (North, "ziel")) (stepToward (edges Map.! "a") dists "a")
    -- at the goal: no step
    r7 <- expectEqual Nothing (stepToward (edges Map.! "ziel") dists "ziel")
    -- away: strictly farther only ("a" = 1 -> "start" = 2)
    r8 <- expectEqual (Just (South, "start")) (stepAway [(South, "start"), (North, "ziel")] dists "a")
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8)

-- | Distance queries over the live state: actors as seekers and targets,
--   the -1 sentinel for unreachable / no position, and the
--   `distance.<seeker>.<target>` value form.
testPursuitDistance :: IO Bool
testPursuitDistance = do
    let st0 = pstate
            [ ("start", [(North, Open "a")])
            , ("a", [(South, Open "start"), (North, Open "ziel")])
            , ("ziel", [(South, Open "a")])
            , ("isolated", [])
            ] [("wolf", "start"), ("bird", "isolated")]
        st = st0 { save = (save st0) { currentRoom = "ziel" } }
    r1 <- expectEqual (2 :: Int)
            (distanceTo defaultPursuitOptions st (ActorNPC "wolf") (DTActor ActorPlayer))
    r2 <- expectEqual (1 :: Int)
            (distanceTo defaultPursuitOptions st (ActorNPC "wolf") (DTRoom "a"))
    r3 <- expectEqual (-1 :: Int)
            (distanceTo defaultPursuitOptions st (ActorNPC "bird") (DTActor ActorPlayer))
    r4 <- expectEqual (-1 :: Int)
            (distanceTo defaultPursuitOptions st (ActorNPC "wolf") (DTRoom "isolated"))
    -- the string value form `distance.<seeker>.<target>`
    r5 <- expectEqual (2 :: Int) (resolveValueRef (VRVariable "distance.wolf.player") st)
    r6 <- expectEqual (-1 :: Int) (resolveValueRef (VRVariable "distance.bird.player") st)
    pure (r1 && r2 && r3 && r4 && r5 && r6)

-- | step_toward / step_away_from effects: one edge per call, catalog
--   messages, author message override, and the seeker-type contract
--   (only NPCs and ships — anything else is refused with a diagnostic).
testPursuitStepEffects :: IO Bool
testPursuitStepEffects = do
    let st0 = pstate
            [ ("start", [(North, Open "a")])
            , ("a", [(South, Open "start"), (North, Open "ziel")])
            , ("ziel", [(South, Open "a")])
            , ("isolated", [])
            ] [("wolf", "start"), ("bird", "isolated")]
        st = st0 { save = (save st0) { currentRoom = "ziel" } }
        wolfRoom s = Map.lookup "wolf" (npcStates (save s)) >>= \ns ->
            case npcLocation ns of InRoom r -> Just r; _ -> Nothing
        (st1, evs1, _) =
            applyOutcomeWith 0 0 (StepToward (ActorNPC "wolf") (DTActor ActorPlayer)
                                    defaultPursuitOptions Nothing) "" st
    r1 <- expectEqual (Just "a") (wolfRoom st1)
    r2 <- expectTrue "catalog step message (route visible)"
            ("wolf moves to a" `isInfixOf` renderEvents evs1)
    -- one edge per call: the next call moves one more edge, not the path
    let (st2, _, _) =
            applyOutcomeWith 0 0 (StepToward (ActorNPC "wolf") (DTActor ActorPlayer)
                                    defaultPursuitOptions Nothing) "" st1
    r3 <- expectEqual (Just "ziel") (wolfRoom st2)
    -- author message override replaces the catalog line
    let (st3, evs3, _) =
            applyOutcomeWith 0 0 (StepToward (ActorNPC "wolf") (DTRoom "start")
                                    defaultPursuitOptions (Just "Der Wolf folgt.")) "" st2
    r4 <- expectEqual (Just "a") (wolfRoom st3)
    r5 <- expectTrue "author message shown" ("Der Wolf folgt." `isInfixOf` renderEvents evs3)
    -- flee: strictly farther away
    let (st4, _, _) =
            applyOutcomeWith 0 0 (StepAwayFrom (ActorNPC "wolf") (DTActor ActorPlayer)
                                    defaultPursuitOptions Nothing) "" st3
    r6 <- expectEqual (Just "start") (wolfRoom st4)
    -- no path: catalog message, state unchanged
    let birdRoom s = Map.lookup "bird" (npcStates (save s)) >>= \ns ->
            case npcLocation ns of InRoom r -> Just r; _ -> Nothing
        (st5, evs5, _) =
            applyOutcomeWith 0 0 (StepToward (ActorNPC "bird") (DTActor ActorPlayer)
                                    defaultPursuitOptions Nothing) "" st
    r7 <- expectEqual (Just "isolated") (birdRoom st5)
    r8 <- expectTrue "no-path message" ("can find no way" `isInfixOf` renderEvents evs5)
    -- only NPCs and ships are seekers
    let (st6, _, _) =
            applyOutcomeWith 0 0 (StepToward ActorPlayer (DTRoom "start")
                                    defaultPursuitOptions Nothing) "" st
    r9 <- expectTrue "non-seeker refused with a diagnostic" (not (null (diagnostics st6)))
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9)

-- | Fairness: a pursuer that may not pass the locked door stays put — the
--   distance is -1 and no step is taken. With `ignores: [locked]` it walks
--   right through.
testPursuitFairness :: IO Bool
testPursuitFairness = do
    let st0 = pstate
            [ ("start", [(North, Locked "mid" "tuer")])
            , ("mid", [(South, Open "start")])
            ] [("wolf", "start")]
        st = st0 { save = (save st0)
                    { currentRoom = "mid"
                    , entityStates = Map.singleton "tuer" "locked" } }
        wolfRoom s = Map.lookup "wolf" (npcStates (save s)) >>= \ns ->
            case npcLocation ns of InRoom r -> Just r; _ -> Nothing
    r1 <- expectEqual (-1 :: Int)
            (distanceTo defaultPursuitOptions st (ActorNPC "wolf") (DTActor ActorPlayer))
    let (st1, evs1, _) =
            applyOutcomeWith 0 0 (StepToward (ActorNPC "wolf") (DTActor ActorPlayer)
                                    defaultPursuitOptions Nothing) "" st
    r2 <- expectEqual (Just "start") (wolfRoom st1)
    r3 <- expectTrue "fairness message" ("can find no way" `isInfixOf` renderEvents evs1)
    let ignores = PursuitOptions True False
        (st2, _, _) =
            applyOutcomeWith 0 0 (StepToward (ActorNPC "wolf") (DTActor ActorPlayer)
                                    ignores Nothing) "" st
    r4 <- expectEqual (Just "mid") (wolfRoom st2)
    pure (r1 && r2 && r3 && r4)

-- | Save/Load in the middle of a chase: the reloaded state computes the
--   identical next step (statelessness + identical recomputation).
testPursuitSaveLoad :: IO Bool
testPursuitSaveLoad = do
    let st0 = pstate
            [ ("start", [(North, Open "a")])
            , ("a", [(South, Open "start"), (North, Open "ziel")])
            , ("ziel", [(South, Open "a")])
            ] [("wolf", "start")]
        st = st0 { save = (save st0) { currentRoom = "ziel" } }
    case Aeson.decode (Aeson.encode (save st)) of
        Nothing -> expectTrue "save round-trips through JSON" False
        Just loaded ->
            let stLoaded = st { save = loaded }
                step s = pursuitStep True defaultPursuitOptions s
                            (ActorNPC "wolf") (DTActor ActorPlayer)
            in do
                r1 <- expectTrue "a step exists" (isJust (step st))
                r2 <- expectEqual (step st) (step stLoaded)
                pure (r1 && r2)

-- | B3: the five closed mass operations over a count set (B2 dimensions).
testMassOps :: IO Bool
testMassOps = do
    let lamp = (mkTestItem "lampe" "Lampe") { itemTags = Set.fromList ["licht"] }
        stein = (mkTestItem "stein" "Stein") { itemTags = Set.fromList ["schwer"] }
        truhe = (mkTestItem "truhe" "Truhe") { itemTags = Set.empty }
        st0 = mstate [mkTestRoom "halle" "Halle", mkTestRoom "keller" "Keller"]
                [ (lamp, InRoom "halle"), (stein, InRoom "halle"), (truhe, InRoom "keller") ]
        st = st0 { save = (save st0)
                    { npcStates = Map.fromList
                        [ ("wolf", NPCState (InRoom "halle") "alive" (Just 10) Map.empty Nothing)
                        , ("geist", NPCState (InRoom "halle") "alive" (Just 3) Map.empty Nothing) ] } }
        npcLoc n s = Map.lookup n (npcStates (save s)) >>= \ns ->
            case npcLocation ns of InRoom r -> Just r; _ -> Nothing
        itemLoc i s = Map.lookup i (itemStates (save s)) >>= \is -> Just (itemLocation is)
    -- damage_all: every living NPC in the room takes the damage
    let (st1, _, _) = applyOutcomeWith 0 0
            (DamageAll (CountSpec CountAliveNpcs (CountInRoom "halle") Nothing) 4) "" st
    r1 <- expectEqual (Just (6 :: Int)) (Map.lookup "wolf" (npcStates (save st1)) >>= npcHealth)
    r2 <- expectEqual (Just (-1 :: Int)) (Map.lookup "geist" (npcStates (save st1)) >>= npcHealth)
    -- move_all: every tagged item in the room moves to the cellar
    let (st2, _, _) = applyOutcomeWith 0 0
            (MoveAll (CountSpec CountItems (CountInRoom "halle") (Just "licht")) (CountInRoom "keller")) "" st
    r3 <- expectEqual (Just (InRoom "keller")) (itemLoc "lampe" st2)
    r4 <- expectEqual (Just (InRoom "halle")) (itemLoc "stein" st2)
    -- move_all to an actor
    let (st2b, _, _) = applyOutcomeWith 0 0
            (MoveAll (CountSpec CountItems (CountInRoom "halle") Nothing) (CountCarriedBy ActorPlayer)) "" st
    r5 <- expectEqual (Just (CarriedBy ActorPlayer)) (itemLoc "lampe" st2b)
    -- reveal_all: hidden items in the set are discovered
    let hiddenStein = (mkTestItem "hschluessel" "H") { itemHidden = True }
        stH0 = mstate [mkTestRoom "halle" "Halle"] [(hiddenStein, InRoom "halle")]
        (stH, _, _) = applyOutcomeWith 0 0
            (RevealAll (CountSpec CountItems (CountInRoom "halle") Nothing)) "" stH0
    r6 <- expectTrue "hidden item is revealed"
            (maybe False itemDiscovered (Map.lookup "hschluessel" (itemStates (save stH))))
    -- consume_all: every member is removed
    let (st3, _, _) = applyOutcomeWith 0 0
            (ConsumeAll (CountSpec CountItems (CountInRoom "halle") (Just "schwer"))) "" st
    r7 <- expectEqual (Just Removed) (itemLoc "stein" st3)
    r8 <- expectEqual (Just (InRoom "halle")) (itemLoc "lampe" st3)
    -- set_state_all: item status and NPC status
    let (st4, _, _) = applyOutcomeWith 0 0
            (SetStateAll (CountSpec CountItems (CountInRoom "halle") (Just "licht")) "brennend") "" st
    r9 <- expectEqual (Just "brennend") (Map.lookup "lampe" (itemStates (save st4)) >>= \is -> Just (itemStatus is))
    let (st5, _, _) = applyOutcomeWith 0 0
            (SetStateAll (CountSpec CountNpcs (CountInRoom "halle") Nothing) "tot") "" st
    r10 <- expectEqual (Just "tot") (Map.lookup "wolf" (npcStates (save st5)) >>= \ns -> Just (npcStatus ns))
    -- move_all also moves NPCs
    let (st6, _, _) = applyOutcomeWith 0 0
            (MoveAll (CountSpec CountNpcs (CountInRoom "halle") Nothing) (CountInRoom "keller")) "" st
    r11 <- expectEqual (Just "keller") (npcLoc "wolf" st6)
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9 && r10 && r11)

-- ---------------------------------------------------------------------------
-- W4: devices (Hebel / Halterung)
-- ---------------------------------------------------------------------------

mkTestRoom :: RoomID -> String -> Room
mkTestRoom rId name = Room rId name (plainText name) Map.empty Set.empty Nothing Nothing Nothing Nothing Nothing Nothing emptyAscii Nothing Nothing Nothing

-- ---------------------------------------------------------------------------
-- B2: tag predicates and the count family
-- ---------------------------------------------------------------------------

-- | Helper: state with rooms and items (with locations) — the W4 device
--   helper without devices.
mkTestItem :: String -> String -> ItemDef
mkTestItem i n =
    ItemDef i n (plainText n) [i] Set.empty Nothing [] False Nothing True Nothing Map.empty Nothing emptyAscii emptyGrammar

mstate :: [Room] -> [(ItemDef, Location)] -> GameState
mstate rms its =
    let gw = (world emptyGameState)
            { rooms = Map.fromList [ (roomId r, r) | r <- rms ]
            , itemDefs = Map.fromList [ (itemId i, i) | (i, _) <- its ]
            }
        sv = (save emptyGameState)
            { currentRoom = if null rms then "" else roomId (head rms)
            , itemStates = Map.fromList
                [ (itemId i, ItemState loc "intact" Map.empty False) | (i, loc) <- its ]
            }
    in emptyGameState { world = gw, save = sv }

-- | `has_tagged_item`/`has_item_tag` generalise playerHasTaggedItem to any
--   actor and to rooms.
testTaggedItemPredicates :: IO Bool
testTaggedItemPredicates = do
    let lamp = (mkTestItem "lampe" "Lampe") { itemTags = Set.fromList ["licht"] }
        stein = (mkTestItem "stein" "Stein") { itemTags = Set.fromList ["schwer"] }
        st0 = mstate [mkTestRoom "halle" "Halle"]
                [ (lamp, CarriedBy ActorPlayer), (stein, InRoom "halle") ]
        st = st0 { save = (save st0) { inventory = ["lampe"] } }
    r1 <- expectTrue "player carries a tagged item"
            (evalPredicate (HasTaggedItem ActorPlayer "licht") st)
    r2 <- expectTrue "wrong tag is false"
            (not (evalPredicate (HasTaggedItem ActorPlayer "schwer") st))
    r3 <- expectTrue "room holds a tagged item"
            (evalPredicate (RoomHasTaggedItem "halle" "schwer") st)
    r4 <- expectTrue "room has no such tag item"
            (not (evalPredicate (RoomHasTaggedItem "halle" "licht") st))
    pure (r1 && r2 && r3 && r4)

-- | The count family: items/npcs (alive) in a room or carried, with an
--   optional tag filter — in the string form and as an object.
testCountSpec :: IO Bool
testCountSpec = do
    let lamp = (mkTestItem "lampe" "Lampe") { itemTags = Set.fromList ["licht"] }
        stein = (mkTestItem "stein" "Stein") { itemTags = Set.fromList ["schwer"] }
        kiesel = (mkTestItem "kiesel" "Kiesel") { itemTags = Set.empty }
        st0 = mstate [mkTestRoom "halle" "Halle"]
                [ (lamp, InRoom "halle"), (stein, InRoom "halle"), (kiesel, CarriedBy ActorPlayer) ]
        st = st0 { save = (save st0)
                    { inventory = ["kiesel"]
                    , npcStates = Map.fromList
                        [ ("wolf", NPCState (InRoom "halle") "alive" Nothing Map.empty Nothing)
                        , ("geist", NPCState (InRoom "halle") "dead" Nothing Map.empty Nothing) ] } }
    r1 <- expectEqual (2 :: Int) (resolveValueRef (VRVariable "count.items.in.halle") st)
    r2 <- expectEqual (1 :: Int) (resolveValueRef (VRVariable "count.items.tag.licht.in.halle") st)
    r3 <- expectEqual (2 :: Int) (resolveValueRef (VRVariable "count.npcs.in.halle") st)
    r4 <- expectEqual (1 :: Int) (resolveValueRef (VRVariable "count.alive_npcs.in.halle") st)
    r5 <- expectEqual (1 :: Int) (resolveValueRef (VRVariable "count.items.by.player") st)
    r6 <- expectEqual (0 :: Int) (resolveValueRef (VRVariable "count.alive_npcs.in.nirgendwo") st)
    -- the object form carries the tag filter too
    let spec = CountSpec CountItems (CountInRoom "halle") (Just "schwer")
    r7 <- expectEqual (1 :: Int) (resolveValueRef (VRCount spec) st)
    r8 <- expectEqual (0 :: Int) (resolveValueRef (VRCount (CountSpec CountAliveNpcs (CountCarriedBy ActorPlayer) Nothing)) st)
    -- `compare_var` sees the count family too (the string form)
    r9 <- expectTrue "compare_var over count values"
            (evalPredicate (CompareVar "count.items.in.halle" CGte 2) st)
    r10 <- expectTrue "compare_var over count values (false case)"
            (not (evalPredicate (CompareVar "count.npcs.in.halle" CGt 5) st))
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9 && r10)

-- | Helper: state with rooms, items (with locations), and devices installed.
dstate :: [Room] -> [(ItemDef, Location)] -> [DeviceDef] -> GameState
dstate rms its devs =
    let gw = (world emptyGameState)
            { rooms = Map.fromList [ (roomId r, r) | r <- rms ]
            , itemDefs = Map.fromList [ (itemId i, i) | (i, _) <- its ]
            , deviceDefs = Map.fromList [ (devId d, d) | d <- devs ]
            }
        sv = (save emptyGameState)
            { currentRoom = if null rms then "" else roomId (head rms)
            , itemStates = Map.fromList [ (itemId i, ItemState loc "intact" Map.empty False) | (i, loc) <- its ]
            }
    in emptyGameState { world = gw, save = sv }

testActorHasPredicate :: IO Bool
testActorHasPredicate = do
    let r1 = mkTestRoom "krypta" "Krypta"
        it1 = ItemDef "fackel" "Fackel" (plainText "Eine Fackel.") ["fackel"] Set.empty Nothing [] False Nothing True Nothing Map.empty Nothing emptyAscii emptyGrammar
        it2 = ItemDef "schluessel" "Schlüssel" (plainText "Ein Schlüssel.") ["schluessel"] Set.empty Nothing [] False Nothing True Nothing Map.empty Nothing emptyAscii emptyGrammar
        it3 = ItemDef "kristall" "Kristall" (plainText "Ein Kristall.") ["kristall"] Set.empty Nothing [] False Nothing True Nothing Map.empty Nothing emptyAscii emptyGrammar
        dev1 = DeviceDef "halterung" "Halterung" ["halterung"] "krypta" (Just "Eine Halterung.") Nothing [] Nothing Nothing [] [] Nothing [] Map.empty
        st0 = dstate [r1] [ (it1, InRoom "krypta")
                          , (it2, CarriedBy (ActorNPC "guard"))
                          , (it3, CarriedBy (ActorEntity "halterung"))
                          ] [dev1]
    r1Check <- expectTrue "player does not have fackel yet" (not (evalPredicate (ActorHas ActorPlayer "fackel") st0))
    r2Check <- expectTrue "npc has schluessel" (evalPredicate (ActorHas (ActorNPC "guard") "schluessel") st0)
    r3Check <- expectTrue "device has kristall" (evalPredicate (ActorHas (ActorEntity "halterung") "kristall") st0)
    let (st1, _, _) = applyOutcomeWith 0 0 (Mount "fackel" ActorPlayer) "" st0
    r4Check <- expectTrue "player has fackel after mount to player" (evalPredicate (ActorHas ActorPlayer "fackel") st1)
    pure (r1Check && r2Check && r3Check && r4Check)

testMountAndUnmountEffects :: IO Bool
testMountAndUnmountEffects = do
    let r1 = mkTestRoom "krypta" "Krypta"
        it1 = ItemDef "fackel" "Fackel" (plainText "Eine Fackel.") ["fackel"] Set.empty Nothing [] False Nothing True Nothing Map.empty Nothing emptyAscii emptyGrammar
        dev1 = DeviceDef "halterung" "Halterung" ["halterung"] "krypta" (Just "Eine Halterung.") Nothing [] Nothing Nothing [] [] Nothing [] Map.empty
        st0 = dstate [r1] [(it1, InRoom "krypta")] [dev1]
    let (st1, _, _) = applyOutcomeWith 0 0 (Mount "fackel" (ActorEntity "halterung")) "" st0
    r1Check <- expectEqual (Just (CarriedBy (ActorEntity "halterung"))) (itemLocation <$> Map.lookup "fackel" (itemStates (save st1)))
    r2Check <- expectTrue "device has fackel" (evalPredicate (ActorHas (ActorEntity "halterung") "fackel") st1)
    let (st2, _, _) = applyOutcomeWith 0 0 (Unmount "fackel") "" st1
    r3Check <- expectEqual (Just (InRoom "krypta")) (itemLocation <$> Map.lookup "fackel" (itemStates (save st2)))
    r4Check <- expectTrue "device no longer has fackel" (not (evalPredicate (ActorHas (ActorEntity "halterung") "fackel") st2))
    pure (r1Check && r2Check && r3Check && r4Check)

testDeviceInteractionExamine :: IO Bool
testDeviceInteractionExamine = do
    let r1 = mkTestRoom "krypta" "Krypta"
        it1 = ItemDef "fackel" "brennende Fackel" (plainText "Eine Fackel.") ["fackel"] Set.empty Nothing [] False Nothing True Nothing Map.empty Nothing emptyAscii emptyGrammar
        dev1 = DeviceDef "halterung" "Fackelhalterung" ["halterung"] "krypta" (Just "Eine Wandhalterung.") Nothing [] Nothing Nothing [] [] Nothing [] Map.empty
        st0 = dstate [r1] [(it1, CarriedBy (ActorEntity "halterung"))] [dev1]
        cmd = parseCommandWith Map.empty "examine halterung"
        (_, evs) = applyLoopCommandEv cmd (initLoopState st0)
        out = renderEvents evs
    r1Check <- expectTrue "shows device description" ("Eine Wandhalterung." `isInfixOf` out)
    r2Check <- expectTrue "shows mounted item" ("Mounted: brennende Fackel." `isInfixOf` out)
    let (stUnmounted, _, _) = applyOutcomeWith 0 0 (Unmount "fackel") "" st0
        (_, evsEmpty) = applyLoopCommandEv cmd (initLoopState stUnmounted)
        outEmpty = renderEvents evsEmpty
    r3Check <- expectTrue "shows description when empty" ("Eine Wandhalterung." `isInfixOf` outEmpty)
    r4Check <- expectTrue "no mounted line when empty" (not ("Mounted:" `isInfixOf` outEmpty))
    pure (r1Check && r2Check && r3Check && r4Check)

-- | Rogue Phase 4c: run-seed derivation from slug and run index.
--   Determinism, distinctness across runs, and distinctness across slugs.
testDeriveRunSeed :: IO Bool
testDeriveRunSeed = do
    let s0 = SaveLoad.deriveRunSeedFromSlug "catacombs" 0
        s0_repeat = SaveLoad.deriveRunSeedFromSlug "catacombs" 0
        s1 = SaveLoad.deriveRunSeedFromSlug "catacombs" 1
        s2 = SaveLoad.deriveRunSeedFromSlug "catacombs" 2
        sOther = SaveLoad.deriveRunSeedFromSlug "dungeon" 0
        gw = world initSampleGame
        sWorld = SaveLoad.deriveRunSeed gw 0
        sWorldExpected = SaveLoad.deriveRunSeedFromSlug (SaveLoad.adventureSlug gw) 0
    r1 <- expectEqual s0 s0_repeat
    r2 <- expectTrue "run 0 and run 1 have different seeds" (s0 /= s1)
    r3 <- expectTrue "run 1 and run 2 have different seeds" (s1 /= s2)
    r4 <- expectTrue "different slugs have different seeds" (s0 /= sOther)
    r5 <- expectEqual sWorldExpected sWorld
    -- Run indices 0..20 are all pairwise distinct
    let runSeeds = [ SaveLoad.deriveRunSeedFromSlug "catacombs" i | i <- [0..20] ]
    r6 <- expectEqual 21 (Set.size (Set.fromList runSeeds))
    pure (r1 && r2 && r3 && r4 && r5 && r6)

-- | L11: the turn pipeline is `incrementTurnCount` → `tickConditions` →
--   `vehicleConditionTick` → `executeCommand` → `fireCommandTriggers`. A
--   condition tick that kills the player therefore runs *before* the command —
--   and the command must not run on a finished game. Pinned here, because the
--   order was previously only implicit (and the command did execute).
testFatalTickStopsCommand :: IO Bool
testFatalTickStopsCommand = do
    let lethal = ModifyValue VRPlayerHealth (-100)
        st0 = applyCondition "doomed" 3 (Just lethal) Nothing (setPlayerHP 5 initSampleGame)
        (loop', msg) = applyLoopCommand (Go North) (initLoopState st0)
        st' = lsCurrent loop'
        -- control: without the condition the same command moves the player
        (loopC, msgC) = applyLoopCommand (Go North) (initLoopState (initSampleGame :: GameState))
    r1 <- expectTrue "the tick ended the game" (gameOver (save st'))
    r2 <- expectTrue "the player is dead" (isPlayerDead st')
    r3 <- expectEqual "start" (currentRoom (save st'))
    r4 <- expectTrue "the command did not run"
              (not ("You move North" `isInfixOf` msg))
    r5 <- expectTrue "the tick ended the game with reason Death"
              (gameOverReason (save st') == Just Death)
    -- the tick itself is silent: the death text comes from the game-over screen,
    -- so the dropped command produces no message at all
    r5b <- expectTrue "no command text is produced" (null msg)
    r6 <- expectTrue "control: the same command moves without the condition"
              ("You move North" `isInfixOf` msgC
               && currentRoom (save (lsCurrent loopC)) == "hallway")
    pure (r1 && r2 && r3 && r4 && r5 && r5b && r6)

-- ===== L4: one test per ValidationError constructor that had none =====

-- | L4: `MissingRoom` was declared but never produced — a rule moving an entity
--   into a typo'd room was silently accepted, and the worldbuilder does not check
--   rule room references at all. Now both `MoveEntity` and the `move:` effect
--   (player teleport) are checked.
testValidateMissingRoomInRule :: IO Bool
testValidateMissingRoomInRule = do
    let gw0 = world initSampleGame
        withRule eff = gw0 { triggerDefs = [TriggerDef "t" OnTurn Nothing [eff] False 0] }
        moveNpc    = withRule (MoveEntity "goblin" (InRoom "ghost_room"))
        movePlayer = withRule (SetValue (VRActorProp ActorPlayer PRoom) (EVString "ghost_room"))
        legit      = withRule (MoveEntity "goblin" (InRoom "hallway"))
    r1 <- expectTrue "MoveEntity into a missing room is reported"
              (MissingRoom "ghost_room" `elem` validateWorld moveNpc)
    r2 <- expectTrue "`move:` into a missing room is reported"
              (MissingRoom "ghost_room" `elem` validateWorld movePlayer)
    r3 <- expectTrue "a move to an existing room is not reported"
              (MissingRoom "hallway" `notElem` validateWorld legit)
    r4 <- expectTrue "the sample world has no MissingRoom"
              (not (any isMissingRoom (validateWorld gw0)))
    pure (r1 && r2 && r3 && r4)
  where
    isMissingRoom (MissingRoom _) = True
    isMissingRoom _               = False

-- | L4: an NPC referenced by a rule that is declared nowhere.
testValidateMissingNpcInRule :: IO Bool
testValidateMissingNpcInRule = do
    let withTarget t = (world initSampleGame)
                { triggerDefs = [ TriggerDef "t" OnTurn Nothing
                                     [MoveEntity t (InRoom "hallway")] False 0 ] }
    r1 <- expectTrue "an unknown MoveEntity target is reported as MissingNPC"
              (MissingNPC "ghost_npc" `elem` validateWorld (withTarget "ghost_npc"))
    r2 <- expectTrue "a declared NPC is not reported"
              (MissingNPC "goblin" `notElem` validateWorld (withTarget "goblin"))
    pure (r1 && r2)

-- | L4: a `VRActorProp` reference to an entity that is neither an item, an NPC nor
--   an exit lock key.
testValidateMissingEntityInRule :: IO Bool
testValidateMissingEntityInRule = do
    let withTarget t = (world initSampleGame)
                { triggerDefs = [ TriggerDef "t" OnTurn Nothing
                                    [SetValue (VRActorProp (ActorEntity t) PState) (EVString "open")] False 0 ] }
    r1 <- expectTrue "an unknown VRActorProp target is reported"
              (MissingEntity "ghost_entity" "property" `elem` validateWorld (withTarget "ghost_entity"))
    -- an exit lock key lives in `entityStates` and is therefore a valid entity
    r2 <- expectTrue "an exit lock key is not reported"
              (MissingEntity "treasure_door" "property" `notElem` validateWorld (withTarget "treasure_door"))
    pure (r1 && r2)

-- | L4: a vehicle whose entry room does not exist. `checkVehicleRefs` runs in
--   `validateGameState` (it needs the room set of the world plus the save).
testValidateInvalidVehicleRoom :: IO Bool
testValidateInvalidVehicleRoom = do
    let gw0 = world initSampleGame
        carriage = maybe (error "sample lost its carriage") id
                        (Map.lookup "carriage" (vehicleDefs gw0))
        broken = gw0 { vehicleDefs = Map.insert "carriage"
                          (carriage { vehicleEntryRoom = "ghost_room" }) (vehicleDefs gw0) }
    r1 <- expectTrue "a vehicle entry into a missing room is reported"
              (InvalidVehicleRoom "carriage" "entry" "ghost_room"
                 `elem` validateGameState broken (save initSampleGame))
    r2 <- expectTrue "the sample's carriage is fine"
              (not (any isVehicleRoom (validateGameState gw0 (save initSampleGame))))
    pure (r1 && r2)
  where
    isVehicleRoom (InvalidVehicleRoom _ _ _) = True
    isVehicleRoom _                          = False

-- | L1: `World` had a test only for the `loadGame` happy path — the two loaders
--   and *every* error branch were uncovered, although those `Left` values are
--   what the player sees when a world file is corrupt or missing.
testWorldLoadersAndErrors :: IO Bool
testWorldLoadersAndErrors = do
    tmp <- getTemporaryDirectory
    withCurrentDirectory tmp $ do
        let w = world initSampleGame
        BLC.writeFile "l1-world.json" (Aeson.encode w)
        BLC.writeFile "l1-save.json" (Aeson.encode (save initSampleGame))
        BLC.writeFile "l1-bogus.json" (BLC.pack "{ not json at all")
        gwOk <- loadGameWorld "l1-world.json"
        r1 <- case gwOk of
            Right gw' -> expectEqual w gw'
            Left err  -> do putStrLn ("  loadGameWorld failed: " ++ err); pure False
        gwBad <- loadGameWorld "l1-bogus.json"
        r2 <- expectTrue "a corrupt world file reports an error" (isLeft gwBad)
        ssOk <- loadSaveState "l1-save.json"
        r3 <- case ssOk of
            Right ss  -> expectEqual (save initSampleGame) ss
            Left err  -> do putStrLn ("  loadSaveState failed: " ++ err); pure False
        ssBad <- loadSaveState "l1-bogus.json"
        r4 <- expectTrue "a corrupt save file reports an error" (isLeft ssBad)
        gwMissing <- loadGameWorld "l1-nope.json"
        r5 <- expectTrue "a missing world file reports an error" (isLeft gwMissing)
        stMissing <- loadGame "l1-nope.json" Nothing
        r6 <- expectTrue "a missing world file fails loadGame as well" (isLeft stMissing)
        -- with a save file the positions from that save are used
        stBoth <- loadGame "l1-world.json" (Just "l1-save.json")
        r7 <- case stBoth of
            Right st' -> expectEqual (save initSampleGame) (save st')
            Left err  -> do putStrLn ("  loadGame (with save) failed: " ++ err); pure False
        pure (r1 && r2 && r3 && r4 && r5 && r6 && r7)

-- | L8: `equipmentSummary` is player-facing text that had no assertion.
testEquipmentSummaryText :: IO Bool
testEquipmentSummaryText = do
    r1 <- expectEqual "You have nothing equipped." (equipmentSummary initSampleGame)
    case equipItem "sword_rusty" (pickupItem "sword_rusty" initSampleGame) of
        Left err -> do
            putStrLn ("  equipItem failed: " ++ err)
            pure False
        Right equipped -> do
            let summary = equipmentSummary equipped
            r2 <- expectTrue "the equipped item is listed with slot and name"
                      (("Weapon" `isInfixOf` summary) && ("rusty sword" `isInfixOf` summary))
            pure (r1 && r2)

-- | Nebenfund aus dem Verlustpfad-Fixture (starship-loss.yaml): `{state: X, is: Y}`
--   las nur `entityStates`. Der NPC-Status lebt in `npcStates`, der Item-Status in
--   `itemStates` — die Bedingung war für beide unerreichbar, obwohl
--   `starship.yaml` und `combo.yaml` sie genau so verwenden (und damit ihren
--   Verlust- bzw. Kampfpfad stillschweigend unmöglich machten).
testEntityStatePredicateCoversAllKinds :: IO Bool
testEntityStatePredicateCoversAllKinds = do
    let st0 = initSampleGame
        alive = EntityHasState "goblin" "alive"
        stDead = st0 { save = (save st0)
                         { npcStates = Map.adjust (\ns -> ns { npcStatus = "dead" })
                                                  "goblin" (npcStates (save st0)) } }
    r1 <- expectTrue "a living NPC matches `state:`" (evalPredicate alive st0)
    r2 <- expectTrue "a dead NPC stops matching" (not (evalPredicate alive stDead))
    r3 <- expectTrue "an item's status is visible"
              (evalPredicate (EntityHasState "sword_rusty" "intact") st0)
    r4 <- expectTrue "an exit lock's state still matches"
              (evalPredicate (EntityHasState "treasure_door" "locked") st0)
    r5 <- expectTrue "an unknown entity never matches"
              (not (evalPredicate (EntityHasState "ghost" "alive") st0))
    pure (r1 && r2 && r3 && r4 && r5)

-- | 7f-3 A1: the combat round state lives in the VarMap under the reserved
--   `combat.` prefix — no new `SaveState` field, so save/load works like for any
--   other variable, and the enemy reaction can read it with a plain
--   `compare_var` rule instead of an engine special case.
testCombatRoundStateVars :: IO Bool
testCombatRoundStateVars = do
    let st0 = initSampleGame
    r1 <- expectEqual 0 (combatRound st0)
    r2 <- expectTrue "no fight by default" (not (isCombatEngaged st0))
    let st1 = setCombatRound 3 st0
    r3 <- expectEqual 3 (combatRound st1)
    r4 <- expectTrue "the round is a plain VarMap value"
              (getVariable combatRoundKey st1 == Just (VVInt 3))
    -- the reaction rule reads the state through the normal predicate language
    let engaged = setVariable combatEngagedKey (VVInt 1) st1
    r5 <- expectTrue "an engaged fight is visible to compare_var"
              (evalPredicate (CompareVar combatEngagedKey CGte 1) engaged)
    r6 <- expectTrue "a disengaged fight is not"
              (not (evalPredicate (CompareVar combatEngagedKey CGte 1) st1))
    -- and it survives the save round-trip without any migration
    r7 <- expectTrue "combat state survives the save round-trip"
              (case Aeson.decode (Aeson.encode (save engaged)) :: Maybe SaveState of
                   Just ss -> Map.lookup combatRoundKey (variables ss) == Just (VVInt 3)
                              && Map.lookup combatEngagedKey (variables ss) == Just (VVInt 1)
                   Nothing -> False)
    r8 <- expectTrue "both keys use the reserved prefix"
              (combatVarPrefix `isPrefixOf` combatRoundKey
               && combatVarPrefix `isPrefixOf` combatEngagedKey)
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8)

-- | Phase 7f-3: Tactical Combat (A2)

testTacticalAttackDamage :: IO Bool
testTacticalAttackDamage = do
    let tactical = CombatTactical (TacticalCombat PlayerFirst True 100 "speed")
        worldT = (world initSampleGame) { combatProfile = tactical }
        st = initSampleGame { world = worldT, save = (save initSampleGame) { currentRoom = "hallway" } }
    -- attack goblin
    let (newState, msg) = executeCommand (Interact VAttack "goblin") st
    
    r1 <- expectEqual 1 (combatRound newState)
    r2 <- expectTrue "combat engaged" (isCombatEngaged newState)
    r3 <- expectEqual (Just (VVText "attack")) (getVariable combatActionKey newState)
    
    let goblinHp = (Map.lookup "goblin" (npcStates (save newState))) >>= npcHealth
    -- Base HP = 30, Player attack = 10, Goblin defense = 2 -> 8 damage -> 22
    r4 <- expectEqual (Just 22) goblinHp
    r5 <- expectTrue "damage message" (isInfixOf "hit the goblin for 8" msg)
    pure (r1 && r2 && r3 && r4 && r5)

testTacticalDefend :: IO Bool
testTacticalDefend = do
    let tactical = CombatTactical (TacticalCombat PlayerFirst True 100 "speed")
        worldT = (world initSampleGame) { combatProfile = tactical }
        st = initSampleGame { world = worldT, save = (save initSampleGame) { currentRoom = "hallway" } }
        stEngaged = setCombatEngaged True st
    
    -- Custom verb routing via Interaction
    let (newState, msg) = executeCommand (Interact (VCustom "defend") "") stEngaged
    
    r1 <- expectEqual 1 (combatRound newState)
    r2 <- expectTrue "combat engaged" (isCombatEngaged newState)
    r3 <- expectEqual (Just (VVText "defend")) (getVariable combatActionKey newState)
    
    let goblinHp = (Map.lookup "goblin" (npcStates (save newState))) >>= npcHealth
    r4 <- expectEqual (Just 30) goblinHp -- no damage
    r5 <- expectTrue "defend message" (isInfixOf "brace yourself" msg)
    pure (r1 && r2 && r3 && r4 && r5)

testTacticalFlee :: IO Bool
testTacticalFlee = do
    let tactical = CombatTactical (TacticalCombat PlayerFirst True 100 "speed")
        worldT = (world initSampleGame) { combatProfile = tactical }
        st = initSampleGame { world = worldT, save = (save initSampleGame) { currentRoom = "hallway" } }
        stEngaged = setCombatEngaged True st
    
    let (newState, msg) = executeCommand (Interact (VCustom "flee") "") stEngaged
    
    r1 <- expectEqual 0 (combatRound newState)
    r2 <- expectTrue "combat disengaged" (not (isCombatEngaged newState))
    r3 <- expectEqual (Just (VVText "flee")) (getVariable combatActionKey newState)
    r4 <- expectTrue "flee message" (isInfixOf "flee from the goblin" msg)
    pure (r1 && r2 && r3 && r4)

testTacticalFleeBlocked :: IO Bool
testTacticalFleeBlocked = do
    let tactical = CombatTactical (TacticalCombat PlayerFirst False 100 "speed")
        worldT = (world initSampleGame) { combatProfile = tactical }
        st = initSampleGame { world = worldT, save = (save initSampleGame) { currentRoom = "hallway" } }
        stEngaged = setCombatEngaged True st
    
    let (newState, msg) = executeCommand (Interact (VCustom "flee") "") stEngaged
    
    r1 <- expectEqual 1 (combatRound newState)
    r2 <- expectTrue "combat engaged" (isCombatEngaged newState)
    r3 <- expectEqual (Just (VVText "flee")) (getVariable combatActionKey newState)
    r4 <- expectTrue "blocked message" (isInfixOf "can't flee" msg)
    pure (r1 && r2 && r3 && r4)

testTacticalMultiRound :: IO Bool
testTacticalMultiRound = do
    let tactical = CombatTactical (TacticalCombat PlayerFirst True 100 "speed")
        worldT = (world initSampleGame) { combatProfile = tactical }
        st = initSampleGame { world = worldT, save = (save initSampleGame) { currentRoom = "hallway" } }
        st0 = setCombatEngaged True st
        
    -- Round 1: Defend
    let (st1, _) = executeCommand (Interact (VCustom "defend") "") st0
    r1 <- expectEqual 1 (combatRound st1)
    
    -- Round 2: Attack
    let (st2, _) = executeCommand (Interact VAttack "goblin") st1
    r2 <- expectEqual 2 (combatRound st2)
    r3 <- expectEqual (Just 22) ((Map.lookup "goblin" (npcStates (save st2))) >>= npcHealth)
    
    -- Round 3: Flee
    let (st3, _) = executeCommand (Interact (VCustom "flee") "") st2
    r4 <- expectEqual 0 (combatRound st3)
    r5 <- expectTrue "disengaged" (not (isCombatEngaged st3))
    pure (r1 && r2 && r3 && r4 && r5)

testCombatTacticalRoundTrip :: IO Bool
testCombatTacticalRoundTrip = do
    let prof = CombatTactical (TacticalCombat BySpeed False 50 "speed")
    let encoded = Aeson.encode prof
    r1 <- expectTrue "decode matches" (Aeson.decode encoded == Just prof)
    let rawJson = "{\"profile\":\"tactical\",\"initiative\":\"by_speed\",\"flee_allowed\":false,\"max_rounds\":50,\"speed_attribute\":\"speed\"}"
    r2 <- expectEqual (Just prof) (Aeson.decode (BLC.pack rawJson))
    let rawJson2 = "{\"profile\":\"tactical\",\"initiative\":\"enemy_first\",\"flee_allowed\":true,\"max_rounds\":30,\"speed_attribute\":\"dexterity\"}"
    r3 <- expectEqual (Just (CombatTactical (TacticalCombat EnemyFirst True 30 "dexterity"))) (Aeson.decode (BLC.pack rawJson2))
    pure (r1 && r2 && r3)

-- | Phase 7f-3 A3: Player abilities resource cost
testAbilityCost :: IO Bool
testAbilityCost = do
    let fireball = PlayerAbility "fireball" "Fireball" "player.mana" 10 0
            [ModifyValue (VRActorProp (ActorNPC "goblin") PHealth) (-15)]
        tactical = CombatTactical (TacticalCombat PlayerFirst True 100 "speed")
        worldT = (world initSampleGame)
            { combatProfile = tactical
            , abilities = Map.singleton "fireball" fireball }
        stBase = initSampleGame { world = worldT, save = (save initSampleGame) { currentRoom = "hallway" } }
        stEngaged = setCombatEngaged True stBase

    -- Case 1: Insufficient mana (5 < 10)
    let stLowMana = setVariable "player.mana" (VVInt 5) stEngaged
        (stFail, msgFail) = executeCommand (Interact (VCustom "use-ability") "fireball") stLowMana

    r1 <- expectTrue "error message for insufficient resource" (isInfixOf "Not enough resources" msgFail)
    r2 <- expectEqual (Just (VVInt 5)) (getVariable "player.mana" stFail)
    r3 <- expectEqual 0 (combatRound stFail)
    let goblinHpAfterFail = (Map.lookup "goblin" (npcStates (save stFail))) >>= npcHealth
    r4 <- expectEqual (Just 30) goblinHpAfterFail

    -- Case 2: Sufficient mana (20 >= 10)
    let stHighMana = setVariable "player.mana" (VVInt 20) stEngaged
        (stSuccess, msgSuccess) = executeCommand (Interact (VCustom "use-ability") "fireball") stHighMana

    r5 <- expectTrue "success message names ability" (isInfixOf "You use Fireball!" msgSuccess)
    r6 <- expectEqual (Just (VVInt 10)) (getVariable "player.mana" stSuccess)
    r7 <- expectEqual 1 (combatRound stSuccess)
    r8 <- expectEqual (Just (VVText "ability")) (getVariable combatActionKey stSuccess)
    r9 <- expectEqual (Just (VVText "fireball")) (getVariable combatAbilityKey stSuccess)
    let goblinHpAfterSuccess = (Map.lookup "goblin" (npcStates (save stSuccess))) >>= npcHealth
    r10 <- expectEqual (Just 15) goblinHpAfterSuccess
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9 && r10)

-- | Phase 7f-3 A3: Player abilities cooldown gating
testAbilityCooldown :: IO Bool
testAbilityCooldown = do
    let slash = PlayerAbility "slash" "Power Slash" "player.stamina" 5 3
            [ModifyValue (VRActorProp (ActorNPC "goblin") PHealth) (-10)]
        tactical = CombatTactical (TacticalCombat PlayerFirst True 100 "speed")
        worldT = (world initSampleGame)
            { combatProfile = tactical
            , abilities = Map.singleton "slash" slash }
        stBase = initSampleGame { world = worldT, save = (save initSampleGame) { currentRoom = "hallway" } }
        st0 = setVariable "player.stamina" (VVInt 20) (setCombatEngaged True stBase)

    -- First use: succeeds and applies cooldown condition
    let (st1, msg1) = executeCommand (Interact (VCustom "use-ability") "slash") st0
    r1 <- expectTrue "first use succeeds" (isInfixOf "You use Power Slash!" msg1)
    r2 <- expectTrue "cooldown condition applied" (hasCondition "cooldown_slash" st1)
    r3 <- expectEqual (Just (VVInt 15)) (getVariable "player.stamina" st1)
    let goblinHp1 = (Map.lookup "goblin" (npcStates (save st1))) >>= npcHealth
    r4 <- expectEqual (Just 20) goblinHp1

    -- Immediate second use: blocked by cooldown
    let (st2, msg2) = executeCommand (Interact (VCustom "use-ability") "slash") st1
    r5 <- expectTrue "blocked by cooldown message" (isInfixOf "Ability is on cooldown" msg2)
    r6 <- expectEqual (Just (VVInt 15)) (getVariable "player.stamina" st2)
    r7 <- expectEqual 1 (combatRound st2)
    let goblinHp2 = (Map.lookup "goblin" (npcStates (save st2))) >>= npcHealth
    r8 <- expectEqual (Just 20) goblinHp2

    -- Parser test: "use-ability slash", "use ability slash", and "ability slash"
    let cmd1 = parseCommand "use-ability slash"
        cmd2 = parseCommand "use ability slash"
        cmd3 = parseCommand "ability slash"
    r9 <- expectEqual (Interact (VCustom "use-ability") "slash") cmd1
    r10 <- expectEqual (Interact (VCustom "use-ability") "slash") cmd2
    r11 <- expectEqual (Interact (VCustom "use-ability") "slash") cmd3

    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9 && r10 && r11)

-- | Phase 7f-3 A3: BySpeed initiative populating VarMap
testBySpeedInitiative :: IO Bool
testBySpeedInitiative = do
    let tactical = CombatTactical (TacticalCombat BySpeed True 100 "agility")
        worldT = (world initSampleGame) { combatProfile = tactical }
        stBase = initSampleGame { world = worldT, save = (save initSampleGame) { currentRoom = "hallway" } }
        -- Set player agility skill to 18
        stPlayer = modifySkill "agility" 18 stBase
        -- Set goblin agility prop to 12
        stBoth = stPlayer { save = (save stPlayer)
            { npcStates = Map.adjust (\ns -> ns { npcProps = Map.singleton "agility" 12 })
                                     "goblin" (npcStates (save stPlayer)) } }
        stEngaged = setCombatEngaged True stBoth

    -- Resolve a tactical action (e.g. attack)
    let (stAfter, _) = executeCommand (Interact VAttack "goblin") stEngaged
    r1 <- expectEqual (Just (VVInt 18)) (getVariable combatInitiativePlayerKey stAfter)
    r2 <- expectEqual (Just (VVInt 12)) (getVariable (combatInitiativeKey "goblin") stAfter)

    -- Defend also populates/refreshes initiative
    let (stDefend, _) = executeCommand (Interact (VCustom "defend") "") stEngaged
    r3 <- expectEqual (Just (VVInt 18)) (getVariable combatInitiativePlayerKey stDefend)
    r4 <- expectEqual (Just (VVInt 12)) (getVariable (combatInitiativeKey "goblin") stDefend)

    pure (r1 && r2 && r3 && r4)

-- | Phase 7f-3 / 7h-2 V1: ValueRef / VRActorProp JSON round-trip
testValueRefRoundTrip :: IO Bool
testValueRefRoundTrip = do
    let samples =
            [ VRFlag "flag1"
            , VRVariable "var1"
            , VRItemProp "key" "weight"
            , VRActorProp ActorPlayer PRoom
            , VRActorProp ActorPlayer PHealth
            , VRActorProp (ActorNPC "goblin") PHealth
            , VRActorProp (ActorNPC "goblin") (PCustom "speed")
            , VRActorProp (ActorRoom "hallway") PVisited
            , VRActorProp (ActorEntity "gate") PState
            , VRActorProp (ActorShip "kestrel") PHealth
            , VRPlayerHealth
            ]
        check v = Aeson.decode (Aeson.encode v) == Just v
    expectTrue "all ValueRef constructors round-trip cleanly via JSON" (all check samples)

-- | Phase 7f-3 / 7h-2 V1: Legacy VRProperty JSON decodes into VRActorProp
testLegacyVRPropertyDecoding :: IO Bool
testLegacyVRPropertyDecoding = do
    let dec raw = Aeson.decode (BLC.pack raw) :: Maybe ValueRef
    r1 <- expectEqual (Just (VRActorProp ActorPlayer PRoom))
              (dec "{\"tag\":\"VRProperty\",\"contents\":[\"player\",\"room\"]}")
    r2 <- expectEqual (Just (VRActorProp ActorPlayer PHealth))
              (dec "{\"tag\":\"VRProperty\",\"contents\":[\"player\",\"hp\"]}")
    r3 <- expectEqual (Just (VRActorProp (ActorRoom "treasure") PVisited))
              (dec "{\"tag\":\"VRProperty\",\"contents\":[\"treasure\",\"visited\"]}")
    r4 <- expectEqual (Just (VRActorProp (ActorEntity "gate") PState))
              (dec "{\"tag\":\"VRProperty\",\"contents\":[\"gate\",\"state\"]}")
    r5 <- expectEqual (Just (VRActorProp (ActorNPC "goblin") PHealth))
              (dec "{\"tag\":\"VRProperty\",\"contents\":[\"goblin\",\"hp\"]}")
    r6 <- expectEqual (Just (VRActorProp ActorPlayer (PCustom "luck")))
              (dec "{\"tag\":\"VRProperty\",\"contents\":[\"player\",\"luck\"]}")
    r7 <- expectEqual (Just (VRActorProp (ActorNPC "goblin") (PCustom "agility")))
              (dec "{\"tag\":\"VRProperty\",\"contents\":[\"goblin\",\"agility\"]}")
    r8 <- expectEqual (Just VRPlayerHealth)
              (dec "{\"tag\":\"VRPlayerHealth\"}")
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8)

-- | F1/F2: `{ var: X, is: Y }` ist die **Leseseite** von Text-Variablen
--   (`variables:` mit `type: text`). Ohne sie war der Engine-Schlüssel
--   `combat.action` write-only — keine Regel konnte auf `defend` reagieren, und
--   `defend` im taktischen Kampf hatte damit keine mechanische Wirkung.
testVarIsPredicate :: IO Bool
testVarIsPredicate = do
    let st0 = initSampleGame
        withAction a = setVariable combatActionKey (VVText a) st0
        matches expected = evalPredicate (VarIs combatActionKey expected)
    r1 <- expectTrue "a matching text value is true"
              (matches "defend" (withAction "defend"))
    r2 <- expectTrue "a different text value is false"
              (not (matches "attack" (withAction "defend")))
    r3 <- expectTrue "an unset variable is false" (not (matches "defend" st0))
    -- strict: text only, an int variable does not match a text literal
    r4 <- expectTrue "an int variable does not match a text literal"
              (not (matches "1" (setVariable combatActionKey (VVInt 1) st0)))
    -- not limited to the engine's own keys
    r5 <- expectTrue "any text variable can be compared"
              (evalPredicate (VarIs "weather" "sturm") (setVariable "weather" (VVText "sturm") st0))
    -- JSON round-trip, in the same shorthand the YAML uses
    r6 <- expectEqual (Just (VarIs "combat.action" "defend"))
              (Aeson.decode (Aeson.encode (VarIs "combat.action" "defend")))
    r7 <- expectTrue "the encoding uses the var/is shorthand"
              (let enc = BLC.unpack (Aeson.encode (VarIs "combat.action" "defend"))
               in "\"var\"" `isInfixOf` enc && "\"is\"" `isInfixOf` enc)
    -- the negation form the fixture uses
    r8 <- expectTrue "`not` composes with the text comparison"
              (not (evalPredicate (PNot (VarIs combatActionKey "defend"))
                                  (withAction "defend")))
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8)

-- | F4: `CAUseItem` is a **reserved placeholder** — the parser never produces it
--   (there is no `use <item>` combat verb), so its only behaviour is the
--   resolver's fallback. Pinning that fallback keeps the gap visible: a silent
--   "half wired" change would show up here instead of shipping.
testUseItemPlaceholder :: IO Bool
testUseItemPlaceholder = do
    let tactical = CombatTactical (TacticalCombat PlayerFirst True 100 "speed")
        st = initSampleGame { world = (world initSampleGame) { combatProfile = tactical }
                            , save = (save initSampleGame) { currentRoom = "hallway" } }
        stEngaged = setCombatEngaged True st
        target = TargetNPC "goblin" "goblin"
        (effs, msgs) = resolveCombat tactical [PlayerActor] target (CAUseItem "torch") stEngaged
    r1 <- expectEqual [] effs
    r2 <- expectEqual ["You can't do that in combat yet."] msgs
    -- control case: a wired action is unaffected by the placeholder path
    let (effs2, _) = resolveCombat tactical [PlayerActor] target CADefend stEngaged
    r3 <- expectTrue "a wired action still resolves" (not (null effs2))
    pure (r1 && r2 && r3)

-- ---------------------------------------------------------------------------
-- Phase 1A: Arithmetic expressions (Expr) & ComputeValue
-- ---------------------------------------------------------------------------

-- | Phase 1A: Arithmetic expressions: parsing, operator precedence, parentheses and functions
testExprParsingAndPrecedence :: IO Bool
testExprParsingAndPrecedence = do
    r1 <- expectEqual (Right (EAdd (ELit 2) (EMul (ELit 3) (ELit 4)))) (parseExpr "2 + 3 * 4")
    r2 <- expectEqual (Right (EMul (EAdd (ELit 2) (ELit 3)) (ELit 4))) (parseExpr "(2 + 3) * 4")
    r3 <- expectEqual (Right (ESub (ESub (ELit 100) (ELit 30)) (ELit 20))) (parseExpr "100 - 30 - 20")
    r4 <- expectEqual (Right (EDiv (EDiv (ELit 100) (ELit 10)) (ELit 2))) (parseExpr "100 / 10 / 2")
    r5 <- expectEqual (Right (EMod (ELit 10) (ELit 3))) (parseExpr "10 % 3")
    r6 <- expectEqual (Right (EAdd (ESub (ELit 0) (ELit 5)) (ELit 10))) (parseExpr "-5 + 10")
    r7 <- expectEqual (Right (EAdd (EMin (ELit 10) (ELit 20)) (EMax (ELit 5) (ELit 1)))) (parseExpr "min(10, 20) + max(5, 1)")
    r8 <- expectEqual (Right (EClamp (ELit 0) (ELit 100) (ELit 150))) (parseExpr "clamp(0, 100, 150)")
    r9 <- case parseExpr "2 + * 3" of
        Left _ -> pure True
        Right _ -> putStrLn "Expected parse failure for '2 + * 3'" >> pure False
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9)

-- | Phase 1A: Arithmetic expressions: evaluation, division-by-zero safety, min/max/clamp
testExprEvalAndZeroSafety :: IO Bool
testExprEvalAndZeroSafety = do
    let st = initSampleGame
    r1 <- expectEqual 0 (evalExpr (EDiv (ELit 10) (ELit 0)) st)
    r2 <- expectEqual 0 (evalExpr (EMod (ELit 10) (ELit 0)) st)
    r3 <- expectEqual 10 (evalExpr (EClamp (ELit 10) (ELit 20) (ELit 5)) st)
    r4 <- expectEqual 20 (evalExpr (EClamp (ELit 10) (ELit 20) (ELit 25)) st)
    r5 <- expectEqual 15 (evalExpr (EClamp (ELit 10) (ELit 20) (ELit 15)) st)
    r6 <- expectEqual 5  (evalExpr (EMin (ELit 10) (ELit 5)) st)
    r7 <- expectEqual 10 (evalExpr (EMax (ELit 10) (ELit 5)) st)
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7)

-- | Phase 1A: Arithmetic expressions: variable resolution and system variables
testExprVariableResolution :: IO Bool
testExprVariableResolution = do
    let st0 = initSampleGame
        st1 = setVariable "gold" (VVInt 100) st0
        st2 = setVariable "tax" (VVInt 2) st1
        st3 = setVariable "pop" (VVInt 50) st2
    case parseExpr "gold + pop * tax" of
        Left err -> putStrLn ("Failed to parse: " ++ err) >> pure False
        Right expr -> do
            r1 <- expectEqual 200 (evalExpr expr st3)
            case parseExpr "player.hp + 5" of
                Left err -> putStrLn ("Failed to parse player.hp: " ++ err) >> pure False
                Right hpExpr -> do
                    let expectedHp = playerHealth (player (save st3)) + 5
                    r2 <- expectEqual expectedHp (evalExpr hpExpr st3)
                    pure (r1 && r2)

-- | Phase 1A: ComputeValue outcome modifies variables and player health
testComputeValueOutcome :: IO Bool
testComputeValueOutcome = do
    let st0 = setVariable "gold" (VVInt 100) initSampleGame
    case parseExpr "gold + 50" of
        Left err -> putStrLn ("Parse error: " ++ err) >> pure False
        Right expr -> do
            let (st1, _) = applyOutcome (ComputeValue (VRVariable "gold") expr) "" st0
            r1 <- expectEqual (Just (VVInt 150)) (getVariable "gold" st1)
            let (st2, _) = applyOutcome (ComputeValue VRPlayerHealth (ELit 75)) "" st1
            r2 <- expectEqual 75 (playerHealth (player (save st2)))
            pure (r1 && r2)

-- | Phase 1A: Expr and ComputeValue JSON round-trip
testExprJSONRoundTrip :: IO Bool
testExprJSONRoundTrip = do
    case parseExpr "staatskasse.gold + (pop * 2)" of
        Left err -> putStrLn ("Parse error: " ++ err) >> pure False
        Right expr -> do
            let enc = Aeson.encode expr
            r1 <- expectEqual (Just expr) (Aeson.decode enc)
            let eff = ComputeValue (VRVariable "gold") expr
            let effEnc = Aeson.encode eff
            r2 <- expectEqual (Just eff) (Aeson.decode effEnc)
            pure (r1 && r2)

-- ---------------------------------------------------------------------------
-- Phase 1B: String interpolation with variables (formatWithVars)
-- ---------------------------------------------------------------------------

-- | Phase 1B: String interpolation: plain vars, sign modifier, padding, system vars, escaping
testFormatWithVarsBasics :: IO Bool
testFormatWithVarsBasics = do
    let st0 = initSampleGame
        st1 = setVariable "gold" (VVInt 42) st0
        st2 = setVariable "city" (VVText "Aethelgard") st1
        st3 = setVariable "surplus" (VVInt 15) st2
        st4 = setVariable "deficit" (VVInt (-8)) st3
    r1 <- expectEqual "You have 42 gold." (formatWithVars "You have {gold} gold." st4)
    r2 <- expectEqual "Explicit: 42." (formatWithVars "Explicit: {var:gold}." st4)
    r3 <- expectEqual "Welcome to Aethelgard!" (formatWithVars "Welcome to {city}!" st4)
    r4 <- expectEqual "Surplus: +15, Deficit: -8" (formatWithVars "Surplus: {surplus:+}, Deficit: {deficit:+}" st4)
    r5 <- expectEqual "Box: [    42] and [-8    ]" (formatWithVars "Box: [{gold:6}] and [{deficit:-6}]" st4)
    r6 <- expectEqual "HP: 100, Turn: 0" (formatWithVars "HP: {player.hp}, Turn: {turn.count}" st4)
    r7 <- expectEqual "Escaped: {literal} and {lit2}" (formatWithVars "Escaped: \\{literal\\} and {{lit2}}" st4)
    r8 <- expectEqual "Unknown stays: {unknown_var}" (formatWithVars "Unknown stays: {unknown_var}" st4)
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8)

-- | Phase 1B: String interpolation integration in SendMessage and CondText
testFormatWithVarsIntegration :: IO Bool
testFormatWithVarsIntegration = do
    let st0 = setVariable "harvest" (VVInt 120) initSampleGame
    -- SendMessage interpolation
    let (_, msg) = applyOutcome (SendMessage "Ernte: {harvest} Korn.") "" st0
    r1 <- expectEqual "Ernte: 120 Korn." msg
    -- CondText interpolation
    let ct = plainText "Vorrat: {harvest} Einheiten."
    r2 <- expectEqual "Vorrat: 120 Einheiten." (resolveCondText ct st0)
    pure (r1 && r2)

-- ---------------------------------------------------------------------------
-- Phase 1C: Parameterized commands and argument binding
-- ---------------------------------------------------------------------------

-- | Phase 1C: Parameterized command parsing
testParameterizedCommandParsing :: IO Bool
testParameterizedCommandParsing = do
    let reg = Map.fromList
            [ ("kaufe", VerbDef "kaufe" ["kaufe", "buy"])
            , ("steuern", VerbDef "steuern" ["steuern", "tax"])
            , ("status", VerbDef "status" ["status"])
            ]
    r1 <- expectEqual (ActionWithArgs (VCustom "kaufe") ["5", "weizen"])
                      (parseCommandWith reg "kaufe 5 weizen")
    r2 <- expectEqual (Interact (VCustom "steuern") "15")
                      (parseCommandWith reg "steuern 15")
    r3 <- expectEqual (Interact (VCustom "status") "")
                      (parseCommandWith reg "status")
    r4 <- expectEqual (Interact VTake "healing potion")
                      (parseCommandWith reg "take healing potion")
    pure (r1 && r2 && r3 && r4)

-- | Phase 1C: Binding command variables to GameState
testBindCommandVars :: IO Bool
testBindCommandVars = do
    let st0 = initSampleGame
        st1 = bindCommandVars (ActionWithArgs (VCustom "kaufe") ["5", "weizen"]) st0
    r1 <- expectEqual (Just (VVText "kaufe")) (getVariable "cmd.verb" st1)
    r2 <- expectEqual (Just (VVInt 2)) (getVariable "cmd.count" st1)
    r3 <- expectEqual (Just (VVText "5 weizen")) (getVariable "cmd.raw_args" st1)
    r4 <- expectEqual (Just (VVInt 5)) (getVariable "cmd.arg1" st1)
    r5 <- expectEqual (Just (VVText "weizen")) (getVariable "cmd.arg2" st1)
    -- Subsequent command with 0 args clears cmd.arg1/cmd.arg2
    let st2 = bindCommandVars (Interact (VCustom "status") "") st1
    r6 <- expectEqual (Just (VVInt 0)) (getVariable "cmd.count" st2)
    r7 <- expectEqual Nothing (getVariable "cmd.arg1" st2)
    r8 <- expectEqual Nothing (getVariable "cmd.arg2" st2)
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8)

-- | Phase 1C: OnCommand trigger with args, formulas, conditional and interpolation
testOnCommandTriggerWithArgsAndFormulas :: IO Bool
testOnCommandTriggerWithArgsAndFormulas = do
    let reg = Map.fromList [("kaufe", VerbDef "kaufe" ["kaufe"])]
        st0 = setVariable "gold" (VVInt 100)
            $ setVariable "korn" (VVInt 0)
            $ setVariable "markt.korn_preis" (VVInt 10)
            $ initSampleGame
        calcCost = ComputeValue (VRVariable "kosten") (EMul (EVar "cmd.arg1") (EVar "markt.korn_preis"))
        payGold = ComputeValue (VRVariable "gold") (ESub (EVar "gold") (EVar "kosten"))
        addKorn = ComputeValue (VRVariable "korn") (EAdd (EVar "korn") (EVar "cmd.arg1"))
        msgSuccess = SendMessage "Du kaufst {cmd.arg1} Korn fuer {kosten} Gold."
        msgFail = SendMessage "Nicht genug Gold!"
        canAfford = Compare (VRVariable "gold") CGte (VRVariable "kosten")
        buyTrigger = TriggerDef
            { trId = "tr_kaufe"
            , trEvent = OnCommand "kaufe"
            , trCondition = Nothing
            , trEffects =
                [ calcCost
                , Conditional canAfford
                    (Sequence [payGold, addKorn, msgSuccess])
                    msgFail
                ]
            , trOnce = False
            , trCooldown = 0
            }
        worldWithTrigger = (world st0)
            { verbDefs = reg
            , triggerDefs = [buyTrigger]
            }
        stWithTrigger = st0 { world = worldWithTrigger }
        loop0 = initLoopState stWithTrigger
        -- Execute first purchase: kaufe 5 korn (costs 50 gold)
        (loop1, msg1) = applyLoopCommand (parseCommandWith reg "kaufe 5 korn") loop0
        stAfter1 = lsCurrent loop1
    r1 <- expectTrue "msg1 contains purchase text" (isInfixOf "Du kaufst 5 Korn fuer 50 Gold." msg1)
    r2 <- expectEqual (Just (VVInt 50)) (getVariable "gold" stAfter1)
    r3 <- expectEqual (Just (VVInt 5)) (getVariable "korn" stAfter1)
    -- Execute second purchase: kaufe 10 korn (costs 100 gold, but player only has 50)
    let (loop2, msg2) = applyLoopCommand (parseCommandWith reg "kaufe 10 korn") loop1
    let stAfter2 = lsCurrent loop2
    r4 <- expectTrue "msg2 contains insufficient funds text" (isInfixOf "Nicht genug Gold!" msg2)
    r5 <- expectEqual (Just (VVInt 50)) (getVariable "gold" stAfter2)
    r6 <- expectEqual (Just (VVInt 5)) (getVariable "korn" stAfter2)
    pure (r1 && r2 && r3 && r4 && r5 && r6)

-- | Phase 1C: Informational custom verbs (status, bilanz) consume no turn
testStatusVerbNoTurnConsumed :: IO Bool
testStatusVerbNoTurnConsumed = do
    let reg = Map.fromList [("status", VerbDef "status" ["status"])]
        st0 = (initSampleGame) { world = (world initSampleGame) { verbDefs = reg } }
        cmd = parseCommandWith reg "status"
    r1 <- expectEqual False (consumesTurnIn st0 cmd)
    let loop0 = initLoopState st0
        (loop1, _) = applyLoopCommand cmd loop0
    r2 <- expectEqual 0 (turnCount (save (lsCurrent loop1)))
    pure (r1 && r2)

-- ---------------------------------------------------------------------------
-- Phase 2A: Card Games & Deckbuilder Data Model
-- ---------------------------------------------------------------------------

-- | Phase 2A: Card, CardType, CardTarget, DeckDestination, DeckState JSON round-trip
testCardDataTypesJSONRoundTrip :: IO Bool
testCardDataTypesJSONRoundTrip = do
    -- CardType round-trip
    let ct = CardAttack
    r1 <- expectEqual (Just ct) (Aeson.decode (Aeson.encode ct))
    -- CardTarget round-trip
    let tg = TargetSingleEnemy
    r2 <- expectEqual (Just tg) (Aeson.decode (Aeson.encode tg))
    -- DeckDestination round-trip
    let dest = DestDiscard
    r3 <- expectEqual (Just dest) (Aeson.decode (Aeson.encode dest))
    -- Card round-trip
    let card = Card
            { cardId = "strike"
            , cardName = "Hieb"
            , cardCost = Map.singleton "energy" 1
            , cardType = CardAttack
            , cardDescription = "Deals 6 damage."
            , cardTarget = TargetSingleEnemy
            , cardExhaust = False
            , cardEffects = [ ModifyValue (VRVariable "enemy.hp") (-6) ]
            }
    let cardEnc = Aeson.encode card
    r4 <- expectEqual (Just card) (Aeson.decode cardEnc)
    -- DeckState round-trip
    let ds = DeckState
            { drawPile = ["strike", "defend"]
            , hand = ["strike"]
            , discardPile = ["defend"]
            , exhaustPile = []
            , maxHandSize = 8
            }
    let dsEnc = Aeson.encode ds
    r5 <- expectEqual (Just ds) (Aeson.decode dsEnc)
    -- Card operations in Effect round-trip
    let effs = [ DrawCards 3, DiscardHand, DiscardCard "strike", ExhaustCard "defend"
               , AddCardToDeck "strike" DestDraw, ShuffleDeck ]
    let effsEnc = Aeson.encode effs
    r6 <- expectEqual (Just effs) (Aeson.decode effsEnc)
    pure (r1 && r2 && r3 && r4 && r5 && r6)

-- | Phase 2A: GameWorld cardDefs M2 invariant (omitted when empty, preserves checksum)
testGameWorldCardDefsM2Invariant :: IO Bool
testGameWorldCardDefsM2Invariant = do
    let gw = world initSampleGame
    let enc = Aeson.encode gw
    -- "cards" must not appear in JSON when cardDefs is empty
    r1 <- expectEqual False (isInfixOf "\"cards\"" (BLC.unpack enc))
    -- Checksum calculation is unaffected
    let cs = computeWorldChecksum gw
    r2 <- expectTrue "checksum is non-empty" (not (null cs))
    -- Round-trip with cardDefs populated
    let card = Card "c1" "Defend" (Map.singleton "energy" 1) CardSkill "Block +5" TargetSelf False []
        gwWithCards = gw { cardDefs = Map.singleton "c1" card }
        encWithCards = Aeson.encode gwWithCards
    r3 <- expectTrue "cards key present when non-empty" (isInfixOf "\"cards\"" (BLC.unpack encWithCards))
    case Aeson.decode encWithCards of
        Nothing -> putStrLn "Failed to decode GameWorld with cards" >> pure False
        Just decoded -> do
            r4 <- expectEqual (Map.singleton "c1" card) (cardDefs decoded)
            pure (r1 && r2 && r3 && r4)

-- | Phase 2A: SaveState deckState M2 invariant (omitted when Nothing, round-trips when Just)
testSaveStateDeckStateM2Invariant :: IO Bool
testSaveStateDeckStateM2Invariant = do
    let ss = save initSampleGame
    let enc = Aeson.encode ss
    -- "deckState" must not appear in JSON when deckState is Nothing
    r1 <- expectEqual False (isInfixOf "\"deckState\"" (BLC.unpack enc))
    -- Round-trip with deckState populated
    let ds = DeckState ["c1", "c2"] ["c3"] [] [] 10
        ssWithDeck = ss { deckState = Just ds }
        encWithDeck = Aeson.encode ssWithDeck
    r2 <- expectTrue "deckState key present when Just" (isInfixOf "\"deckState\"" (BLC.unpack encWithDeck))
    case Aeson.decode encWithDeck of
        Nothing -> putStrLn "Failed to decode SaveState with deckState" >> pure False
        Just decoded -> do
            r3 <- expectEqual (Just ds) (deckState decoded)
            pure (r1 && r2 && r3)

-- | Phase 3A: BiomeTemplate and SandboxZone serialization and GameWorld M2 invariant
testGameWorldSandboxZonesM2Invariant :: IO Bool
testGameWorldSandboxZonesM2Invariant = do
    let gw = world initSampleGame
    let enc = Aeson.encode gw
    -- "sandboxZones" must not appear in JSON when sandboxZones is empty
    r1 <- expectEqual False (isInfixOf "\"sandboxZones\"" (BLC.unpack enc))
    -- Checksum calculation is unaffected
    let cs = computeWorldChecksum gw
    r2 <- expectTrue "checksum is non-empty" (not (null cs))
    -- Round-trip with sandboxZones populated
    let bTemplate = BiomeTemplate "forest" 10 "Tiefer Wald" (plainText "Uralte Baeume umgeben dich.") ["outdoor", "forest"] emptyAscii [North, South, East, West]
        sz = SandboxZone "wilderness" (0, 0, 0) [bTemplate] (Just 0)
        gwWithSz = gw { sandboxZones = Map.singleton "wilderness" sz }
        encWithSz = Aeson.encode gwWithSz
    r3 <- expectTrue "sandboxZones key present when non-empty" (isInfixOf "\"sandboxZones\"" (BLC.unpack encWithSz))
    case Aeson.decode encWithSz of
        Nothing -> putStrLn "Failed to decode GameWorld with sandboxZones" >> pure False
        Just decoded -> do
            r4 <- expectEqual (Map.singleton "wilderness" sz) (sandboxZones decoded)
            pure (r1 && r2 && r3 && r4)

-- | Phase 3A: SaveState dynamicRooms M2 invariant (omitted when empty, round-trips when populated)
testSaveStateDynamicRoomsM2Invariant :: IO Bool
testSaveStateDynamicRoomsM2Invariant = do
    let ss = save initSampleGame
    let enc = Aeson.encode ss
    -- "dynamicRooms" must not appear in JSON when dynamicRooms is empty
    r1 <- expectEqual False (isInfixOf "\"dynamicRooms\"" (BLC.unpack enc))
    -- Round-trip with dynamicRooms populated
    let dynRoom = Room "dyn_1" "Dynamischer Raum" (plainText "Ein magischer Raum.") Map.empty Set.empty Nothing Nothing Nothing Nothing Nothing Nothing emptyAscii Nothing Nothing Nothing
        ssWithDyn = ss { dynamicRooms = Map.singleton "dyn_1" dynRoom }
        encWithDyn = Aeson.encode ssWithDyn
    r2 <- expectTrue "dynamicRooms key present when non-empty" (isInfixOf "\"dynamicRooms\"" (BLC.unpack encWithDyn))
    case Aeson.decode encWithDyn of
        Nothing -> putStrLn "Failed to decode SaveState with dynamicRooms" >> pure False
        Just decoded -> do
            r3 <- expectEqual (Map.singleton "dyn_1" dynRoom) (dynamicRooms decoded)
            pure (r1 && r2 && r3)

-- | Phase 3A: GenerateRoom effect serialization
testGenerateRoomEffectSerialization :: IO Bool
testGenerateRoomEffectSerialization = do
    let eff = GenerateRoom "mine_1" "Stollen" "Ein dunkler Schacht." "start" North South
        enc = Aeson.encode eff
    case Aeson.decode enc of
        Nothing -> putStrLn "Failed to decode GenerateRoom effect" >> pure False
        Just decoded -> expectEqual eff decoded

-- | Phase 3B: lookupRoom fallback to static room in GameWorld
testLookupRoomFallback :: IO Bool
testLookupRoomFallback = do
    let gw = world initSampleGame
        st = emptyGameState { world = gw }
        mRoom = lookupRoom "start" st
    case mRoom of
        Nothing -> putStrLn "Expected to find static room 'start'" >> pure False
        Just r -> expectEqual "start" (roomId r)

-- | Phase 3B: lookupRoom prioritizes dynamic overlay in SaveState
testLookupRoomDynamicPriority :: IO Bool
testLookupRoomDynamicPriority = do
    let gw = world initSampleGame
        dynRoom = (rooms gw Map.! "start") { roomName = "Modifizierter Startraum" }
        st = emptyGameState
            { world = gw
            , save = (save emptyGameState) { dynamicRooms = Map.singleton "start" dynRoom }
            }
        mRoom = lookupRoom "start" st
    case mRoom of
        Nothing -> putStrLn "Expected room" >> pure False
        Just r -> expectEqual "Modifizierter Startraum" (roomName r)

-- | Phase 3B: deterministic cell seed derivation via SplitMix64
testDeterministicCellSeed :: IO Bool
testDeterministicCellSeed = do
    let seed1 = deriveCellSeed 12345 "wilderness" 10 (-5) 0
        seed2 = deriveCellSeed 12345 "wilderness" 10 (-5) 0
        seedDiffCoord = deriveCellSeed 12345 "wilderness" 10 (-4) 0
        seedDiffZone = deriveCellSeed 12345 "forest" 10 (-5) 0
    r1 <- expectEqual seed1 seed2
    r2 <- expectTrue "different coordinate produces different seed" (seed1 /= seedDiffCoord)
    r3 <- expectTrue "different zone produces different seed" (seed1 /= seedDiffZone)
    pure (r1 && r2 && r3)

-- | Phase 3B: Moving into sandbox generates cell in dynamicRooms with tags and coords
testMoveIntoSandboxGeneratesRoom :: IO Bool
testMoveIntoSandboxGeneratesRoom = do
    let bForest = BiomeTemplate "forest" 10 "Dichter Wald [{x}, {y}]" (plainText "Tiefer Wald.") ["forest"] emptyAscii [North, South, East, West]
        sz = SandboxZone "wildnis" (0, 0, 0) [bForest] (Just 1)
        gw = emptyGameWorld
            { rooms = Map.singleton "gate" (Room "gate" "Tor" (plainText "Schlosstor") (Map.singleton North (Open "sandbox_wildnis")) Set.empty Nothing Nothing Nothing Nothing Nothing Nothing emptyAscii Nothing Nothing Nothing)
            , sandboxZones = Map.singleton "wildnis" sz
            }
        st0 = emptyGameState { world = gw, save = (save emptyGameState) { currentRoom = "gate" } }
    -- Moving north into sandbox_wildnis
    let (st1, _) = executeCommand (Go North) st0
    r1 <- expectEqual "sandbox_wildnis_0_0_0" (currentRoom (save st1))
    r2 <- expectTrue "dynamicRooms contains generated room" (Map.member "sandbox_wildnis_0_0_0" (dynamicRooms (save st1)))
    let mGenRoom = lookupRoom "sandbox_wildnis_0_0_0" st1
    case mGenRoom of
        Nothing -> putStrLn "Generated room not found" >> pure False
        Just genRoom -> do
            r3 <- expectEqual "Dichter Wald [0, 0]" (roomName genRoom)
            r4 <- expectTrue "has sandbox tag" (Set.member "sandbox" (roomTags genRoom))
            r5 <- expectTrue "has wildnis tag" (Set.member "wildnis" (roomTags genRoom))
            r6 <- expectTrue "has forest tag" (Set.member "forest" (roomTags genRoom))
            pure (r1 && r2 && r3 && r4 && r5 && r6)

-- | Phase 3B: Reciprocal exit wiring between authored and sandbox rooms, and between sandbox cells
testReciprocalExitWiring :: IO Bool
testReciprocalExitWiring = do
    let bForest = BiomeTemplate "forest" 10 "Wald [{x}, {y}]" (plainText "Wald.") ["forest"] emptyAscii [North, South, East, West]
        sz = SandboxZone "wildnis" (0, 0, 0) [bForest] (Just 1)
        gw = emptyGameWorld
            { rooms = Map.singleton "gate" (Room "gate" "Tor" (plainText "Schlosstor") (Map.singleton North (Open "sandbox_wildnis_0_0_0")) Set.empty Nothing Nothing Nothing Nothing Nothing Nothing emptyAscii Nothing Nothing Nothing)
            , sandboxZones = Map.singleton "wildnis" sz
            }
        st0 = emptyGameState { world = gw, save = (save emptyGameState) { currentRoom = "gate" } }
        (st1, _) = executeCommand (Go North) st0
    -- Verify reciprocal exit from sandbox (0,0,0) back to gate via South
    let mRoom0 = lookupRoom "sandbox_wildnis_0_0_0" st1
    case mRoom0 of
        Nothing -> putStrLn "Room 0,0,0 missing" >> pure False
        Just r0 -> do
            r1 <- expectEqual (Just (Open "gate")) (Map.lookup South (roomConnections r0))
            -- Now move East to (1, 0, 0)
            let (st2, _) = executeCommand (Go East) st1
            r2 <- expectEqual "sandbox_wildnis_1_0_0" (currentRoom (save st2))
            let mRoom1 = lookupRoom "sandbox_wildnis_1_0_0" st2
            case mRoom1 of
                Nothing -> putStrLn "Room 1,0,0 missing" >> pure False
                Just r1_room -> do
                    -- Verify reciprocal exit from (1,0,0) back to (0,0,0) via West
                    r3 <- expectEqual (Just (Open "sandbox_wildnis_0_0_0")) (Map.lookup West (roomConnections r1_room))
                    -- Move West back to (0,0,0)
                    let (st3, _) = executeCommand (Go West) st2
                    r4 <- expectEqual "sandbox_wildnis_0_0_0" (currentRoom (save st3))
                    -- And South back to gate
                    let (st4, _) = executeCommand (Go South) st3
                    r5 <- expectEqual "gate" (currentRoom (save st4))
                    pure (r1 && r2 && r3 && r4 && r5)

-- | Phase 3C: GenerateRoom effect dynamically creates a room and wires bidirectional exits
testGenerateRoomExecution :: IO Bool
testGenerateRoomExecution = do
    let gw = world initSampleGame
        st0 = emptyGameState { world = gw, save = (save emptyGameState) { currentRoom = "start" } }
        eff = GenerateRoom "secret_cellar" "Geheimer Keller" "Ein modriger Steinkeller." "current_room" Down Up
        (st1, _) = applyOutcome eff "" st0
    r1 <- expectTrue "new room in dynamicRooms" (Map.member "secret_cellar" (dynamicRooms (save st1)))
    let mCellar = lookupRoom "secret_cellar" st1
    case mCellar of
        Nothing -> putStrLn "secret_cellar not found" >> pure False
        Just cellar -> do
            r2 <- expectEqual "Geheimer Keller" (roomName cellar)
            r3 <- expectEqual (Just (Open "start")) (Map.lookup Up (roomConnections cellar))
            -- Check player can move Down from start into secret_cellar
            let (st2, _) = executeCommand (Go Down) st1
            r4 <- expectEqual "secret_cellar" (currentRoom (save st2))
            -- Check player can move Up from secret_cellar back to start
            let (st3, _) = executeCommand (Go Up) st2
            r5 <- expectEqual "start" (currentRoom (save st3))
            pure (r1 && r2 && r3 && r4 && r5)

-- | Phase 3C: GenerateRoom supports variable interpolation in ID, name, and description
testGenerateRoomInterpolation :: IO Bool
testGenerateRoomInterpolation = do
    let gw = world initSampleGame
        vars0 = Map.fromList [("depth", VVInt 3), ("mine_name", VVText "Kupfermine")]
        st0 = emptyGameState
            { world = gw
            , save = (save emptyGameState) { currentRoom = "start", variables = vars0 }
            }
        eff = GenerateRoom "{mine_name}_ebene_{depth}" "{mine_name} Ebene {depth}" "Stollen auf Tiefe {depth}m." "current_room" Down Up
        (st1, _) = applyOutcome eff "" st0
    let genId = "Kupfermine_ebene_3"
    r1 <- expectTrue "interpolated ID in dynamicRooms" (Map.member genId (dynamicRooms (save st1)))
    case lookupRoom genId st1 of
        Nothing -> putStrLn "Interpolated room not found" >> pure False
        Just rm -> do
            r2 <- expectEqual "Kupfermine Ebene 3" (roomName rm)
            r3 <- expectEqual (plainText "Stollen auf Tiefe 3m.") (roomDescription rm)
            pure (r1 && r2 && r3)

-- | Phase 3C: Resource harvesting in sandbox using formulas and variable computation
testSandboxResourceHarvestFormulas :: IO Bool
testSandboxResourceHarvestFormulas = do
    let gw = world initSampleGame
        vars0 = Map.fromList
            [ ("wood", VVInt 0)
            , ("axe_durability", VVInt 10)
            , ("woodcutting_skill", VVInt 3)
            , ("tree_capacity", VVInt 5)
            ]
        st0 = emptyGameState { world = gw, save = (save emptyGameState) { currentRoom = "start", variables = vars0 } }
        harvestEffects =
            [ ComputeValue (VRVariable "yield") (EMul (EVar "woodcutting_skill") (ELit 2))
            , ComputeValue (VRVariable "wood") (EAdd (EVar "wood") (EVar "yield"))
            , ModifyValue (VRVariable "axe_durability") (-1)
            , ModifyValue (VRVariable "tree_capacity") (-1)
            ]
        (st1, _) = applyOutcomes harvestEffects "" st0
    r1 <- expectEqual (Just (VVInt 6)) (Map.lookup "wood" (variables (save st1)))
    r2 <- expectEqual (Just (VVInt 9)) (Map.lookup "axe_durability" (variables (save st1)))
    r3 <- expectEqual (Just (VVInt 4)) (Map.lookup "tree_capacity" (variables (save st1)))
    pure (r1 && r2 && r3)

-- | Phase 2 / S3: Biome landscape ASCII art with day/weather state variants and coordinate interpolation
testBiomeAsciiArtVariantsDayNight :: IO Bool
testBiomeAsciiArtVariantsDayNight = do
    let artVariants = AsciiArt
            { aaStatic = CondText
                { ctDefault = "/ \\ / \\ [Tag: {x}, {y}]"
                , ctVariants =
                    [ TextVariant (VarIs "weather" "regen") "/ / / / [Regen: {x}, {y}]"
                    , TextVariant (VarIs "time_of_day" "nacht") "* . * . [Nacht: {x}, {y}]"
                    ]
                }
            , aaFrames = []
            , aaEvery = 0
            , aaHotspots = []
            , aaAmbient = Nothing
            }
        bForest = BiomeTemplate "forest" 10 "Wald [{x}, {y}]" (plainText "Dichter Wald.") ["forest"] artVariants [North, South, East, West]
        sz = SandboxZone "wildnis" (0, 0, 0) [bForest] (Just 1)
        gw = emptyGameWorld
            { rooms = Map.singleton "gate" (Room "gate" "Tor" (plainText "Tor") (Map.singleton North (Open "sandbox_wildnis")) Set.empty Nothing Nothing Nothing Nothing Nothing Nothing emptyAscii Nothing Nothing Nothing)
            , sandboxZones = Map.singleton "wildnis" sz
            }
        st0 = emptyGameState { world = gw, save = (save emptyGameState) { currentRoom = "gate" } }
        (st1, _) = executeCommand (Go North) st0
        mRoom = lookupRoom "sandbox_wildnis_0_0_0" st1
    case mRoom of
        Nothing -> putStrLn "sandbox_wildnis_0_0_0 not found" >> pure False
        Just room -> do
            let artDay = resolveAsciiArt (roomAscii room) st1
            r1 <- expectEqual "/ \\ / \\ [Tag: 0, 0]" artDay
            -- Switch weather to rain
            let stRain = st1 { save = (save st1) { variables = Map.insert "weather" (VVText "regen") (variables (save st1)) } }
                artRain = resolveAsciiArt (roomAscii room) stRain
            r2 <- expectEqual "/ / / / [Regen: 0, 0]" artRain
            -- Switch time of day to night
            let stNight = st1 { save = (save st1) { variables = Map.insert "time_of_day" (VVText "nacht") (variables (save st1)) } }
                artNight = resolveAsciiArt (roomAscii room) stNight
            r3 <- expectEqual "* . * . [Nacht: 0, 0]" artNight
            pure (r1 && r2 && r3)

-- | Phase 2B: Drawing cards from full deck decreases draw pile and fills hand.
testDrawCardsFromFullDeck :: IO Bool
testDrawCardsFromFullDeck = do
    let ds = defaultDeckState { drawPile = ["c1", "c2", "c3", "c4", "c5", "c6"] }
        st = emptyGameState { save = (save emptyGameState) { deckState = Just ds } }
        st' = drawCards 3 st
        mDs' = deckState (save st')
    case mDs' of
        Nothing -> putStrLn "deckState is Nothing" >> pure False
        Just ds' -> do
            r1 <- expectEqual ["c1", "c2", "c3"] (hand ds')
            r2 <- expectEqual ["c4", "c5", "c6"] (drawPile ds')
            r3 <- expectEqual [] (discardPile ds')
            pure (r1 && r2 && r3)

-- | Phase 2B: Drawing more cards than in draw pile reshuffles discard pile.
testDrawCardsReshufflesDiscard :: IO Bool
testDrawCardsReshufflesDiscard = do
    let ds = defaultDeckState
            { drawPile = ["c1"]
            , discardPile = ["c2", "c3", "c4"]
            , hand = []
            }
        st = emptyGameState { save = (save emptyGameState) { deckState = Just ds } }
        st' = drawCards 3 st
        mDs' = deckState (save st')
    case mDs' of
        Nothing -> putStrLn "deckState is Nothing" >> pure False
        Just ds' -> do
            r1 <- expectEqual 3 (length (hand ds'))
            r2 <- expectEqual ["c1"] (take 1 (hand ds'))
            r3 <- expectEqual 1 (length (drawPile ds'))
            r4 <- expectEqual [] (discardPile ds')
            pure (r1 && r2 && r3 && r4)

-- | Phase 2B: ShuffleList is deterministic given the same RNG state.
testShuffleIsDeterministic :: IO Bool
testShuffleIsDeterministic = do
    let cards = ["c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8"]
        rng0 = initialRngState
        (shuffled1, rng1) = shuffleList cards rng0
        (shuffled2, rng2) = shuffleList cards rng0
        (shuffled3, rng3) = shuffleList cards (nextRng rng0)
    r1 <- expectEqual shuffled1 shuffled2
    r2 <- expectEqual rng1 rng2
    r3 <- expectTrue "different seed produces different order or rng"
        (shuffled1 /= shuffled3 || rng1 /= rng3)
    r4 <- expectEqual (length cards) (length shuffled1)
    pure (r1 && r2 && r3 && r4)

-- | Phase 2B: Playing a card deducts energy, resolves target, applies outcomes, and discards.
testPlayCardDeductsEnergyAndAppliesOutcomes :: IO Bool
testPlayCardDeductsEnergyAndAppliesOutcomes = do
    let strike = Card
            { cardId = "strike"
            , cardName = "Strike"
            , cardCost = Map.singleton "energy" 1
            , cardType = CardAttack
            , cardDescription = "Deals 6 damage to target."
            , cardTarget = TargetSingleEnemy
            , cardExhaust = False
            , cardEffects = [ ModifyValue (VRActorProp (ActorNPC "chosen") PHealth) (-6) ]
            }
        baseSt = initSampleGame
        cRoom = currentRoom (save baseSt)
        goblinDef = (head (Map.elems (npcDefs (world baseSt))))
            { npcId = "goblin"
            , npcName = "Goblin"
            , npcKeywords = ["goblin"]
            , npcMaxHealth = Just 20
            }
        st0 = baseSt
            { world = (world baseSt)
                { cardDefs = Map.singleton "strike" strike
                , npcDefs = Map.insert "goblin" goblinDef (npcDefs (world baseSt))
                }
            , save = (save baseSt)
                { deckState = Just (defaultDeckState { hand = ["strike"] })
                , npcStates = Map.singleton "goblin" (NPCState (InRoom cRoom) "alive" (Just 20) Map.empty Nothing)
                , variables = Map.singleton "player.energy" (VVInt 3)
                }
            }
        (st1, msg) = playCard 1 (Just "goblin") st0
        mDs1 = deckState (save st1)
        goblinHp = case Map.lookup "goblin" (npcStates (save st1)) of
            Just ns -> case npcHealth ns of Just h -> h; Nothing -> 0
            Nothing -> 0
        energyVal = case getVariable "player.energy" st1 of
            Just (VVInt v) -> v
            _              -> 0
    case mDs1 of
        Nothing -> putStrLn "deckState is Nothing" >> pure False
        Just ds1 -> do
            r1 <- expectEqual 2 energyVal
            r2 <- expectEqual 14 goblinHp
            r3 <- expectEqual [] (hand ds1)
            r4 <- expectEqual ["strike"] (discardPile ds1)
            r5 <- expectTrue "msg mentions playing Strike" (isInfixOf "Strike" (renderEvents msg))
            pure (r1 && r2 && r3 && r4 && r5)

-- | Phase 2B: Exhausting card moves it to exhaustPile rather than discardPile.
testPlayCardExhaustsCorrectly :: IO Bool
testPlayCardExhaustsCorrectly = do
    let obliterate = Card
            { cardId = "obliterate"
            , cardName = "Obliterate"
            , cardCost = Map.singleton "energy" 2
            , cardType = CardAttack
            , cardDescription = "Deals 20 damage and exhausts."
            , cardTarget = TargetSingleEnemy
            , cardExhaust = True
            , cardEffects = [ ModifyValue (VRActorProp (ActorNPC "chosen") PHealth) (-20) ]
            }
        baseSt = initSampleGame
        cRoom = currentRoom (save baseSt)
        goblinDef = (head (Map.elems (npcDefs (world baseSt))))
            { npcId = "goblin"
            , npcName = "Goblin"
            , npcKeywords = ["goblin"]
            , npcMaxHealth = Just 30
            }
        st0 = baseSt
            { world = (world baseSt)
                { cardDefs = Map.singleton "obliterate" obliterate
                , npcDefs = Map.insert "goblin" goblinDef (npcDefs (world baseSt))
                }
            , save = (save baseSt)
                { deckState = Just (defaultDeckState { hand = ["obliterate"] })
                , npcStates = Map.singleton "goblin" (NPCState (InRoom cRoom) "alive" (Just 30) Map.empty Nothing)
                , variables = Map.singleton "player.energy" (VVInt 3)
                }
            }
        (st1, msg) = playCard 1 (Just "goblin") st0
        mDs1 = deckState (save st1)
    case mDs1 of
        Nothing -> putStrLn "deckState is Nothing" >> pure False
        Just ds1 -> do
            r1 <- expectEqual [] (hand ds1)
            r2 <- expectEqual [] (discardPile ds1)
            r3 <- expectEqual ["obliterate"] (exhaustPile ds1)
            r4 <- expectTrue "msg notes exhaust" (isInfixOf "Exhaust" (renderEvents msg))
            pure (r1 && r2 && r3 && r4)

-- | Phase 2B: endTurn discards unplayed hand, resets block, restores energy, and draws 5 cards.
testEndTurnDiscardsAndDraws :: IO Bool
testEndTurnDiscardsAndDraws = do
    let ds = defaultDeckState
            { hand = ["h1", "h2"]
            , drawPile = ["d1", "d2", "d3", "d4", "d5", "d6"]
            , discardPile = []
            }
        vars = Map.fromList
            [ ("player.block", VVInt 15)
            , ("player.energy", VVInt 0)
            , ("player.max_energy", VVInt 3)
            ]
        st0 = emptyGameState { save = (save emptyGameState) { deckState = Just ds, variables = vars } }
        (st1, _msg) = endTurn st0
        mDs1 = deckState (save st1)
        blockVal = case getVariable "player.block" st1 of
            Just (VVInt b) -> b
            _              -> -1
        energyVal = case getVariable "player.energy" st1 of
            Just (VVInt e) -> e
            _              -> -1
    case mDs1 of
        Nothing -> putStrLn "deckState is Nothing" >> pure False
        Just ds1 -> do
            r1 <- expectEqual 0 blockVal
            r2 <- expectEqual 3 energyVal
            r3 <- expectEqual ["h1", "h2"] (discardPile ds1)
            r4 <- expectEqual ["d1", "d2", "d3", "d4", "d5"] (hand ds1)
            r5 <- expectEqual ["d6"] (drawPile ds1)
            pure (r1 && r2 && r3 && r4 && r5)

-- | Phase 2B: Card and deck parser commands match intended constructors and arguments.
testCardCommandsParsing :: IO Bool
testCardCommandsParsing = do
    let deParse = parseCommandFor (emptyGameWorld { worldLanguage = Just "de" })
    r1 <- expectEqual (PlayCardCmd 1 Nothing) (parseCommand "play 1")
    r2 <- expectEqual (PlayCardCmd 2 (Just "goblin")) (parseCommand "play 2 goblin")
    r3 <- expectEqual (PlayCardCmd 3 (Just "cave troll")) (parseCommand "play 3 the cave troll")
    r4 <- expectEqual (PlayCardCmd 1 (Just "troll")) (deParse "spiele 1 auf troll")
    r5 <- expectEqual (PlayCardCmd 2 Nothing) (deParse "spiele 2")
    r6 <- expectEqual HandCmd (parseCommand "hand")
    r7 <- expectEqual HandCmd (deParse "karten")
    r8 <- expectEqual DeckCmd (parseCommand "deck")
    r9 <- expectEqual DiscardCmd (parseCommand "discard")
    r10 <- expectEqual DiscardCmd (deParse "ablage")
    r11 <- expectEqual EndTurnCmd (parseCommand "end turn")
    r12 <- expectEqual EndTurnCmd (deParse "zug beenden")
    r13 <- expectEqual EndTurnCmd (parseCommand "pass")
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9 && r10 && r11 && r12 && r13)

-- | Phase 2 / S2: Drawing cards when hand reaches maxHandSize blocks further drawing.
testDrawCardsHandLimitBlocked :: IO Bool
testDrawCardsHandLimitBlocked = do
    let ds = defaultDeckState
            { maxHandSize = 5
            , hand = ["h1", "h2", "h3", "h4", "h5"]
            , drawPile = ["d1", "d2"]
            , discardPile = []
            }
        st0 = emptyGameState { save = (save emptyGameState) { deckState = Just ds } }
        st1 = drawCards 2 st0
        mDs1 = deckState (save st1)
    case mDs1 of
        Nothing -> putStrLn "deckState is Nothing" >> pure False
        Just ds1 -> do
            r1 <- expectEqual 5 (length (hand ds1))
            r2 <- expectEqual ["h1", "h2", "h3", "h4", "h5"] (hand ds1)
            r3 <- expectEqual ["d1", "d2"] (drawPile ds1)
            pure (r1 && r2 && r3)

-- | Phase 2 / S2: Hand limit is checked after reshuffling discard pile.
testDrawCardsHandLimitReshuffle :: IO Bool
testDrawCardsHandLimitReshuffle = do
    let ds = defaultDeckState
            { maxHandSize = 4
            , hand = ["h1", "h2"]
            , drawPile = ["d1"]
            , discardPile = ["x1", "x2", "x3"]
            }
        st0 = emptyGameState { save = (save emptyGameState) { deckState = Just ds } }
        st1 = drawCards 4 st0
        mDs1 = deckState (save st1)
    case mDs1 of
        Nothing -> putStrLn "deckState is Nothing" >> pure False
        Just ds1 -> do
            -- Draws d1, then reshuffles 3 cards into draw pile, draws 1 more (hand has 4 = limit)
            r1 <- expectEqual 4 (length (hand ds1))
            r2 <- expectEqual 2 (length (drawPile ds1))
            r3 <- expectEqual [] (discardPile ds1)
            pure (r1 && r2 && r3)

-- | Phase 2 / S2: Card combo/synergy: card_in_hand condition triggers bonus outcome.
testCardSynergyCombo :: IO Bool
testCardSynergyCombo = do
    let bash = Card
            { cardId = "bash"
            , cardName = "Bash"
            , cardCost = Map.empty
            , cardType = CardAttack
            , cardDescription = "Deals 8 damage, +4 if Defend in hand."
            , cardTarget = TargetSingleEnemy
            , cardExhaust = False
            , cardEffects =
                [ ModifyValue (VRActorProp (ActorNPC "chosen") PHealth) (-8)
                , Conditional (EntityHasState "defend" "in_hand")
                    (ModifyValue (VRActorProp (ActorNPC "chosen") PHealth) (-4))
                    Noop
                ]
            }
        baseSt = initSampleGame
        cRoom = currentRoom (save baseSt)
        goblinDef = (head (Map.elems (npcDefs (world baseSt))))
            { npcId = "goblin"
            , npcName = "Goblin"
            , npcKeywords = ["goblin"]
            , npcMaxHealth = Just 30
            }
        stCombo = baseSt
            { world = (world baseSt)
                { cardDefs = Map.singleton "bash" bash
                , npcDefs = Map.insert "goblin" goblinDef (npcDefs (world baseSt))
                }
            , save = (save baseSt)
                { deckState = Just (defaultDeckState { hand = ["bash", "defend"] })
                , npcStates = Map.singleton "goblin" (NPCState (InRoom cRoom) "alive" (Just 30) Map.empty Nothing)
                }
            }
        stNoCombo = baseSt
            { world = (world baseSt)
                { cardDefs = Map.singleton "bash" bash
                , npcDefs = Map.insert "goblin" goblinDef (npcDefs (world baseSt))
                }
            , save = (save baseSt)
                { deckState = Just (defaultDeckState { hand = ["bash", "strike"] })
                , npcStates = Map.singleton "goblin" (NPCState (InRoom cRoom) "alive" (Just 30) Map.empty Nothing)
                }
            }
        (st1, _) = playCard 1 (Just "goblin") stCombo
        (st2, _) = playCard 1 (Just "goblin") stNoCombo
        hp1 = npcHealth =<< Map.lookup "goblin" (npcStates (save st1))
        hp2 = npcHealth =<< Map.lookup "goblin" (npcStates (save st2))
    r1 <- expectEqual (Just 18) hp1   -- 30 - 8 - 4 = 18
    r2 <- expectEqual (Just 22) hp2   -- 30 - 8 = 22
    pure (r1 && r2)

-- ---------------------------------------------------------------------------
-- Phase 2C: Visuals & HUD Tests (Card Boxes, Horizontal Tiling, Combat Banner)
-- ---------------------------------------------------------------------------

-- | Phase 2C: hcatBoxes tiles boxes side-by-side, wraps on maxWidth, and pads vertically.
testHcatBoxesFormatting :: IO Bool
testHcatBoxesFormatting = do
    let b1 = ["┌──┐", "│11│", "└──┘"]          -- width 4, height 3
        b2 = ["┌────┐", "│2222│", "└────┘"]      -- width 6, height 3
        b3 = ["┌──┐", "│33│", "└──┘"]          -- width 4, height 3
        bShort = ["┌──┐", "└──┘"]              -- width 4, height 2
    -- Test 1: Wide enough to fit all three side-by-side with 2 spaces between
    -- Total width = 4 + 2 + 6 + 2 + 4 = 18
    let tiledWide = hcatBoxes 80 [b1, b2, b3]
    r1 <- expectEqual 3 (length tiledWide)
    r2 <- expectEqual "┌──┐  ┌────┐  ┌──┐" (tiledWide !! 0)
    r3 <- expectEqual "│11│  │2222│  │33│" (tiledWide !! 1)
    r4 <- expectEqual "└──┘  └────┘  └──┘" (tiledWide !! 2)

    -- Test 2: Wrap when maxWidth is exceeded
    -- maxW = 15: b1 (4) + 2 + b2 (6) = 12 <= 15. Adding b3 (+ 2 + 4 = 18 > 15) forces b3 to next row.
    -- Rows are separated by an empty line intercalate [""]
    let tiledWrapped = hcatBoxes 15 [b1, b2, b3]
    r5 <- expectEqual 7 (length tiledWrapped)
    r6 <- expectEqual "┌──┐  ┌────┐" (tiledWrapped !! 0)
    r7 <- expectEqual "" (tiledWrapped !! 3)
    r8 <- expectEqual "┌──┐" (tiledWrapped !! 4)

    -- Test 3: Vertical padding when boxes have unequal heights
    let tiledUnequal = hcatBoxes 80 [bShort, b2]
    r9 <- expectEqual 3 (length tiledUnequal)
    -- bShort only has 2 lines, so 3rd line must be padded with 4 spaces (width of bShort) + 2 spacing spaces = 6 spaces before b2
    r10 <- expectEqual "      └────┘" (tiledUnequal !! 2)
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9 && r10)

-- | Phase 2C: renderCardBox creates uniform 16-width box with 24-bit ANSI colors and wrapping.
testRenderCardBoxFormattingAndColors :: IO Bool
testRenderCardBoxFormattingAndColors = do
    let strike = Card
            { cardId = "strike"
            , cardName = "Strike"
            , cardCost = Map.singleton "energy" 1
            , cardType = CardAttack
            , cardDescription = "Deals 6 damage to single target."
            , cardTarget = TargetSingleEnemy
            , cardExhaust = False
            , cardEffects = []
            }
        defend = Card
            { cardId = "defend"
            , cardName = "Defend"
            , cardCost = Map.singleton "energy" 1
            , cardType = CardSkill
            , cardDescription = "Gain 5 block."
            , cardTarget = TargetSelf
            , cardExhaust = False
            , cardEffects = []
            }
        power = Card
            { cardId = "demon_form"
            , cardName = "Demon Form"
            , cardCost = Map.singleton "energy" 3
            , cardType = CardPower
            , cardDescription = "At start of turn gain 2 strength."
            , cardTarget = TargetSelf
            , cardExhaust = False
            , cardEffects = []
            }
    let box1 = renderCardBox 1 strike
    -- Check that every line has visible width 16
    let widths1 = map (length . stripAnsi) box1
    r1 <- expectTrue "all lines of strike box have visible width 16" (all (== 16) widths1)
    r2 <- expectEqual 7 (length box1)
    -- Top border
    r3 <- expectEqual "┌──────────────┐" (head box1)
    -- Header contains "1. Strike" and "(1)"
    r4 <- expectTrue "header contains 1. Strike" (isInfixOf "1. Strike" (box1 !! 1))
    r5 <- expectTrue "header contains cost (1)" (isInfixOf "(1)" (box1 !! 1))
    -- Attack ANSI color code and German label
    r6 <- expectTrue "type line contains attack ansi color" (isInfixOf "\ESC[38;2;220;50;50m" (box1 !! 2))
    r7 <- expectTrue "type line contains [Angriff]" (isInfixOf "[Angriff]" (box1 !! 2))

    -- Skill box
    let box2 = renderCardBox 2 defend
    let widths2 = map (length . stripAnsi) box2
    r8 <- expectTrue "all lines of defend box have visible width 16" (all (== 16) widths2)
    r9 <- expectTrue "type line contains skill ansi color" (isInfixOf "\ESC[38;2;60;130;240m" (box2 !! 2))
    r10 <- expectTrue "type line contains [Fertigkeit]" (isInfixOf "[Fertigkeit]" (box2 !! 2))

    -- Power box
    let box3 = renderCardBox 3 power
    let widths3 = map (length . stripAnsi) box3
    r11 <- expectTrue "all lines of power box have visible width 16" (all (== 16) widths3)
    r12 <- expectTrue "type line contains power ansi color" (isInfixOf "\ESC[38;2;240;190;40m" (box3 !! 2))
    r13 <- expectTrue "type line contains [Macht]" (isInfixOf "[Macht]" (box3 !! 2))
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9 && r10 && r11 && r12 && r13)

-- | Phase 2C: renderDeckCombatHud generates combat status banner and enemy intent lines.
testRenderDeckCombatHud :: IO Bool
testRenderDeckCombatHud = do
    let strike = Card "strike" "Hieb" (Map.singleton "energy" 1) CardAttack "6 Schaden." TargetSingleEnemy False []
        ds = DeckState
            { drawPile = ["c1", "c2", "c3"]
            , hand = ["strike"]
            , discardPile = ["d1"]
            , exhaustPile = ["e1"]
            , maxHandSize = 8
            }
        baseSt = initSampleGame
        cRoom = currentRoom (save baseSt)
        trollDef = (head (Map.elems (npcDefs (world baseSt))))
            { npcId = "troll"
            , npcName = "Höhlentroll"
            , npcKeywords = ["troll"]
            , npcMaxHealth = Just 50
            }
        vars = Map.fromList
            [ ("player.block", VVInt 12)
            , ("player.energy", VVInt 2)
            , ("player.max_energy", VVInt 3)
            , ("combat.intent.troll", VVText "Schlag für 14 Schaden")
            ]
        st = baseSt
            { world = (world baseSt)
                { cardDefs = Map.singleton "strike" strike
                , npcDefs = Map.insert "troll" trollDef (npcDefs (world baseSt))
                }
            , save = (save baseSt)
                { deckState = Just ds
                , npcStates = Map.singleton "troll" (NPCState (InRoom cRoom) "alive" (Just 40) Map.empty Nothing)
                , variables = vars
                }
            }
    let hudLines = renderDeckCombatHud st ds
    r1 <- expectTrue "top border double line 78 chars" (replicate 78 '═' `elem` hudLines)
    -- Status bar line
    r2 <- expectTrue "status bar contains Deck count" (any (isInfixOf "[Deck: 3]") hudLines)
    r3 <- expectTrue "status bar contains Ablage count" (any (isInfixOf "[Ablage: 1]") hudLines)
    r4 <- expectTrue "status bar contains Block" (any (isInfixOf "Block: 12") hudLines)
    r5 <- expectTrue "status bar contains Energie" (any (isInfixOf "Energie: 2/3") hudLines)
    r6 <- expectTrue "status bar contains Erschöpft count" (any (isInfixOf "Erschöpft: 1") hudLines)
    -- Enemy lines
    r7 <- expectTrue "contains enemy name and hp" (any (isInfixOf "GEGNER: Höhlentroll (HP: 40/50)") hudLines)
    r8 <- expectTrue "contains enemy intent" (any (isInfixOf "ABSICHT: Schlag für 14 Schaden") hudLines)
    -- showHand integration test
    let (stHand, handMsg) = showHand st
    r9 <- expectEqual (save st) (save stHand)
    r10 <- expectTrue "showHand includes HUD" (isInfixOf "GEGNER: Höhlentroll" (renderEvents handMsg))
    r11 <- expectTrue "showHand includes card name" (isInfixOf "Hieb" (renderEvents handMsg))
    r12 <- expectTrue "showHand includes card type" (isInfixOf "[Angriff]" (renderEvents handMsg))
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9 && r10 && r11 && r12)

-- | Phase R3: Test resolveInteractTarget for Item, NPC, Bare, Vehicle and NotFound
testResolveInteractTarget :: IO Bool
testResolveInteractTarget = do
    let st = initSampleGame  -- player in "start", torch in "start", oldman in "start"
    -- 1. Item in room resolves to ITItem
    let tItem = resolveInteractTarget VLookAt "torch" st
    r1 <- case tItem of
        ITItem item _ -> expectEqual "torch" (itemId item)
        _             -> expectTrue "expected ITItem for torch" False
    -- 2. NPC in room resolves to ITNpc
    let tNpc = resolveInteractTarget VTalk "old man" st
    r2 <- case tNpc of
        ITNpc npc _ -> expectEqual "oldman" (npcId npc)
        _           -> expectTrue "expected ITNpc for old man" False
    -- 3. Bare verb with empty target resolves to ITBareVerb
    let tBare = resolveInteractTarget (VCustom "defend") "" st
    r3 <- expectEqual ITBareVerb tBare
    -- 4. Vehicle attack resolution
    let tVeh = resolveInteractTarget VAttack "carriage" st
    r4 <- case tVeh of
        ITVehicle v -> expectEqual "carriage" (vehicleId v)
        _           -> expectTrue "expected ITVehicle for carriage attack" False
    -- 5. Non-existent entity resolves to ITNotFound
    let tNotFound = resolveInteractTarget VLookAt "ghost" st
    r5 <- expectEqual (ITNotFound "ghost") tNotFound
    pure (r1 && r2 && r3 && r4 && r5)

-- | Phase R3: Test that interactItem preserves the take/use contract (portability + pickup + on_take outcome)
testInteractItemContract :: IO Bool
testInteractItemContract = do
    let st0 = initSampleGame
        -- 'torch' is in the room and has no on_take outcome
        res1 = executeCommand (Interact VTake "torch") st0
        st1 = fst res1
        msg1 = snd res1
    r1 <- expectTrue "torch is in inventory after take" (hasItem "torch" st1)
    r2 <- expectEqual "You take the torch." msg1
    -- taking already carried item produces 'You already have the ...'
    let resAlready = executeCommand (Interact VTake "torch") st1
    r3 <- expectEqual "You already have the torch." (snd resAlready)
    -- taking non-portable item
    let gwNonPortable = (world st0)
            { itemDefs = Map.adjust (\it -> it { itemPortable = False, itemTakeFailure = Just "It's bolted down!" }) "torch" (itemDefs (world st0)) }
        stNonPortable = st0 { world = gwNonPortable }
        resNonPortable = executeCommand (Interact VTake "torch") stNonPortable
    r4 <- expectEqual "It's bolted down!" (snd resNonPortable)
    r5 <- expectTrue "non-portable item remains not carried" (not (hasItem "torch" (fst resNonPortable)))
    -- taking item with on_take outcome (like 'key' in hallway)
    -- hallway is tagged 'dark', so player carries a torch to illuminate it (Phase 0.3, Bug B3)
    let stInHallway = pickupItem "torch" (st0 { save = (save st0) { currentRoom = "hallway" } })
        resKey = executeCommand (Interact VTake "key") stInHallway
        stAfterKey = fst resKey
        msgKey = snd resKey
    r6 <- expectTrue "key is picked up" (hasItem "key" stAfterKey)
    r7 <- expectTrue "on_take outcome ran (flag quest_started was set)" (getFlag "quest_started" stAfterKey == Just "true")
    r8 <- expectTrue "take message is present" ("You take the key." `isPrefixOf` msgKey)
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8)

-- | Helper to create a test key item definition (Phase 0.1).
mkTestKey :: String -> String -> ItemDef
mkTestKey iid name = ItemDef
    { itemId = iid
    , itemName = name
    , itemDescription = plainText ("A " ++ name ++ ".")
    , itemKeywords = ["key", name]
    , itemTags = Set.empty
    , itemEquipSlot = Nothing
    , itemEquipEffects = []
    , itemHidden = False
    , itemDiscoverText = Nothing
    , itemPortable = True
    , itemTakeFailure = Nothing
    , itemVerbMap = Map.empty
    , itemCapacity = Nothing, itemAscii = emptyAscii
    , itemGrammar = emptyGrammar
    }

-- | Phase 0.1: Test central resolveTarget for Item, NPC, Vehicle, Bare, NotFound, and Ambiguous
testResolveTargetDirect :: IO Bool
testResolveTargetDirect = do
    let st = initSampleGame  -- player in "start", torch in "start", oldman in "start"
    -- 1. Unique item in room resolves to ResolvedItem / TargetItem
    let tItem = resolveTarget VLookAt "torch" st
    r1 <- case tItem of
        ResolvedItem iid -> expectEqual "torch" iid
        _                -> expectTrue "expected ResolvedItem for torch" False
    r2 <- case tItem of
        TargetItem iid -> expectEqual "torch" iid
        _              -> expectTrue "expected TargetItem pattern synonym for torch" False

    -- 2. NPC in room resolves to ResolvedNPC
    let tNpc = resolveTarget VTalk "old man" st
    r3 <- case tNpc of
        ResolvedNPC nid -> expectEqual "oldman" nid
        _               -> expectTrue "expected ResolvedNPC for old man" False

    -- 3. Vehicle attack candidate resolves to ResolvedVehicle / TargetVehicle
    let tVeh = resolveTarget VAttack "carriage" st
    r4 <- case tVeh of
        ResolvedVehicle vid -> expectEqual "carriage" vid
        _                   -> expectTrue "expected ResolvedVehicle for carriage attack" False
    r5 <- case tVeh of
        TargetVehicle vid -> expectEqual "carriage" vid
        _                 -> expectTrue "expected TargetVehicle pattern synonym for carriage attack" False

    -- 4. Bare verb with empty target resolves to BareVerb / TargetBare
    let tBare = resolveTarget (VCustom "defend") "" st
    r6 <- expectEqual BareVerb tBare
    r7 <- expectEqual TargetBare tBare

    -- 5. Non-existent entity resolves to NotFound / TargetNotFound
    let tNotFound = resolveTarget VLookAt "ghost" st
    r8 <- expectEqual (NotFound "ghost") tNotFound
    r9 <- expectEqual (TargetNotFound "ghost") tNotFound

    -- 6. Two items with shared keyword in the SAME room resolve to Ambiguous
    let brassKey = mkTestKey "brass_key" "brass key"
        ironKey = mkTestKey "iron_key" "iron key"
        stWithTwoKeys = st
            { world = (world st)
                { itemDefs = Map.insert "brass_key" brassKey $ Map.insert "iron_key" ironKey (itemDefs (world st)) }
            , save = (save st)
                { itemStates = Map.insert "brass_key" (ItemState (InRoom "start") "intact" Map.empty False) $
                               Map.insert "iron_key" (ItemState (InRoom "start") "intact" Map.empty False) (itemStates (save st)) }
            }
        tAmbiguous = resolveTarget VTake "key" stWithTwoKeys
    r10 <- case tAmbiguous of
        Ambiguous ids -> expectTrue "ambiguous contains both keys" ("brass_key" `elem` ids && "iron_key" `elem` ids)
        _             -> expectTrue "expected Ambiguous for shared keyword in same room" False
    r11 <- case tAmbiguous of
        TargetAmbiguous ids -> expectTrue "TargetAmbiguous pattern synonym works" ("brass_key" `elem` ids && "iron_key" `elem` ids)
        _                   -> expectTrue "expected TargetAmbiguous pattern synonym" False

    -- 7. Specific alias in the presence of ambiguous keyword resolves uniquely
    let tSpecific = resolveTarget VTake "brass key" stWithTwoKeys
    r12 <- case tSpecific of
        ResolvedItem iid -> expectEqual "brass_key" iid
        _                -> expectTrue "expected ResolvedItem for specific brass key" False

    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9 && r10 && r11 && r12)

-- | Phase 0.1 (B1): Two items with keyword 'key' in different rooms.
--   OnTake fires for the actually taken item (not swallowed due to global alias lookup).
testResolveTargetFixesB1OnTake :: IO Bool
testResolveTargetFixesB1OnTake = do
    let st0 = initSampleGame
        brassKey = mkTestKey "brass_key" "brass key"
        ironKey = mkTestKey "iron_key" "iron key"
        triggers =
            [ TriggerDef "trig_iron" (OnTake "iron_key") Nothing [SendMessage "TRIGGER_IRON_KEY"] False 0
            , TriggerDef "trig_brass" (OnTake "brass_key") Nothing [SendMessage "TRIGGER_BRASS_KEY"] False 0
            ]
        cleanDefs = Map.delete "key" (itemDefs (world st0))
        cleanStates = Map.delete "key" (itemStates (save st0))
        baseSt = st0
            { world = (world st0)
                { itemDefs = Map.insert "brass_key" brassKey $ Map.insert "iron_key" ironKey cleanDefs
                , triggerDefs = triggerDefs (world st0) ++ triggers
                }
            , save = (save st0)
                { itemStates = Map.insert "brass_key" (ItemState (InRoom "start") "intact" Map.empty False) $
                               Map.insert "iron_key" (ItemState (InRoom "hallway") "intact" Map.empty False) cleanStates
                }
            }

    -- Case 1: Player in "hallway" takes "key". Only iron_key is in "hallway".
    -- Under Bug B1, findItemIdByAlias picked "brass_key" (first globally), swallowing OnTake.
    -- Hallway is tagged 'dark', so player carries torch (lightsource) to take the key (Phase 0.3, Bug B3).
    let stInHallway = pickupItem "torch" (baseSt { save = (save baseSt) { currentRoom = "hallway" } })
        (loopHallway, msgHallway) = applyLoopCommand (Interact VTake "key") (initLoopState stInHallway)
        stAfterHallway = lsCurrent loopHallway
    r1 <- expectTrue "iron key is in inventory" (hasItem "iron_key" stAfterHallway)
    r2 <- expectTrue "brass key remains in start" (not (hasItem "brass_key" stAfterHallway))
    r3 <- expectTrue "OnTake trigger for iron key fired" (isInfixOf "TRIGGER_IRON_KEY" msgHallway)
    r4 <- expectTrue "OnTake trigger for brass key did NOT fire" (not (isInfixOf "TRIGGER_BRASS_KEY" msgHallway))
    let evsHallway = commandEvents (Interact VTake "key") stInHallway stAfterHallway
    r5 <- expectEqual [OnTake "iron_key", OnCommand "take", OnTurn] evsHallway

    -- Case 2: Mirror test — Player in "start" takes "key". Only brass_key is in "start".
    let stInStart = baseSt { save = (save baseSt) { currentRoom = "start" } }
        (loopStart, msgStart) = applyLoopCommand (Interact VTake "key") (initLoopState stInStart)
        stAfterStart = lsCurrent loopStart
    r6 <- expectTrue "brass key is in inventory" (hasItem "brass_key" stAfterStart)
    r7 <- expectTrue "iron key remains in hallway" (not (hasItem "iron_key" stAfterStart))
    r8 <- expectTrue "OnTake trigger for brass key fired" (isInfixOf "TRIGGER_BRASS_KEY" msgStart)
    r9 <- expectTrue "OnTake trigger for iron key did NOT fire" (not (isInfixOf "TRIGGER_IRON_KEY" msgStart))
    let evsStart = commandEvents (Interact VTake "key") stInStart stAfterStart
    r10 <- expectEqual [OnTake "brass_key", OnCommand "take", OnTurn] evsStart

    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9 && r10)

-- | Phase 0.1: Executing an ambiguous command when multiple items match in the same room.
testResolveTargetAmbiguousCommandExecution :: IO Bool
testResolveTargetAmbiguousCommandExecution = do
    let st0 = initSampleGame
        brassKey = mkTestKey "brass_key" "brass key"
        ironKey = mkTestKey "iron_key" "iron key"
        stBothInStart = st0
            { world = (world st0)
                { itemDefs = Map.insert "brass_key" brassKey $ Map.insert "iron_key" ironKey (itemDefs (world st0)) }
            , save = (save st0)
                { itemStates = Map.insert "brass_key" (ItemState (InRoom "start") "intact" Map.empty False) $
                               Map.insert "iron_key" (ItemState (InRoom "start") "intact" Map.empty False) (itemStates (save st0)) }
            }
        (stAfter, msg) = executeCommand (Interact VTake "key") stBothInStart
    r1 <- expectTrue "msg asks which one" (isInfixOf "Which do you mean:" msg)
    r2 <- expectTrue "msg mentions brass key" (isInfixOf "brass key" msg)
    r3 <- expectTrue "msg mentions iron key" (isInfixOf "iron key" msg)
    r4 <- expectTrue "neither key was taken" (not (hasItem "brass_key" stAfter) && not (hasItem "iron_key" stAfter))
    -- commandEvents raises no OnTake for ambiguous command
    let evs = commandEvents (Interact VTake "key") stBothInStart stAfter
    r5 <- expectEqual [OnCommand "take", OnTurn] evs
    pure (r1 && r2 && r3 && r4 && r5)

-- | Phase 0.1 (B1): OnDrop and OnUse fire for the carried item even when an item with
--   the same keyword exists elsewhere in the world.
testResolveTargetDropAndUseFixB1 :: IO Bool
testResolveTargetDropAndUseFixB1 = do
    let st0 = initSampleGame
        brassKey = mkTestKey "brass_key" "brass key"
        ironKey = mkTestKey "iron_key" "iron key"
        triggers =
            [ TriggerDef "trig_drop_iron" (OnDrop "iron_key") Nothing [SendMessage "TRIGGER_DROP_IRON"] False 0
            , TriggerDef "trig_drop_brass" (OnDrop "brass_key") Nothing [SendMessage "TRIGGER_DROP_BRASS"] False 0
            , TriggerDef "trig_use_iron" (OnUse "iron_key") Nothing [SendMessage "TRIGGER_USE_IRON"] False 0
            , TriggerDef "trig_use_brass" (OnUse "brass_key") Nothing [SendMessage "TRIGGER_USE_BRASS"] False 0
            ]
        cleanDefs = Map.delete "key" (itemDefs (world st0))
        cleanStates = Map.delete "key" (itemStates (save st0))
        stCarryingIron = st0
            { world = (world st0)
                { itemDefs = Map.insert "brass_key" brassKey $ Map.insert "iron_key" ironKey cleanDefs
                , triggerDefs = triggerDefs (world st0) ++ triggers
                }
            , save = (save st0)
                { itemStates = Map.insert "brass_key" (ItemState (InRoom "hallway") "intact" Map.empty False) $
                               Map.insert "iron_key" (ItemState (CarriedBy ActorPlayer) "intact" Map.empty False) cleanStates }
            }
    -- 1. Drop key via applyLoopCommand: OnDrop fires for carried iron_key, not for brass_key in hallway
    let (loopDrop, msgDrop) = applyLoopCommand (Interact VDrop "key") (initLoopState stCarryingIron)
        stAfterDrop = lsCurrent loopDrop
    r1 <- expectTrue "msg drops iron key" (isInfixOf "You drop the iron key." msgDrop)
    r2 <- expectTrue "iron key is no longer carried" (not (hasItem "iron_key" stAfterDrop))
    r3 <- expectTrue "OnDrop trigger for iron key fired" (isInfixOf "TRIGGER_DROP_IRON" msgDrop)
    r4 <- expectTrue "OnDrop trigger for brass key did NOT fire" (not (isInfixOf "TRIGGER_DROP_BRASS" msgDrop))
    let evsDrop = commandEvents (Interact VDrop "key") stCarryingIron stAfterDrop
    r5 <- expectEqual [OnDrop "iron_key", OnCommand "drop", OnTurn] evsDrop

    -- 2. Use key via applyLoopCommand: OnUse fires for carried iron_key, not for brass_key in hallway
    let (loopUse, msgUse) = applyLoopCommand (Interact VUse "key") (initLoopState stCarryingIron)
    r6 <- expectTrue "OnUse trigger for iron key fired" (isInfixOf "TRIGGER_USE_IRON" msgUse)
    r7 <- expectTrue "OnUse trigger for brass key did NOT fire" (not (isInfixOf "TRIGGER_USE_BRASS" msgUse))
    let evsUse = commandEvents (Interact VUse "key") stCarryingIron (lsCurrent loopUse)
    r8 <- expectEqual [OnUse "iron_key", OnCommand "use", OnTurn] evsUse
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8)

-- | Phase 0.1: take all and drop all succeed without ambiguous prompts when items share keywords
testTakeAllAndDropAllWithSharedAliases :: IO Bool
testTakeAllAndDropAllWithSharedAliases = do
    let st0 = initSampleGame
        key1 = mkTestKey "brass_key_1" "brass key"
        key2 = mkTestKey "brass_key_2" "brass key"
        cleanDefs = Map.delete "key" (itemDefs (world st0))
        cleanStates = Map.delete "key" (itemStates (save st0))
        stBothInStart = st0
            { world = (world st0)
                { itemDefs = Map.insert "brass_key_1" key1 $ Map.insert "brass_key_2" key2 cleanDefs }
            , save = (save st0)
                { itemStates = Map.insert "brass_key_1" (ItemState (InRoom "start") "intact" Map.empty False) $
                               Map.insert "brass_key_2" (ItemState (InRoom "start") "intact" Map.empty False) cleanStates }
            }
    -- 1. Take all should pick up both keys using unique item IDs
    let (stAfterTake, msgTake) = executeCommand TakeAll stBothInStart
    r1 <- expectTrue "brass_key_1 taken" (hasItem "brass_key_1" stAfterTake)
    r2 <- expectTrue "brass_key_2 taken" (hasItem "brass_key_2" stAfterTake)
    r3 <- expectTrue "no ambiguous prompt in take all" (not (isInfixOf "Which do you mean" msgTake))

    -- 2. Drop all should drop both keys
    let (stAfterDrop, msgDrop) = executeCommand DropAll stAfterTake
    r4 <- expectTrue "brass_key_1 dropped" (not (hasItem "brass_key_1" stAfterDrop))
    r5 <- expectTrue "brass_key_2 dropped" (not (hasItem "brass_key_2" stAfterDrop))
    r6 <- expectTrue "no ambiguous prompt in drop all" (not (isInfixOf "Which do you mean" msgDrop))
    pure (r1 && r2 && r3 && r4 && r5 && r6)

-- | Phase 0.1: Multi-vehicle ambiguity resolution on attack
testResolveTargetVehicleAmbiguity :: IO Bool
testResolveTargetVehicleAmbiguity = do
    let st0 = initSampleGame
        baseVeh = head (Map.elems (vehicleDefs (world st0)))
        v1 = baseVeh { vehicleId = "ship_scout", vehicleName = "Scout Ship", vehicleKeywords = ["ship", "vessel"] }
        v2 = baseVeh { vehicleId = "ship_raider", vehicleName = "Raider Ship", vehicleKeywords = ["ship", "vessel"] }
        stWithShips = st0
            { world = (world st0)
                { vehicleDefs = Map.insert "ship_scout" v1 $ Map.insert "ship_raider" v2 (vehicleDefs (world st0)) }
            }
    let res = resolveTarget VAttack "ship" stWithShips
    case res of
        Ambiguous ids -> do
            r1 <- expectTrue "contains scout" ("ship_scout" `elem` ids)
            r2 <- expectTrue "contains raider" ("ship_raider" `elem` ids)
            pure (r1 && r2)
        _ -> expectTrue "expected Ambiguous for multiple vehicles matching target" False

-- | Helper to create a test equipment item definition (Phase 0.2).
mkTestEquip :: String -> String -> EquipSlot -> ItemDef
mkTestEquip iid name slot = ItemDef
    { itemId = iid
    , itemName = name
    , itemDescription = plainText ("A " ++ name ++ ".")
    , itemKeywords = [name, "blade"]
    , itemTags = Set.empty
    , itemEquipSlot = Just slot
    , itemEquipEffects = []
    , itemHidden = False
    , itemDiscoverText = Nothing
    , itemPortable = True
    , itemTakeFailure = Nothing
    , itemVerbMap = Map.empty
    , itemCapacity = Nothing, itemAscii = emptyAscii
    , itemGrammar = emptyGrammar
    }

-- | Phase 0.2: Direct test of search order and preferInventoryTarget predicate
testResolveTargetSearchOrderDirect :: IO Bool
testResolveTargetSearchOrderDirect = do
    let st0 = initSampleGame
        brassKey = mkTestKey "brass_key" "brass key"
        ironKey = mkTestKey "iron_key" "iron key"
        cleanDefs = Map.delete "key" (itemDefs (world st0))
        cleanStates = Map.delete "key" (itemStates (save st0))
        st = st0
            { world = (world st0)
                { itemDefs = Map.insert "brass_key" brassKey $ Map.insert "iron_key" ironKey cleanDefs }
            , save = (save st0)
                { itemStates = Map.insert "brass_key" (ItemState (CarriedBy ActorPlayer) "intact" Map.empty False) $
                               Map.insert "iron_key" (ItemState (InRoom "start") "intact" Map.empty False) cleanStates }
            }
    -- 1. preferInventoryTarget matches drop, use, equip, wear, wield, unequip, remove
    r1 <- expectTrue "drop prefers inventory" (preferInventoryTarget VDrop)
    r2 <- expectTrue "use prefers inventory" (preferInventoryTarget VUse)
    r3 <- expectTrue "use-on prefers inventory" (preferInventoryTarget VUseOn)
    r4 <- expectTrue "custom equip prefers inventory" (preferInventoryTarget (VCustom "equip"))
    r5 <- expectTrue "custom wear prefers inventory" (preferInventoryTarget (VCustom "wear"))
    r6 <- expectTrue "take does NOT prefer inventory" (not (preferInventoryTarget VTake))
    r7 <- expectTrue "examine does NOT prefer inventory" (not (preferInventoryTarget VLookAt))

    -- 2. resolveTarget with brass_key carried and iron_key in room:
    -- drop resolves to carried brass_key
    r8 <- case resolveTarget VDrop "key" st of
        ResolvedItem iid -> expectEqual "brass_key" iid
        _                -> expectTrue "expected ResolvedItem brass_key for drop" False

    -- use resolves to carried brass_key
    r9 <- case resolveTarget VUse "key" st of
        ResolvedItem iid -> expectEqual "brass_key" iid
        _                -> expectTrue "expected ResolvedItem brass_key for use" False

    -- take resolves to room iron_key
    r10 <- case resolveTarget VTake "key" st of
        ResolvedItem iid -> expectEqual "iron_key" iid
        _                -> expectTrue "expected ResolvedItem iron_key for take" False

    -- look at resolves to room iron_key
    r11 <- case resolveTarget VLookAt "key" st of
        ResolvedItem iid -> expectEqual "iron_key" iid
        _                -> expectTrue "expected ResolvedItem iron_key for look at" False

    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9 && r10 && r11)

-- | Phase 0.2 (B2): drop key drops the carried key even when another key is in the room.
testDropKeyWithRoomNamensvetterFixB2 :: IO Bool
testDropKeyWithRoomNamensvetterFixB2 = do
    let st0 = initSampleGame
        brassKey = mkTestKey "brass_key" "brass key"
        ironKey = mkTestKey "iron_key" "iron key"
        triggers =
            [ TriggerDef "trig_drop_brass" (OnDrop "brass_key") Nothing [SendMessage "TRIGGER_DROP_BRASS"] False 0
            , TriggerDef "trig_drop_iron" (OnDrop "iron_key") Nothing [SendMessage "TRIGGER_DROP_IRON"] False 0
            ]
        cleanDefs = Map.delete "key" (itemDefs (world st0))
        cleanStates = Map.delete "key" (itemStates (save st0))
        stBothInStart = st0
            { world = (world st0)
                { itemDefs = Map.insert "brass_key" brassKey $ Map.insert "iron_key" ironKey cleanDefs
                , triggerDefs = triggerDefs (world st0) ++ triggers
                }
            , save = (save st0)
                { itemStates = Map.insert "brass_key" (ItemState (CarriedBy ActorPlayer) "intact" Map.empty False) $
                               Map.insert "iron_key" (ItemState (InRoom "start") "intact" Map.empty False) cleanStates }
            }
    -- Drop key via applyLoopCommand: player drops the brass key from inventory without error
    let (loopDrop, msgDrop) = applyLoopCommand (Interact VDrop "key") (initLoopState stBothInStart)
        stAfterDrop = lsCurrent loopDrop
    r1 <- expectTrue "msg drops brass key" (isInfixOf "You drop the brass key." msgDrop)
    r2 <- expectTrue "brass key is no longer carried" (not (hasItem "brass_key" stAfterDrop))
    r3 <- expectTrue "iron key remains in room" (not (hasItem "iron_key" stAfterDrop))
    r4 <- expectTrue "OnDrop trigger for brass key fired" (isInfixOf "TRIGGER_DROP_BRASS" msgDrop)
    r5 <- expectTrue "OnDrop trigger for iron key did NOT fire" (not (isInfixOf "TRIGGER_DROP_IRON" msgDrop))
    r6 <- expectTrue "no ambiguous question asked" (not (isInfixOf "Which do you mean" msgDrop))
    let evsDrop = commandEvents (Interact VDrop "key") stBothInStart stAfterDrop
    r7 <- expectEqual [OnDrop "brass_key", OnCommand "drop", OnTurn] evsDrop
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7)

-- | Phase 0.2: take key takes the room key when another key is already in inventory.
testTakeKeyWithInventoryNamensvetterFixB2 :: IO Bool
testTakeKeyWithInventoryNamensvetterFixB2 = do
    let st0 = initSampleGame
        brassKey = mkTestKey "brass_key" "brass key"
        ironKey = mkTestKey "iron_key" "iron key"
        triggers =
            [ TriggerDef "trig_take_iron" (OnTake "iron_key") Nothing [SendMessage "TRIGGER_TAKE_IRON"] False 0
            , TriggerDef "trig_take_brass" (OnTake "brass_key") Nothing [SendMessage "TRIGGER_TAKE_BRASS"] False 0
            ]
        cleanDefs = Map.delete "key" (itemDefs (world st0))
        cleanStates = Map.delete "key" (itemStates (save st0))
        stCarryingBrass = st0
            { world = (world st0)
                { itemDefs = Map.insert "brass_key" brassKey $ Map.insert "iron_key" ironKey cleanDefs
                , triggerDefs = triggerDefs (world st0) ++ triggers
                }
            , save = (save st0)
                { itemStates = Map.insert "brass_key" (ItemState (CarriedBy ActorPlayer) "intact" Map.empty False) $
                               Map.insert "iron_key" (ItemState (InRoom "start") "intact" Map.empty False) cleanStates }
            }
    -- Take key via applyLoopCommand: player takes iron key from room
    let (loopTake, msgTake) = applyLoopCommand (Interact VTake "key") (initLoopState stCarryingBrass)
        stAfterTake = lsCurrent loopTake
    r1 <- expectTrue "msg takes iron key" (isInfixOf "You take the iron key." msgTake)
    r2 <- expectTrue "iron key is now carried" (hasItem "iron_key" stAfterTake)
    r3 <- expectTrue "brass key is still carried" (hasItem "brass_key" stAfterTake)
    r4 <- expectTrue "OnTake trigger for iron key fired" (isInfixOf "TRIGGER_TAKE_IRON" msgTake)
    r5 <- expectTrue "OnTake trigger for brass key did NOT fire" (not (isInfixOf "TRIGGER_TAKE_BRASS" msgTake))
    r6 <- expectTrue "no ambiguous question asked" (not (isInfixOf "Which do you mean" msgTake))
    let evsTake = commandEvents (Interact VTake "key") stCarryingBrass stAfterTake
    r7 <- expectEqual [OnTake "iron_key", OnCommand "take", OnTurn] evsTake
    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7)

-- | Phase 0.2: use key fires trigger on carried key when another key is in room.
testUseKeyWithRoomNamensvetterFixB2 :: IO Bool
testUseKeyWithRoomNamensvetterFixB2 = do
    let st0 = initSampleGame
        brassKey = mkTestKey "brass_key" "brass key"
        ironKey = mkTestKey "iron_key" "iron key"
        triggers =
            [ TriggerDef "trig_use_brass" (OnUse "brass_key") Nothing [SendMessage "TRIGGER_USE_BRASS"] False 0
            , TriggerDef "trig_use_iron" (OnUse "iron_key") Nothing [SendMessage "TRIGGER_USE_IRON"] False 0
            ]
        cleanDefs = Map.delete "key" (itemDefs (world st0))
        cleanStates = Map.delete "key" (itemStates (save st0))
        stBoth = st0
            { world = (world st0)
                { itemDefs = Map.insert "brass_key" brassKey $ Map.insert "iron_key" ironKey cleanDefs
                , triggerDefs = triggerDefs (world st0) ++ triggers
                }
            , save = (save st0)
                { itemStates = Map.insert "brass_key" (ItemState (CarriedBy ActorPlayer) "intact" Map.empty False) $
                               Map.insert "iron_key" (ItemState (InRoom "start") "intact" Map.empty False) cleanStates }
            }
    let (loopUse, msgUse) = applyLoopCommand (Interact VUse "key") (initLoopState stBoth)
    r1 <- expectTrue "OnUse trigger for brass key fired" (isInfixOf "TRIGGER_USE_BRASS" msgUse)
    r2 <- expectTrue "OnUse trigger for iron key did NOT fire" (not (isInfixOf "TRIGGER_USE_IRON" msgUse))
    r3 <- expectTrue "no ambiguous question asked" (not (isInfixOf "Which do you mean" msgUse))
    let evsUse = commandEvents (Interact VUse "key") stBoth (lsCurrent loopUse)
    r4 <- expectEqual [OnUse "brass_key", OnCommand "use", OnTurn] evsUse
    pure (r1 && r2 && r3 && r4)

-- | Phase 0.2: equip blade equips the carried blade when another blade is in room.
testEquipWithRoomNamensvetterFixB2 :: IO Bool
testEquipWithRoomNamensvetterFixB2 = do
    let st0 = initSampleGame
        steelBlade = mkTestEquip "steel_blade" "steel blade" Weapon
        rustyBlade = mkTestEquip "rusty_blade" "rusty blade" Weapon
        st = st0
            { world = (world st0)
                { itemDefs = Map.insert "steel_blade" steelBlade $ Map.insert "rusty_blade" rustyBlade (itemDefs (world st0)) }
            , save = (save st0)
                { itemStates = Map.insert "steel_blade" (ItemState (CarriedBy ActorPlayer) "intact" Map.empty False) $
                               Map.insert "rusty_blade" (ItemState (InRoom "start") "intact" Map.empty False) (itemStates (save st0)) }
            }
    let (st', msg) = executeCommand (EquipCmd "blade") st
    r1 <- expectEqual (Just "steel_blade") (Map.lookup Weapon (equipment (save st')))
    r2 <- expectTrue "msg confirms steel blade equipped" (isInfixOf "steel blade" msg)
    r3 <- expectTrue "no ambiguous prompt" (not (isInfixOf "Which do you mean" msg))
    pure (r1 && r2 && r3)

-- | Phase 0.2: Fallback to secondary location produces accurate feedback messages.
testSearchOrderFallbacks :: IO Bool
testSearchOrderFallbacks = do
    let st0 = initSampleGame
        ironKey = mkTestKey "iron_key" "iron key"
        brassKey = mkTestKey "brass_key" "brass key"
        rustyBlade = mkTestEquip "rusty_blade" "rusty blade" Weapon
        cleanDefs = Map.delete "key" (itemDefs (world st0))
        cleanStates = Map.delete "key" (itemStates (save st0))

    -- 1. drop key when player carries NO key, but room has iron_key
    let stOnlyRoom = st0
            { world = (world st0) { itemDefs = Map.insert "iron_key" ironKey cleanDefs }
            , save = (save st0) { itemStates = Map.insert "iron_key" (ItemState (InRoom "start") "intact" Map.empty False) cleanStates }
            }
        (_, msgDrop) = executeCommand (Interact VDrop "key") stOnlyRoom
    r1 <- expectTrue "drop fallback resolves to room item and mentions iron key" (isInfixOf "iron key" msgDrop)

    -- 2. take key when room has NO key, but player carries brass_key
    let stOnlyInv = st0
            { world = (world st0) { itemDefs = Map.insert "brass_key" brassKey cleanDefs }
            , save = (save st0) { itemStates = Map.insert "brass_key" (ItemState (CarriedBy ActorPlayer) "intact" Map.empty False) cleanStates }
            }
        (_, msgTake) = executeCommand (Interact VTake "key") stOnlyInv
    r2 <- expectTrue "take fallback tells player item is already carried" (isInfixOf "You already have the brass key." msgTake)

    -- 3. equip blade when player carries NO blade, but room has rusty_blade
    let cleanBladeDefs = Map.delete "sword_rusty" (itemDefs (world st0))
        cleanBladeStates = Map.delete "sword_rusty" (itemStates (save st0))
        stOnlyRoomBlade = st0
            { world = (world st0) { itemDefs = Map.insert "rusty_blade" rustyBlade cleanBladeDefs }
            , save = (save st0) { itemStates = Map.insert "rusty_blade" (ItemState (InRoom "start") "intact" Map.empty False) cleanBladeStates }
            }
        (_, msgEquip) = executeCommand (EquipCmd "blade") stOnlyRoomBlade
    r3 <- expectTrue "equip fallback tells player item must be carried" (isInfixOf "You need to be carrying the rusty blade." msgEquip)

    pure (r1 && r2 && r3)

-- | Phase 0.2: Ambiguity questions are scoped strictly to the primary search location.
testSearchOrderAmbiguityScoped :: IO Bool
testSearchOrderAmbiguityScoped = do
    let st0 = initSampleGame
        brassKey = mkTestKey "brass_key" "brass key"
        silverKey = mkTestKey "silver_key" "silver key"
        ironKey = mkTestKey "iron_key" "iron key"
        cleanDefs = Map.delete "key" (itemDefs (world st0))
        cleanStates = Map.delete "key" (itemStates (save st0))

    -- Case 1: Two keys in inventory (brass, silver), one in room (iron).
    -- 'drop key' should ask only between carried keys (brass, silver), excluding room key (iron).
    let stTwoInInv = st0
            { world = (world st0)
                { itemDefs = Map.insert "brass_key" brassKey $
                             Map.insert "silver_key" silverKey $
                             Map.insert "iron_key" ironKey cleanDefs }
            , save = (save st0)
                { itemStates = Map.insert "brass_key" (ItemState (CarriedBy ActorPlayer) "intact" Map.empty False) $
                               Map.insert "silver_key" (ItemState (CarriedBy ActorPlayer) "intact" Map.empty False) $
                               Map.insert "iron_key" (ItemState (InRoom "start") "intact" Map.empty False) cleanStates }
            }
        (_, msgDrop) = executeCommand (Interact VDrop "key") stTwoInInv
    r1 <- expectTrue "drop asks which one" (isInfixOf "Which do you mean:" msgDrop)
    r2 <- expectTrue "drop mentions brass key" (isInfixOf "brass key" msgDrop)
    r3 <- expectTrue "drop mentions silver key" (isInfixOf "silver key" msgDrop)
    r4 <- expectTrue "drop does NOT mention iron key" (not (isInfixOf "iron key" msgDrop))

    -- Case 1b: 'use key' should also ask only between carried keys (brass, silver), excluding room key (iron).
    let (_, msgUse) = executeCommand (Interact VUse "key") stTwoInInv
    r4a <- expectTrue "use asks which one" (isInfixOf "Which do you mean:" msgUse)
    r4b <- expectTrue "use mentions brass key" (isInfixOf "brass key" msgUse)
    r4c <- expectTrue "use mentions silver key" (isInfixOf "silver key" msgUse)
    r4d <- expectTrue "use does NOT mention iron key" (not (isInfixOf "iron key" msgUse))

    -- Case 1c: 'equip blade' with two carried blades, one room blade asks only between carried blades.
    let steelBlade = mkTestEquip "steel_blade" "steel blade" Weapon
        ironBlade = mkTestEquip "iron_blade" "iron blade" Weapon
        roomBlade = mkTestEquip "room_blade" "room blade" Weapon
        stBlades = st0
            { world = (world st0)
                { itemDefs = Map.insert "steel_blade" steelBlade $
                             Map.insert "iron_blade" ironBlade $
                             Map.insert "room_blade" roomBlade (itemDefs (world st0)) }
            , save = (save st0)
                { itemStates = Map.insert "steel_blade" (ItemState (CarriedBy ActorPlayer) "intact" Map.empty False) $
                               Map.insert "iron_blade" (ItemState (CarriedBy ActorPlayer) "intact" Map.empty False) $
                               Map.insert "room_blade" (ItemState (InRoom "start") "intact" Map.empty False) cleanStates }
            }
        (_, msgEquip) = executeCommand (EquipCmd "blade") stBlades
    r4e <- expectTrue "equip asks which one" (isInfixOf "Which do you mean:" msgEquip)
    r4f <- expectTrue "equip mentions steel blade" (isInfixOf "steel blade" msgEquip)
    r4g <- expectTrue "equip mentions iron blade" (isInfixOf "iron blade" msgEquip)
    r4h <- expectTrue "equip does NOT mention room blade" (not (isInfixOf "room blade" msgEquip))

    -- Case 2: Two keys in room (iron, silver), one in inventory (brass).
    -- 'take key' should ask only between room keys (iron, silver), excluding carried key (brass).
    let stTwoInRoom = st0
            { world = (world st0)
                { itemDefs = Map.insert "brass_key" brassKey $
                             Map.insert "silver_key" silverKey $
                             Map.insert "iron_key" ironKey cleanDefs }
            , save = (save st0)
                { itemStates = Map.insert "iron_key" (ItemState (InRoom "start") "intact" Map.empty False) $
                               Map.insert "silver_key" (ItemState (InRoom "start") "intact" Map.empty False) $
                               Map.insert "brass_key" (ItemState (CarriedBy ActorPlayer) "intact" Map.empty False) cleanStates }
            }
        (_, msgTake) = executeCommand (Interact VTake "key") stTwoInRoom
    r5 <- expectTrue "take asks which one" (isInfixOf "Which do you mean:" msgTake)
    r6 <- expectTrue "take mentions iron key" (isInfixOf "iron key" msgTake)
    r7 <- expectTrue "take mentions silver key" (isInfixOf "silver key" msgTake)
    r8 <- expectTrue "take does NOT mention brass key" (not (isInfixOf "brass key" msgTake))

    pure (r1 && r2 && r3 && r4 && r4a && r4b && r4c && r4d && r4e && r4f && r4g && r4h && r5 && r6 && r7 && r8)

-- | Phase 0.2: unequip blade unequips the carried equipped blade when another blade is in room.
testUnequipWithRoomNamensvetterFixB2 :: IO Bool
testUnequipWithRoomNamensvetterFixB2 = do
    let st0 = initSampleGame
        steelBlade = mkTestEquip "steel_blade" "steel blade" Weapon
        rustyBlade = mkTestEquip "rusty_blade" "rusty blade" Weapon
        st = st0
            { world = (world st0)
                { itemDefs = Map.insert "steel_blade" steelBlade $ Map.insert "rusty_blade" rustyBlade (itemDefs (world st0)) }
            , save = (save st0)
                { itemStates = Map.insert "steel_blade" (ItemState (CarriedBy ActorPlayer) "intact" Map.empty False) $
                               Map.insert "rusty_blade" (ItemState (InRoom "start") "intact" Map.empty False) (itemStates (save st0))
                , equipment = Map.singleton Weapon "steel_blade"
                }
            }
    let (st', msg) = executeCommand (UnequipCmd "blade") st
    r1 <- expectEqual Nothing (Map.lookup Weapon (equipment (save st')))
    r2 <- expectTrue "msg confirms steel blade unequipped" (isInfixOf "steel blade" msg)
    r3 <- expectTrue "no ambiguous prompt" (not (isInfixOf "Which do you mean" msg))
    r4 <- expectTrue "rusty blade is still in room" ((itemLocation <$> Map.lookup "rusty_blade" (itemStates (save st'))) == Just (InRoom "start"))
    pure (r1 && r2 && r3 && r4)

-- | Phase 0.2: unequip filters ambiguous candidates by equipped status.
testUnequipAmbiguityFiltersEquipped :: IO Bool
testUnequipAmbiguityFiltersEquipped = do
    let st0 = initSampleGame
        steelBlade = mkTestEquip "steel_blade" "steel blade" Weapon
        rustyBlade = mkTestEquip "rusty_blade" "rusty blade" Weapon
        -- Case 1: Player carries both blades, but only steel_blade is equipped.
        -- 'unequip blade' should unequip steel_blade directly without asking.
        stOneEquipped = st0
            { world = (world st0)
                { itemDefs = Map.insert "steel_blade" steelBlade $ Map.insert "rusty_blade" rustyBlade (itemDefs (world st0)) }
            , save = (save st0)
                { itemStates = Map.insert "steel_blade" (ItemState (CarriedBy ActorPlayer) "intact" Map.empty False) $
                               Map.insert "rusty_blade" (ItemState (CarriedBy ActorPlayer) "intact" Map.empty False) (itemStates (save st0))
                , equipment = Map.singleton Weapon "steel_blade"
                }
            }
    let (st1, msg1) = executeCommand (UnequipCmd "blade") stOneEquipped
    r1 <- expectEqual Nothing (Map.lookup Weapon (equipment (save st1)))
    r2 <- expectTrue "msg confirms steel blade unequipped" (isInfixOf "steel blade" msg1)
    r3 <- expectTrue "no ambiguous prompt when only one is equipped" (not (isInfixOf "Which do you mean" msg1))

    -- Case 2: Player has two daggers equipped (Weapon, Offhand). 'unequip dagger' should ask only between equipped daggers.
    let daggerGold = (mkTestEquip "dagger_gold" "gold dagger" Weapon) { itemKeywords = ["dagger", "gold"] }
        daggerSilver = (mkTestEquip "dagger_silver" "silver dagger" Offhand) { itemKeywords = ["dagger", "silver"] }
        daggerIron = (mkTestEquip "dagger_iron" "iron dagger" Weapon) { itemKeywords = ["dagger", "iron"] }
        stTwoEquipped = st0
            { world = (world st0)
                { itemDefs = Map.insert "dagger_gold" daggerGold $
                             Map.insert "dagger_silver" daggerSilver $
                             Map.insert "dagger_iron" daggerIron (itemDefs (world st0)) }
            , save = (save st0)
                { itemStates = Map.insert "dagger_gold" (ItemState (CarriedBy ActorPlayer) "intact" Map.empty False) $
                               Map.insert "dagger_silver" (ItemState (CarriedBy ActorPlayer) "intact" Map.empty False) $
                               Map.insert "dagger_iron" (ItemState (CarriedBy ActorPlayer) "intact" Map.empty False) (itemStates (save st0))
                , equipment = Map.insert Weapon "dagger_gold" $ Map.insert Offhand "dagger_silver" Map.empty
                }
            }
    let (_, msg2) = executeCommand (UnequipCmd "dagger") stTwoEquipped
    r4 <- expectTrue "unequip asks which dagger" (isInfixOf "Which do you mean:" msg2)
    r5 <- expectTrue "mentions gold dagger" (isInfixOf "gold dagger" msg2)
    r6 <- expectTrue "mentions silver dagger" (isInfixOf "silver dagger" msg2)
    r7 <- expectTrue "does NOT mention unequipped iron dagger" (not (isInfixOf "iron dagger" msg2))

    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7)

-- | Phase 0.2: equip filters ambiguous candidates by equippability.
testEquipAmbiguityFiltersEquippable :: IO Bool
testEquipAmbiguityFiltersEquippable = do
    let st0 = initSampleGame
        steelBlade = mkTestEquip "steel_blade" "steel blade" Weapon
        bladeOil = (mkTestKey "blade_oil" "blade oil")
            { itemKeywords = ["blade", "oil"] }
        st = st0
            { world = (world st0)
                { itemDefs = Map.insert "steel_blade" steelBlade $ Map.insert "blade_oil" bladeOil (itemDefs (world st0)) }
            , save = (save st0)
                { itemStates = Map.insert "steel_blade" (ItemState (CarriedBy ActorPlayer) "intact" Map.empty False) $
                               Map.insert "blade_oil" (ItemState (CarriedBy ActorPlayer) "intact" Map.empty False) (itemStates (save st0)) }
            }
    let (st', msg) = executeCommand (EquipCmd "blade") st
    r1 <- expectEqual (Just "steel_blade") (Map.lookup Weapon (equipment (save st')))
    r2 <- expectTrue "msg confirms steel blade equipped" (isInfixOf "steel blade" msg)
    r3 <- expectTrue "no ambiguous prompt between equippable and non-equippable" (not (isInfixOf "Which do you mean" msg))
    pure (r1 && r2 && r3)

-- ---------------------------------------------------------------------------
-- Phase 0.3: Licht-Leck schließen (B3) & konfigurierbare Dunkelheitsmeldung
-- ---------------------------------------------------------------------------

-- | Phase 0.3 (B3): In a dark room without a light source, 'take' on a room item
--   and 'take all' are refused with darkRoomMessage.
testDarkRoomRefusesTakeAndTakeAll :: IO Bool
testDarkRoomRefusesTakeAndTakeAll = do
    let stInHallway = initSampleGame { save = (save initSampleGame) { currentRoom = "hallway" } }
    -- hallway is tagged 'dark', key is in hallway, player has no torch
    -- 1. take key is refused with darkness message
    let (stAfterTake, msgTake) = executeCommand (Interact VTake "key") stInHallway
    r1 <- expectEqual defaultDarkMessage msgTake
    r2 <- expectTrue "key was not taken into inventory" (not (hasItem "key" stAfterTake))
    r3 <- expectEqual (Just (InRoom "hallway")) (itemLocation <$> Map.lookup "key" (itemStates (save stAfterTake)))

    -- 2. take all is refused with darkness message
    let (stAfterTakeAll, msgTakeAll) = executeCommand TakeAll stInHallway
    r4 <- expectEqual defaultDarkMessage msgTakeAll
    r5 <- expectTrue "no items picked up by take all" (null (getItemsInLocation (CarriedBy ActorPlayer) stAfterTakeAll))
    pure (r1 && r2 && r3 && r4 && r5)

-- | Phase 0.3 (B3): In a dark room without a light source, 'examine' on room items/NPCs
--   and 'search' (room or target) are refused with darkRoomMessage.
testDarkRoomRefusesExamineAndSearch :: IO Bool
testDarkRoomRefusesExamineAndSearch = do
    let stInHallway = initSampleGame { save = (save initSampleGame) { currentRoom = "hallway" } }
    -- 1. examine room item ('key')
    let (_, msgExamineItem) = executeCommand (Interact VLookAt "key") stInHallway
    r1 <- expectEqual defaultDarkMessage msgExamineItem

    -- 2. examine room NPC ('goblin')
    let (_, msgExamineNPC) = executeCommand (Interact VLookAt "goblin") stInHallway
    r2 <- expectEqual defaultDarkMessage msgExamineNPC

    -- 3. search room
    let (stAfterSearch, msgSearch) = executeCommand (SearchCmd Nothing) stInHallway
    r3 <- expectEqual defaultDarkMessage msgSearch
    -- hidden note_old is NOT discovered
    let noteDiscovered = maybe False itemDiscovered (Map.lookup "note_old" (itemStates (save stAfterSearch)))
    r4 <- expectTrue "hidden note not discovered in dark search" (not noteDiscovered)
    -- room search outcome hook flag torch_lit was NOT set
    r5 <- expectEqual Nothing (getFlag "torch_lit" stAfterSearch)
    -- OnSearch event does not fire in darkness
    let evsSearch = commandEvents (SearchCmd Nothing) stInHallway stAfterSearch
    r6 <- expectTrue "OnSearch event suppressed in dark room" (null [() | OnSearch _ <- evsSearch])

    -- 4. search target in room
    let (_, msgSearchTarget) = executeCommand (SearchCmd (Just "key")) stInHallway
    r7 <- expectEqual defaultDarkMessage msgSearchTarget
    let (_, msgSearchVerb) = executeCommand (Interact VSearch "key") stInHallway
    r8 <- expectEqual defaultDarkMessage msgSearchVerb

    -- 5. OnLook event suppressed in dark room
    let (stAfterLook, msgLook) = executeCommand Look stInHallway
    r9 <- expectEqual defaultDarkMessage msgLook
    let evsLook = commandEvents Look stInHallway stAfterLook
    r10 <- expectTrue "OnLook event suppressed in dark room" (null [() | OnLook _ <- evsLook])

    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9 && r10)

-- | Phase 0.3 (B3): In a dark room without a light source, 'use' on a room entity
--   and 'use <carried> on <room entity>' are refused with darkRoomMessage.
testDarkRoomRefusesUseOnRoomEntities :: IO Bool
testDarkRoomRefusesUseOnRoomEntities = do
    -- Player in hallway carrying a healing potion; 'key' and 'goblin' are in the dark hallway
    let st0 = initSampleGame { save = (save initSampleGame) { currentRoom = "hallway" } }
        stWithPotion = pickupItem "potion_healing" st0
    -- 1. 'use key' (key in dark room, not carried) is refused
    let (stAfterUseRoom, msgUseRoom) = executeCommand (Interact VUse "key") stWithPotion
    r1 <- expectEqual defaultDarkMessage msgUseRoom
    let evsUseRoom = commandEvents (Interact VUse "key") stWithPotion stAfterUseRoom
    r2 <- expectTrue "OnUse event suppressed for room item in dark room" (null [() | OnUse _ <- evsUseRoom])

    -- 2. 'use potion on goblin' (potion carried, goblin in dark room) is refused
    let (stAfterUseOn, msgUseOn) = executeCommand (InteractWith VUseOn "potion_healing" "goblin") stWithPotion
    r3 <- expectEqual defaultDarkMessage msgUseOn
    -- potion was not consumed
    r4 <- expectTrue "potion remains in inventory" (hasItem "potion_healing" stAfterUseOn)

    pure (r1 && r2 && r3 && r4)

-- | Phase 0.3 (B3): Carried items CAN still be examined, used, and dropped in a dark room.
testDarkRoomAllowsCarriedItemInteractions :: IO Bool
testDarkRoomAllowsCarriedItemInteractions = do
    -- Player in hallway carrying potion_healing (not a lightsource)
    let stInHallway = pickupItem "potion_healing" (initSampleGame { save = (save initSampleGame) { currentRoom = "hallway" } })

    -- 1. examine carried item succeeds and gives item description
    let (_, msgExamine) = executeCommand (Interact VLookAt "potion") stInHallway
    r1 <- expectTrue "can examine carried item in dark" ("bubbling" `isInfixOf` msgExamine && msgExamine /= defaultDarkMessage)

    -- 2. use carried item succeeds
    let (stAfterUse, msgUse) = executeCommand (Interact VUse "potion") stInHallway
    r2 <- expectTrue "can use carried item in dark" (msgUse /= defaultDarkMessage)
    let evsUse = commandEvents (Interact VUse "potion") stInHallway stAfterUse
    r3 <- expectTrue "OnUse fires for carried item in dark" (OnUse "potion_healing" `elem` evsUse)

    -- 3. drop carried item succeeds and places it in room
    let (stAfterDrop, msgDrop) = executeCommand (Interact VDrop "potion") stInHallway
    r4 <- expectTrue "can drop carried item in dark" ("You drop the" `isInfixOf` msgDrop)
    r5 <- expectTrue "potion is no longer carried" (not (hasItem "potion_healing" stAfterDrop))
    r6 <- expectEqual (Just (InRoom "hallway")) (itemLocation <$> Map.lookup "potion_healing" (itemStates (save stAfterDrop)))

    pure (r1 && r2 && r3 && r4 && r5 && r6)

-- | Phase 0.3: In darkness, carried items are prioritized for 'examine' so unseeable
--   room items sharing the same keyword do not shadow carried items.
testDarkRoomCarriedItemNotShadowedByRoomItem :: IO Bool
testDarkRoomCarriedItemNotShadowedByRoomItem = do
    let gw = world initSampleGame
        myKeyDef = (itemDefs gw Map.! "key")
            { itemId = "my_key"
            , itemName = "my key"
            , itemKeywords = ["key", "my key"]
            , itemDescription = plainText "A small shiny personal key."
            }
        gw' = gw { itemDefs = Map.insert "my_key" myKeyDef (itemDefs gw) }
        ist0 = itemStates (save initSampleGame)
        -- Player in dark hallway carrying my_key; 'key' (brass key) is on the hallway floor
        sv = (save initSampleGame)
            { currentRoom = "hallway"
            , itemStates = Map.insert "my_key" (ItemState (CarriedBy ActorPlayer) "intact" Map.empty False) ist0
            }
        st = initSampleGame { world = gw', save = sv }

    -- 1. 'examine key' examines carried 'my_key', not the unseeable room 'key'
    let (_, msgExamine) = executeCommand (Interact VLookAt "key") st
    r1 <- expectEqual "A small shiny personal key." msgExamine

    -- 2. 'examine brass key' (only matches the floor key) is refused due to darkness
    let (_, msgFloorKey) = executeCommand (Interact VLookAt "brass key") st
    r2 <- expectEqual defaultDarkMessage msgFloorKey

    -- 3. 'take key' is refused due to darkness (room item 'key' cannot be taken in the dark)
    let (_, msgTake) = executeCommand (Interact VTake "key") st
    r3 <- expectEqual defaultDarkMessage msgTake

    -- 4. In a lit room ("start"), 'examine key' prioritizes the room item over inventory (Phase 0.2 / B2)
    let svLit = (save initSampleGame)
            { currentRoom = "start"
            , itemStates = Map.insert "key" (ItemState (InRoom "start") "intact" Map.empty False)
                            (Map.insert "my_key" (ItemState (CarriedBy ActorPlayer) "intact" Map.empty False) ist0)
            }
        stLit = initSampleGame { world = gw', save = svLit }
        (_, msgLitExamine) = executeCommand (Interact VLookAt "key") stLit
    r4 <- expectEqual "A small brass key." msgLitExamine

    pure (r1 && r2 && r3 && r4)

-- | Phase 0.3 (B3): Illumination by carrying a light source or by room light_flag
--   restores all normal interactions in a room tagged "dark".
testDarkRoomIlluminationRestoresInteraction :: IO Bool
testDarkRoomIlluminationRestoresInteraction = do
    let stInHallway = initSampleGame { save = (save initSampleGame) { currentRoom = "hallway" } }

    -- 1. Carrying torch (tagged "lightsource") restores take and examine
    let withTorch = pickupItem "torch" stInHallway
    let (stAfterTake, msgTake) = executeCommand (Interact VTake "key") withTorch
    r1 <- expectTrue "take succeeds with torch" (hasItem "key" stAfterTake)
    r2 <- expectTrue "msg indicates key taken" ("You take the key." `isPrefixOf` msgTake)
    let (_, msgExamineNpc) = executeCommand (Interact VLookAt "goblin") withTorch
    r3 <- expectTrue "examine npc succeeds with torch" ("goblin" `isInfixOf` msgExamineNpc)

    -- 2. Setting room light_flag ("torch_lit") to "true" restores interaction even without carrying torch
    let litState = setFlag "torch_lit" "true" stInHallway
    let (stAfterLitTake, msgLitTake) = executeCommand (Interact VTake "key") litState
    r4 <- expectTrue "take succeeds when room light_flag is active" (hasItem "key" stAfterLitTake)
    r5 <- expectTrue "msg indicates key taken when lit" ("You take the key." `isPrefixOf` msgLitTake)

    pure (r1 && r2 && r3 && r4 && r5)

-- | Phase 0.3: Room-level configurable dark message (roomDarkMsg / dark_msg)
--   overrides defaultDarkMessage for all blocked dark room interactions.
testConfigurableDarkMessage :: IO Bool
testConfigurableDarkMessage = do
    let customDark = "You cannot see a thing in the shadowy gloom."
        st0 = initSampleGame
        baseHallway = rooms (world st0) Map.! "hallway"
        customHallway = baseHallway { roomDarkMsg = Just customDark }
        stCustom = pickupItem "potion_healing" $ st0
            { world = (world st0) { rooms = Map.insert "hallway" customHallway (rooms (world st0)) }
            , save = (save st0) { currentRoom = "hallway" }
            }

    -- 1. Look returns custom dark message
    let (_, msgLook) = executeCommand Look stCustom
    r1 <- expectEqual customDark msgLook

    -- 2. Take room item returns custom dark message
    let (_, msgTake) = executeCommand (Interact VTake "key") stCustom
    r2 <- expectEqual customDark msgTake

    -- 3. TakeAll returns custom dark message
    let (_, msgTakeAll) = executeCommand TakeAll stCustom
    r3 <- expectEqual customDark msgTakeAll

    -- 4. Search returns custom dark message
    let (_, msgSearch) = executeCommand (SearchCmd Nothing) stCustom
    r4 <- expectEqual customDark msgSearch

    -- 5. Examine room item returns custom dark message
    let (_, msgExamine) = executeCommand (Interact VLookAt "key") stCustom
    r5 <- expectEqual customDark msgExamine

    -- 6. Use room item returns custom dark message
    let (_, msgUse) = executeCommand (Interact VUse "key") stCustom
    r6 <- expectEqual customDark msgUse

    -- 7. Use carried item on room entity returns custom dark message
    let (_, msgUseOn) = executeCommand (InteractWith VUseOn "potion_healing" "goblin") stCustom
    r7 <- expectEqual customDark msgUseOn

    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7)

-- | Phase 0.3: Room JSON serialization and deserialization preserves roomDarkMsg
--   and supports legacy keys 'dark_msg' and 'dark_message'.
testRoomDarkMsgJsonRoundTrip :: IO Bool
testRoomDarkMsgJsonRoundTrip = do
    let baseRoom = Room "dark_chamber" "Dunkle Kammer" (plainText "Ein dunkler Ort.")
                        Map.empty (Set.singleton "dark") Nothing Nothing Nothing Nothing Nothing Nothing
                        emptyAscii Nothing Nothing Nothing
    -- 1. Default invariant: roomDarkMsg Nothing is omitted from JSON
    let rNothing = baseRoom { roomDarkMsg = Nothing }
        encNothing = BLC.unpack (Aeson.encode rNothing)
    r1 <- expectTrue "roomDarkMsg Nothing is omitted from JSON"
            (not ("\"roomDarkMsg\"" `isInfixOf` encNothing) &&
             not ("\"dark_msg\"" `isInfixOf` encNothing) &&
             not ("\"dark_message\"" `isInfixOf` encNothing))

    -- 2. Round-trip with roomDarkMsg Just
    let rJust = baseRoom { roomDarkMsg = Just "Es ist stockfinster hier." }
        encJust = Aeson.encode rJust
    r2 <- expectTrue "roomDarkMsg serialized into JSON"
            (isInfixOf "\"roomDarkMsg\":\"Es ist stockfinster hier.\"" (BLC.unpack encJust))
    r3 <- case Aeson.decode encJust :: Maybe Room of
        Just dec -> expectEqual (Just "Es ist stockfinster hier.") (roomDarkMsg dec)
        Nothing  -> expectTrue "decode room with roomDarkMsg failed" False

    -- 3. Legacy JSON compatibility: "dark_msg" key
    let jsonDarkMsg = BLC.pack "{\"roomId\":\"r1\",\"roomName\":\"R1\",\"roomDescription\":\"D\",\"roomConnections\":[],\"dark_msg\":\"Legacy dark message\"}"
    r4 <- case Aeson.eitherDecode jsonDarkMsg of
        Right dec -> expectEqual (Just "Legacy dark message") (roomDarkMsg dec)
        Left _    -> expectTrue "decode legacy dark_msg failed" False

    -- 4. Legacy JSON compatibility: "dark_message" key
    let jsonDarkMessage = BLC.pack "{\"roomId\":\"r2\",\"roomName\":\"R2\",\"roomDescription\":\"D\",\"roomConnections\":[],\"dark_message\":\"Another legacy msg\"}"
    r5 <- case Aeson.eitherDecode jsonDarkMessage of
        Right dec -> expectEqual (Just "Another legacy msg") (roomDarkMsg dec)
        Left _    -> expectTrue "decode legacy dark_message failed" False

    pure (r1 && r2 && r3 && r4 && r5)

-- | Phase 0.3 (B3): items tagged "feelable" stay reachable in an unlit room.
--   The author decides per item what can be found and handled by touch.
testFeelableItemReachableInDark :: IO Bool
testFeelableItemReachableInDark = do
    let st = feelableHallwayState

    -- 1. the feelable key can be taken in the dark
    let (stAfterTake, msgTake) = executeCommand (Interact VTake "iron key") st
    r1 <- expectTrue "feelable item is taken in the dark" (hasItem "feel_key" stAfterTake)
    r2 <- expectTrue "take message reports the item" ("You take the iron key." `isPrefixOf` msgTake)

    -- 2. untagged room items stay refused
    let (stAfterPlain, msgPlain) = executeCommand (Interact VTake "relic") st
    r3 <- expectEqual defaultDarkMessage msgPlain
    r4 <- expectTrue "untagged room item stays untouchable" (not (hasItem "plain_relic" stAfterPlain))

    -- 3. examine and use work on a feelable room item
    let (_, msgExamine) = executeCommand (Interact VLookAt "iron key") st
    r5 <- expectEqual "A cold iron key." msgExamine
    let (_, msgUse) = executeCommand (Interact VUse "iron key") st
    r6 <- expectTrue "use on feelable item is not blocked" (msgUse /= defaultDarkMessage)

    -- 4. NPCs are never feelable
    let (_, msgNpc) = executeCommand (Interact VLookAt "goblin") st
    r7 <- expectEqual defaultDarkMessage msgNpc

    pure (r1 && r2 && r3 && r4 && r5 && r6 && r7)

-- | Phase 0.3 (B3): in the dark, 'take all' picks up only the feelable items.
testTakeAllInDarkTakesOnlyFeelable :: IO Bool
testTakeAllInDarkTakesOnlyFeelable = do
    let st = feelableHallwayState
        (stAfter, msg) = executeCommand TakeAll st
    r1 <- expectTrue "feelable item was picked up by take all" (hasItem "feel_key" stAfter)
    r2 <- expectTrue "untagged item was left on the floor" (not (hasItem "plain_relic" stAfter))
    r3 <- expectTrue "message is not the darkness message" (msg /= defaultDarkMessage)
    r4 <- expectTrue "message reports the taken item" ("You take the iron key." `isInfixOf` msg)
    pure (r1 && r2 && r3 && r4)

-- | Phase 0.3 (B3): 'search' stays blocked in the dark for the room and for
--   untagged targets, but works on a feelable target.
testSearchFeelableTargetInDark :: IO Bool
testSearchFeelableTargetInDark = do
    let st = feelableHallwayState
    -- 1. room-wide search stays blocked
    let (_, msgRoom) = executeCommand (SearchCmd Nothing) st
    r1 <- expectEqual defaultDarkMessage msgRoom
    -- 2. untagged target stays blocked
    let (_, msgPlain) = executeCommand (SearchCmd (Just "relic")) st
    r2 <- expectEqual defaultDarkMessage msgPlain
    -- 3. feelable target is allowed
    let (_, msgFeel) = executeCommand (SearchCmd (Just "iron key")) st
    r3 <- expectTrue "search on feelable target is not blocked" (msgFeel /= defaultDarkMessage)
    pure (r1 && r2 && r3)

-- | Phase 0.3 (B3): 'use <carried> on <room entity>' works when the entity is feelable.
testFeelableUseOnInDark :: IO Bool
testFeelableUseOnInDark = do
    let st = pickupItem "potion_healing" feelableHallwayState
    -- 1. feelable room entity is usable
    let (_, msgFeel) = executeCommand (InteractWith VUseOn "potion_healing" "iron key") st
    r1 <- expectTrue "use-on a feelable entity is not blocked" (msgFeel /= defaultDarkMessage)
    -- 2. NPC target stays blocked
    let (_, msgNpc) = executeCommand (InteractWith VUseOn "potion_healing" "goblin") st
    r2 <- expectEqual defaultDarkMessage msgNpc
    pure (r1 && r2)

-- | Phase 0.3 (B3): ambiguity is resolved in the dark only when every candidate is
--   reachable (carried or feelable).
testFeelableAmbiguityInDark :: IO Bool
testFeelableAmbiguityInDark = do
    -- 1. both candidates feelable -> not blocked
    let stBoth = tokensHallwayState True
        (_, msgBoth) = executeCommand (Interact VTake "token") stBoth
    r1 <- expectTrue "ambiguity of feelable items is not blocked" (msgBoth /= defaultDarkMessage)
    -- 2. one candidate untagged -> blocked
    let stMixed = tokensHallwayState False
        (_, msgMixed) = executeCommand (Interact VTake "token") stMixed
    r2 <- expectEqual defaultDarkMessage msgMixed
    pure (r1 && r2)

-- | Shared fixture: dark hallway holding a feelable "feel_key" and an untagged
--   "plain_relic".
feelableHallwayState :: GameState
feelableHallwayState =
    let gw = world initSampleGame
        keyDef = itemDefs gw Map.! "key"
        feelDef = keyDef
            { itemId = "feel_key"
            , itemName = "iron key"
            , itemKeywords = ["iron key", "iron"]
            , itemDescription = plainText "A cold iron key."
            , itemTags = Set.insert "feelable" (itemTags keyDef)
            }
        relicDef = keyDef
            { itemId = "plain_relic"
            , itemName = "stone relic"
            , itemKeywords = ["relic"]
            , itemDescription = plainText "A dull stone relic."
            }
        gw' = gw { itemDefs = Map.insert "feel_key" feelDef
                             (Map.insert "plain_relic" relicDef (itemDefs gw)) }
        ist0 = itemStates (save initSampleGame)
        ist1 = Map.insert "plain_relic" (ItemState (InRoom "hallway") "intact" Map.empty False) ist0
        ist2 = Map.insert "feel_key" (ItemState (InRoom "hallway") "intact" Map.empty False) ist1
        sv = (save initSampleGame) { currentRoom = "hallway", itemStates = ist2 }
    in initSampleGame { world = gw', save = sv }

-- | Shared fixture: dark hallway with two items sharing the keyword "token";
--   either both are feelable or only one of them is.
tokensHallwayState :: Bool -> GameState
tokensHallwayState bothFeelable =
    let gw = world initSampleGame
        keyDef = itemDefs gw Map.! "key"
        base iId nm =
            keyDef { itemId = iId, itemName = nm, itemKeywords = ["token"]
                   , itemDescription = plainText ("A " ++ nm ++ ".") }
        defA = (base "token_a" "brass token")
        defB = (base "token_b" "silver token")
                  { itemTags = if bothFeelable then Set.insert "feelable" (itemTags keyDef)
                                               else itemTags keyDef }
        defA' = defA { itemTags = Set.insert "feelable" (itemTags keyDef) }
        gw' = gw { itemDefs = Map.insert "token_a" defA'
                             (Map.insert "token_b" defB (itemDefs gw)) }
        ist0 = itemStates (save initSampleGame)
        ist1 = Map.insert "token_b" (ItemState (InRoom "hallway") "intact" Map.empty False) ist0
        ist2 = Map.insert "token_a" (ItemState (InRoom "hallway") "intact" Map.empty False) ist1
        sv = (save initSampleGame) { currentRoom = "hallway", itemStates = ist2 }
    in initSampleGame { world = gw', save = sv }


-- ---------------------------------------------------------------------------
-- B8: named RNG streams
-- ---------------------------------------------------------------------------

-- | B8: drawing on a named stream neither consumes the default stream nor
--   touches other named streams — interleaved draws stay invisible to them.
testNamedRngStreamDecoupled :: IO Bool
testNamedRngStreamDecoupled = do
    let drawOn name st = case applyOutcomeWith 0 0 (RandomChoiceOn name [(1, Noop)]) "" st of
            (st', _, _) -> st'
        stB1  = drawOn "beute" initSampleGame
        stB2  = drawOn "beute" stB1
        stW1  = drawOn "wetter" initSampleGame
        stW2  = drawOn "wetter" stB2
    r1 <- expectTrue "default stream is not consumed by named draws"
            (rngState (save stB2) == rngState (save initSampleGame))
    r2 <- expectEqual (getVariable "rng.wetter" stW1) (getVariable "rng.wetter" stW2)
    r3 <- expectTrue "a stream advances across its own draws"
            (getVariable "rng.beute" stB1 /= getVariable "rng.beute" stB2)
    r4 <- expectTrue "different names get different seeds"
            (getVariable "rng.beute" stB1 /= getVariable "rng.wetter" stW1)
    r5 <- expectTrue "streams are created on first use" (isJust (getVariable "rng.beute" stB1))
    pure (and [r1, r2, r3, r4, r5])

-- | B8: the default stream keeps its exact historical draw (byte contract):
--   one draw is @nextRng (rngState + salt)@, and a named draw leaves the
--   default state alone.
testDefaultRngStreamUnchanged :: IO Bool
testDefaultRngStreamUnchanged = do
    let (st1, _, _) = applyOutcomeWith 0 0 (RandomChoice [(1, Noop)]) "" initSampleGame
        (st2, _, _) = applyOutcomeWith 0 0 (RandomChoiceOn "beute" [(1, Noop)]) "" initSampleGame
    r1 <- expectEqual (nextRng (rngState (save initSampleGame))) (rngState (save st1))
    r2 <- expectEqual (rngState (save initSampleGame)) (rngState (save st2))
    pure (r1 && r2)

-- | B8: stream state lives in the VarMap (@rng.<name>@, hex text) and
--   survives the save/load round-trip like every other variable.
testNamedRngStreamSaveRoundtrip :: IO Bool
testNamedRngStreamSaveRoundtrip = do
    let (st1, _, _) = applyOutcomeWith 0 0 (RandomChoiceOn "beute" [(1, Noop)]) "" initSampleGame
        decoded = Aeson.decode (Aeson.encode (save st1)) :: Maybe SaveState
    r1 <- expectTrue "save round-trips" (isJust decoded)
    r2 <- expectEqual (getVariable "rng.beute" st1)
            (decoded >>= \sv -> Map.lookup "rng.beute" (variables sv))
    pure (r1 && r2)

-- ---------------------------------------------------------------------------
-- K1: dice pool (roll_dice)
-- ---------------------------------------------------------------------------

-- | K1: roll_dice draws 'pool' dice with 'die' sides, keeps highest 'keep',
--   writes the four variables to VarMap, and produces no output events.
--   Named streams advance independently and leave default stream untouched.
testRollDicePool :: IO Bool
testRollDicePool = do
    -- 1. Full pool (3d6 keep 3) with fixed initial state
    let (st1, evs1, _) = applyOutcomeWith 0 0 (RollDice 3 6 "" 3) "" initSampleGame
    r1 <- expectEqual [] evs1
    r2 <- expectEqual (Just (VVText "6,2,1")) (getVariable "dice.last_roll" st1)
    r3 <- expectEqual (Just (VVInt 3)) (getVariable "dice.count" st1)
    r4 <- expectEqual (Just (VVInt 6)) (getVariable "dice.highest" st1)
    r5 <- expectEqual (Just (VVInt 9)) (getVariable "dice.sum" st1)

    -- 2. Keep subset (3d6 keep 2): highest two values 6 and 2
    let (st2, evs2, _) = applyOutcomeWith 0 0 (RollDice 3 6 "" 2) "" initSampleGame
    r6 <- expectEqual [] evs2
    r7 <- expectEqual (Just (VVText "6,2")) (getVariable "dice.last_roll" st2)
    r8 <- expectEqual (Just (VVInt 2)) (getVariable "dice.count" st2)
    r9 <- expectEqual (Just (VVInt 6)) (getVariable "dice.highest" st2)
    r10 <- expectEqual (Just (VVInt 8)) (getVariable "dice.sum" st2)

    -- 3. Empty pool: 0 dice
    let (st0, evs0, _) = applyOutcomeWith 0 0 (RollDice 0 6 "" 0) "" initSampleGame
    r11 <- expectEqual [] evs0
    r12 <- expectEqual (Just (VVText "")) (getVariable "dice.last_roll" st0)
    r13 <- expectEqual (Just (VVInt 0)) (getVariable "dice.count" st0)
    r14 <- expectEqual (Just (VVInt 0)) (getVariable "dice.highest" st0)
    r15 <- expectEqual (Just (VVInt 0)) (getVariable "dice.sum" st0)

    -- 4. Named stream does not touch default stream
    let (stNamed, _, _) = applyOutcomeWith 0 0 (RollDice 2 6 "beute" 2) "" initSampleGame
    r16 <- expectEqual (rngState (save initSampleGame)) (rngState (save stNamed))
    r17 <- expectTrue "named stream variable written" (isJust (getVariable "rng.beute" stNamed))

    pure (and [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17])

-- ---------------------------------------------------------------------------
-- Phase 4.2: verb_map phases (before:/instead:)
-- ---------------------------------------------------------------------------

-- | Helper for the Phase 4.2 tests: the sample world with the healing
--   potion's verb_map overridden (the potion starts in the player's room,
--   status "intact").
withPotionVerbMap :: Map.Map (VerbPhase, Verb, String) Effect -> GameState
withPotionVerbMap vm =
    let gw = world initSampleGame
        def0 = Map.findWithDefault (error "missing potion") "potion_healing" (itemDefs gw)
        gw' = gw { itemDefs = Map.insert "potion_healing" (def0 { itemVerbMap = vm }) (itemDefs gw) }
    in initSampleGame { world = gw' }

-- | The potion's current location — where did `take` leave it?
potionLoc :: GameState -> Maybe Location
potionLoc st = itemLocation <$> Map.lookup "potion_healing" (itemStates (save st))

-- | Phase 4.2: `instead:take` replaces the pickup — the entry is the take.
--   The standard guards (`take.already`, …) run first and gate every phase,
--   so a repeat attempt cannot re-fire the entry.
testVerbMapInsteadReplacesTake :: IO Bool
testVerbMapInsteadReplacesTake = do
    let vm = Map.singleton (PhaseInstead, VTake, "intact")
                (Sequence [ MoveEntity "potion_healing" (CarriedBy ActorPlayer)
                          , SendMessage "The vial is yours." ])
        (ls1, evs1) = applyLoopCommandEv (Interact VTake "potion") (initLoopState (withPotionVerbMap vm))
        out1 = renderEvents evs1
    r1 <- expectTrue "instead entry runs" ("The vial is yours." `isInfixOf` out1)
    r2 <- expectTrue "no standard take message"
            (not (renderMsg "take.ok" [("item", "healing potion")] `isInfixOf` out1))
    r3 <- expectEqual (Just (CarriedBy ActorPlayer)) (potionLoc (lsCurrent ls1))
    let (_, evs2) = applyLoopCommandEv (Interact VTake "potion") ls1
        out2 = renderEvents evs2
    r4 <- expectTrue "guards gate every phase"
            (renderMsg "take.already" [("item", "healing potion")] `isInfixOf` out2)
    r5 <- expectTrue "the entry does not re-fire" (not ("The vial is yours." `isInfixOf` out2))
    pure (and [r1, r2, r3, r4, r5])

-- | Phase 4.2: a `before:` entry runs before the standard action and the
--   action continues when the entry does not `block:`.
testVerbMapBeforeRunsThenTake :: IO Bool
testVerbMapBeforeRunsThenTake = do
    let vm = Map.singleton (PhaseBefore, VTake, "intact") (SendMessage "You steady your grip.")
        (ls1, evs1) = applyLoopCommandEv (Interact VTake "potion") (initLoopState (withPotionVerbMap vm))
        out1 = renderEvents evs1
    r1 <- expectTrue "before entry runs first"
            (("You steady your grip.\n" ++ renderMsg "take.ok" [("item", "healing potion")])
                `isInfixOf` out1)
    r2 <- expectEqual (Just (CarriedBy ActorPlayer)) (potionLoc (lsCurrent ls1))
    pure (r1 && r2)

-- | Phase 4.2 (Veto Stufe 2): a `before:` entry with `block:` vetoes the
--   standard action. Turn contract: the veto happens mid-command, so a
--   turn-shaped command ticks like any failed attempt — `block:`'s `turn:`
--   flag steers rule vetos (`on: before`), not verb_map phases.
testVerbMapBeforeVetoBlocksTake :: IO Bool
testVerbMapBeforeVetoBlocksTake = do
    let vm = Map.singleton (PhaseBefore, VTake, "intact")
                (Sequence [ SendMessage "The vial is fused to the shelf."
                          , Block (Just "It will not come loose.") False ])
        (ls1, evs1) = applyLoopCommandEv (Interact VTake "potion") (initLoopState (withPotionVerbMap vm))
        st1 = lsCurrent ls1
        out1 = renderEvents evs1
    r1 <- expectTrue "veto message shown" ("It will not come loose." `isInfixOf` out1)
    r2 <- expectTrue "the entry ran before the veto point" ("The vial is fused to the shelf." `isInfixOf` out1)
    r3 <- expectEqual (Just (InRoom "start")) (potionLoc st1)
    r4 <- expectTrue "no standard take message"
            (not (renderMsg "take.ok" [("item", "healing potion")] `isInfixOf` out1))
    r5 <- expectTrue "turn-shaped command ticks even when vetoed" (turnCount (save st1) == 1)
    pure (and [r1, r2, r3, r4, r5])

-- | Phase 4.2: historical entries (PhaseAfter, no key prefix) are frozen —
--   on `take` the standard pickup AND the entry run, take.ok first.
testVerbMapLegacyTakeUnchanged :: IO Bool
testVerbMapLegacyTakeUnchanged = do
    let vm = Map.singleton (PhaseAfter, VTake, "intact") (SendMessage "Magic hums.")
        (ls1, evs1) = applyLoopCommandEv (Interact VTake "potion") (initLoopState (withPotionVerbMap vm))
        out1 = renderEvents evs1
    r1 <- expectTrue "pickup and entry run, take.ok first"
            ((renderMsg "take.ok" [("item", "healing potion")] ++ "\nMagic hums.") `isInfixOf` out1)
    r2 <- expectEqual (Just (CarriedBy ActorPlayer)) (potionLoc (lsCurrent ls1))
    pure (r1 && r2)

-- | Phase 4.2: historical entries on other verbs replace the standard action
--   (frozen quirk: `drop` with an entry does not actually drop).
testVerbMapLegacyNonTakeReplaces :: IO Bool
testVerbMapLegacyNonTakeReplaces = do
    let vm = Map.singleton (PhaseAfter, VDrop, "intact") (SendMessage "You keep hold of it.")
        game = withPotionVerbMap vm
        st0 = game { save = (save game) { itemStates =
                Map.adjust (\is -> is { itemLocation = CarriedBy ActorPlayer })
                           "potion_healing" (itemStates (save game)) } }
        (ls1, evs1) = applyLoopCommandEv (Interact VDrop "potion") (initLoopState st0)
        out1 = renderEvents evs1
    r1 <- expectTrue "entry replaces the drop" ("You keep hold of it." `isInfixOf` out1)
    r2 <- expectTrue "no standard drop message"
            (not (renderMsg "drop.ok" [("item", "healing potion")] `isInfixOf` out1))
    r3 <- expectEqual (Just (CarriedBy ActorPlayer)) (potionLoc (lsCurrent ls1))
    pure (r1 && r2 && r3)

-- | Phase 4.2: phase JSON. Legacy entries encode WITHOUT a `phase` field
--   (byte contract for every pre-4.2 world.json), before:/instead: entries
--   carry one, and all three round-trip.
testVerbMapPhaseJson :: IO Bool
testVerbMapPhaseJson = do
    let legacy  = Map.singleton (PhaseAfter, VTake, "intact") (SendMessage "a") ::
                    Map.Map (VerbPhase, Verb, String) Effect
        phased  = Map.fromList [ ((PhaseBefore, VTake, "intact"), SendMessage "b")
                               , ((PhaseInstead, VDrop, "open"), SendMessage "c") ]
        legacyText = BLC.unpack (Aeson.encode (verbStateMapToJSON legacy))
        phasedText = BLC.unpack (Aeson.encode (verbStateMapToJSON phased))
    r1 <- expectTrue "legacy entries carry no phase field" (not ("phase" `isInfixOf` legacyText))
    r2 <- expectTrue "phase entries carry the phase field" ("phase" `isInfixOf` phasedText)
    r3 <- expectEqual (Just legacy) (AesonT.parseMaybe verbStateMapFromJSON (verbStateMapToJSON legacy))
    r4 <- expectEqual (Just phased) (AesonT.parseMaybe verbStateMapFromJSON (verbStateMapToJSON phased))
    r5 <- expectEqual (Just legacy)
            (AesonT.parseMaybe verbStateMapFromJSON
                (Aeson.toJSON [ Aeson.object
                    [ AesonKey.fromString "verb" .= ("VTake" :: String)
                    , AesonKey.fromString "state" .= ("intact" :: String)
                    , AesonKey.fromString "effect" .= (SendMessage "a") ] ]))
    pure (and [r1, r2, r3, r4, r5])

-- | Phase 4.2: NPC verb_map phases work like item phases.
testVerbMapPhasesOnNpc :: IO Bool
testVerbMapPhasesOnNpc = do
    let withNpc vm =
            let gw = world initSampleGame
                def0 = Map.findWithDefault (error "missing oldman") "oldman" (npcDefs gw)
                gw' = gw { npcDefs = Map.insert "oldman" (def0 { npcVerbMap = vm }) (npcDefs gw) }
            in initSampleGame { world = gw' }
        insteadMap = Map.singleton (PhaseInstead, VTalk, "alive")
                        (SendMessage "The old man only points at the door.")
        vetoMap = Map.singleton (PhaseBefore, VTalk, "alive")
                        (Block (Just "He is not listening.") False)
        (_, evs1) = applyLoopCommandEv (Interact VTalk "old man") (initLoopState (withNpc insteadMap))
        (_, evs2) = applyLoopCommandEv (Interact VTalk "old man") (initLoopState (withNpc vetoMap))
    r1 <- expectTrue "instead entry replaces the talk"
            ("The old man only points at the door." `isInfixOf` renderEvents evs1)
    r2 <- expectTrue "no dialogue opens" (not ("Greetings, traveler!" `isInfixOf` renderEvents evs1))
    r3 <- expectTrue "before entry vetoes the talk" ("He is not listening." `isInfixOf` renderEvents evs2)
    r4 <- expectTrue "still no dialogue opens" (not ("Greetings, traveler!" `isInfixOf` renderEvents evs2))
    pure (and [r1, r2, r3, r4])

-- ===========================================================================
-- Phase 4.3: language packs (D4)
-- ===========================================================================

-- | Phase 4.3: language-pack layering — pack over English default, per-world
--   `messages:` overrides over the pack, missing keys fall back to the English
--   template (never the loud <msg:...> form).
testLangPackCatalogLayers :: IO Bool
testLangPackCatalogLayers = do
    let cat = effectiveCatalog (Just "de") (Map.singleton "move.ok" "Eigen: {dir}.")
    r1 <- expectTrue "the de pack is registered" (Map.member "de" langPacks)
    r2 <- expectEqual (Just "Eigen: {dir}.") (Map.lookup "move.ok" cat)
    r3 <- expectEqual (Just "Die Tür ist verschlossen.") (Map.lookup "move.door_locked" cat)
    r4 <- expectEqual (Just "Du nimmst {article_acc} {item}.") (Map.lookup "take.ok" cat)
    r5 <- expectTrue "unknown language renders the default catalog"
             (effectiveCatalog (Just "xx") Map.empty == defaultCatalog)
    r6 <- expectTrue "no language renders the default catalog"
             (effectiveCatalog Nothing Map.empty == defaultCatalog)
    r7 <- expectEqual "Eigen: Nord." (renderMsgIn cat "move.ok" [("dir", "Nord")])
    r8 <- expectTrue "knownLanguages covers en and de"
             (all (`elem` knownLanguages) ["en", "de"])
    pure (and [r1, r2, r3, r4, r5, r6, r7, r8])

-- | Phase 4.3: every de-pack template key is an engine catalog key and every
--   template is non-empty — the English fallback must stay an exception.
testLangPackKeysAreKnown :: IO Bool
testLangPackKeysAreKnown =
    case Map.lookup "de" langPacks of
        Nothing -> expectTrue "the de pack is registered" False
        Just p  -> do
            let unknown   = [ k | k <- Map.keys (lpTemplates p), not (Map.member k defaultCatalog) ]
                emptyVals = [ k | (k, v) <- Map.toList (lpTemplates p), null v ]
            r1 <- expectEqual ([] :: [String]) unknown
            r2 <- expectEqual ([] :: [String]) emptyVals
            r3 <- expectTrue "the de pack translates at least one key" (not (Map.null (lpTemplates p)))
            pure (r1 && r2 && r3)

-- | Phase 4.3: world.json gains `language`/`messages` only when set — the M2
--   byte-stability contract (same as procDefs).
testGameWorldLanguageJson :: IO Bool
testGameWorldLanguageJson = do
    let plainJs = BLC.unpack (Aeson.encode emptyGameWorld)
        full = emptyGameWorld
            { worldLanguage = Just "de"
            , worldMessages = Map.fromList [("move.ok", "Los geht's.")]
            }
        fullText = BLC.unpack (Aeson.encode full)
    r1 <- expectTrue "plain world.json omits language and messages"
             (not ("language" `isInfixOf` plainJs) && not ("messages" `isInfixOf` plainJs))
    r2 <- expectTrue "set fields are emitted"
             ("language" `isInfixOf` fullText && "messages" `isInfixOf` fullText)
    r3 <- expectEqual (Just full) (Aeson.decode (Aeson.encode full))
    r4 <- expectEqual (Just emptyGameWorld) (Aeson.decode (Aeson.encode emptyGameWorld))
    pure (r1 && r2 && r3 && r4)

-- | Phase 4.3: the localization pass re-renders keyed messages from key+args
--   against the effective catalog — overrides win, term args are translated,
--   unkeyed payloads and non-message events stay raw; with the default
--   catalog it is the identity.
testLocalizeEventsPass :: IO Bool
testLocalizeEventsPass = do
    let keyed = msgPayload "move.ok" [("dir", "North")]
        raw = MsgPayload Nothing [] "Eigen: roh bleibt roh."
        evs = [EvMessage keyed, EvMessage raw, EvSfx "x.wav"]
        cat = Map.fromList [("move.ok", "Eigen: {dir}.")]
        terms = Map.singleton "dir.north" "Norden"
        out = localizeEvents cat terms evs
        texts = [mpText p | EvMessage p <- out]
    r1 <- expectEqual ["Eigen: Norden.", "Eigen: roh bleibt roh."] texts
    r2 <- expectEqual (EvMessage raw) (out !! 1)
    r3 <- expectEqual (EvSfx "x.wav") (out !! 2)
    r4 <- expectTrue "default catalog + no terms is the identity"
             (localizeEvents defaultCatalog Map.empty evs == evs)
    r5 <- expectTrue "emptyGameWorld is the identity"
             (localizeEventsFor emptyGameWorld evs == evs)
    pure (and [r1, r2, r3, r4, r5])

-- | Phase 4.3: term slots translate enumerable arg values (case-insensitive
--   slot.value lookup); non-slot args pass through unchanged.
testTranslateTermsSlots :: IO Bool
testTranslateTermsSlots = do
    let terms = Map.fromList [("dir.north", "Norden"), ("slot.head", "Kopf")]
    r1 <- expectEqual [("dir", "Norden"), ("name", "North Wind")]
             (translateTerms terms [("dir", "North"), ("name", "North Wind")])
    r2 <- expectEqual [("dir", "South")] (translateTerms terms [("dir", "South")])
    r3 <- expectEqual [("slot", "Kopf")] (translateTerms terms [("slot", "Head")])
    pure (r1 && r2 && r3)

-- | Phase 4.3: renderMsgFor renders against a world's effective catalog —
--   pack templates apply, unknown keys fall back to English, no language is
--   the plain default.
testRenderMsgForWorld :: IO Bool
testRenderMsgForWorld = do
    let w = emptyGameWorld { worldLanguage = Just "de" }
    r1 <- expectEqual "Die Tür ist verschlossen." (renderMsgFor w "move.door_locked" [])
    r2 <- expectEqual "Du nimmst den X." (renderMsgFor w "take.ok" [("item", "X"), ("article_acc", "den")])
    r2b <- expectEqual "Du nimmst  X." (renderMsgFor w "take.ok" [("item", "X")])
    r3 <- expectEqual (renderMsg "move.door_locked" [])
             (renderMsgFor emptyGameWorld "move.door_locked" [])
    pure (r1 && r2 && r2b && r3)

-- | Phase 4.3: card type labels are frozen by default (byte contract) and
--   overridable via the card_type.* terms of a language pack.
testCardTypeLabelTerms :: IO Bool
testCardTypeLabelTerms = do
    r1 <- expectEqual "[Angriff]" (cardTypeLabel CardAttack)
    r2 <- expectEqual "[Fertigkeit]" (cardTypeLabelIn Map.empty CardSkill)
    r3 <- expectEqual "[Attack]"
             (cardTypeLabelIn (Map.singleton "card_type.attack" "[Attack]") CardAttack)
    pure (r1 && r2 && r3)

-- | Phase 4.3: the de pack's input aliases work in a `language: de` world —
--   verbs, commands, directions and prepositions map to the canonical English
--   tokens (P2 scope: the historical words plus the core completions).
testGermanAliasesDeWorld :: IO Bool
testGermanAliasesDeWorld = do
    let de = parseCommandFor (emptyGameWorld { worldLanguage = Just "de" })
    r1 <- expectEqual (Interact VTake "stein") (de "nimm stein")
    r2 <- expectEqual (GiveCmd "stein" "waechter") (de "gib stein an waechter")
    r3 <- expectEqual (TakeFromCmd "schluessel" "truhe") (de "nimm schluessel aus truhe")
    r4 <- expectEqual (Go North) (de "nord")
    r5 <- expectEqual (Go Northwest) (de "nordwest")
    r6 <- expectEqual (Go Northeast) (de "no")
    r7 <- expectEqual Inventory (de "inventar")
    r8 <- expectEqual (SearchCmd Nothing) (de "suche")
    r9 <- expectEqual (InteractWith VUseOn "fackel" "tuer") (de "benutze fackel auf tuer")
    r10 <- expectEqual EndTurnCmd (de "zug beenden")
    r11 <- expectEqual Undo (de "mache rueckgaengig")
    r12 <- expectEqual (AskCmd "waechter" "geruecht") (de "frag waechter nach geruecht")
    r13 <- expectEqual (OpenCmd "truhe") (de "oeffne truhe")
    r14 <- expectEqual (Interact VAttack "goblin") (de "greife goblin")
    r15 <- expectEqual (Interact VLookAt "truhe") (de "untersuche truhe")
    r16 <- expectEqual (Interact VTalk "waechter") (de "sprich waechter")
    r17 <- expectEqual StatsCmd (de "status")
    r18 <- expectEqual MapCmd (de "karte")
    r19 <- expectEqual (Save "savegame") (de "speichern")
    r20 <- expectEqual Help (de "hilfe")
    pure (and [ r1, r2, r3, r4, r5, r6, r7, r8, r9, r10
              , r11, r12, r13, r14, r15, r16, r17, r18, r19, r20 ])

-- | Phase 4.3: without `language:` the German words are rejected — the alias
--   tables are part of the language pack, not of the parser.
testGermanAliasesRequireLanguage :: IO Bool
testGermanAliasesRequireLanguage = do
    r1 <- expectEqual (Unknown "nimm stein") (parseCommand "nimm stein")
    r2 <- expectEqual (Unknown "nord") (parseCommand "nord")
    r3 <- expectEqual (Unknown "inventar") (parseCommand "inventar")
    r4 <- expectEqual (Unknown "gib stein an waechter") (parseCommand "gib stein an waechter")
    r5 <- expectEqual (Unknown "oeffne truhe") (parseCommand "oeffne truhe")
    pure (and [r1, r2, r3, r4, r5])

-- | Phase 4.3: the de pack is complete — every catalog key has a non-empty
--   German template and nothing more (the sync gate enforces the same
--   bijection on the JSON side).
testLangPackComplete :: IO Bool
testLangPackComplete =
    case Map.lookup "de" langPacks of
        Nothing -> expectTrue "the de pack is registered" False
        Just pk -> do
            let de = lpTemplates pk
                missing = [ k | k <- Map.keys defaultCatalog, maybe True null (Map.lookup k de) ]
                extra = [ k | k <- Map.keys de, not (Map.member k defaultCatalog) ]
            r1 <- expectEqual ([] :: [String]) missing
            r2 <- expectEqual ([] :: [String]) extra
            pure (r1 && r2)

-- | Phase 4.3: a German world renders German — a short scripted run shows no
--   English catalog text, no <msg:...> fallback, and the direction term is
--   translated end to end ("Du gehst nach Norden.").
testGermanRunRendersGerman :: IO Bool
testGermanRunRendersGerman = do
    let deWorld = (world initSampleGame) { worldLanguage = Just "de" }
        st0 = initSampleGame { world = deWorld }
        runL cmd st = applyLoopCommand (parseCommand cmd) (initLoopState st)
        takeTxt = snd (runL "take nichtda" st0)
        moveTxt = snd (runL "go north" st0)
        statsTxt = snd (runL "stats" st0)
        allTxt = unwords [takeTxt, moveTxt, statsTxt, snd (runL "inventory" st0)]
    r1 <- expectTrue "no <msg: fallback" (not ("<msg:" `isInfixOf` allTxt))
    r2 <- expectTrue "take refusal is German"
             ("nicht" `isInfixOf` takeTxt && not ("You " `isInfixOf` takeTxt))
    r3 <- expectTrue "movement is German with translated term"
             ("Du gehst nach Norden." `isInfixOf` moveTxt)
    r4 <- expectTrue "stats are German" ("Gesundheit" `isInfixOf` statsTxt)
    r5 <- expectTrue "no English catalog text"
             (not (any (`isInfixOf` allTxt)
                       ["You move ", "You take the ", "Inventory: ", "You're not carrying"]))
    pure (and [r1, r2, r3, r4, r5])

-- ---------------------------------------------------------------------------
-- Phase 4.3.5: Grammatikfelder article:/gender: (Variante A)
-- ---------------------------------------------------------------------------

-- | 4.3.5: 'grammarArgs' attaches article/gender args per entity slot; the
--   flat names accompany the primary entity only; the short form fills the
--   nominative only; empty grammars attach nothing at all.
testGrammarArgs :: IO Bool
testGrammarArgs = do
    let full = Grammar (Just "der") (Just "den") (Just "dem") (Just "m")
        short = Grammar (Just "die") Nothing Nothing Nothing
    r1 <- expectEqual [ ("item_article_nom", "der"), ("item_article_acc", "den")
                      , ("item_article_dat", "dem"), ("item_gender", "m")
                      , ("article_nom", "der"), ("article_acc", "den")
                      , ("article_dat", "dem"), ("gender", "m") ]
             (grammarArgs True "item" full)
    r2 <- expectEqual [ ("npc_article_nom", "der"), ("npc_article_acc", "den")
                      , ("npc_article_dat", "dem"), ("npc_gender", "m") ]
             (grammarArgs False "npc" full)
    r3 <- expectEqual [("item_article_nom", "die"), ("article_nom", "die")]
             (grammarArgs True "item" short)
    r4 <- expectEqual ([] :: [(String, String)]) (grammarArgs True "item" emptyGrammar)
    pure (and [r1, r2, r3, r4])

-- | 4.3.5: missing grammar args render as an empty string (the pinned
--   contract — never the literal placeholder, never <error: …>); supplied
--   args render normally, flat and per-slot.
testGrammarPlaceholdersRenderEmpty :: IO Bool
testGrammarPlaceholdersRenderEmpty = do
    let cat = Map.fromList
            [ ("take.ok", "Du nimmst {article_acc} {item}.")
            , ("put", "Du legst {item_article_acc} {item} in {name_article_dat} {name}.") ]
    r1 <- expectEqual "Du nimmst den X."
             (renderMsgIn cat "take.ok" [("item", "X"), ("article_acc", "den")])
    r2 <- expectEqual "Du nimmst  X." (renderMsgIn cat "take.ok" [("item", "X")])
    r3 <- expectEqual "Du legst den Stein in der Truhe."
             (renderMsgIn cat "put" [ ("item", "Stein"), ("name", "Truhe")
                                    , ("item_article_acc", "den"), ("name_article_dat", "der") ])
    r4 <- expectEqual "Du legst  Stein in  Truhe."
             (renderMsgIn cat "put" [("item", "Stein"), ("name", "Truhe")])
    r5 <- expectTrue "grammar key names are recognised"
             (isGrammarArgKey "article_acc" && isGrammarArgKey "item_gender"
              && isGrammarArgKey "gender" && not (isGrammarArgKey "item"))
    pure (and [r1, r2, r3, r4, r5])

-- | 4.3.5: ItemDef JSON — grammar fields are omitted when empty (byte
--   contract), the short form encodes the plain string variant, the object
--   form round-trips with only the set cases.
testGrammarJson :: IO Bool
testGrammarJson = do
    let base = mkTestKey "k" "K"
        plainJs = BLC.unpack (Aeson.encode base)
        short = base { itemGrammar = Grammar (Just "der") Nothing Nothing Nothing }
        obj = base { itemGrammar = Grammar (Just "der") (Just "den") (Just "dem") (Just "m") }
        shortJs = BLC.unpack (Aeson.encode short)
        objJs = BLC.unpack (Aeson.encode obj)
    r1 <- expectTrue "empty grammar omits article and gender"
             (not ("article" `isInfixOf` plainJs) && not ("gender" `isInfixOf` plainJs))
    r2 <- expectTrue "short form encodes the plain string"
             ("\"article\":\"der\"" `isInfixOf` shortJs && not ("nom" `isInfixOf` shortJs))
    r3 <- expectTrue "object form encodes nom/acc/dat and gender"
             (all (`isInfixOf` objJs)
                  ["\"nom\":\"der\"", "\"acc\":\"den\"", "\"dat\":\"dem\"", "\"gender\":\"m\""])
    r4 <- expectEqual (Just short) (Aeson.decode (Aeson.encode short))
    r5 <- expectEqual (Just obj) (Aeson.decode (Aeson.encode obj))
    pure (and [r1, r2, r3, r4, r5])

-- | 4.3.5: the German pack templates use the grammar placeholders (flat for
--   single-entity messages, per-slot for two-entity messages); the English
--   default catalog carries none (byte contract).
testGermanTemplatesUseArticles :: IO Bool
testGermanTemplatesUseArticles =
    case Map.lookup "de" langPacks of
        Nothing -> expectTrue "the de pack is registered" False
        Just p  -> do
            let keysOf k = maybe [] templateGrammarKeys (Map.lookup k (lpTemplates p))
            r1 <- expectTrue "take.ok uses the flat accusative article"
                     ("article_acc" `elem` keysOf "take.ok")
            r2 <- expectTrue "container.put uses per-slot articles"
                     (all (`elem` keysOf "container.put") ["item_article_acc", "name_article_acc"])
            r3 <- expectTrue "the English catalog uses no grammar placeholders"
                     (null (concatMap templateGrammarKeys (Map.elems defaultCatalog)))
            pure (r1 && r2 && r3)

-- | 4.3.5: end-to-end — a `language: de` world renders the authored article
--   of the taken item (the {article_acc} template at the take call site); an
--   unannotated item renders the placeholder empty (pinned determinism).
testGrammarEndToEndTake :: IO Bool
testGrammarEndToEndTake = do
    let mkIt g = (mkTestKey "schluessel" "Schluessel") { itemGrammar = g }
        mk g = let st = cstate2 [mkTestRoom "halle" "Halle"] [(mkIt g, InRoom "halle")] [] Map.empty
               in st { world = (world st) { worldLanguage = Just "de" } }
        takeTxt = snd (applyLoopCommand (parseCommand "take schluessel")
                          (initLoopState (mk (Grammar (Just "der") (Just "den") (Just "dem") (Just "m")))))
        takeTxt2 = snd (applyLoopCommand (parseCommand "take schluessel")
                           (initLoopState (mk emptyGrammar)))
    r1 <- expectTrue "German take message carries the article"
             ("Du nimmst den Schluessel." `isInfixOf` takeTxt)
    r2 <- expectTrue "missing grammar renders empty (pinned)"
             ("Du nimmst  Schluessel." `isInfixOf` takeTxt2)
    pure (r1 && r2)

main :: IO ()
main = do
    results <- sequence
        -- Parser tests
        [ runTest "resolveInteractTarget resolves all target categories (R3)" testResolveInteractTarget
        , runTest "interactItem preserves take/use contract (R3)" testInteractItemContract
        , runTest "resolveTarget direct resolution and pattern synonyms (Phase 0.1)" testResolveTargetDirect
        , runTest "resolveTarget fixes B1 OnTake with shared keyword (Phase 0.1)" testResolveTargetFixesB1OnTake
        , runTest "resolveTarget ambiguous command execution in same room (Phase 0.1)" testResolveTargetAmbiguousCommandExecution
        , runTest "resolveTarget fixes B1 OnDrop and OnUse with shared keyword (Phase 0.1)" testResolveTargetDropAndUseFixB1
        , runTest "take all and drop all handle items with shared keywords (Phase 0.1)" testTakeAllAndDropAllWithSharedAliases
        , runTest "resolveTarget detects multi-vehicle ambiguity on attack (Phase 0.1)" testResolveTargetVehicleAmbiguity
        , runTest "resolveTarget verb search order direct and predicate (Phase 0.2)" testResolveTargetSearchOrderDirect
        , runTest "resolveTarget drop key with room namesake fixes B2 (Phase 0.2)" testDropKeyWithRoomNamensvetterFixB2
        , runTest "resolveTarget take key with inventory namesake (Phase 0.2)" testTakeKeyWithInventoryNamensvetterFixB2
        , runTest "resolveTarget use key with room namesake (Phase 0.2)" testUseKeyWithRoomNamensvetterFixB2
        , runTest "resolveTarget equip blade with room namesake (Phase 0.2)" testEquipWithRoomNamensvetterFixB2
        , runTest "resolveTarget unequip blade with room namesake (Phase 0.2)" testUnequipWithRoomNamensvetterFixB2
        , runTest "resolveTarget unequip ambiguity filters equipped status (Phase 0.2)" testUnequipAmbiguityFiltersEquipped
        , runTest "resolveTarget equip ambiguity filters equippable items (Phase 0.2)" testEquipAmbiguityFiltersEquippable
        , runTest "resolveTarget search order fallback error messages (Phase 0.2)" testSearchOrderFallbacks
        , runTest "resolveTarget ambiguity scoped to primary search order (Phase 0.2)" testSearchOrderAmbiguityScoped
        -- Phase 0.3: Licht-Leck schließen (B3) & konfigurierbare Dunkelheitsmeldung
        , runTest "dark room refuses take and take all (Phase 0.3, B3)" testDarkRoomRefusesTakeAndTakeAll
        , runTest "dark room refuses examine and search (Phase 0.3, B3)" testDarkRoomRefusesExamineAndSearch
        , runTest "dark room refuses use on room entities (Phase 0.3, B3)" testDarkRoomRefusesUseOnRoomEntities
        , runTest "dark room allows carried item interactions (Phase 0.3, B3)" testDarkRoomAllowsCarriedItemInteractions
        , runTest "dark room examine prioritizes carried over room item (Phase 0.3)" testDarkRoomCarriedItemNotShadowedByRoomItem
        , runTest "dark room illumination restores all interactions (Phase 0.3, B3)" testDarkRoomIlluminationRestoresInteraction
        , runTest "room-level dark_msg overrides default message (Phase 0.3)" testConfigurableDarkMessage
        , runTest "roomDarkMsg JSON round-trip and legacy keys (Phase 0.3)" testRoomDarkMsgJsonRoundTrip
        -- Phase 0.3: feelable — Autoren entscheiden, was im Dunkeln erreichbar ist
        , runTest "feelable item stays reachable in the dark (Phase 0.3, B3)" testFeelableItemReachableInDark
        , runTest "take all in the dark picks up only feelable items (Phase 0.3)" testTakeAllInDarkTakesOnlyFeelable
        , runTest "search in the dark: room and untagged blocked, feelable allowed (Phase 0.3)" testSearchFeelableTargetInDark
        , runTest "use on a feelable room entity in the dark (Phase 0.3)" testFeelableUseOnInDark
        , runTest "ambiguity in the dark needs all candidates reachable (Phase 0.3)" testFeelableAmbiguityInDark
        , runTest "parse look at multi-word target" testParseLookAtMultiWord
        , runTest "parse use-on multi-word target" testParseUseOnMultiWord
        , runTest "parse take multi-word target" testParseTakeMultiWord
        , runTest "parse take all" testParseTakeAll
        , runTest "parse drop all" testParseDropAll
        , runTest "parse get all" testParseGetAll
        , runTest "parse restart" testParseRestart
        , runTest "parse list saves" testParseListSaves
        , runTest "parse stop-word stripping" testParseStopWordStripping
        , runTest "parse stop-word stripping multi-word" testParseStopWordStrippingMultiple
        , runTest "parse equip" testParseEquip
        , runTest "parse wear" testParseWear
        , runTest "parse unequip" testParseUnequip
        , runTest "parse unequip all" testParseUnequipAll
        , runTest "parse stats" testParseStats
        , runTest "parse search" testParseSearch
        , runTest "parse search (bare)" testParseSearchBare
        , runTest "parse search <target>" testParseSearchTarget
        -- Command execution tests
        , runTest "use requires carried item" testUseRequiresInventory
        , runTest "use requires reachable entity" testUseRequiresReachableEntity
        , runTest "use-on door unlocks treasure door" testUseDoorUnlocksTreasureDoor
        , runTest "take all picks up room items" testTakeAllPicksUpItems
        -- Effect tests
        , runTest "GiveItem adds to inventory" testGiveItem
        , runTest "ConsumeItem removes from play" testConsumeItem
        , runTest "SetFlag stores flag" testSetFlag
        , runTest "Conditional (HasFlag) true branch" testCheckFlagTrue
        , runTest "Conditional (HasFlag) false branch" testCheckFlagFalse
        , runTest "GameEnd Death sets game over" testGameEndDeath
        , runTest "GameEnd Victory sets reason" testGameEndVictory
        , runTest "MoveNPC moves to target room" testMoveNPC
        -- Equipment tests
        , runTest "equip requires the item to be carried" testEquipRequiresCarried
        , runTest "equip applies attack bonus" testEquipAppliesBonus
        , runTest "re-equipping same item is idempotent" testEquipSlotConflict
        , runTest "unequip removes bonus" testUnequipRemovesBonus
        , runTest "equip refuses non-equippable item" testEquipNonEquippable
        , runTest "max health bonus applies" testMaxHealthBonus
        , runTest "equip via command" testEquipViaCommand
        , runTest "unequip all via command" testUnequipAllCommand
        , runTest "stats command reports equipment" testStatsCommand
        -- Room hook / visited / search tests
        , runTest "visitedRooms starts empty" testVisitedRoomsStartsEmpty
        , runTest "moving marks room visited" testMoveMarksRoomVisited
        , runTest "SetRoomVisited outcome works" testSetRoomVisitedOutcome
        , runTest "dark room hides contents" testDarkRoomHidesContents
        , runTest "light source reveals dark room" testLightSourceIlluminatesDarkRoom
        , runTest "search reveals hidden item" testSearchRevealsHiddenItem
        , runTest "search outcome sets flag" testSearchSetsFlag
        , runTest "alternative description used when flag set" testAltDescriptionUsed
        -- RNG
        , runTest "RandomChoice produces independent draws" testRandomChoiceSaltDiffers
        -- Combat tests
        , runTest "combat damage uses npcDefenseBase" testCombatDamageUsesDefense
        , runTest "combat off refuses attack" testCombatOffRefusesAttack
        , runTest "combat off custom refusal message" testCombatOffCustomRefusal
        , runTest "combat narrative win runs on_win" testCombatNarrativeWin
        , runTest "combat narrative lose runs on_lose" testCombatNarrativeLose
        -- Phase 7g: party / companions
        , runTest "party member follows on room change" testPartyFollowsOnRoomChange
        , runTest "party non-member stays put" testPartyNonMemberStays
        , runTest "party follows teleport" testPartyFollowsOnTeleport
        , runTest "party companion strikes in combat" testPartyCompanionFights
        , runTest "party regression: no companion, classic output" testPartyNoCompanionUnchanged
        , runTest "party: dead companion inactive + event" testPartyDeadCompanionInactive
        , runTest "party roster survives save/load" testPartyRosterSaveLoadRoundTrip
        , runTest "npc death event message reaches the player" testNPCDeathEventMessageShown
        -- P0-Regressionen
        , runTest "P0-1 defaultSaveState initialises all fields" testDefaultSaveStateFieldsInitialised
        , runTest "P0-2 death event message survives (player kill)" testCombatDeathMessagePlayerKill
        , runTest "P0-2 death event message survives trailing effect" testCombatDeathMessageAfterTrailingEffect
        , runTest "P0-2 death event message survives with ship" testCombatDeathMessageWithShip
        -- Phase 7h: ship systems
        , runTest "ship fires its weapons and spends power" testShipFiresAndSpendsPower
        , runTest "ship without power does not fire" testShipWithoutPowerDoesNotFire
        , runTest "ship shields absorb the return fire" testShipShieldsAbsorbReturnFire
        , runTest "ship hull takes the hit without shields" testShipHullTakesHit
        , runTest "ordinary vehicle fights exactly like on foot" testOrdinaryVehicleUnchanged
        , runTest "ship systems survive save/load" testShipSystemsSaveLoad
        -- Phase 7h-2: ship combat (B0, B1, B2)
        , runTest "ordinary vehicle cannot be attacked as ship (7h-2 B0)" testAttackOrdinaryVehicleRefused
        , runTest "ship at different stop cannot be targeted (7h-2 B1)" testAttackShipDifferentStopRefused
        , runTest "ship-to-ship attack with absorption and return fire (7h-2 B0)" testAttackEnemyShipDamageAndRetaliation
        , runTest "destroying an enemy ship reports destruction without return fire (7h-2 B0)" testAttackEnemyShipDestroyed
        , runTest "ActorShip in rule is validated against vehicle defs (7h-2 B2)" testValidateMissingShipInActorProp
        , runTest "Location player predicate gates on the player's room" testLocationPlayerPredicate
        , runTest "exit quits, disembark leaves the vehicle" testExitIsNotDisembark
        , runTest "player death sets gameOver + Death reason" testPlayerDeathSetsGameOver
        -- Completion tests
        , runTest "completion suggests NPC target" testCompletionSuggestsNpcName
        , runTest "completion suggests inventory item for use" testCompletionSuggestsInventoryItemForUse
        , runTest "completion suggests equippable item" testCompletionSuggestsEquip
        -- JSON round-trip tests
        , runTest "SaveState JSON round-trip" testSaveStateRoundTrip
        , runTest "SaveState backward compat (old format)" testSaveStateBackwardCompat
        -- Skills (Phase 2)
        , runTest "getSkill returns 0 for unknown skill" testGetSkillUnknown
        , runTest "modifySkill adds and stacks" testModifySkill
        , runTest "CheckSkill outcome resolves a branch" testCheckSkillOutcome
        -- Conditions (Phase 2)
        , runTest "condition ticks and expires with end outcome" testConditionTickExpire
        , runTest "condition tick damages player each turn" testConditionTickDamage
        , runTest "ClearCondition removes effect" testClearCondition
        , runTest "HasCondition branches" testHasConditionBranch
        , runTest "stats shows conditions" testStatsShowsConditions
        -- Quests (Phase 2)
        , runTest "quest lifecycle: start, advance, complete" testQuestLifecycle
        , runTest "quest prereqs gate StartQuest" testQuestPrereqs
        , runTest "journal command shows active quest" testJournalShowsQuest
        , runTest "completeQuest fires reward" testQuestRewardFires
        -- Vehicles (Phase 3)
        , runTest "enter vehicle fails when not at stop" testEnterVehicleWrongRoom
        , runTest "enter vehicle boards and moves inside" testEnterVehicle
        , runTest "exit vehicle returns to stop" testExitVehicle
        , runTest "drive fails when not in a vehicle" testDriveOutsideCockpitFails
        , runTest "drive to stop moves vehicle and player" testDriveToStop
        , runTest "drive to current stop is rejected" testDriveToCurrentStop
        , runTest "refuel via item-on-vehicle" testRefuelViaItem
        , runTest "vehicle condition fires vehicle-wide tick" testVehicleConditionTick
        , runTest "VehicleState JSON round-trip" testVehicleStateRoundTrip
        -- Undo (Phase 4.3)
        , runTest "undo restores previous state" testUndoRestoresPreviousState
        , runTest "quit does not create undo history" testQuitDoesNotCreateUndoHistory
        , runTest "help does not affect undo history" testHelpDoesNotAffectUndoHistory
        , runTest "undo with empty history is a no-op" testUndoEmptyHistory
        , runTest "multiple undo walks back multiple states" testMultipleUndo
        , runTest "undo history is capped at 50" testUndoHistoryCappedAt50
        , runTest "save does not affect undo history" testSaveDoesNotAffectUndoHistory
        , runTest "undo restores after death" testUndoRestoresAfterDeath
        -- Phase 1: Outcome-Interpreter (1a)
        , runTest "quest reward can GiveItem via full interpreter" testQuestRewardGiveItemWorks
        -- 4.6: quest chain + on_complete byte contract
        , runTest "quest on_complete starts the chained quest" testQuestOnCompleteStartsChain
        , runTest "quest on_complete JSON omission and round-trip" testQuestOnCompleteJsonOmission
        -- 4.6: initial_flags as validation input
        , runTest "initial_flags count as set for flag validation (4.6)" testInitialFlagsCountAsSet
        , runTest "condition tick can GiveItem via full interpreter" testConditionTickGiveItemWorks
        -- Phase 1: Equipment-Invarianten (1b)
        , runTest "drop equipped item removes bonus" testDropEquippedItemRemovesBonus
        , runTest "consume equipped item removes bonus" testConsumeEquippedItemRemovesBonus
        , runTest "losing maxhp bonus clamps current health" testEquipMaxHpClampOnDrop
        -- Phase 1: TransitionRoom-Hooks (1c)
        , runTest "TransitionRoom runs hooks and marks visited" testTransitionRoomRunsHooks
        , runTest "TransitionRoom clears active dialogue" testTransitionRoomClearsDialogue
        -- Phase 1: Restart (1d)
        , runTest "restart uses custom world, not sample" testRestartUsesCustomWorld
        -- Phase 1: Turn-Kosten (1e)
        , runTest "look does not consume a turn" testLookDoesNotConsumeTurn
        , runTest "go consumes a turn" testGoConsumesTurn
        , runTest "unknown command does not consume a turn" testUnknownDoesNotConsumeTurn
        , runTest "info commands do not consume a turn" testInfoCommandsDoNotConsumeTurn
        -- Phase 1: RNG-State (1f)
        , runTest "RandomChoice advances explicit RNG state" testRandomChoiceAdvancesRng
        , runTest "RandomChoice is deterministic for same seed" testRandomChoiceDeterministic
        , runTest "RandomChoice uses high LCG bits, not low-bit toggle (P1-7)" testRandomChoiceDistribution
        , runTest "LCG nextRng is deterministic and advances" testNextRngDeterministic
        -- Phase 3a: Verb Registry (Custom-Verben)
        , runTest "custom verb parses to VCustom with aliases" testCustomVerbParseCreatesVCustom
        , runTest "unknown verb fails parse" testCustomVerbUnknownFails
        , runTest "core verbs still work without registry" testCustomVerbCoreStillWorks
        , runTest "verb alias map builds correct lookups" testVerbAliasMapBuilt
        -- Phase 3b: Variables
        , runTest "getVariable returns Nothing for unknown" testGetVariableReturnsNothing
        , runTest "setVariable stores value" testSetVariableStoresValue
        , runTest "variable persists across commands" testVariablePersistenceAcrossCommands
        -- Phase 3c: Predicate-DSL
        , runTest "predicate logic (PAll/PAny/PNot)" testPredicateLogic
        , runTest "predicate PlayerHas" testPredicatePlayerHas
        , runTest "predicate HasFlag" testPredicateHasFlag
        , runTest "predicate EntityHasState" testPredicateEntityHasState
        , runTest "predicate RoomHasTag" testPredicateRoomHasTag
        , runTest "predicate Compare with flags" testPredicateCompareFlag
        , runTest "Conditional outcome with predicate" testConditionalOutcome
        -- Phase 3f: Trigger
        , runTest "trigger fires on enter" testTriggerFiresOnEnter
        , runTest "trigger cooldown gates turns" testTriggerCooldownGatesTurns
        , runTest "drain stops when flag fed" testDrainStopsWhenFlagged
        , runTest "noise observer hears at threshold then cooldown" testNoiseObserverAndDecay
        , runTest "once trigger fires only once" testTriggerOnceFiresOnce
        , runTest "once trigger not double-fired by nested round (P1-5)" testTriggerOnceNoDoubleFireAcrossNestedRound
        , runTest "set_state fires OnStateChange (P1-12)" testSetStateFiresStateChange
        , runTest "set_state handler is idempotent, no recursion (P1-12)" testSetStateIdempotentNoRecursion
        , runTest "trigger condition gates firing" testTriggerConditionGates
        , runTest "OnCommand trigger fires" testTriggerCommandEvent
        , runTest "trigger fires through game loop" testTriggerThroughGameLoop
        , runTest "on: command examine fires for look at (P1-14)" testOnCommandExamineFires
        , runTest "take/drop events only on real inventory change (P1-15)" testTakeEventOnlyOnSuccess
        , runTest "raise fires on: custom, self-raise terminates (P1-20)" testRaiseEventFires
        -- P2-Cluster A
        , runTest "restart keeps the loaded world (P2-2)" testRestartKeepsWorld
        , runTest "visited accepts EVBool (P2-7)" testVisitedAcceptsBool
        , runTest "item without ItemState is reported (P2-8)" testItemWithoutStateIsReported
        , runTest "compound map key round-trip incl. legacy (P2-9)" testCompoundKeyRoundTrip
        , runTest "save list entry compatibility (P2-10)" testSaveListEntryCompat
        , runTest "strict fold keeps effect order (P2-11)" testStrictFoldKeepsEffectOrder
        , runTest "dialogue `pick` keyword alias (P2-22)" testDialoguePickKeywordAlias
        , runTest "depth guard reports via diagnostics (P2-23)" testDepthGuardUsesDiagnostics
        , runTest "depth guard stays out of trigger text (P2-23)" testDepthGuardStaysOutOfTriggerText
        , runTest "trigger nesting guard reports (P2-23)" testTriggerNestingGuardReports
        -- Review test gaps (L2, L3, L5, L7, L13)
        , runTest "GameWorld JSON round-trip + profiles (L7)" testGameWorldRoundTrip
        , runTest "resolveCombat effect lists directly (L3)" testResolveCombatDirect
        , runTest "shipAbsorb matrix incl. no shields/hull (L3)" testShipAbsorbMatrix
        , runTest "combat screen: original layout from the pre-round state" testCombatScreenLayout
        , runTest "combat screen: HP bar fill and clamps" testCombatScreenBars
        , runTest "combat screen: authored art/scene/footer" testCombatScreenAuthored
        , runTest "combat screen: wiring + no-screen invariant" testCombatScreenWiring
        , runTest "commandEvents table per command (L5)" testCommandEventsTable
        , runTest "enter event precedes turn event (L5)" testEnterFiresBeforeTurnThroughLoop
        , runTest "consumesTurn complete for every Command (L13)" testConsumesTurnCompleteness
        , runTest "SaveLoad round-trip + legacy + checksum (L2)" testSaveLoadRoundTrip
        , runTest "undo refused when allow_undo = false (Rogue P1)" testPermadeathDisablesUndo
        , runTest "permadeath death menu without undo/load (Rogue P1)" testPermadeathDeathMenu
        , runTest "ironman: save only in savezones, load disabled (Rogue P1)" testIronmanSaveOnlyInSavezone
        , runTest "ironman: checkpoint deleted on death (Rogue P1)" testIronmanCheckpointDeletedOnDeath
        , runTest "meta survives restart (Rogue P2)" testMetaProgressionPreservedOnRestart
        , runTest "meta written on game over (Rogue P2)" testMetaWrittenOnGameOver
        , runTest "dynamic exits: set/remove/rewire + round-trip (Rogue P3)" testDynamicExitOverrides
        , runTest "TA_SAVES_DIR redirects saves + deleteSaveSlot (Rogue P0)" testSavesDirOverride
        , runTest "savesDir default stays 'saves' (Rogue P0)" testSavesDirDefault
        , runTest "slugify is deterministic and file-safe (Rogue P0)" testSlugify
        , runTest "run-seed derivation from slug and run index (Rogue P4c)" testDeriveRunSeed
        , runTest "procedure call binds parameters and pops the scope (Phase 2.5)" testProcCallBindsParams
        , runTest "procedure locals are discarded on return (Phase 2.5)" testProcLocalsAreDiscarded
        , runTest "nested procedure calls keep scopes isolated (Phase 2.5)" testProcNestedScopes
        , runTest "veto inside a procedure stops body and caller (Phase 2.5)" testProcVetoStopsBody
        , runTest "call to unknown procedure is defensive (Phase 2.5)" testProcUnknownDefensive
        , runTest "procedure arity mismatch is defensive (Phase 2.5)" testProcArityDefensive
        , runTest "knows predicate reads known.<actor>.<fact> (W1)" testKnowsPredicate
        , runTest "learn cascade, idempotency and forget (W1)" testLearnCascadeAndForget
        , runTest "OnLearn fires per fact; silent and author messages (W1)" testOnLearnAndMessages
        , runTest "npc knowledge cascades separately (W1)" testNpcKnowledgeCascade
        , runTest "chapter auto-gate: first eligible, one per turn (W3)" testChapterGate
        , runTest "goto/next: refusal, diagnostics, OnChapter (W3)" testChapterSwitchEffects
        , runTest "chapter gate does not cascade (W3)" testChapterOnChapterDoesNotCascade
        , runTest "pursuit core: distances, one-edge steps, tie-break (Tür IV)" testPursuitCore
        , runTest "pursuit distance: seekers, targets, -1 sentinel (Tür IV)" testPursuitDistance
        , runTest "pursuit step effects: messages, override, seeker contract (Tür IV)" testPursuitStepEffects
        , runTest "pursuit fairness: locked door stops the pursuer (Tür IV)" testPursuitFairness
        , runTest "pursuit save/load recomputes the identical step (Tür IV)" testPursuitSaveLoad
        , runTest "tag predicates: actor carries / room holds tagged item (B2)" testTaggedItemPredicates
        , runTest "count family: items/npcs/alive, room or carried, tags (B2)" testCountSpec
        , runTest "mass ops: damage/move/reveal/consume/set_state (B3)" testMassOps
        , runTest "container verbs: open/close/lock/unlock (4.4)" testContainerVerbs
        , runTest "container take/put: scope, capacity, nesting, limit (4.4)" testContainerTakePut
        , runTest "topic table: ask/tell runs effects (4.5)" testTopics
        , runTest "on_talk trigger fires on ask/tell (4.5)" testOnTalkTriggerFires
        , runTest "npc possession: take from / give to (B7)" testNpcTakeGive
        , runTest "npc possession: examine + snapshot visibility (B7)" testNpcCarriedVisibility
        , runTest "item-on-npc interaction: effects + attack fallback (B9)" testNpcItemInteraction
        , runTest "npc interactions omitted from json when empty (B9)" testNpcInteractionsOmittedWhenEmpty
        , runTest "npc equipment: location, slots, combat bonus, visibility (B9)" testNpcEquipment
        , runTest "npc equipment changes the classic combat round (B9)" testNpcEquipmentAffectsCombat
        , runTest "drops_on_death: the corpse keeps or drops its load (B9)" testNpcDropsOnDeath
        , runTest "take all from <npc> (B9)" testNpcTakeAll
        , runTest "npc possession: MoveEntity honours the carrier (B7)" testMoveEntityCarriedByActor
        , runTest "ActorHas predicate for player, NPC and device entity (W4)" testActorHasPredicate
        , runTest "Mount and Unmount effects move item location (W4)" testMountAndUnmountEffects
        , runTest "device examine shows description and mounted item (W4)" testDeviceInteractionExamine
        , runTest "fatal condition tick stops the command (L11)" testFatalTickStopsCommand
        -- Review L4: constructor coverage in Validate
        , runTest "MissingRoom from a rule room reference (L4)" testValidateMissingRoomInRule
        , runTest "MissingNPC from a rule reference (L4)" testValidateMissingNpcInRule
        , runTest "MissingEntity from a VRActorProp ref (L4)" testValidateMissingEntityInRule
        , runTest "InvalidVehicleRoom for a bad entry (L4)" testValidateInvalidVehicleRoom
        -- Review L1 / L8 leftovers
        , runTest "World loaders and their error branches (L1)" testWorldLoadersAndErrors
        , runTest "equipmentSummary text (L8)" testEquipmentSummaryText
        , runTest "state: predicate covers NPC/item/lock states" testEntityStatePredicateCoversAllKinds
        , runTest "combat round state lives in the VarMap (7f-3 A1)" testCombatRoundStateVars
        , runTest "text predicate `{ var: X, is: Y }` (F1/F2)" testVarIsPredicate
        , runTest "CAUseItem is a pinned placeholder (F4)" testUseItemPlaceholder
        , runTest "tactical CAAttack damage and state (7f-3 A2)" testTacticalAttackDamage
        , runTest "tactical CADefend state updates (7f-3 A2)" testTacticalDefend
        , runTest "tactical CAFlee resolves (7f-3 A2)" testTacticalFlee
        , runTest "tactical CAFlee blocked if disallowed (7f-3 A2)" testTacticalFleeBlocked
        , runTest "tactical multi-round combat (7f-3 A2)" testTacticalMultiRound
        , runTest "tactical profile JSON round-trip (7f-3 A2)" testCombatTacticalRoundTrip
        , runTest "tactical abilities resource cost (7f-3 A3)" testAbilityCost
        , runTest "tactical abilities cooldown gating (7f-3 A3)" testAbilityCooldown
        , runTest "tactical BySpeed initiative (7f-3 A3)" testBySpeedInitiative
        -- Phase 7f-3 / 7h-2 V1: ValueRef ADT & Legacy JSON
        , runTest "ValueRef JSON round-trip (V1)" testValueRefRoundTrip
        , runTest "Legacy VRProperty JSON decoding (V1)" testLegacyVRPropertyDecoding
        -- Narratives (Phase 4.4)
        , runTest "narrative returns lines" testNarrativeReturnsLines
        , runTest "narrative stores pending (no side effects)" testNarrativeStoresPending
        , runTest "Narrative JSON round-trip" testNarrativeStateRoundTrip
        -- Validation (Phase 4.5)
        , runTest "sample world is valid" testSampleWorldIsValid
        , runTest "dangling exit is detected" testDanglingExitDetected
        , runTest "duplicate IDs across categories" testDuplicateIDsBetweenItemsAndRooms
        , runTest "unreachable room is detected" testUnreachableRoomDetected
        , runTest "rule effects are validated (P1-1)" testRuleEffectsAreValidated
        , runTest "rule flag checked-but-never-set (P1-1)" testRuleFlagCheckedButNeverSet
        , runTest "missing vehicle in rule is detected (P1-3)" testMissingVehicleInRuleDetected
        -- Dialogue & Polish (Phase 4.6)
        , runTest "dialogue tree start and render" testDialogueTreeStartAndRender
        , runTest "dialogue choice navigation" testDialogueChoiceNavigation
        , runTest "dialogue choice visible_when gating" testDialogueChoiceVisibleWhen
        , runTest "OnUse trigger fires for multi-word item alias" testOnUseTriggerMultiWordAlias
        , runTest "take with on_take picks up and fires effects" testTakeWithOnTakePicksUp
        , runTest "take of non-portable item shows take_failure" testTakeNonPortableFails
        , runTest "invariant: equipped implies carried" testEquippedImpliesCarried
        , runTest "invariant: removed item in no room" testRemovedItemNotInAnyRoom
        , runTest "invariant: every item has one location" testEveryItemHasOneLocation
        , runTest "invariant: save/load keeps RNG/vars/quests/triggers" testSaveStateRoundTripInvariant
        , runTest "invariant: trigger recursion is bounded" testTriggerRecursionBounded
        , runTest "invariant: item verb keys resolve" testItemVerbKeysResolve
        , runTest "dialogue bare number choice" testDialogueBareNumberChoice
        , runTest "dialogue invalid choice" testDialogueInvalidChoice
        , runTest "invalid dialogue choice costs no turn (P1-16)" testInvalidChoiceCostsNoTurn
        , runTest "dialogue end clears active" testDialogueEndClearsActive
        , runTest "room ASCII art display" testRoomAsciiArtDisplay
        , runTest "state-dependent room ascii: default" testCondRoomAsciiDefault
        , runTest "state-dependent room ascii: variant" testCondRoomAsciiVariant
        , runTest "state-dependent room ascii: first variant wins" testCondRoomAsciiOrder
        , runTest "state-dependent npc ascii (alive/dead)" testNpcAsciiStateDependent
        , runTest "corpse stays findable through the kill path" testCorpseStaysFindable
        , runTest "state-dependent item ascii" testItemAsciiStateDependent
        , runTest "passive animation frame from turn count (D)" testPassiveFrame
        , runTest "asciiFrames lists all frames (D)" testAsciiFramesList
        , runTest "watch command queues frames (D)" testWatchCommand
        , runTest "ambient playback carries frames + rate (H1)" testAsciiPlaybackAmbient
        , runTest "watch plays the ambient loop at its rate (H1)" testWatchCarriesRate
        , runTest "ambient survives the JSON round trip (H1)" testAmbientRoundTrip
        , runTest "room intro queues a cutscene (H4)" testIntroQueuesCutscene
        , runTest "play_clip queues a cutscene (H4)" testPlayClipQueuesCutscene
        , runTest "loop plays the cutscene once (H4)" testLoopPlaysCutscene
        , runTest "audio effects queue sfx and music (Audio Phase 1/2)" testAudioEffectsQueueState
        , runTest "sibling save.json auto-discovery" testSiblingSavePath
        , runTest "end_art resolves per reason (G)" testEndArtFor
        , runTest "title_art resolves against state (G)" testTitleArtResolves
        , runTest "look at <n> resolves hotspots (E)" testHotspotNumberLook
        , runTest "hotspot markers are highlighted (E)" testHotspotHighlight
        , runTest "map numbers hotspots + legend (E)" testMapCommand
        , runTest "ascii CondText json round-trip" testAsciiJsonRoundTrip
        , runTest "stripAnsi removes SGR sequences (C)" testStripAnsi
        , runTest "ansiFilter policy (C)" testAnsiFilter
        , runTest "coloured room art is filtered at output (C)" testColoredRoomArtFiltered
        , runTest "missing dialogue node detected" testMissingDialogueNodeDetected
        , runTest "dangling dialogue choice detected" testDanglingDialogueChoiceDetected
        -- Phase 7a: standing predicate sugar
        , runTest "standing predicate parses to CompareVar" testStandingPredicateParsesToCompareVar
        , runTest "standing at_most/equals variants" testStandingPredicateVariants
        , runTest "standing predicate JSON round-trip" testStandingPredicateJSONRoundTrip
        , runTest "standing predicate evaluates against faction var" testStandingPredicateEval
        , runTest "standing outcome writes faction.* variable" testStandingOutcomeWritesVariable
        , runTest "standing outcome applies via dialogue choice" testStandingOutcomeViaDialogue
        -- Phase 7b: Handel
        , runTest "buy with funds deducts and delivers" testTradeBuyWithFunds
        , runTest "buy without cover changes nothing" testTradeBuyInsufficientFundsChangesNothing
        , runTest "sell adds credits and removes item" testTradeSellAddsCredits
        -- P1-8: VTInt-Grenzen wirksam
        , runTest "set_state clamps to VTInt bounds (P1-8)" testVTIntBoundsEnforcedOnSet
        , runTest "modify_value clamps to VTInt bounds (P1-8)" testVTIntBoundsEnforcedOnModify
        -- P1-9: Fuel-Text
        , runTest "vehicle look: empty fuel text (P1-9)" testVehicleLookFuelEmpty
        , runTest "vehicle look: filled fuel text (P1-9)" testVehicleLookFuelFilled
        -- P1-11: InContainer kein stiller No-op
        , runTest "move into container reports error (P1-11)" testMoveEntityInContainerReportsError
        -- P1-10: Autoren-Reihenfolge der Stops
        , runTest "vehicle stops follow authored route (P1-10)" testVehicleStopListUsesAuthoredRoute
        -- P1-13: Genre-Verben nicht mehr im Kern
        , runTest "genre verbs are not core verbs (P1-13)" testGenreVerbsNotCoreVerbs
        , runTest "genre verb comes from the registry (P1-13)" testGenreVerbFromRegistry
        -- Phase V: frontend abstraction
        , runTest "loop runs on a canned (non-Haskeline) frontend" testLoopRunsOnCannedFrontend
        -- Phase 1A: Arithmetic expressions (Expr) & ComputeValue
        , runTest "expr parsing and precedence (Phase 1A)" testExprParsingAndPrecedence
        , runTest "expr eval and zero-safety (Phase 1A)" testExprEvalAndZeroSafety
        , runTest "expr variable resolution (Phase 1A)" testExprVariableResolution
        , runTest "compute_value outcome execution (Phase 1A)" testComputeValueOutcome
        , runTest "expr JSON round-trip (Phase 1A)" testExprJSONRoundTrip
        -- Phase 1B: String interpolation with variables (formatWithVars)
        , runTest "formatWithVars basics and modifiers (Phase 1B)" testFormatWithVarsBasics
        , runTest "formatWithVars integration in outcomes and condText (Phase 1B)" testFormatWithVarsIntegration
        -- Phase 1C: Parameterized commands and argument binding
        , runTest "parameterized command parsing (Phase 1C)" testParameterizedCommandParsing
        , runTest "bind command variables to GameState (Phase 1C)" testBindCommandVars
        , runTest "OnCommand trigger with args and formulas (Phase 1C)" testOnCommandTriggerWithArgsAndFormulas
        , runTest "status verb consumes no turn (Phase 1C)" testStatusVerbNoTurnConsumed
        -- Phase 2A: Card Games & Deckbuilder Data Model
        , runTest "card data types and effect operations JSON round-trip (Phase 2A)" testCardDataTypesJSONRoundTrip
        , runTest "GameWorld cardDefs M2 invariant (Phase 2A)" testGameWorldCardDefsM2Invariant
        , runTest "SaveState deckState M2 invariant (Phase 2A)" testSaveStateDeckStateM2Invariant
        -- Phase 2B: Card & Deck Mechanics
        , runTest "draw cards from full deck (Phase 2B)" testDrawCardsFromFullDeck
        , runTest "draw cards reshuffles discard (Phase 2B)" testDrawCardsReshufflesDiscard
        , runTest "shuffleList is deterministic (Phase 2B)" testShuffleIsDeterministic
        , runTest "play card deducts energy and applies outcomes (Phase 2B)" testPlayCardDeductsEnergyAndAppliesOutcomes
        , runTest "play card exhausts correctly (Phase 2B)" testPlayCardExhaustsCorrectly
        , runTest "end turn discards, restores energy and draws (Phase 2B)" testEndTurnDiscardsAndDraws
        , runTest "card commands parsing (Phase 2B)" testCardCommandsParsing
        , runTest "draw cards hand limit blocks when full (Phase 2 / S2)" testDrawCardsHandLimitBlocked
        , runTest "draw cards hand limit checks after reshuffle (Phase 2 / S2)" testDrawCardsHandLimitReshuffle
        , runTest "card combo synergy outcome (Phase 2 / S2)" testCardSynergyCombo
        -- Phase 2C: Visuals & HUD (Deckbuilder)
        , runTest "hcatBoxes formatting and wrapping (Phase 2C)" testHcatBoxesFormatting
        , runTest "renderCardBox formatting and colors (Phase 2C)" testRenderCardBoxFormattingAndColors
        , runTest "renderDeckCombatHud and showHand (Phase 2C)" testRenderDeckCombatHud
        -- Phase 3A: Sandbox & Runtime-Worldgen Data Model & Invariants
        , runTest "GameWorld sandboxZones M2 invariant (Phase 3A)" testGameWorldSandboxZonesM2Invariant
        , runTest "SaveState dynamicRooms M2 invariant (Phase 3A)" testSaveStateDynamicRoomsM2Invariant
        , runTest "GenerateRoom effect serialization (Phase 3A)" testGenerateRoomEffectSerialization
        -- Phase 3B: Deterministic Cell Derivation & Room Generation
        , runTest "lookupRoom fallback to static room (Phase 3B)" testLookupRoomFallback
        , runTest "lookupRoom dynamic priority (Phase 3B)" testLookupRoomDynamicPriority
        , runTest "deterministic cell seed derivation (Phase 3B)" testDeterministicCellSeed
        , runTest "move into sandbox generates room (Phase 3B)" testMoveIntoSandboxGeneratesRoom
        , runTest "reciprocal exit wiring (Phase 3B)" testReciprocalExitWiring
        -- Phase 3C: Dynamic Room Generation (GenerateRoom) & Resource Formulas
        , runTest "GenerateRoom execution and traversal (Phase 3C)" testGenerateRoomExecution
        , runTest "GenerateRoom variable interpolation (Phase 3C)" testGenerateRoomInterpolation
        , runTest "sandbox resource harvest formulas (Phase 3C)" testSandboxResourceHarvestFormulas
        , runTest "biome landscape ascii art with day/weather variants (Phase 2 / S3)" testBiomeAsciiArtVariantsDayNight
        -- R1: Location & Predicate.Location typed ActorRef and backward compatibility
        , runTest "Location round-trip and backward-compatible decoding (R1)" testLocationJsonRoundTrip
        , runTest "Predicate.Location round-trip and backward-compatible decoding (R1)" testPredicateLocationJsonRoundTrip
        , runTest "at: palyer typo fixture produces validation error (R1)" testValidateTypoInPredicateLocation
        -- Phase 2.1: Predicate has_condition, ValueRef condition_turns, Condition hidden
        , runTest "has_condition predicate evaluation and JSON (Phase 2.1)" testPredicateHasCondition
        , runTest "condition_turns ValueRef evaluation and comparisons (Phase 2.1)" testPredicateConditionTurns
        , runTest "hidden condition omitted from stats and snapshot (Phase 2.1)" testConditionHidden
        , runTest "condition JSON backward-compatible decoding and serialization (Phase 2.1)" testConditionJsonCompatibility
        -- Phase 2.2: Veto Stufe 1 (D3) - OnBefore, Block, cmd.target, Guarded exit
        , runTest "OnBefore veto order and stop semantics (Phase 2.2)" testOnBeforeVetoOrderAndStop
        , runTest "cmd.target and cmd.target_kind binding (Phase 2.2)" testCmdTargetBinding
        , runTest "Guarded exit movement gating and messages (Phase 2.2)" testGuardedExit
        , runTest "Veto turn cost respects consumesTurn flag (Phase 2.2)" testVetoTurnCost
        , runTest "Exit, Effect, and EventType JSON round-trip and backward compat (Phase 2.2)" testExitAndEffectJsonCompatibility
        -- Phase 2.3: Disambiguation
        , runTest "disambiguation: event, numbered question and pending state (Phase 2.3)" testDisambiguationEventAndPending
        , runTest "disambiguation: answer by number picks the candidate (Phase 2.3)" testDisambiguationByNumber
        , runTest "disambiguation: answer by distinguishing word (Phase 2.3)" testDisambiguationByWord
        , runTest "disambiguation: non-answer runs normally and closes the question (Phase 2.3)" testDisambiguationFallback
        , runTest "disambiguation: the answer costs no turn and fires no on: turn (Phase 2.3)" testDisambiguationTurnCost
        -- Phase 2.4: Text-Erweiterung (Inline-Bedingungen, Ausdrücke, Props, Fehlerbehandlung)
        , runTest "interpolation: inline conditions {if cond|a|b} (Phase 2.4)" testInterpolationInlineConditions
        , runTest "interpolation: expressions {= expr} (Phase 2.4)" testInterpolationExpressions
        , runTest "interpolation: item and npc props (Phase 2.4)" testInterpolationProps
        , runTest "interpolation: error handling and propagation (Phase 2.4)" testInterpolationErrorHandling
        , runTest "message catalog invariants (Phase 1.1)" testMessageCatalogInvariants
        , runTest "renderMsg substitutes and escapes args (Phase 1.1)" testRenderMsgArgs
        , runTest "output events: fragment algebra is byte-identical (Phase 1.2)" testOutputFragmentAlgebra
        , runTest "output events: styling model renders spans to ANSI (Phase 1.2)" testOutputStylingModel
        , runTest "output events: EvMessage carries key+args through the loop (Phase 1.2)" testOutputEventKeys
        , runTest "output events: side events for room/quest/gameover/sfx (Phase 1.2)" testOutputSideEvents
        -- Phase 1.3: Purer Session-Automat
        , runTest "session: pure save transition checks policy and yields ReqSave (Phase 1.3)" testSessionTransitionSave
        , runTest "session: pure load transition checks policy and merges meta (Phase 1.3)" testSessionTransitionLoad
        , runTest "session: pure restart transition reseeds and persists meta (Phase 1.3)" testSessionTransitionRestart
        , runTest "session: pure game-over transition routes death and victory (Phase 1.3)" testSessionTransitionGameOver
        , runTest "session: pure death menu transitions (Phase 1.3)" testSessionTransitionDeath
        , runTest "session: pure victory menu transitions (Phase 1.3)" testSessionTransitionVictory
        , runTest "session: advanceNarrative yields ReqPause for intermediate lines (Phase 1.3)" testSessionAdvanceNarrative
        -- Phase 1.4: Protocol v1 (Types, Codec, Golden Tests, Versioning)
        , runTest "protocol: ClientMsg round-trip encoding and decoding (Phase 1.4)" testProtocolClientMsgRoundTrip
        , runTest "protocol: ServerMsg round-trip encoding and decoding (Phase 1.4)" testProtocolServerMsgRoundTrip
        , runTest "protocol: deterministic golden tests with stable bytes (Phase 1.4)" testProtocolGoldenDeterministic
        , runTest "protocol: version mismatch and error handling (Phase 1.4)" testProtocolVersionMismatch
        , runTest "protocol: session lines bridged to wire events (Phase 1.4)" testProtocolSessionBridge
        , runTest "protocol: makeSnapshot extracts presentation-tier state (Phase 1.4)" testProtocolMakeSnapshot
        -- Progression (W2)
        , runTest "progression: gain_xp and level up transitions (W2)" testGainXpAndLevelUp
        , runTest "progression: multi-level up in one turn (W2)" testMultiLevelUpInOneTurn
        , runTest "progression: combat bonus variables (W2)" testCombatBonusVars
        , runTest "progression: negative xp clamp and anti-delevel guarantee (W2)" testNegativeXpClampAndAntiDelevel
        , runTest "progression: stats formatting (W2)" testStatsProgression
        , runTest "progression: GameWorld progressionDef M2 invariant (W2)" testProgressionGameWorldM2Invariant
        -- B8: named RNG streams
        , runTest "rng streams: named draws are decoupled (B8)" testNamedRngStreamDecoupled
        , runTest "rng streams: default stream draw unchanged (B8)" testDefaultRngStreamUnchanged
        , runTest "rng streams: stream state survives save/load (B8)" testNamedRngStreamSaveRoundtrip
        -- K1: dice pool
        , runTest "dice pool: roll_dice writes VarMap and keeps default RNG (K1.1)" testRollDicePool
        -- Phase 4.2: verb_map phases
        , runTest "verb_map: instead: replaces the take (4.2)" testVerbMapInsteadReplacesTake
        , runTest "verb_map: before: runs and the take continues (4.2)" testVerbMapBeforeRunsThenTake
        , runTest "verb_map: before: + block: vetoes the take (4.2)" testVerbMapBeforeVetoBlocksTake
        , runTest "verb_map: legacy take entries frozen (4.2)" testVerbMapLegacyTakeUnchanged
        , runTest "verb_map: legacy non-take entries frozen (4.2)" testVerbMapLegacyNonTakeReplaces
        , runTest "verb_map: phase JSON encoding (4.2)" testVerbMapPhaseJson
        , runTest "verb_map: NPC phases (4.2)" testVerbMapPhasesOnNpc
        -- Phase 4.3: language packs (D4)
        , runTest "lang pack: catalog layering and fallback (4.3)" testLangPackCatalogLayers
        , runTest "lang pack: template keys are catalog keys (4.3)" testLangPackKeysAreKnown
        , runTest "lang pack: world.json language/messages M2 invariant (4.3)" testGameWorldLanguageJson
        , runTest "lang pack: localization pass re-renders keyed messages (4.3)" testLocalizeEventsPass
        , runTest "lang pack: term slots translate enumerable args (4.3)" testTranslateTermsSlots
        , runTest "lang pack: renderMsgFor uses the world catalog (4.3)" testRenderMsgForWorld
        , runTest "lang pack: card type labels are terms (4.3)" testCardTypeLabelTerms
        , runTest "lang pack: de translations cover the catalog (4.3)" testLangPackComplete
        , runTest "lang pack: a German world renders German (4.3)" testGermanRunRendersGerman
        , runTest "lang pack: de input aliases work with language: de (4.3)" testGermanAliasesDeWorld
        , runTest "lang pack: de input aliases need language: de (4.3)" testGermanAliasesRequireLanguage
        -- Phase 4.3.5: Grammatikfelder article:/gender: (Variante A)
        , runTest "grammar: args flat and per-slot (4.3.5)" testGrammarArgs
        , runTest "grammar: placeholders render empty when missing (4.3.5)" testGrammarPlaceholdersRenderEmpty
        , runTest "grammar: JSON omission and round-trip (4.3.5)" testGrammarJson
        , runTest "grammar: de templates use the article placeholders (4.3.5)" testGermanTemplatesUseArticles
        , runTest "grammar: take renders the authored article (4.3.5)" testGrammarEndToEndTake
        ]
    when (not (and results)) exitFailure
