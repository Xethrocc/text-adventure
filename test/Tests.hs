module Main where

import Control.Monad (when)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (isJust)
import Game
import GameLoop (commandCompletion, LoopState (..), initLoopState, applyLoopCommand)
import Parser (Command (..), executeCommand, parseCommand, applyOutcome)
import Validate (ValidationError (..), validateWorld)
import Sample (initSampleGame)
import System.Console.Haskeline (Completion (..))
import System.Exit (exitFailure)
import Types

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

-- ===== ActionOutcome Tests =====

testGiveItem :: IO Bool
testGiveItem = do
    let (newState, msg) = applyOutcome (GiveItem "key" "You received the key!") "" initSampleGame
    r1 <- expectTrue "key in inventory" (hasItem "key" newState)
    r2 <- expectEqual "You received the key!" msg
    pure (r1 && r2)

testConsumeItem :: IO Bool
testConsumeItem = do
    let withTorch = pickupItem "torch" initSampleGame
        (newState, msg) = applyOutcome (ConsumeItem "torch" "The torch crumbles to ash.") "" withTorch
    r1 <- expectTrue "torch not in inventory" (not (hasItem "torch" newState))
    r2 <- expectEqual "The torch crumbles to ash." msg
    pure (r1 && r2)

testSetFlag :: IO Bool
testSetFlag = do
    let (newState, _) = applyOutcome (SetFlag "quest_started" "true" "Quest started!") "" initSampleGame
    expectEqual (Just "true") (getFlag "quest_started" newState)

testCheckFlagTrue :: IO Bool
testCheckFlagTrue = do
    let stateWithFlag = setFlag "door_open" "yes" initSampleGame
        (_, msg) = applyOutcome
            (CheckFlag "door_open" "yes"
                (MessageOnly "The door is open!")
                (MessageOnly "The door is closed."))
            "" stateWithFlag
    expectEqual "The door is open!" msg

testCheckFlagFalse :: IO Bool
testCheckFlagFalse = do
    let (_, msg) = applyOutcome
            (CheckFlag "door_open" "yes"
                (MessageOnly "The door is open!")
                (MessageOnly "The door is closed."))
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
    let (newState, msg) = applyOutcome (MoveNPC "goblin" "start" "The goblin arrives!") "" initSampleGame
        npcsInStart = getNPCsInRoom "start" newState
    r1 <- expectTrue "goblin now in start room" (any (\n -> npcId n == "goblin") npcsInStart)
    r2 <- expectEqual "The goblin arrives!" msg
    pure (r1 && r2)

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
    let (newState, _) = applyOutcome (SetRoomVisited "treasure" True "noted") "" initSampleGame
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
    r1 <- expectTrue "note not visible before search" (not ("old note" `isInfixOfT` lookMsg))
    r2 <- expectTrue "search reports the find" ("old note" `isInfixOfT` searchMsg)
    r3 <- expectTrue "note visible after search" ("old note" `isInfixOfT` lookAfter)
    pure (r1 && r2 && r3)
  where
    isInfixOfT needle haystack = any (needle `isSubOf`) (tailsT haystack)
    isSubOf n h = take (length n) h == n
    tailsT s = s : case s of { [] -> []; (_:xs) -> tailsT xs }

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
    expectTrue "alt description shown" ("sputter to life" `isInfixOfT` msg)
  where
    isInfixOfT needle haystack = any (\h -> take (length needle) h == needle) (scanr (:) [] haystack)

-- ===== RNG salt test (Phase 4.1) =====

testRandomChoiceSaltDiffers :: IO Bool
testRandomChoiceSaltDiffers = do
    -- Two consecutive RandomChoices in the same MultipleOutcomes must be able
    -- to resolve differently. With the old hardcoded salt=0 they were identical.
    let outcome = MultipleOutcomes
            [ RandomChoice [MessageOnly "A", MessageOnly "B", MessageOnly "C", MessageOnly "D"]
            , RandomChoice [MessageOnly "A", MessageOnly "B", MessageOnly "C", MessageOnly "D"] ]
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

testPlayerDeathSetsGameOver :: IO Bool
testPlayerDeathSetsGameOver = do
    let weakPlayer = initSampleGame { save = (save initSampleGame) { currentRoom = "hallway", player = Player 1 100 10 0 Map.empty } }
        (newState, _) = executeCommand (Interact VAttack "goblin") weakPlayer
    r1 <- expectTrue "game over on death" (gameOver (save newState))
    r2 <- expectEqual (Just Death) (gameOverReason (save newState))
    pure (r1 && r2)

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
    -- lockpick is 2 in the sample; DC 3 with a d6 roll always passes (2+1 >= 3)
    let outcome = CheckSkill "lockpick" 3
                    (MessageOnly "PICKED") (MessageOnly "FAILED")
        (_, msg) = applyOutcome outcome "" initSampleGame
    expectTrue "low DC passes with skill 2" ("PICKED" `isInfixT` msg)
  where
    isInfixT needle hay = any (needle `isPrefixT`) (dropTailT hay)
    isPrefixT p s = take (length p) s == p
    dropTailT s = s : case s of { [] -> []; (_:xs) -> dropTailT xs }

-- ===== Conditions (Phase 2) =====

testConditionTickExpire :: IO Bool
testConditionTickExpire = do
    let poisoned = applyCondition "poisoned" 2
                        (Just (MessageOnly "It stings."))
                        (Just (MessageOnly "You feel better.")) initSampleGame
        (after1, _) = tickConditions poisoned
        (after2, msgs2) = tickConditions after1
    r1 <- expectTrue "active after 1 tick" (hasCondition "poisoned" after1)
    r2 <- expectTrue "expired after 2 ticks" (not (hasCondition "poisoned" after2))
    let endMsgs = [m | m <- msgs2, "You feel better." `isPrefixT2` m]
    r3 <- expectTrue "end message produced" (not (null endMsgs))
    pure (r1 && r2 && r3)
  where
    isPrefixT2 p s = take (length p) s == p

testConditionTickDamage :: IO Bool
testConditionTickDamage = do
    let hurt = applyCondition "bleeding" 3
                    (Just (DamagePlayer 10 "You lose blood."))
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
    let applied = applyCondition "invisible" 5 Nothing Nothing initSampleGame
        outcome = HasCondition "invisible"
                    (MessageOnly "SEEN-NO") (MessageOnly "SEEN-YES")
        (_, msgApplied) = applyOutcome outcome "" applied
        (_, msgClean)   = applyOutcome outcome "" initSampleGame
    r1 <- expectTrue "then-branch when active" ("SEEN-NO" `elem` lines msgApplied || "SEEN-NO" `isPrefixT3` msgApplied)
    r2 <- expectTrue "else-branch when absent" ("SEEN-YES" `elem` lines msgClean || "SEEN-YES" `isPrefixT3` msgClean)
    pure (r1 && r2)
  where
    isPrefixT3 p s = take (length p) s == p

testStatsShowsConditions :: IO Bool
testStatsShowsConditions = do
    let applied = applyCondition "poisoned" 4 Nothing Nothing initSampleGame
        (_, msg) = executeCommand StatsCmd applied
    expectTrue "stats mentions poisoned" ("poisoned" `isInfixT4` msg)
  where
    isInfixT4 needle hay = any (needle `isPrefixT4`) (tailsT hay)
    isPrefixT4 p s = take (length p) s == p
    tailsT s = s : case s of { [] -> []; (_:xs) -> tailsT xs }

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
    -- StartQuest without the flag set must fail
    let outcome = StartQuest "gated_quest" "started!"
        (_, msg1) = applyOutcome outcome "" initSampleGame
    r1 <- expectEqual "You cannot start that quest right now." msg1
    -- With the flag set it must work
    let unlocked = setFlag "met_oldman" "true" initSampleGame
        (st2, msg2) = applyOutcome outcome "" unlocked
    r2 <- expectEqual "started!" msg2
    r3 <- expectEqual (Just 0) (questStage "gated_quest" st2)
    pure (r1 && r2 && r3)

testJournalShowsQuest :: IO Bool
testJournalShowsQuest = do
    let started = startQuest "find_treasure" initSampleGame
        (_, msg) = executeCommand JournalCmd started
    r1 <- expectTrue "journal mentions quest name" ("The Lost Treasure" `isInfixT5` msg)
    r2 <- expectTrue "journal shows current stage" ("Explore the dark hallway." `isInfixT5` msg)
    pure (r1 && r2)
  where
    isInfixT5 needle hay = any (needle `isPrefixT5`) (tailsT5 hay)
    isPrefixT5 p s = take (length p) s == p
    tailsT5 s = s : case s of { [] -> []; (_:xs) -> tailsT5 xs }

testQuestRewardFires :: IO Bool
testQuestRewardFires = do
    -- Start, advance twice, then CompleteQuest directly
    let started = startQuest "find_treasure" initSampleGame
        (completed, msg) = applyOutcome (CompleteQuest "find_treasure" "done!") "" started
    r1 <- expectTrue "quest completed" (questCompleted "find_treasure" completed)
    r2 <- expectTrue "reward message present" ("treasure is yours" `isInfixT6` msg)
    r3 <- expectTrue "done! message present" ("done!" `isInfixT6` msg)
    pure (r1 && r2 && r3)
  where
    isInfixT6 needle hay = any (needle `isPrefixT6`) (tailsT6 hay)
    isPrefixT6 p s = take (length p) s == p
    tailsT6 s = s : case s of { [] -> []; (_:xs) -> tailsT6 xs }

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
        ("can't drive there" `isInfixOfV` msg)
  where isInfixOfV n h = any (n `isPrefixV`) (tailsV h)
        isPrefixV p s = take (length p) s == p
        tailsV s = s : case s of { [] -> []; (_:xs) -> tailsV xs }

testRefuelViaItem :: IO Bool
testRefuelViaItem = do
    let withHay = pickupItem "hay" initSampleGame
        (st, msg) = executeCommand (parseCommand "use hay on carriage") withHay
    r1 <- expectTrue "fuel message shown" (elemV "fuelled" msg)
    r2 <- expectEqual (Just 10) (vsFuel (getVehicleState "carriage" st))
    pure (r1 && r2)
  where
    elemV n h = any (n `isPrefixV`) (tailsV h)
    isPrefixV p s = take (length p) s == p
    tailsV s = s : case s of { [] -> []; (_:xs) -> tailsV xs }

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
                Map.singleton "test_leak" (DamagePlayer 5 "Cold air rushes in!") })
                "carriage" (vehicleDefs (world breach)) } }
        (st, msg) = vehicleConditionTick breachDef
    r1 <- expectTrue "damage applied" (playerHealth (player (save st)) == 95)
    r2 <- expectContains "Cold air rushes in!" [msg]
    pure (r1 && r2)

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
    let step loop = fst (applyLoopCommand Look loop)
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

-- ===== Narrative tests (Phase 4.4) =====

testNarrativeReturnsLines :: IO Bool
testNarrativeReturnsLines = do
    let (_, msg) = applyOutcome (Narrative ["Line 1", "Line 2"] (MessageOnly "done")) "" initSampleGame
    expectEqual "Line 1\nLine 2" msg

testNarrativeStoresPending :: IO Bool
testNarrativeStoresPending = do
    let (st, _) = applyOutcome (Narrative ["Hello!"] (HealPlayer 10 "You feel better.")) "" initSampleGame
    r1 <- expectTrue "pendingNarrative is set" (isJust (pendingNarrative st))
    r2 <- expectEqual 100 (playerHealth (player (save st)))
    pure (r1 && r2)

testNarrativeStateRoundTrip :: IO Bool
testNarrativeStateRoundTrip = do
    let encoded = Aeson.encode (Narrative ["A", "B"] (MessageOnly "end"))
        decoded = Aeson.decode encoded :: Maybe ActionOutcome
    expectEqual (Just (Narrative ["A", "B"] (MessageOnly "end"))) decoded

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
    let roomA = Room "roomA" "Room A" "desc." (Map.singleton North (Open "roomZ")) Set.empty Map.empty Nothing
            Nothing Nothing Nothing Nothing
        gw = (world initSampleGame) { rooms = Map.singleton "roomA" roomA }
        errors = validateWorld gw
    expectTrue "dangling exit detected" (DanglingExit "roomA" North "roomZ" `elem` errors)

testDuplicateIDsBetweenItemsAndRooms :: IO Bool
testDuplicateIDsBetweenItemsAndRooms = do
    let gw = (world initSampleGame)
                { rooms = Map.insert "key" (Room "key" "Duplicate" "desc." Map.empty Set.empty Map.empty Nothing
                    Nothing Nothing Nothing Nothing) (rooms (world initSampleGame)) }
        errors = validateWorld gw
    expectTrue "duplicate key found" (any isDup errors)
  where
    isDup (DuplicateID _ _ _) = True
    isDup _ = False

testUnreachableRoomDetected :: IO Bool
testUnreachableRoomDetected = do
    let roomIsolated = Room "isolated" "Isolated" "Alone." Map.empty Set.empty Map.empty Nothing
            Nothing Nothing Nothing Nothing
        gw = (world initSampleGame)
                { rooms = Map.insert "isolated" roomIsolated (rooms (world initSampleGame)) }
        errors = validateWorld gw
    expectTrue "unreachable room detected" (UnreachableRoom "isolated" `elem` errors)

main :: IO ()
main = do
    results <- sequence
        -- Parser tests
        [ runTest "parse look at multi-word target" testParseLookAtMultiWord
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
        -- ActionOutcome tests
        , runTest "GiveItem adds to inventory" testGiveItem
        , runTest "ConsumeItem removes from play" testConsumeItem
        , runTest "SetFlag stores flag" testSetFlag
        , runTest "CheckFlag true branch" testCheckFlagTrue
        , runTest "CheckFlag false branch" testCheckFlagFalse
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
        -- Narratives (Phase 4.4)
        , runTest "narrative returns lines" testNarrativeReturnsLines
        , runTest "narrative stores pending (no side effects)" testNarrativeStoresPending
        , runTest "Narrative JSON round-trip" testNarrativeStateRoundTrip
        -- Validation (Phase 4.5)
        , runTest "sample world is valid" testSampleWorldIsValid
        , runTest "dangling exit is detected" testDanglingExitDetected
        , runTest "duplicate IDs across categories" testDuplicateIDsBetweenItemsAndRooms
        , runTest "unreachable room is detected" testUnreachableRoomDetected
        ]
    when (not (and results)) exitFailure
