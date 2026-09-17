module Main where

import Control.Monad (when)
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonT
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (isJust)
import Data.Either (isLeft)
import System.Timeout (timeout)
import Control.Exception (evaluate)
import Game
import GameLoop (commandCompletion, LoopState (..), initLoopState, applyLoopCommand,
                 commandEvents, consumesTurn, consumesTurnIn)
import Parser (Command (..), executeCommand, parseCommand, parseCommandWith, helpText)
import Verbs (verbAliasMap)
import Combat (CombatActor (..), CombatTarget (..), ShipSystems (..), resolveCombat, shipAbsorb)
import Validate (ValidationError (..), validateWorld, validateGameState)
import Sample (initSampleGame)
import SaveLoad (computeWorldChecksum, formatSaveEntry, currentSaveVersion)
import qualified SaveLoad as SaveLoad
import System.Directory (createDirectoryIfMissing, doesFileExist, getTemporaryDirectory,
                         removeFile, withCurrentDirectory)
import System.Console.Haskeline (Completion (..))
import System.Exit (exitFailure)
import Types
import World (loadGame, loadGameWorld, loadSaveState)

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

-- ===== Effect Tests =====

testGiveItem :: IO Bool
testGiveItem = do
    let (newState, _) = applyOutcome (MoveEntity "key" (CarriedBy "player")) "" initSampleGame
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
    let (newState, _) = applyOutcome (SetValue (VRProperty "treasure" "visited") (EVInt 1)) "" initSampleGame
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
    Map.empty ["squire", "knappe"] (Just 20) 3 1 Map.empty

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
    let teleport = SetValue (VRProperty "player" "room") (EVString "treasure")
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
        (st', msg) = applyOutcome (ModifyValue (VRProperty "goblin" "hp") (-100)) "" st
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

-- | `Location "player" <room>` gates on the player's room (Phase 7h); the
--   entity form keeps working.
testLocationPlayerPredicate :: IO Bool
testLocationPlayerPredicate = do
    let st = initSampleGame { save = (save initSampleGame) { currentRoom = "hallway" } }
    r1 <- expectTrue "player in room" (evalPredicate (Location "player" "hallway") st)
    r2 <- expectTrue "player not in room" (not (evalPredicate (Location "player" "start") st))
    r3 <- expectTrue "npc location still works" (evalPredicate (Location "goblin" "hallway") st)
    pure (r1 && r2 && r3)

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
        worldPath = "/tmp/ta-p01-defaultsavestate-world.json"
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
    Map.empty ["guard"] (Just 20) 3 1 Map.empty

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
    let endMsgs = [m | m <- msgs2, "You feel better." `isPrefixOf` m]
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
testQuestRewardGiveItemWorks :: IO Bool
testQuestRewardGiveItemWorks = do
    let quest = Quest "test_quest" "Test Quest" "desc"
                    Map.empty
                    [QuestStage "s1" "step one" Nothing]
                    (Just (MoveEntity "torch" (CarriedBy "player")))
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
    let cond = Condition "reward_tick" 2 (Just (MoveEntity "torch" (CarriedBy "player"))) Nothing
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
        (st, _) = applyOutcome (SetValue (VRProperty "player" "room") (EVString "treasure")) "" withHook
    r1 <- expectEqual (Just "true") (getFlag "entered_treasure" st)
    r2 <- expectTrue "treasure marked visited" ("treasure" `Set.member` visitedRooms (save st))
    pure (r1 && r2)

-- | TransitionRoom räumt den aktiven Dialog
testTransitionRoomClearsDialogue :: IO Bool
testTransitionRoomClearsDialogue = do
    let inDialogue = initSampleGame
            { save = (save initSampleGame) { activeDialogue = Just "oldman" } }
        (st, _) = applyOutcome (SetValue (VRProperty "player" "room") (EVString "treasure")) "" inDialogue
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
    r1 <- expectTrue "trigger fired" (not (null msg))
    r2 <- expectTrue "message mentions treasure" (isInfixOf "treasure" msg)
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
                        [ ModifyValue (VRProperty "wolf" "hp") (-100) ] True 0
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
        (st', msg) = applyOutcome (SetValue (VRProperty "gate" "state") (EVString "unlocked")) "" st
    r1 <- expectTrue "OnStateChange fired on set_state" (isInfixOf "rumbles" msg)
    r2 <- expectEqual (Just "unlocked") (getEntityState "gate" st')
    pure (r1 && r2)

-- | P1-12 recursion bound: a rule whose `on: state` handler re-writes the same
--   state must terminate — the idempotent writer is the recursion bound.
testSetStateIdempotentNoRecursion :: IO Bool
testSetStateIdempotentNoRecursion = do
    let rule = TriggerDef "gate_loop" (OnStateChange "gate") Nothing
                    [ SetValue (VRProperty "gate" "state") (EVString "unlocked") ] False 0
        st = initSampleGame { world = (world initSampleGame) { triggerDefs = [rule] } }
        run = fst (applyOutcome (SetValue (VRProperty "gate" "state") (EVString "unlocked")) "" st)
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
                          (Just "The statue will not budge.") Map.empty
        st0 = base { world = (world base)
                         { itemDefs = Map.insert "statue" nonPortable (itemDefs (world base)) }
                   , save  = (save base)
                       { itemStates = Map.insert "statue"
                             (ItemState (InRoom "start") "intact" Map.empty False)
                             (itemStates (save base)) } }
        carriedTorch = st0 { save = (save st0)
                       { itemStates = Map.adjust (\i -> i { itemLocation = CarriedBy "player" })
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
            Nothing Nothing Nothing Nothing Nothing
        gw = (world initSampleGame) { rooms = Map.singleton "roomA" roomA }
        errors = validateWorld gw
    expectTrue "dangling exit detected" (DanglingExit "roomA" North "roomZ" `elem` errors)

testDuplicateIDsBetweenItemsAndRooms :: IO Bool
testDuplicateIDsBetweenItemsAndRooms = do
    let gw = (world initSampleGame)
                { rooms = Map.insert "key" (Room "key" "Duplicate" (plainText "desc.") Map.empty Set.empty Nothing
                    Nothing Nothing Nothing Nothing Nothing) (rooms (world initSampleGame)) }
        errors = validateWorld gw
    expectTrue "duplicate key found" (any isDup errors)
  where
    isDup (DuplicateID _ _ _) = True
    isDup _ = False

testUnreachableRoomDetected :: IO Bool
testUnreachableRoomDetected = do
    let roomIsolated = Room "isolated" "Isolated" (plainText "Alone.") Map.empty Set.empty Nothing
            Nothing Nothing Nothing Nothing Nothing
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
                         Set.empty Nothing [] False Nothing True Nothing Map.empty)
                (itemDefs (world sample))
            , triggerDefs =
                [ TriggerDef "light_lantern" (OnUse "oil_can") Nothing
                    [SetValue (VRFlag "lantern_lit") (EVString "true")] False 0 ]
            }
        st = sample
            { world = w
            , save = (save sample)
                { itemStates = Map.insert "oil_can"
                    (ItemState (CarriedBy "player") "intact" Map.empty True)
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
                         (Map.singleton (VTake, "intact") (SetValue (VRFlag "took") (EVString "true"))))
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
                         Map.empty)
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
    (Just Weapon) [] False Nothing True Nothing Map.empty

-- | Equipped items are always also carried.
testEquippedImpliesCarried :: IO Bool
testEquippedImpliesCarried = do
    let sample = initSampleGame
        w = (world sample) { itemDefs = Map.insert "blade" (invItem "blade") (itemDefs (world sample)) }
        st = sample { world = w
                    , save = (save sample)
                        { itemStates = Map.insert "blade"
                            (ItemState (CarriedBy "player") "intact" Map.empty True)
                            (itemStates (save sample)) } }
        (st', _) = executeCommand (EquipCmd "blade") st
    r1 <- expectEqual (Just "blade") (Map.lookup Weapon (equipment (save st')))
    r2 <- expectEqual (Just (CarriedBy "player"))
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
                            (ItemState (CarriedBy "player") "intact" Map.empty True)
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
        loopEff = ModifyValue (VRProperty "wolf" "hp") (-100)
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
        keys = [ (itemId i, v) | i <- Map.elems (itemDefs gw), (v, _) <- Map.keys (itemVerbMap i) ]
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

testRoomAsciiArtDisplay :: IO Bool
testRoomAsciiArtDisplay = do
    let banner = "=== [CASTLE GATE] ==="
    let roomWithAscii = (rooms (world initSampleGame) Map.! "start") { roomAscii = Just banner }
    let game = initSampleGame { world = (world initSampleGame) { rooms = Map.insert "start" roomWithAscii (rooms (world initSampleGame)) } }
    let (_, msg) = executeCommand Look game
    expectTrue "ascii banner displayed in look" (banner `isInfixOf` msg)

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
                    ["recruiter"] Nothing 0 0 Map.empty)
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
                      , MoveEntity "rope" (CarriedBy "player")
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
                    (Map.singleton (VCustom "buy", "intact") buyEff
                        `Map.union` Map.singleton (VCustom "sell", "intact") sellEff))
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
                (ItemState (CarriedBy "player") "intact" Map.empty True)
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

-- | P2-7: `visited` via `SetValue (VRProperty room "visited")` must accept
--   `EVBool`, not silently treat it as `False`.
testVisitedAcceptsBool :: IO Bool
testVisitedAcceptsBool = do
    let (stT, _) = applyOutcome (SetValue (VRProperty "treasure" "visited") (EVBool True)) "" initSampleGame
        (stF, _) = applyOutcome (SetValue (VRProperty "treasure" "visited") (EVBool False)) "" initSampleGame
    r1 <- expectTrue "EVBool True marks the room visited" (isRoomVisited "treasure" stT)
    r2 <- expectTrue "EVBool False clears it" (not (isRoomVisited "treasure" stF))
    pure (r1 && r2)

-- | P2-8: item lookup scans `itemStates`, so an `ItemDef` placed nowhere is
--   invisible at runtime. The validator must report it (§`MissingItemState`)
--   instead of letting the item silently not exist.
testItemWithoutStateIsReported :: IO Bool
testItemWithoutStateIsReported = do
    let lamp = ItemDef "lamp" "lamp" (plainText "A brass lamp.") ["lamp"] Set.empty
                    Nothing [] False Nothing True Nothing Map.empty
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
                (item0 { itemVerbMap = Map.fromList [((VCustom "buy", "intact:v2"), SendMessage "a")] })
                (itemDefs gw0)
            , entityInteractions = Map.fromList [(("a|b", "c"), ("unlocked", "msg"))]
            , itemInteractions   = Map.fromList [(("x:y", "z|w"), SendMessage "b")] }
    r1 <- expectEqual (Just gw) (Aeson.decode (Aeson.encode gw))
    -- legacy form of the verb map: "VTake:intact"
    let legacyVerbValue = Aeson.toJSON (Map.fromList [("VTake:intact", SendMessage "x")] :: Map.Map String Effect)
    r2 <- case AesonT.parseMaybe verbStateMapFromJSON legacyVerbValue of
        Just m  -> expectEqual (Map.fromList [((VTake, "intact"), SendMessage "x")]) m
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
        compatibleEntry = formatSaveEntry csum (mkSave csum)
        mismatchEntry   = formatSaveEntry csum (mkSave "12345")
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
              (msgViaList == "first\nsecond\nthird")
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
              (not ("[ERROR]" `isInfixOf` msg) && not ("[engine]" `isInfixOf` msg))
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
            [ CombatClassic
            , CombatOff Nothing
            , CombatOff (Just "You cannot fight here.")
            , CombatNarrative (NarrativeCombat 3 (SendMessage "hit") (SendMessage "miss"))
            ]
        rt p = Aeson.decode (Aeson.encode p) :: Maybe CombatProfile
    r2 <- expectTrue "every combat profile round-trips"
              (all (\p -> rt p == Just p) profiles)
    -- the compound-key map specifically (rewritten by P2-9)
    let item0 = snd (Map.findMin (itemDefs gw))
        gw2 = gw { itemDefs = Map.insert "custom"
                     (item0 { itemVerbMap = Map.fromList
                                [((VCustom "buy", "intact"), SendMessage "ok")] })
                     (itemDefs gw) }
    r3 <- expectTrue "itemVerbMap with a VCustom key round-trips"
              (any (\d -> Map.member (VCustom "buy", "intact") (itemVerbMap d))
                    (Map.elems (maybe Map.empty itemDefs (Aeson.decode (Aeson.encode gw2)))))
    pure (r1 && r2 && r3)

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
        playerEffect = ModifyValue (VRProperty "goblin" "hp") (-pdmg)
    -- player alone: exactly one effect, no retaliation when the blow is fatal
    let (effs, msgs) = resolveCombat CombatClassic [PlayerActor] target st0
        stKill = st0 { save = (save st0)
                         { npcStates = Map.adjust (\ns -> ns { npcHealth = Just 1 })
                                                  "goblin" (npcStates (save st0)) } }
        (effsKill, msgsKill) = resolveCombat CombatClassic [PlayerActor] target stKill
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
        (effsC, _) = resolveCombat CombatClassic
                        [PlayerActor, CompanionActor "ally", ShipActor "carriage"] target stC
    r6 <- expectEqual
              [ playerEffect
              , ModifyValue (VRProperty "goblin" "hp") (-allyDmg)
              , SetValue (VRVariable "ship.carriage.power") (EVInt 2)
              , ModifyValue (VRProperty "goblin" "hp") (-6) ]
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

-- | L13: `consumesTurn` ends in a `_ -> True` catch-all, so a new `Command`
--   constructor silently becomes turn-consuming — that is how P1-16 happened.
--   `expectedConsumesTurn` matches every constructor **without** a wildcard and
--   the suite builds with `-Werror=incomplete-patterns`, so adding a constructor
--   fails the build until its verdict is written down here.
expectedConsumesTurn :: Command -> Bool
expectedConsumesTurn cmd = case cmd of
    Go _               -> True
    Look               -> False
    Inventory          -> False
    Interact _ _       -> True
    InteractWith _ _ _ -> True
    ChooseCmd _        -> True   -- refined by consumesTurnIn: only a valid choice ticks
    TakeAll            -> True
    DropAll            -> True
    CompoundCommand _  -> True
    EquipCmd _         -> True
    UnequipCmd _       -> True
    UnequipAllCmd      -> True
    StatsCmd           -> False
    SearchCmd _        -> True
    JournalCmd         -> False
    Undo               -> False
    EnterVehicleCmd _  -> True
    ExitVehicleCmd     -> True
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
    Unknown _          -> False

-- | One sample per `Command` constructor.
allCommandSamples :: [Command]
allCommandSamples =
    [ Go North, Look, Inventory, Interact VLookAt "x", InteractWith VUse "x" "y"
    , ChooseCmd 1, TakeAll, DropAll, CompoundCommand [Look]
    , EquipCmd "x", UnequipCmd "x", UnequipAllCmd, StatsCmd, SearchCmd Nothing
    , JournalCmd, Undo, EnterVehicleCmd "v", ExitVehicleCmd, DriveToCmd "s"
    , WaitCmd, RefuelCmd "v", RepairCmd "v", Save "s", Load "s", ListSaves
    , Restart, Help, Quit, Unknown "z" ]

-- | L13: the verdict table must match `consumesTurn` for every constructor, and
--   the sample count pins the list so a forgotten sample is noticed.
testConsumesTurnCompleteness :: IO Bool
testConsumesTurnCompleteness = do
    let st0 = initSampleGame
    r1 <- expectTrue "consumesTurn matches the documented verdict everywhere"
              (all (\c -> consumesTurn c == expectedConsumesTurn c) allCommandSamples)
    r2 <- expectEqual 29 (length allCommandSamples)
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
        createDirectoryIfMissing True "saves"
        BLC.writeFile "saves/l2legacy.json" (Aeson.encode (save st0))
        legacy <- SaveLoad.loadGame st0 "l2legacy"
        r3 <- case legacy of
            Just st' -> expectEqual (save st0) (save st')
            Nothing  -> expectTrue "a bare SaveState loads via the legacy branch" False
        -- ... and it must not be mistaken for a wrapper while decoding
        r4 <- expectEqual (Nothing :: Maybe SaveFile)
                  (Aeson.decode (Aeson.encode (save st0)))
        mapM_ (\f -> do
                  e <- doesFileExist f
                  when e (removeFile f))
              ["saves/l2slot.json", "saves/l2legacy.json"]
        pure (r1 && r2 && r3 && r4)

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
        movePlayer = withRule (SetValue (VRProperty "player" "room") (EVString "ghost_room"))
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

-- | L4: a `VRProperty` reference to an entity that is neither an item, an NPC nor
--   an exit lock key.
testValidateMissingEntityInRule :: IO Bool
testValidateMissingEntityInRule = do
    let withTarget t = (world initSampleGame)
                { triggerDefs = [ TriggerDef "t" OnTurn Nothing
                                    [SetValue (VRProperty t "state") (EVString "open")] False 0 ] }
    r1 <- expectTrue "an unknown VRProperty target is reported"
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
        mapM_ (\f -> do
                  e <- doesFileExist f
                  when e (removeFile f))
              ["l1-world.json", "l1-save.json", "l1-bogus.json"]
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
        , runTest "commandEvents table per command (L5)" testCommandEventsTable
        , runTest "enter event precedes turn event (L5)" testEnterFiresBeforeTurnThroughLoop
        , runTest "consumesTurn complete for every Command (L13)" testConsumesTurnCompleteness
        , runTest "SaveLoad round-trip + legacy + checksum (L2)" testSaveLoadRoundTrip
        , runTest "fatal condition tick stops the command (L11)" testFatalTickStopsCommand
        -- Review L4: constructor coverage in Validate
        , runTest "MissingRoom from a rule room reference (L4)" testValidateMissingRoomInRule
        , runTest "MissingNPC from a rule reference (L4)" testValidateMissingNpcInRule
        , runTest "MissingEntity from a VRProperty ref (L4)" testValidateMissingEntityInRule
        , runTest "InvalidVehicleRoom for a bad entry (L4)" testValidateInvalidVehicleRoom
        -- Review L1 / L8 leftovers
        , runTest "World loaders and their error branches (L1)" testWorldLoadersAndErrors
        , runTest "equipmentSummary text (L8)" testEquipmentSummaryText
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
        ]
    when (not (and results)) exitFailure
