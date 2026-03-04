module Main where

import Control.Monad (when)
import Game (getEntityState, pickupItem)
import GameLoop (commandCompletion, initSampleGame)
import Parser (Command (..), executeCommand, parseCommand)
import System.Console.Haskeline (Completion (..))
import System.Exit (exitFailure)
import Types (Verb (..))

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

getCompletions :: String -> IO [String]
getCompletions input = do
    (_, comps) <- commandCompletion initSampleGame (input, "")
    pure [replacement c | c <- comps]

getCompletionsWithState :: String -> IO [String]
getCompletionsWithState input = do
    let withTorch = pickupItem "torch" initSampleGame
    (_, comps) <- commandCompletion withTorch (input, "")
    pure [replacement c | c <- comps]

testParseLookAtMultiWord :: IO Bool
testParseLookAtMultiWord =
    expectEqual (Interact VLookAt "old man") (parseCommand "look at old man")

testParseUseOnMultiWord :: IO Bool
testParseUseOnMultiWord =
    expectEqual (InteractWith VUseOn "brass key" "treasure door") (parseCommand "use brass key on treasure door")

testParseTakeMultiWord :: IO Bool
testParseTakeMultiWord =
    expectEqual (Interact VTake "healing potion") (parseCommand "take healing potion")

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

testCompletionSuggestsNpcName :: IO Bool
testCompletionSuggestsNpcName = do
    suggestions <- getCompletions "look at ol"
    expectContains "old man" suggestions

testCompletionSuggestsInventoryItemForUse :: IO Bool
testCompletionSuggestsInventoryItemForUse = do
    suggestions <- getCompletionsWithState "use t"
    expectContains "torch" suggestions

main :: IO ()
main = do
    results <- sequence
        [ runTest "parse look at multi-word target" testParseLookAtMultiWord
        , runTest "parse use-on multi-word target" testParseUseOnMultiWord
        , runTest "parse take multi-word target" testParseTakeMultiWord
        , runTest "use requires carried item" testUseRequiresInventory
        , runTest "use requires reachable entity" testUseRequiresReachableEntity
        , runTest "use-on door unlocks treasure door" testUseDoorUnlocksTreasureDoor
        , runTest "completion suggests NPC target" testCompletionSuggestsNpcName
        , runTest "completion suggests inventory item for use" testCompletionSuggestsInventoryItemForUse
        ]
    when (not (and results)) exitFailure
