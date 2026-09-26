-- | Quest logic and state tracking for the text adventure engine
module Quests
    ( lookupQuest
    , questStage
    , questCompleted
    , canStartQuest
    , startQuest
    , completeQuest
    , advanceQuest
    , completeQuestWith
    , journalText
    ) where

import Types
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Look up a quest definition
lookupQuest :: QuestID -> GameState -> Maybe Quest
lookupQuest qId state = Map.lookup qId (questDefs (world state))

-- | Is the quest active, and if so at which stage index (0-based)?
questStage :: QuestID -> GameState -> Maybe Int
questStage qId state = Map.lookup qId (activeQuests (save state))

-- | Is the quest completed?
questCompleted :: QuestID -> GameState -> Bool
questCompleted qId state = Set.member qId (completedQuests (save state))

-- | Can the quest be started? (exists, not active, not completed, prereqs met)
canStartQuest :: QuestID -> GameState -> Bool
canStartQuest qId state = case lookupQuest qId state of
    Nothing -> False
    Just q ->
        not (questCompleted qId state)
            && not (Map.member qId (activeQuests (save state)))
            && all (\(f, v) -> Map.lookup f (flags (save state)) == Just v) (Map.toList (questPrereqs q))

-- | Start a quest at stage 0
startQuest :: QuestID -> GameState -> GameState
startQuest qId state = state
    { save = (save state) { activeQuests = Map.insert qId 0 (activeQuests (save state)) } }

-- | Complete a quest in the save state
completeQuest :: QuestID -> GameState -> GameState
completeQuest qId state = state
    { save = (save state)
        { activeQuests = Map.delete qId (activeQuests (save state))
        , completedQuests = Set.insert qId (completedQuests (save state)) } }

-- | Advance a quest to the next stage, or complete it if already at the last
advanceQuest :: QuestID -> GameState -> GameState
advanceQuest qId state =
    case Map.lookup qId (activeQuests (save state)) of
        Nothing -> state
        Just idx -> case lookupQuest qId state of
            Nothing -> state
            Just q
                | idx + 1 >= length (questStages q) -> completeQuest qId state
                | otherwise -> state
                    { save = (save state) { activeQuests = Map.insert qId (idx + 1) (activeQuests (save state)) } }

-- | Mark a quest completed and fire its reward with a runner.
--   Returns the new state plus the reward message ("" if none).
completeQuestWith :: (Effect -> EntityID -> GameState -> (GameState, String))
                  -> QuestID -> GameState -> (GameState, String)
completeQuestWith runOutcome qId state =
    let withoutActive = completeQuest qId state
    in case lookupQuest qId state >>= questReward of
        Nothing -> (withoutActive, "")
        Just outcome -> runOutcome outcome "" withoutActive

-- | Journal text: active quests with their current stage, then completed ones
journalText :: GameState -> String
journalText state =
    let active = Map.toList (activeQuests (save state))
        completed = Set.toList (completedQuests (save state))
        activeLines =
            [ "- " ++ questName q ++ ": " ++ stageText q idx
            | (qId, idx) <- active
            , Just q <- [lookupQuest qId state] ]
        completedLines =
            [ "- " ++ questName q ++ " (completed)"
            | qId <- completed
            , Just q <- [lookupQuest qId state] ]
    in case (activeLines, completedLines) of
        ([], []) -> "Your journal is empty."
        _ -> "=== Journal ===\n" ++
             (if null activeLines then "" else unlines ("Active:" : activeLines)) ++
             (if null completedLines then "" else unlines ("Completed:" : completedLines))
  where
    stageText q idx =
        case drop idx (questStages q) of
            (s:_) -> qsText s ++ maybe "" (\h -> " (Hint: " ++ h ++ ")") (qsHint s)
            []    -> "?"
