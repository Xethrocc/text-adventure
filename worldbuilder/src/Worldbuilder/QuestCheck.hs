{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | B4: dead quest content (4.6, S3).
--
--   The hard half of quest validation lives in the engine's 'Validate'
--   ('UnknownQuestPrereq', 'UnknownQuestEffect', 'UnknownOnComplete',
--   'EmptyQuestStages'). This module covers what is left and is deliberately
--   *quiet*: a quest that nothing starts, and a quest that can be started but
--   never moves on.
--
--   Both are warnings, never errors, and both are conservative by
--   construction — the point of the B4 channel is to catch dead content, not
--   to second-guess an author. In particular:
--
--   * a prerequisite flag that is never set is **not** reported here. The
--     engine already refuses that hard ('UnknownQuestPrereq'), and a second,
--     softer spelling of the same finding would only be noise.
--   * reachability of the 'start_quest' effect itself is not modelled (a rule
--     may fire in a room the player never visits), so a quest can be reported
--     whose starter is itself hard to reach. Accepted: the finding is about
--     dead content, and the reachability channels report their own half.
--
--   The outcomes are **passed in** rather than collected here. 'allAOutcomes'
--   lives in "Worldbuilder.Compile", which needs this module; collecting them
--   again would mean a second, silently outdated list of outcome surfaces --
--   and a quest whose starter sits in a surface this module forgot would be
--   reported as dead. One list, one owner.
module Worldbuilder.QuestCheck
    ( QuestDiagnostic (..)
    , questDiagnostics
    , startableQuests
    , progressedQuests
    ) where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Worldbuilder.Types

-- | One dead-content finding about a quest.
data QuestDiagnostic = QuestDiagnostic
    { qdQuest  :: String      -- ^ quest id
    , qdPath   :: String      -- ^ dotted path in the authored YAML
    , qdCode   :: String      -- ^ 'QuestNeverStarted' / 'QuestNeverProgressed'
    , qdReason :: String      -- ^ human sentence
    } deriving (Eq, Show)

-- | All quest findings, deterministically ordered by quest declaration order.
--
--   'QuestNeverStarted' wins over 'QuestNeverProgressed': a quest nothing
--   starts has no stage progress to speak of, and reporting both would be two
--   complaints about one problem.
questDiagnostics :: [AActionOutcome] -> Adventure -> [QuestDiagnostic]
questDiagnostics outs a = concat [ perQuest q | q <- advQuests a ]
  where
    startable = startableQuests outs a
    progressed = progressedQuests outs
    perQuest q = case qdReasonFor q of
        Nothing -> []
        Just reason -> [ QuestDiagnostic
                     { qdQuest = aqId q
                     , qdPath = "quests." ++ aqId q
                     , qdCode = code
                     , qdReason = reason } ]
      where
        code = if Set.member (aqId q) startable then "QuestNeverProgressed"
                                                  else "QuestNeverStarted"
        qdReasonFor q'
            | not (Set.member (aqId q') startable) =
                Just $ "nothing can start quest '" ++ aqId q'
                    ++ "': no 'start_quest: " ++ aqId q' ++ "' effect and no 'on_complete:' points to it"
            | not (Set.member (aqId q') progressed) =
                Just $ "quest '" ++ aqId q'
                    ++ "' can be started but never moves on: no 'advance_quest: " ++ aqId q'
                    ++ "' or 'complete_quest: " ++ aqId q' ++ "' effect exists"
            | otherwise = Nothing

-- | Quests that can become active.
--
--   Two ways in: a direct 'start_quest' effect, or an @on_complete:@ chain from
--   a startable quest — the transitive forward reachability of the chain edges
--   from every direct start.
--
--   A pure cycle (@a: b@ / @b: a@ with no direct start) is therefore **not**
--   startable, and both of its quests are reported. That is correct rather than
--   conservative: no sequence of events can ever reach them, which is exactly
--   what 'QuestNeverStarted' claims. Suppressing it would need a *greatest*
--   fixpoint, which calls every quest startable and so reports nothing at all.
startableQuests :: [AActionOutcome] -> Adventure -> Set String
startableQuests outs a = closure Set.empty (Set.fromList direct)
  where
    direct = [ q | AOStartQuest q <- outs ]
    chain :: Map String String
    chain = Map.fromList [ (aqId q, next) | q <- advQuests a, Just next <- [aqOnComplete q] ]
    closure :: Set String -> Set String -> Set String
    closure seen frontier
        | Set.null frontier = seen
        | otherwise          = closure (seen `Set.union` frontier) (step frontier)
      where
        step fs = Set.fromList
            [ to | from <- Set.toList fs, Just to <- [Map.lookup from chain] ]
            Set.\\ seen

-- | Quests that have at least one 'advance_quest' or 'complete_quest' effect.
--
--   Both count: 'advanceQuest' on the last stage completes the quest, so a
--   single-stage quest with only 'advance_quest' is not dead.
progressedQuests :: [AActionOutcome] -> Set String
progressedQuests outs = Set.fromList (direct ++ viaAdvance)
  where
    direct     = [ q | AOCompleteQuest q <- outs ]
    viaAdvance = [ q | AOAdvanceQuest q <- outs ]