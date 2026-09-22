-- | Pure tab completion: word lists and contextual suggestions (Phase V).
--
--   This module is deliberately free of I/O: the Haskeline plumbing lives in
--   "Frontend" (haskelineFrontend wraps 'completionFor' in a CompletionFunc),
--   and a future TUI frontend consumes 'completionFor' directly from its edit
--   widget. Keeping the suggestion logic pure keeps it testable without a
--   terminal.
module Completion
  ( completionFor
  , commandWords
  , directionWords
  ) where

import Types
import Game (getCurrentRoom, getItemsInLocation, getNPCsInRoom)
import Data.Char (toLower)
import Data.List (isPrefixOf, nub)
import qualified Data.Map.Strict as Map

commandWords :: [String]
commandWords =
    [ "go", "move", "walk", "look", "examine", "inspect", "read", "take", "pick", "drop", "put"
    , "search", "watch", "map", "legend", "inventory", "inv", "i", "use", "talk", "speak", "choose", "option", "attack", "hit", "kill"
    , "equip", "wear", "wield", "unequip", "remove", "stats"
    , "enter", "board", "disembark", "drive", "wait", "refuel", "repair"
    , "undo", "save", "load", "saves", "restart", "help", "quit", "exit", "q"
    , "activate"
    ]

directionWords :: [String]
directionWords = ["north", "south", "east", "west", "up", "down"
    , "northeast", "northwest", "southeast", "southwest"
    , "ne", "nw", "se", "sw"]

itemCompletionTerms :: [ItemDef] -> [String]
itemCompletionTerms items = nub (concatMap (\i -> itemName i : itemKeywords i) items)

npcCompletionTerms :: [NPCDef] -> [String]
npcCompletionTerms npcs = nub (concatMap (\n -> npcName n : npcKeywords n) npcs)

roomTargets :: GameState -> [String]
roomTargets state =
    let currentRoomId = currentRoom (save state)
        roomItems = getItemsInLocation (InRoom currentRoomId) state
        roomNpcs = getNPCsInRoom currentRoomId state
    in nub (itemCompletionTerms roomItems ++ npcCompletionTerms roomNpcs)

inventoryTargets :: GameState -> [String]
inventoryTargets state = itemCompletionTerms (getItemsInLocation (CarriedBy "player") state)

-- | Local copy of the reachable locked-exit entities. (The parser has its own
--   variant for its own purposes; the two are deliberately independent.)
reachableExitEntities :: GameState -> [String]
reachableExitEntities state = case getCurrentRoom state of
    Just room -> [entity | Locked _ entity <- Map.elems (roomConnections room)]
    Nothing   -> []

entityTargets :: GameState -> [String]
entityTargets state =
    let exits = reachableExitEntities state
        doorAliases = if null exits then [] else ["door", "locked door"]
    in nub (roomTargets state ++ exits ++ doorAliases)

-- | Words for adventure-declared custom verbs (canonical names + aliases)
customVerbWords :: GameState -> [String]
customVerbWords state =
    concatMap (\def -> vdName def : vdAliases def)
        (Map.elems (verbDefs (world state)))

contextualSuggestions :: GameState -> [String] -> [String]
contextualSuggestions state prevWords = case prevWords of
    [] -> commandWords ++ customVerbWords state ++ directionWords
    ("go" : _) -> directionWords
    ("move" : _) -> directionWords
    ("walk" : _) -> directionWords
    ("look" : "at" : _) -> roomTargets state
    ("talk" : "to" : _) -> npcCompletionTerms (getNPCsInRoom (currentRoom (save state)) state)
    ("speak" : "with" : _) -> npcCompletionTerms (getNPCsInRoom (currentRoom (save state)) state)
    ("pick" : "up" : _) -> roomTargets state
    ("put" : "down" : _) -> inventoryTargets state
    ("equip" : _) -> inventoryTargets state
    ("wear" : _) -> inventoryTargets state
    ("wield" : _) -> inventoryTargets state
    ("unequip" : _) -> itemCompletionTerms (getEquippedItems state)
    ("remove" : _) -> itemCompletionTerms (getEquippedItems state)
    ("use" : _)
        | "on" `elem` prevWords || "with" `elem` prevWords -> entityTargets state
        | otherwise -> inventoryTargets state
    (verb : _)
        | verb `elem` ["take", "drop", "attack", "hit", "kill", "examine", "inspect", "read"] ->
            roomTargets state ++ inventoryTargets state
        | otherwise -> commandWords ++ customVerbWords state ++ directionWords
                       ++ roomTargets state ++ entityTargets state ++ inventoryTargets state

-- | ItemDefs currently worn/wielded
getEquippedItems :: GameState -> [ItemDef]
getEquippedItems state =
    [ def
    | iId <- Map.elems (equipment (save state))
    , Just def <- [Map.lookup iId (itemDefs (world state))]
    ]

-- | Pure completion: given the already-typed text, return the word being
--   completed and the suggestions that still match it (case-insensitive,
--   deduplicated). The frontend decides how to present the options.
completionFor :: GameState -> String -> (String, [String])
completionFor state left =
    let loweredLeft = map toLower left
        tokens = words loweredLeft
        (prevWords, currentWord)
            | not (null loweredLeft) && last loweredLeft /= ' ' && not (null tokens) =
                (init tokens, last tokens)
            | otherwise = (tokens, "")
        suggestions = contextualSuggestions state prevWords
        loweredPrefix = map toLower currentWord
    in (currentWord, filter (\opt -> loweredPrefix `isPrefixOf` map toLower opt) (nub suggestions))
