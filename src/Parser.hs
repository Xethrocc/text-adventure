-- | Command parsing and processing for the text adventure engine
module Parser where

import Types
import Game
import Data.Char (toLower)
import Data.List (isPrefixOf, find, intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map

-- | Parsed command structure

data Command = Go Direction
              | Look
              | LookAt String
              | Take String
              | Drop String
              | Inventory
              | Use String
              | UseOn String String
              | Talk String
              | Quit
              | Help
              | Unknown String
    deriving (Show, Eq)

-- | Parse user input into a command

parseCommand :: String -> Command
parseCommand input = case words (map toLower input) of
    []                     -> Unknown ""
    ["go", dir]            -> parseDirection dir
    ["go", "to", dir]      -> parseDirection dir
    ["move", dir]          -> parseDirection dir
    ["move", "to", dir]    -> parseDirection dir
    ["walk", dir]          -> parseDirection dir
    ["walk", "to", dir]    -> parseDirection dir
    ["north"]              -> Go North
    ["south"]              -> Go South
    ["east"]               -> Go East
    ["west"]               -> Go West
    ["up"]                 -> Go Up
    ["down"]               -> Go Down
    ["look"]               -> Look
    ["look", "at", target] -> LookAt target
    ["examine", target]     -> LookAt target
    ["inspect", target]     -> LookAt target
    ["take", target]        -> Take target
    ["pick", "up", target]  -> Take target
    ["grab", target]        -> Take target
    ["get", target]         -> Take target
    ["drop", target]        -> Drop target
    ["put", "down", target] -> Drop target
    ["inventory"]           -> Inventory
    ["inv"]                 -> Inventory
    ["i"]                   -> Inventory
    ["use", item, "on", entity]   -> UseOn item entity
    ["use", item, "with", entity] -> UseOn item entity
    ["use", target]         -> Use target
    ["talk", "to", target]  -> Talk target
    ["speak", "with", target] -> Talk target
    ["chat", "with", target] -> Talk target
    ["help"]               -> Help
    ["quit"]               -> Quit
    ["exit"]               -> Quit
    ["q"]                  -> Quit
    _                      -> Unknown input
    
    where
        parseDirection dir = case dir of
            "north" -> Go North
            "south" -> Go South
            "east"  -> Go East
            "west"  -> Go West
            "up"    -> Go Up
            "down"  -> Go Down
            _       -> Unknown dir

-- | Execute a command and return updated game state and message

type CommandResult = (GameState, String)

executeCommand :: Command -> GameState -> CommandResult
executeCommand (Go dir) state
    | canMove dir state = case getExitInDirection dir state of
        Just (Open roomName) -> (moveToRoom roomName state, "You move " ++ show dir ++ ".")
        Just (Locked roomName entityTarget)
            | getEntityState entityTarget state == Just "unlocked" -> 
                (moveToRoom roomName state, "You move " ++ show dir ++ " through the unlocked " ++ entityTarget ++ ".")
            | otherwise -> (state, "The door is locked.")
        Nothing -> (state, "There's nothing in that direction.")
    | otherwise = (state, "You can't go that way.")

executeCommand Look state = case getCurrentRoom state of
    Just room -> (state, roomDescription room ++ lookAtItems (roomItems room) ++ lookAtNPCs (roomNPCs room))
    Nothing   -> (state, "You're in a void. There's nothing here.")
    where
        lookAtItems items = if null items
            then "\nYou see nothing of interest."
            else "\nYou see: " ++ intercalate ", " (map itemName items) ++ "."
        lookAtNPCs npcs = if null npcs
            then ""
            else "\nAlso here: " ++ intercalate ", " (map npcName npcs) ++ "."

executeCommand (LookAt target) state = case getCurrentRoom state of
    Just room -> case findMatchingItem target (roomItems room) of
        Just item -> (state, itemDescription item)
        Nothing   -> case findMatchingItem target (inventory state) of
            Just item -> (state, itemDescription item)
            Nothing   -> case findMatchingNPC target (roomNPCs room) of
                Just npc -> (state, npcDescription npc)
                Nothing -> (state, "You don't see that here.")
    Nothing -> (state, "There's nothing to look at.")
    where
        findMatchingItem target items = find ((target `elem`) . itemKeywords) items
        findMatchingNPC target npcs = find ((target `elem`) . npcKeywords) npcs

executeCommand (Take target) state = case getCurrentRoom state of
    Just room -> case findMatchingItem target (roomItems room) of
        Just item -> (pickupItem item (removeItemFromRoom (itemName item) state),
                     "You take the " ++ itemName item ++ ".")
        Nothing   -> (state, "You can't take that.")
    Nothing -> (state, "There's nothing to take.")
    where
        findMatchingItem target items = find ((target `elem`) . itemKeywords) items

executeCommand (Drop target) state = case findMatchingItem target (inventory state) of
    Just item -> (addItemToRoom item (dropItem item state),
                 "You drop the " ++ itemName item ++ ".")
    Nothing   -> (state, "You don't have that item.")
    where
        findMatchingItem target items = find ((target `elem`) . itemKeywords) items

executeCommand Inventory state = (state, "Inventory: " ++ inventoryList (inventory state))
    where
        inventoryList [] = "You're not carrying anything."
        inventoryList items = intercalate ", " (map itemName items)

executeCommand (UseOn itemTarget entityTarget) state = case getCurrentRoom state of
    Just room -> case findMatchingItem itemTarget (inventory state) of
        Just item -> 
            case Map.lookup (itemName item, entityTarget) (entityInteractions state) of
                Just (newStateVal, msg) -> 
                    let state' = setEntityState entityTarget newStateVal state
                    in (state', msg)
                Nothing -> (state, "You can't use the " ++ itemName item ++ " on the " ++ entityTarget ++ " like that.")
        Nothing -> (state, "You don't have that item.")
    Nothing -> (state, "There is nothing here.")
    where
        findMatchingItem target items = find ((target `elem`) . itemKeywords) items

executeCommand (Use target) state = case getCurrentRoom state of
    Just room -> case findMatchingItem target (inventory state) of
        Just item -> (state, "You need to specify what to use the " ++ itemName item ++ " on (e.g., 'use key on door').")
        Nothing   -> (state, "You don't have that item.")
    Nothing -> (state, "There's nothing to use.")
    where
        findMatchingItem target items = find ((target `elem`) . itemKeywords) items

executeCommand (Talk target) state = case getCurrentRoom state of
    Just room -> case findMatchingNPC target (roomNPCs room) of
        Just npc -> (state, npcName npc ++ " says: \"" ++ npcDialogue npc ++ "\"")
        Nothing -> (state, "You see no one by that name here.")
    Nothing   -> (state, "There's no one to talk to.")
    where
        findMatchingNPC target npcs = find ((target `elem`) . npcKeywords) npcs

executeCommand Help state = (state, "Available commands: go [direction], look, look at [item/npc], take [item], drop [item], inventory, use [item] on [entity], talk to [npc], help, quit")

executeCommand Quit state = (state { gameOver = True }, "Goodbye!")

executeCommand (Unknown cmd) state = (state, "I don't understand '" ++ cmd ++ "'. Type 'help' for available commands.")

executeCommand _ state = (state, "Command not implemented yet.")