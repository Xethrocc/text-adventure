-- | Command parsing and processing for the text adventure engine
module Parser where

import Types
import Game
import Data.Char (toLower)
import Data.List (find, intercalate)
import qualified Data.Map.Strict as Map

-- | Parsed command structure
data Command 
    = Go Direction
    | Look
    | Inventory
    | Interact Verb String
    | InteractWith Verb String String
    | Help
    | Quit
    | Unknown String
    deriving (Show, Eq)

-- | Map common strings to Verbs
parseVerb :: String -> Maybe Verb
parseVerb v = case v of
    "take"    -> Just VTake
    "pick"    -> Just VTake
    "grab"    -> Just VTake
    "get"     -> Just VTake
    "drop"    -> Just VDrop
    "put"     -> Just VDrop
    "examine" -> Just VLookAt
    "inspect" -> Just VLookAt
    "look"    -> Just VLookAt
    "read"    -> Just VLookAt
    "use"     -> Just VUse
    "talk"    -> Just VTalk
    "speak"   -> Just VTalk
    "chat"    -> Just VTalk
    "attack"  -> Just VAttack
    "hit"     -> Just VAttack
    "kill"    -> Just VAttack
    _         -> Nothing

-- | Parse user input into a command
parseCommand :: String -> Command
parseCommand input = case words (map toLower input) of
    []                     -> Unknown ""
    ["go", dir]            -> parseDirection dir
    ["go", "to", dir]      -> parseDirection dir
    ["move", dir]          -> parseDirection dir
    ["walk", dir]          -> parseDirection dir
    ["north"]              -> Go North
    ["south"]              -> Go South
    ["east"]               -> Go East
    ["west"]               -> Go West
    ["up"]                 -> Go Up
    ["down"]               -> Go Down
    ["look"]               -> Look
    ["inventory"]           -> Inventory
    ["inv"]                 -> Inventory
    ["i"]                   -> Inventory
    ["help"]               -> Help
    ["quit"]               -> Quit
    ["exit"]               -> Quit
    ["q"]                  -> Quit
    -- Complex parsing
    ["look", "at", target] -> Interact VLookAt target
    ["pick", "up", target] -> Interact VTake target
    ["put", "down", target] -> Interact VDrop target
    ["talk", "to", target]  -> Interact VTalk target
    ["speak", "with", target] -> Interact VTalk target
    ["use", item, "on", entity]   -> InteractWith VUseOn item entity
    ["use", item, "with", entity] -> InteractWith VUseOn item entity
    -- Generic verb-noun parsing
    [v, target] -> case parseVerb v of
        Just verb -> Interact verb target
        Nothing   -> Unknown input
    _ -> Unknown input
    where
        parseDirection dir = case dir of
            "north" -> Go North
            "south" -> Go South
            "east"  -> Go East
            "west"  -> Go West
            "up"    -> Go Up
            "down"  -> Go Down
            _       -> Unknown dir

type CommandResult = (GameState, String)

-- | Process the actual action outcome from a verb map
applyOutcome :: ActionOutcome -> ItemID -> GameState -> CommandResult
applyOutcome (MessageOnly msg) _ state = (state, msg)
applyOutcome (ChangeItemState newState msg) targetId state = 
    let state' = state { itemStates = Map.adjust (\s -> s { itemStatus = newState }) targetId (itemStates state) }
    in (state', msg)
applyOutcome (ChangeNPCState newState msg) targetId state = 
    let state' = state { npcStates = Map.adjust (\s -> s { npcStatus = newState }) targetId (npcStates state) }
    in (state', msg)
applyOutcome (TransitionRoom newRoom msg) _ state = 
    (moveToRoom newRoom state, msg)

-- | Execute a command and return updated game state and message
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
    Just room -> 
        let itemsInRoom = getItemsInLocation (currentRoom state) state
            npcsInRoom = getNPCsInRoom (currentRoom state) state
            itemDesc = if null itemsInRoom then "\nYou see nothing of interest." else "\nYou see: " ++ intercalate ", " (map itemName itemsInRoom) ++ "."
            npcDesc = if null npcsInRoom then "" else "\nAlso here: " ++ intercalate ", " (map npcName npcsInRoom) ++ "."
        in (state, roomDescription room ++ itemDesc ++ npcDesc)
    Nothing   -> (state, "You're in a void. There's nothing here.")

executeCommand Inventory state = 
    let invItems = getItemsInLocation "inventory" state
    in if null invItems 
       then (state, "You're not carrying anything.")
       else (state, "Inventory: " ++ intercalate ", " (map itemName invItems))

executeCommand (Interact verb targetStr) state = 
    let 
        -- Find potential targets in room or inventory
        roomItems = getItemsInLocation (currentRoom state) state
        invItems = getItemsInLocation "inventory" state
        allReachableItems = roomItems ++ invItems
        roomNPCs = getNPCsInRoom (currentRoom state) state
        
        targetItem = find (\i -> targetStr `elem` itemKeywords i) allReachableItems
        targetNPC = find (\n -> targetStr `elem` npcKeywords n) roomNPCs
    in case (targetItem, targetNPC) of
        (Just item, _) -> 
            -- Found an item target, look up its state and check verb map
            let iId = itemId item
                currentStatus = maybe "unknown" itemStatus (Map.lookup iId (itemStates state))
            in case Map.lookup (verb, currentStatus) (itemVerbMap item) of
                Just outcome -> applyOutcome outcome iId state
                Nothing -> 
                    -- Fallback hardcoded logic for basic verbs if missing from map (for backwards compatibility/ease)
                    if verb == VTake && itemLocation (itemStates state Map.! iId) /= "inventory"
                    then (pickupItem iId state, "You take the " ++ itemName item ++ ".")
                    else if verb == VDrop && hasItem iId state
                    then (dropItem iId state, "You drop the " ++ itemName item ++ ".")
                    else (state, "You can't do that to the " ++ itemName item ++ " right now.")
        
        (Nothing, Just npc) -> 
            -- Found an NPC target
            let nId = npcId npc
                currentStatus = maybe "unknown" npcStatus (Map.lookup nId (npcStates state))
            in case Map.lookup (verb, currentStatus) (npcVerbMap npc) of
                Just outcome -> applyOutcome outcome nId state
                Nothing -> 
                    -- Fallback logic for talk/attack
                    if verb == VTalk 
                    then case Map.lookup currentStatus (npcDialogue npc) of
                            Just speech -> (state, npcName npc ++ " says: \"" ++ speech ++ "\"")
                            Nothing -> (state, npcName npc ++ " has nothing to say.")
                    else if verb == VAttack 
                    then case npcHealth (npcStates state Map.! nId) of
                            Nothing -> (state, "You can't attack the " ++ npcName npc ++ ".")
                            Just hp -> 
                                let 
                                    p = player state
                                    playerDmg = max 1 (playerAttack p - npcAttackBase npc) -- Simplified
                                    newHp = hp - playerDmg
                                in if newHp <= 0 
                                   then (killNPC nId state, "You attack the " ++ targetStr ++ " and kill it!")
                                   else 
                                        let npcDmg = max 0 (npcAttackBase npc - playerDefense p)
                                            state' = updateNPCState nId ((npcStates state Map.! nId) { npcHealth = Just newHp }) state
                                            state'' = updatePlayerHealth (\h -> h - npcDmg) state'
                                        in if isPlayerDead state''
                                           then (state'' { gameOver = True }, "You die!")
                                           else (state'', "You hit for " ++ show playerDmg ++ ", it hits you for " ++ show npcDmg)
                    else (state, "You can't do that to " ++ npcName npc ++ ".")
        
        (Nothing, Nothing) -> (state, "You don't see '" ++ targetStr ++ "' here.")


executeCommand (InteractWith verb itemStr entityStr) state = 
    -- Left mostly as a stub since UseOn was specialized before; ideally handled via verb map too
    (state, "Complex interactions not fully mapped yet.")

executeCommand Help state = (state, "Available commands: go [direction], look, look at [item/npc], take [item], drop [item], inventory, talk to [npc], attack [npc], help, quit")
executeCommand Quit state = (state { gameOver = True }, "Goodbye!")
executeCommand (Unknown cmd) state = (state, "I don't understand '" ++ cmd ++ "'. Type 'help' for available commands.")
executeCommand _ state = (state, "Command not implemented yet.")