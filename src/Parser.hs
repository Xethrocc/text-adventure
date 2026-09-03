-- | Command parsing and processing for the text adventure engine
module Parser where

import Types
import Game
import Data.Char (toLower)
import Data.List (find, intercalate, nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

-- | Parsed command structure
data Command 
    = Go Direction
    | Look
    | Inventory
    | Interact Verb String
    | InteractWith Verb String String
    | TakeAll
    | DropAll
    | CompoundCommand [Command]
    | Save String
    | Load String
    | ListSaves
    | Restart
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

-- | Stop words to strip from target phrases
stopWords :: [String]
stopWords = ["the", "a", "an", "some", "that", "this"]

-- | Strip stop words from a list of tokens (target portion only)
stripStopWords :: [String] -> [String]
stripStopWords = filter (`notElem` stopWords)

-- | Safely strip stop words — falls back to original if stripping empties the list
safeStripStopWords :: [String] -> [String]
safeStripStopWords tokens =
    let stripped = stripStopWords tokens
    in if null stripped then tokens else stripped

-- | Split a token list on the last occurrence of "and" for compound commands.
--   Uses last-occurrence so items like "mortar and pestle" are treated as a single entity
--   when followed by "and sword" → ("mortar and pestle", "sword").
splitOnLastAnd :: [String] -> Maybe ([String], [String])
splitOnLastAnd tokens =
    let indices = [i | (i, t) <- zip [0..] tokens, t == "and"]
    in case indices of
        [] -> Nothing
        _  -> let lastIdx = last indices
                  (before, rest) = splitAt lastIdx tokens
              in case rest of
                  ("and" : after) | not (null before) && not (null after) -> Just (before, after)
                  _ -> Nothing

-- | Parse user input into a command
parseCommand :: String -> Command
parseCommand input =
    let tokens = words (map toLower input)
    in case tokens of
        [] -> Unknown ""
        -- Check for compound commands with "and" (e.g., "take torch and key")
        _ | isCompoundCandidate tokens -> parseCompoundCommand tokens
          | otherwise -> parseSimpleCommand tokens input

-- | Check if this looks like a compound command (has "and" after a verb)
isCompoundCandidate :: [String] -> Bool
isCompoundCandidate (v : rest) = case parseVerb v of
    Just _ -> "and" `elem` rest
    Nothing -> case v of
        "pick" -> "and" `elem` rest
        _      -> False
isCompoundCandidate _ = False

-- | Parse a compound command by splitting on last "and"
parseCompoundCommand :: [String] -> Command
parseCompoundCommand tokens@(v : rest) =
    let targetParts = case rest of
            ("up" : parts) -> parts  -- "pick up X and Y"
            _              -> rest
    in case splitOnLastAnd targetParts of
        Just (_, after) ->
            let cmd1 = parseSimpleCommand (v : rest `takeWhileNotLast` "and") (unwords tokens)
                cmd2 = parseSimpleCommand (v : after) (unwords (v : after))
            in CompoundCommand [cmd1, cmd2]
        Nothing -> parseSimpleCommand tokens (unwords tokens)
  where
    -- Take all elements, replacing everything from the last "and" onward with what's before it
    takeWhileNotLast :: [String] -> String -> [String]
    takeWhileNotLast ts target =
        let indices = [i | (i, t) <- zip [0..] ts, t == target]
        in case indices of
            [] -> ts
            _  -> take (last indices) ts
parseCompoundCommand [] = Unknown ""

-- | Parse a single (non-compound) command
parseSimpleCommand :: [String] -> String -> Command
parseSimpleCommand tokens input = case tokens of
    []                     -> Unknown ""
    ["go", dir]            -> parseDirection dir input
    ["go", "to", dir]      -> parseDirection dir input
    ["move", dir]          -> parseDirection dir input
    ["walk", dir]          -> parseDirection dir input
    ["north"]              -> Go North
    ["south"]              -> Go South
    ["east"]               -> Go East
    ["west"]               -> Go West
    ["up"]                 -> Go Up
    ["down"]               -> Go Down
    ["look"]               -> Look
    ["inventory"]          -> Inventory
    ["inv"]                -> Inventory
    ["i"]                  -> Inventory
    ["help"]               -> Help
    ["quit"]               -> Quit
    ["exit"]               -> Quit
    ["q"]                  -> Quit
    ["restart"]            -> Restart
    ["saves"]              -> ListSaves
    ["list", "saves"]      -> ListSaves
    -- Save/Load
    "save" : []            -> Save "savegame"
    "save" : nameParts     -> Save (unwords nameParts)
    "load" : []            -> Load "savegame"
    "load" : nameParts     -> Load (unwords nameParts)
    -- Take all / Drop all
    [v, "all"] | v `elem` ["take", "get", "grab", "pick"] -> TakeAll
    ["pick", "up", "all"]  -> TakeAll
    [v, "all"] | v `elem` ["drop", "put"]                 -> DropAll
    ["put", "down", "all"] -> DropAll
    -- Complex parsing (supports multi-word targets with stop-word stripping)
    "look"  : "at"   : targetParts | not (null targetParts) -> Interact VLookAt (unwords (safeStripStopWords targetParts))
    "pick"  : "up"   : targetParts | not (null targetParts) -> Interact VTake (unwords (safeStripStopWords targetParts))
    "put"   : "down" : targetParts | not (null targetParts) -> Interact VDrop (unwords (safeStripStopWords targetParts))
    "talk"  : "to"   : targetParts | not (null targetParts) -> Interact VTalk (unwords (safeStripStopWords targetParts))
    "speak" : "with" : targetParts | not (null targetParts) -> Interact VTalk (unwords (safeStripStopWords targetParts))
    "use"   : useParts -> parseUse useParts input
    -- Generic verb-noun parsing with multi-word noun phrases + stop-word stripping
    v : targetParts | not (null targetParts) -> case parseVerb v of
        Just verb -> Interact verb (unwords (safeStripStopWords targetParts))
        Nothing   -> Unknown input
    _ -> Unknown input

parseDirection :: String -> String -> Command
parseDirection dir input = case dir of
    "north" -> Go North
    "south" -> Go South
    "east"  -> Go East
    "west"  -> Go West
    "up"    -> Go Up
    "down"  -> Go Down
    _       -> Unknown input

parseUse :: [String] -> String -> Command
parseUse [] input = Unknown input
parseUse useParts input =
    case break (`elem` ["on", "with"]) useParts of
        (itemParts, _ : entityParts)
            | not (null itemParts) && not (null entityParts) ->
                InteractWith VUseOn (unwords (safeStripStopWords itemParts)) (unwords (safeStripStopWords entityParts))
        (itemParts, []) | not (null itemParts) ->
            Interact VUse (unwords (safeStripStopWords itemParts))
        _ -> Unknown input

type CommandResult = (GameState, String)

normalizeText :: String -> String
normalizeText = map toLower

itemAliases :: ItemDef -> [String]
itemAliases item = nub $ map normalizeText (itemId item : itemName item : itemKeywords item)

npcAliases :: NPCDef -> [String]
npcAliases npc = nub $ map normalizeText (npcId npc : npcName npc : npcKeywords npc)

matchesItemTarget :: String -> ItemDef -> Bool
matchesItemTarget target item = normalizeText target `elem` itemAliases item

matchesNPCTarget :: String -> NPCDef -> Bool
matchesNPCTarget target npc = normalizeText target `elem` npcAliases npc

reachableExitEntities :: GameState -> [String]
reachableExitEntities state = case getCurrentRoom state of
    Just room -> [normalizeText entity | Locked _ entity <- Map.elems (roomConnections room)]
    Nothing   -> []

reachableEntityAliases :: GameState -> [String]
reachableEntityAliases state =
    let currentRoomId = currentRoom (save state)
        roomItems = getItemsInLocation currentRoomId state
        roomNPCs = getNPCsInRoom currentRoomId state
        exitEntities = reachableExitEntities state
        doorAliases = if null exitEntities then [] else ["door", "locked door"]
    in nub (concatMap itemAliases roomItems ++ concatMap npcAliases roomNPCs ++ exitEntities ++ doorAliases)

resolveEntityCandidates :: String -> GameState -> [String]
resolveEntityCandidates entityStr state =
    let target = normalizeText entityStr
        currentRoomId = currentRoom (save state)
        roomItems = getItemsInLocation currentRoomId state
        roomNPCs = getNPCsInRoom currentRoomId state
        exitEntities = reachableExitEntities state
        matchedItem = find (matchesItemTarget target) roomItems
        matchedNPC = find (matchesNPCTarget target) roomNPCs
        doorCandidates
            | target `elem` ["door", "locked door"] && not (null exitEntities) =
                exitEntities ++ ["door", "locked door"]
            | otherwise = []
    in nub $ doorCandidates ++ [target] ++ maybe [] itemAliases matchedItem ++ maybe [] npcAliases matchedNPC

-- | Process the actual action outcome from a verb map
applyOutcome :: ActionOutcome -> ItemID -> GameState -> CommandResult
applyOutcome (MessageOnly msg) _ state = (state, msg)
applyOutcome (ChangeItemState newState msg) targetId state = 
    let state' = state { save = (save state) { itemStates = Map.adjust (\s -> s { itemStatus = newState }) targetId (itemStates (save state)) } }
    in (state', msg)
applyOutcome (ChangeNPCState newState msg) targetId state = 
    let state' = state { save = (save state) { npcStates = Map.adjust (\s -> s { npcStatus = newState }) targetId (npcStates (save state)) } }
    in (state', msg)
applyOutcome (TransitionRoom newRoom msg) _ state = 
    (moveToRoom newRoom state, msg)
applyOutcome (HealPlayer amount msg) _ state = 
    (updatePlayerHealth (+ amount) state, msg)
applyOutcome (DamagePlayer amount msg) _ state = 
    let state' = updatePlayerHealth (subtract amount) state
    in (if isPlayerDead state' then endGame Death state' else state', msg)
applyOutcome (UpdateNPCHealth nId delta msg) _ state = 
    let currentNPC = Map.lookup nId (npcStates (save state))
    in case currentNPC of
        Just n -> 
            let oldHealth = fromMaybe 0 (npcHealth n)
                newHealth = oldHealth + delta
                state' = updateNPCState nId (n { npcHealth = Just newHealth }) state
                state'' = clampNPCHealth nId state'
            in (if newHealth <= 0 then killNPC nId state'' else state'', msg)
        Nothing -> (state, msg)
applyOutcome (ModifyItemProp iId prop delta msg) _ state =
    (modifyItemProp iId prop delta state, msg)
applyOutcome (ModifyNPCProp nId prop delta msg) _ state =
    (modifyNPCProp nId prop delta state, msg)
applyOutcome (SetEntityState entity newState msg) _ state =
    (setEntityState entity newState state, msg)
applyOutcome (MultipleOutcomes outcomes) targetId state =
    foldl (\(st, msgs) outcome -> 
        let (st', msg) = applyOutcome outcome targetId st
        in (st', if null msgs then msg else msgs ++ "\n" ++ msg)
    ) (state, "") outcomes
-- New outcome handlers
applyOutcome (GiveItem iId msg) _ state =
    (giveItem iId state, msg)
applyOutcome (MoveItem iId targetRoom msg) _ state =
    (moveItemToRoom iId targetRoom state, msg)
applyOutcome (ConsumeItem iId msg) _ state =
    (consumeItem iId state, msg)
applyOutcome (MoveNPC nId targetRoom msg) _ state =
    (moveNPCToRoom nId targetRoom state, msg)
applyOutcome (SetRoomVisited rId visited msg) _ state =
    (setRoomVisitedFlag rId visited state, msg)
applyOutcome (SetFlag flagName flagValue msg) _ state =
    (setFlag flagName flagValue state, msg)
applyOutcome (CheckFlag flagName expectedVal thenOutcome elseOutcome) targetId state =
    case getFlag flagName state of
        Just val | val == expectedVal -> applyOutcome thenOutcome targetId state
        _                            -> applyOutcome elseOutcome targetId state
applyOutcome (RandomChoice outcomes) targetId state =
    if null outcomes
    then (state, "")
    else let idx = gameRandomIndex state 0 (length outcomes)
         in applyOutcome (outcomes !! idx) targetId state
applyOutcome (GameEnd reason msg) _ state =
    (endGame reason state, msg)

-- | Execute a command and return updated game state and message
executeCommand :: Command -> GameState -> CommandResult

executeCommand (Go dir) state
    | canMove dir state = case getExitInDirection dir state of
        Just (Open destinationRoom) -> (moveToRoom destinationRoom state, "You move " ++ show dir ++ ".")
        Just (Locked destinationRoom entityTarget)
            | getEntityState entityTarget state == Just "unlocked" -> 
                (moveToRoom destinationRoom state, "You move " ++ show dir ++ " through the unlocked " ++ entityTarget ++ ".")
            | otherwise -> (state, "The door is locked.")
        Nothing -> (state, "There's nothing in that direction.")
    | otherwise = (state, "You can't go that way.")

executeCommand Look state = case getCurrentRoom state of
    Just room -> 
        let itemsInRoom = getItemsInLocation (currentRoom (save state)) state
            npcsInRoom = getNPCsInRoom (currentRoom (save state)) state
            itemDesc = if null itemsInRoom then "\nYou see nothing of interest." else "\nYou see: " ++ intercalate ", " (map itemName itemsInRoom) ++ "."
            npcDesc = if null npcsInRoom then "" else "\nAlso here: " ++ intercalate ", " (map npcName npcsInRoom) ++ "."
        in (state, roomDescription room ++ itemDesc ++ npcDesc)
    Nothing   -> (state, "You're in a void. There's nothing here.")

executeCommand Inventory state = 
    let invItems = getItemsInLocation "inventory" state
    in if null invItems 
       then (state, "You're not carrying anything.")
       else (state, "Inventory: " ++ intercalate ", " (map itemName invItems))

executeCommand TakeAll state =
    let roomItems = getItemsInLocation (currentRoom (save state)) state
    in if null roomItems
       then (state, "There's nothing here to take.")
       else let (finalState, msgs) = foldl (\(s, ms) item ->
                    let (s', m) = executeCommand (Interact VTake (itemName item)) s
                    in (s', ms ++ [m])) (state, []) roomItems
            in (finalState, intercalate "\n" msgs)

executeCommand DropAll state =
    let invItems = getItemsInLocation "inventory" state
    in if null invItems
       then (state, "You're not carrying anything to drop.")
       else let (finalState, msgs) = foldl (\(s, ms) item ->
                    let (s', m) = executeCommand (Interact VDrop (itemName item)) s
                    in (s', ms ++ [m])) (state, []) invItems
            in (finalState, intercalate "\n" msgs)

executeCommand (CompoundCommand cmds) state =
    foldl (\(s, msgs) cmd ->
        let (s', msg) = executeCommand cmd s
        in (s', if null msgs then msg else msgs ++ "\n" ++ msg)
    ) (state, "") cmds

executeCommand (Interact verb targetStr) state = 
    let 
        -- Find potential targets in room or inventory
        roomItems = getItemsInLocation (currentRoom (save state)) state
        invItems = getItemsInLocation "inventory" state
        allReachableItems = roomItems ++ invItems
        roomNPCs = getNPCsInRoom (currentRoom (save state)) state
        
        -- Try matching with stop-word-stripped target, fall back to original
        targetItem = find (matchesItemTarget targetStr) allReachableItems
        targetNPC = find (matchesNPCTarget targetStr) roomNPCs
    in case (targetItem, targetNPC) of
        (Just item, _) -> 
            -- Found an item target, look up its state and check verb map
            let iId = itemId item
                maybeItemState = Map.lookup iId (itemStates (save state))
                currentStatus = maybe "unknown" itemStatus maybeItemState
            in case Map.lookup (verb, currentStatus) (itemVerbMap item) of
                Just outcome -> applyOutcome outcome iId state
                Nothing -> 
                    -- Fallback hardcoded logic for basic verbs if missing from map (for backwards compatibility/ease)
                    if verb == VTake && maybe False ((/= "inventory") . itemLocation) maybeItemState
                    then (pickupItem iId state, "You take the " ++ itemName item ++ ".")
                    else if verb == VDrop && hasItem iId state
                    then (dropItem iId state, "You drop the " ++ itemName item ++ ".")
                    else (state, "You can't do that to the " ++ itemName item ++ " right now.")
        
        (Nothing, Just npc) -> 
            -- Found an NPC target
            let nId = npcId npc
                maybeNpcState = Map.lookup nId (npcStates (save state))
                currentStatus = maybe "unknown" npcStatus maybeNpcState
            in case Map.lookup (verb, currentStatus) (npcVerbMap npc) of
                Just outcome -> applyOutcome outcome nId state
                Nothing -> 
                    -- Fallback logic for talk/attack
                    if verb == VTalk 
                    then case Map.lookup currentStatus (npcDialogue npc) of
                            Just speech -> (state, npcName npc ++ " says: \"" ++ speech ++ "\"")
                            Nothing -> (state, npcName npc ++ " has nothing to say.")
                    else if verb == VAttack 
                    then executeAttack npc maybeNpcState targetStr state
                    else (state, "You can't do that to " ++ npcName npc ++ ".")
        
        (Nothing, Nothing) -> (state, "You don't see '" ++ targetStr ++ "' here.")

-- | Handle "use <item> on <entity>" with weapon→attack fallback
executeCommand (InteractWith VUseOn itemStr entityStr) state =
    let itemTarget = normalizeText itemStr
        entityTarget = normalizeText entityStr
        inventoryItems = getItemsInLocation "inventory" state
        maybeItem = find (matchesItemTarget itemTarget) inventoryItems
    in case maybeItem of
        Nothing -> (state, "You need to be carrying '" ++ itemStr ++ "' to use it.")
        Just item ->
            -- First check entity interactions
            if entityTarget `elem` reachableEntityAliases state
            then
                let itemKeys = nub (itemTarget : itemAliases item)
                    entityKeys = resolveEntityCandidates entityTarget state
                    interactions = entityInteractions (world state)
                    interactionKey = find (`Map.member` interactions) [(iKey, eKey) | iKey <- itemKeys, eKey <- entityKeys]
                in case interactionKey >>= (`Map.lookup` interactions) of
                    Just (newState, msg) ->
                        let resolvedEntity = maybe entityTarget snd interactionKey
                            state' = setEntityState resolvedEntity newState state
                        in (state', msg)
                    -- No entity interaction found — check if target is a living NPC → route to attack
                    Nothing
                        | isLivingNPCInRoom entityTarget state ->
                            executeCommand (Interact VAttack entityTarget) state
                        | otherwise -> (state, "Nothing happens.")
            else
                -- Target not reachable as entity — check if it's a living NPC → route to attack
                if isLivingNPCInRoom entityTarget state
                then executeCommand (Interact VAttack entityTarget) state
                else (state, "You can't reach '" ++ entityStr ++ "' from here.")

executeCommand (InteractWith _ _ _) state = (state, "Nothing happens.")

executeCommand Restart _ = (emptyGameState, "")  -- Handled specially in GameLoop
executeCommand ListSaves state = (state, "")       -- Handled specially in GameLoop

executeCommand Help state = (state, helpText)
executeCommand Quit state = (endGame (Custom "quit") state, "Goodbye!")
executeCommand (Unknown cmd) state = (state, "I don't understand '" ++ cmd ++ "'. Type 'help' for available commands.")
executeCommand (Save _) state = (state, "")  -- Handled in GameLoop
executeCommand (Load _) state = (state, "")  -- Handled in GameLoop

-- | Combat logic extracted for reuse and correctness
executeAttack :: NPCDef -> Maybe NPCState -> String -> GameState -> CommandResult
executeAttack npc maybeNpcState targetStr state =
    case maybeNpcState >>= npcHealth of
        Nothing -> (state, "You can't attack the " ++ npcName npc ++ ".")
        Just hp -> 
            let nId = npcId npc
                p = player (save state)
                -- Fixed: use npcDefenseBase for player's damage reduction against NPC
                playerDmg = max 1 (playerAttack p - npcDefenseBase npc)
                newHp = hp - playerDmg
            in if newHp <= 0 
               then (killNPC nId state, "You attack the " ++ targetStr ++ " and kill it!")
               else
                   -- Fixed: use npcAttackBase for NPC's attack, reduced by player's defense
                   let npcDmg = max 0 (npcAttackBase npc - playerDefense p)
                       state' = case maybeNpcState of
                           Just npcState -> updateNPCState nId (npcState { npcHealth = Just newHp }) state
                           Nothing       -> state
                       state'' = updatePlayerHealth (\h -> h - npcDmg) state'
                   in if isPlayerDead state''
                      then (endGame Death state'', "The " ++ targetStr ++ " strikes back and kills you!")
                      else (state'', "You hit for " ++ show playerDmg ++ ", it hits you for " ++ show npcDmg ++ ".")

-- | Formatted help text
helpText :: String
helpText = intercalate "\n"
    [ "=== Available Commands ==="
    , ""
    , "Movement:"
    , "  go/move/walk <direction>   - Move north/south/east/west/up/down"
    , "  <direction>                - Shorthand (e.g., just 'north')"
    , ""
    , "Interaction:"
    , "  look                       - Examine current room"
    , "  look at / examine <target> - Examine an item or NPC"
    , "  take / get / grab <item>   - Pick up an item"
    , "  take all                   - Pick up all items in the room"
    , "  drop <item>                - Drop an item"
    , "  drop all                   - Drop everything you're carrying"
    , "  use <item>                 - Use an item from inventory"
    , "  use <item> on <target>     - Use an item on something"
    , "  talk to / speak with <npc> - Talk to a character"
    , "  attack / hit <npc>         - Attack an enemy"
    , ""
    , "Multi-item:"
    , "  take <item> and <item>     - Take multiple items"
    , ""
    , "System:"
    , "  inventory / inv / i        - Check what you're carrying"
    , "  save [name]                - Save game (default: savegame)"
    , "  load [name]                - Load a saved game"
    , "  saves                      - List all saved games"
    , "  restart                    - Start a new game"
    , "  help                       - Show this help"
    , "  quit / exit / q            - Exit the game"
    , ""
    , "Tip: Press Tab to auto-complete commands and targets."
    ]
