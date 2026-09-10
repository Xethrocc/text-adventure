{-# LANGUAGE TupleSections #-}

-- | Command parsing and processing for the text adventure engine
module Parser where

import Types
import Game
import Control.Applicative ((<|>))
import Data.Char (toLower, isDigit)
import Data.List (find, intercalate, nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set

-- | Parsed command structure
data Command
    = Go Direction
    | Look
    | Inventory
    | Interact Verb String
    | InteractWith Verb String String
    | ChooseCmd Int              -- ^ select a dialogue option (Phase 4.6)
    | TakeAll
    | DropAll
    | CompoundCommand [Command]
    | EquipCmd String
    | UnequipCmd String
    | UnequipAllCmd
    | StatsCmd
    | SearchCmd (Maybe String)   -- ^ `search` or `search <target>`
    | JournalCmd                 -- ^ show active/completed quests
    | Undo                       -- ^ restore the previous game state
    | EnterVehicleCmd String     -- ^ enter a vehicle
    | ExitVehicleCmd             -- ^ exit the current vehicle
    | DriveToCmd String          -- ^ drive the current vehicle to a station
    | WaitCmd                    -- ^ advance an AutomaticRoute vehicle
    | RefuelCmd String           -- ^ refuel a vehicle (fuel item used via interactions)
    | RepairCmd String           -- ^ repair a vehicle condition
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
    "search"  -> Just VSearch
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
    ["stats"]              -> StatsCmd
    ["journal"]            -> JournalCmd
    ["quests"]             -> JournalCmd
    ["undo"]               -> Undo
    -- Dialogue choice (Phase 4.6)
    ["choose", nStr] | all isDigit nStr && not (null nStr) -> ChooseCmd (read nStr)
    ["pick", nStr]   | all isDigit nStr && not (null nStr) -> ChooseCmd (read nStr)
    ["option", nStr] | all isDigit nStr && not (null nStr) -> ChooseCmd (read nStr)
    ["select", nStr] | all isDigit nStr && not (null nStr) -> ChooseCmd (read nStr)
    [nStr]           | all isDigit nStr && not (null nStr) -> ChooseCmd (read nStr)
    -- Vehicles (Phase 3)
    "enter" : targetParts | not (null targetParts) -> EnterVehicleCmd (unwords (safeStripStopWords targetParts))
    "board" : targetParts | not (null targetParts) -> EnterVehicleCmd (unwords (safeStripStopWords targetParts))
    ["disembark"]          -> ExitVehicleCmd
    "drive" : ("to" : targetParts) | not (null targetParts) ->
        DriveToCmd (unwords (safeStripStopWords targetParts))
    ["drive"]              -> DriveToCmd ""
    ["wait"]               -> WaitCmd
    "refuel" : targetParts -> RefuelCmd (unwords targetParts)
    "repair" : targetParts | not (null targetParts) -> RepairCmd (unwords (safeStripStopWords targetParts))
    ["search"]             -> SearchCmd Nothing
    "search" : targetParts | not (null targetParts) ->
        let t = unwords (safeStripStopWords targetParts)
        in if t `elem` ["room", "area", "here", "around"]
           then SearchCmd Nothing
           else SearchCmd (Just t)
    ["help"]               -> Help
    ["quit"]               -> Quit
    ["exit"]               -> Quit
    ["q"]                  -> Quit
    ["restart"]            -> Restart
    ["saves"]              -> ListSaves
    ["list", "saves"]      -> ListSaves
    ["unequip", "all"]     -> UnequipAllCmd
    ["unequip"]            -> UnequipAllCmd
    "save" : []            -> Save "savegame"
    "save" : nameParts     -> Save (unwords nameParts)
    "load" : []            -> Load "savegame"
    "load" : nameParts     -> Load (unwords nameParts)
    [v, "all"] | v `elem` ["take", "get", "grab", "pick"] -> TakeAll
    ["pick", "up", "all"]  -> TakeAll
    [v, "all"] | v `elem` ["drop", "put"]                 -> DropAll
    ["put", "down", "all"] -> DropAll
    -- Equipment
    "equip" : targetParts | not (null targetParts)   -> EquipCmd (unwords (safeStripStopWords targetParts))
    "wear"  : targetParts | not (null targetParts)   -> EquipCmd (unwords (safeStripStopWords targetParts))
    "wield" : targetParts | not (null targetParts)   -> EquipCmd (unwords (safeStripStopWords targetParts))
    "unequip" : targetParts | not (null targetParts) -> UnequipCmd (unwords (safeStripStopWords targetParts))
    "remove"  : targetParts | not (null targetParts) -> UnequipCmd (unwords (safeStripStopWords targetParts))
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

-- ---------------------------------------------------------------------------
-- Outcome application
-- ---------------------------------------------------------------------------

-- | Maximum nesting depth for outcomes. Prevents runaway recursion from
--   malformed content (e.g. a CheckFlag whose branch loops back).
maxOutcomeDepth :: Int
maxOutcomeDepth = 20

-- | Outcome application with a threaded RNG salt and recursion depth.
--   Returns (state, message, nextSalt, nextDepth).
applyOutcomeWith :: Int -> Int -> ActionOutcome -> ItemID -> GameState -> (GameState, String, Int)
applyOutcomeWith depth salt outcome targetId state
    | depth > maxOutcomeDepth = (state, "[ERROR] Maximum outcome depth exceeded.", salt)
    | otherwise = case outcome of
    MessageOnly msg -> (state, msg, salt)

    ChangeItemState newState msg ->
        let state' = state { save = (save state) { itemStates = Map.adjust (\s -> s { itemStatus = newState }) targetId (itemStates (save state)) } }
        in (state', msg, salt)

    ChangeNPCState newState msg ->
        let state' = state { save = (save state) { npcStates = Map.adjust (\s -> s { npcStatus = newState }) targetId (npcStates (save state)) } }
        in (state', msg, salt)

    TransitionRoom newRoom msg -> (moveToRoom newRoom state, msg, salt)

    HealPlayer amount msg -> (updatePlayerHealth (+ amount) state, msg, salt)

    DamagePlayer amount msg ->
        let state' = updatePlayerHealth (subtract amount) state
        in (if isPlayerDead state' then endGame Death state' else state', msg, salt)

    UpdateNPCHealth nId delta msg ->
        case Map.lookup nId (npcStates (save state)) of
            Nothing -> (state, msg, salt)
            Just n ->
                let oldHealth = fromMaybe 0 (npcHealth n)
                    newHealth = oldHealth + delta
                    state' = updateNPCState nId (n { npcHealth = Just newHealth }) state
                    state'' = clampNPCHealth nId state'
                in (if newHealth <= 0 then killNPC nId state'' else state'', msg, salt)

    ModifyItemProp iId prop delta msg -> (modifyItemProp iId prop delta state, msg, salt)

    ModifyNPCProp nId prop delta msg -> (modifyNPCProp nId prop delta state, msg, salt)

    SetEntityState entity newState msg -> (setEntityState entity newState state, msg, salt)

    -- Salt is threaded through children so consecutive RandomChoices differ
    MultipleOutcomes outcomes ->
        let (st', msg', salt') = foldl (\(st, acc, s) o ->
                let (st2, m2, s2) = applyOutcomeWith (depth + 1) s o targetId st
                in (st2, if null acc then m2 else acc ++ "\n" ++ m2, s2))
                (state, "", salt) outcomes
        in (st', msg', salt')

    GiveItem iId msg -> (giveItem iId state, msg, salt)

    MoveItem iId targetRoom msg -> (moveItemToRoom iId targetRoom state, msg, salt)

    ConsumeItem iId msg -> (consumeItem iId state, msg, salt)

    MoveNPC nId targetRoom msg -> (moveNPCToRoom nId targetRoom state, msg, salt)

    SetRoomVisited rId visited msg -> (setRoomVisited rId visited state, msg, salt)

    SetFlag flagName flagValue msg -> (setFlag flagName flagValue state, msg, salt)

    CheckFlag flagName expectedVal thenOutcome elseOutcome ->
        case getFlag flagName state of
            Just val | val == expectedVal -> applyOutcomeWith (depth + 1) salt thenOutcome targetId state
            _                            -> applyOutcomeWith (depth + 1) salt elseOutcome targetId state

    RandomChoice outcomes ->
        if null outcomes
        then (state, "", salt)
        else let idx = gameRandomIndex state salt (length outcomes)
             in applyOutcomeWith (depth + 1) (salt + 1) (outcomes !! idx) targetId state

    GameEnd reason msg -> (endGame reason state, msg, salt)

    EquipItem iId msg ->
        case equipItem iId state of
            Left err    -> (state, err, salt)
            Right state' -> (state', msg, salt)

    UnequipItem iId msg ->
        if isEquipped iId state
        then (unequipItem iId state, msg, salt)
        else (state, "You don't have that equipped.", salt)

    -- Skills (Phase 2): skill + d(salt-derived) vs DC
    CheckSkill skillId dc passOutcome failOutcome ->
        let skillVal = getSkill skillId state
            roll = 1 + (gameRandom state salt `mod` 6)   -- d6
            total = skillVal + roll
        in if total >= dc
           then let (st', m, s') = applyOutcomeWith (depth + 1) (salt + 1) passOutcome targetId state
                in (st', "[" ++ skillId ++ " " ++ show skillVal ++ "+" ++ show roll ++ " vs " ++ show dc ++ "] " ++ m, s')
           else let (st', m, s') = applyOutcomeWith (depth + 1) (salt + 1) failOutcome targetId state
                in (st', "[" ++ skillId ++ " " ++ show skillVal ++ "+" ++ show roll ++ " vs " ++ show dc ++ "] " ++ m, s')

    ModifySkill skillId delta msg -> (modifySkill skillId delta state, msg, salt)

    -- Conditions (Phase 2)
    ApplyCondition name turns tick end -> (applyCondition name turns tick end state, "", salt)
    ClearCondition name msg -> (clearCondition name state, msg, salt)
    HasCondition name thenOutcome elseOutcome ->
        if hasCondition name state
        then applyOutcomeWith (depth + 1) salt thenOutcome targetId state
        else applyOutcomeWith (depth + 1) salt elseOutcome targetId state

    -- Quests (Phase 2)
    StartQuest qId msg ->
        if canStartQuest qId state
        then (startQuest qId state, msg, salt)
        else (state, "You cannot start that quest right now.", salt)
    AdvanceQuest qId msg ->
        if Map.member qId (activeQuests (save state))
        then (advanceQuest qId state, msg, salt)
        else (state, "That quest is not active.", salt)
    CompleteQuest qId msg ->
        if Map.member qId (activeQuests (save state))
        then let (st', rewardMsg) = completeQuestWithMsg qId state
                 fullMsg = intercalate "\n" (filter (not . null) [msg, rewardMsg])
             in (st', fullMsg, salt)
        else (state, "That quest is not active.", salt)

    -- Narratives (Phase 4.4): store lines + follow-up for interactive rendering
    Narrative lines followUp ->
        (state { pendingNarrative = Just (lines, followUp) }, intercalate "\n" lines, salt)

-- | Public wrapper: apply a single outcome starting at depth 0 / salt 0
applyOutcome :: ActionOutcome -> ItemID -> GameState -> CommandResult
applyOutcome outcome targetId state =
    let (st, msg, _) = applyOutcomeWith 0 0 outcome targetId state
    in (st, msg)

-- | Apply zero or more outcomes in sequence
applyOutcomes :: [ActionOutcome] -> ItemID -> GameState -> CommandResult
applyOutcomes outcomes targetId state =
    let (st, msg, _) = foldl (\(s, acc, slt) o ->
            let (s2, m2, slt2) = applyOutcomeWith 0 slt o targetId s
            in (s2, if null acc then m2 else acc ++ "\n" ++ m2, slt2))
            (state, "", 0) outcomes
    in (st, msg)

-- ---------------------------------------------------------------------------
-- Command execution
-- ---------------------------------------------------------------------------

-- | Execute a command and return updated game state and message
executeCommand :: Command -> GameState -> CommandResult

executeCommand (Go dir) state
    | canMove dir state = case getExitInDirection dir state of
        Just (Open destinationRoom) -> goTo destinationRoom (clearActiveDialogue state)
        Just (Locked destinationRoom entityTarget)
            | getEntityState entityTarget state == Just "unlocked" ->
                goTo destinationRoom (clearActiveDialogue state)
            | otherwise -> (state, "The door is locked.")
        Nothing -> (state, "There's nothing in that direction.")
    | otherwise = (state, "You can't go that way.")
  where
    goTo dest st =
        let (stAfterExit, exitMsg) = runRoomHook roomOnExit (currentRoom (save st)) st
            moved = moveToRoom dest stAfterExit
            visited = markCurrentRoomVisited moved
            (finalState, enterMsg) = runRoomHook roomOnEnter dest visited
            fullMsg = intercalate "\n" (filter (not . null) [exitMsg, "You move " ++ show dir ++ ".", enterMsg])
        in (finalState, fullMsg)

executeCommand Look state = case getCurrentRoom state of
    Nothing -> (state, "You're in a void. There's nothing here.")
    Just room
        | isDark room state ->
            (state, "It's pitch black. You can't see anything.")
        | otherwise ->
            let vIdOverride = case currentVehicle (save state) of
                    Just vId -> Map.lookup (currentRoom (save state))
                                (vsRoomOverrides (getVehicleState vId state))
                    Nothing  -> Nothing
                baseRoom = getCurrentRoom state
                desc = case (vIdOverride, baseRoom) of
                    (Just override, Just _) -> override
                    _ -> maybe "" (\r -> resolveDescription r state) baseRoom
                itemsInRoom = getItemsInLocation (currentRoom (save state)) state
                npcsInRoom = getNPCsInRoom (currentRoom (save state)) state
                itemDesc = if null itemsInRoom
                           then "\nYou see nothing of interest."
                           else "\nYou see: " ++ intercalate ", " (map itemName itemsInRoom) ++ "."
                npcDesc = if null npcsInRoom
                          then ""
                          else "\nAlso here: " ++ intercalate ", " (map npcName npcsInRoom) ++ "."
                (state', hookMsg) = runRoomHook roomOnLook (currentRoom (save state)) state
                vehicleMsg = vehicleLookAddon state'
                asciiArt = case roomAscii room of
                    Just art | not (null art) -> [art]
                    _                         -> []
                full = intercalate "\n" (asciiArt ++ filter (not . null)
                        [desc, itemDesc, npcDesc, hookMsg, fromMaybe "" vehicleMsg])
            in (state', full)

executeCommand Inventory state =
    let invItems = getItemsInLocation "inventory" state
    in if null invItems
       then (state, "You're not carrying anything.")
       else (state, "Inventory: " ++ intercalate ", " (map itemName invItems))

executeCommand StatsCmd state =
    let p = player (save state)
        condList = Map.elems (conditions (save state))
        skillList = Map.toList (playerSkills p)
        skillDesc = if null skillList
                    then ""
                    else "Skills: " ++ intercalate ", " [n ++ " " ++ show v | (n, v) <- skillList] ++ "\n"
        condDesc = if null condList
                   then ""
                   else "Conditions: " ++ intercalate ", "
                        [ condName c ++ " (" ++ show (condRemaining c) ++ " turns)"
                        | c <- condList ] ++ "\n"
        msg = unlines
            [ "Health:  " ++ show (playerHealth p) ++ " / " ++ show (effectiveMaxHealth state)
            , "Attack:  " ++ show (effectiveAttack state) ++ " (base " ++ show (playerAttack p) ++ ")"
            , "Defense: " ++ show (effectiveDefense state) ++ " (base " ++ show (playerDefense p) ++ ")"
            , skillDesc ++ condDesc ++ equipmentSummary state
            ]
    in (state, msg)

executeCommand JournalCmd state = (state, journalText state)
executeCommand Undo state = (state, "Nothing to undo.")

executeCommand (ChooseCmd idx) state =
    case activeDialogue (save state) of
        Nothing -> (state, "You are not in a conversation right now.")
        Just nId -> case Map.lookup nId (npcDefs (world state)) of
            Nothing -> (clearActiveDialogue state, "The person you were talking to is gone.")
            Just npc ->
                let st = Map.lookup nId (npcStates (save state))
                    status = maybe "alive" npcStatus st
                in case Map.lookup status (npcDialogueTrees npc) of
                    Nothing -> (clearActiveDialogue state, npcName npc ++ " has nothing more to say.")
                    Just tree ->
                        let nodeId = fromMaybe (dtEntry tree) (st >>= npcDialogueNode)
                        in case Map.lookup nodeId (dtNodes tree) of
                            Nothing -> (clearActiveDialogue state, npcName npc ++ " has nothing more to say.")
                            Just node ->
                                let choices = dnChoices node
                                in if idx < 1 || idx > length choices
                                   then (state, "Invalid choice. Please select a number from 1 to " ++ show (length choices) ++ ".")
                                   else
                                       let choice = choices !! (idx - 1)
                                           outcome = dcOutcome choice
                                           (stateAfterOutcome, outcomeMsg) = applyOutcome outcome nId state
                                       in case dcNextNode choice of
                                           Nothing ->
                                               -- Dialogue ends
                                               let stateFinal = clearActiveDialogue (setDialogueNode nId Nothing stateAfterOutcome)
                                                   msg = if null outcomeMsg
                                                         then "Dialogue ended."
                                                         else outcomeMsg
                                               in (stateFinal, msg)
                                           Just nextNodeId ->
                                               let stateWithNext = setDialogueNode nId (Just nextNodeId) stateAfterOutcome
                                                   st' = Map.lookup nId (npcStates (save stateWithNext))
                                                   (stateFinal, nextDialogue) = renderDialogue npc tree st' stateWithNext
                                                   fullMsg = if null outcomeMsg
                                                             then nextDialogue
                                                             else outcomeMsg ++ "\n\n" ++ nextDialogue
                                               in (stateFinal, fullMsg)

executeCommand (EquipCmd targetStr) state =
    case findMatchingItem targetStr state of
        Nothing -> (state, "You don't have '" ++ targetStr ++ "'.")
        Just item ->
            case equipItem (itemId item) state of
                Left err -> (state, err)
                Right state' -> (state', "You equip the " ++ itemName item ++ ".")

executeCommand (UnequipCmd targetStr) state =
    case findMatchingItem targetStr state of
        Nothing -> (state, "You don't have '" ++ targetStr ++ "'.")
        Just item
            | isEquipped (itemId item) state -> (unequipItem (itemId item) state, "You unequip the " ++ itemName item ++ ".")
            | otherwise -> (state, "The " ++ itemName item ++ " is not equipped.")

executeCommand UnequipAllCmd state
    | Map.null (equipment (save state)) = (state, "You have nothing equipped.")
    | otherwise = (state { save = (save state) { equipment = Map.empty } }, "You remove all equipment.")

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

executeCommand (SearchCmd maybeTarget) state =
    case maybeTarget of
        Nothing -> searchRoom state
        Just targetStr ->
            -- `search <thing>`: try a matching item/NPC's VSearch verb first
            let roomItems = getItemsInLocation (currentRoom (save state)) state
                roomNPCs = getNPCsInRoom (currentRoom (save state)) state
            in case find (matchesItemTarget targetStr) roomItems of
                Just item ->
                    let iId = itemId item
                        currentStatus = maybe "unknown" itemStatus (Map.lookup iId (itemStates (save state)))
                    in case Map.lookup (VSearch, currentStatus) (itemVerbMap item) of
                        Just outcome -> applyOutcome outcome iId state
                        Nothing -> (state, "You find nothing special about the " ++ itemName item ++ ".")
                Nothing -> case find (matchesNPCTarget targetStr) roomNPCs of
                    Just npc ->
                        let nId = npcId npc
                            currentStatus = maybe "unknown" npcStatus (Map.lookup nId (npcStates (save state)))
                        in case Map.lookup (VSearch, currentStatus) (npcVerbMap npc) of
                            Just outcome -> applyOutcome outcome nId state
                            Nothing -> (state, "You find nothing on " ++ npcName npc ++ ".")
                    Nothing -> (state, "You don't see '" ++ targetStr ++ "' here.")

executeCommand (Interact verb targetStr) state =
    let roomItems = getItemsInLocation (currentRoom (save state)) state
        invItems = getItemsInLocation "inventory" state
        allReachableItems = roomItems ++ invItems
        roomNPCs = getNPCsInRoom (currentRoom (save state)) state
        targetItem = find (matchesItemTarget targetStr) allReachableItems
        targetNPC = find (matchesNPCTarget targetStr) roomNPCs
    in case (targetItem, targetNPC) of
        (Just item, _) ->
            let iId = itemId item
                maybeItemState = Map.lookup iId (itemStates (save state))
                currentStatus = maybe "unknown" itemStatus maybeItemState
            in case Map.lookup (verb, currentStatus) (itemVerbMap item) of
                Just outcome -> applyOutcome outcome iId state
                Nothing ->
                    if verb == VTake && maybe False ((/= "inventory") . itemLocation) maybeItemState
                    then (pickupItem iId state, "You take the " ++ itemName item ++ ".")
                    else if verb == VDrop && hasItem iId state
                    then (dropItem iId state, "You drop the " ++ itemName item ++ ".")
                    else if verb == VLookAt
                    then (state, itemDescription item)
                    else (state, "You can't do that to the " ++ itemName item ++ " right now.")

        (Nothing, Just npc) ->
            let nId = npcId npc
                maybeNpcState = Map.lookup nId (npcStates (save state))
                currentStatus = maybe "unknown" npcStatus maybeNpcState
            in case Map.lookup (verb, currentStatus) (npcVerbMap npc) of
                Just outcome -> applyOutcome outcome nId state
                Nothing
                    | verb == VTalk -> talkTo npc maybeNpcState state
                    | verb == VAttack -> executeAttack npc maybeNpcState targetStr state
                    | verb == VLookAt -> (state, npcDescription npc)
                    | otherwise -> (state, "You can't do that to " ++ npcName npc ++ ".")

        (Nothing, Nothing) -> (state, "You don't see '" ++ targetStr ++ "' here.")

-- | Handle "use <item> on <entity>" with weapon→attack fallback
executeCommand (InteractWith VUseOn itemStr entityStr) state =
    let itemTarget = normalizeText itemStr
        entityTarget = normalizeText entityStr
        inventoryItems = getItemsInLocation "inventory" state
        maybeItem = find (matchesItemTarget itemTarget) inventoryItems
        maybeVehicle = findVehicle entityStr state
    in case maybeItem of
        Nothing -> (state, "You need to be carrying '" ++ itemStr ++ "' to use it.")
        Just item ->
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
                    Nothing
                        | isLivingNPCInRoom entityTarget state ->
                            executeCommand (Interact VAttack entityTarget) state
                        | otherwise ->
                            case tryItemOnItem (itemId item) entityTarget state of
                                Just result -> result
                                Nothing -> tryRefuelByItem item state
            else
                if isLivingNPCInRoom entityTarget state
                then executeCommand (Interact VAttack entityTarget) state
                else case tryItemOnItem (itemId item) entityTarget state of
                        Just result -> result
                        Nothing ->
                            case maybeVehicle of
                                Just _ -> tryRefuelByItem item state
                                Nothing -> (state, "You can't reach '" ++ entityStr ++ "' from here.")
  where
    -- Vehicle refuelling: `use <fuel item> on <vehicle>` adds the item's
    -- "fuel" prop value (default 1) to the vehicle's tank, consuming the item.
    tryRefuelByItem item st =
        let st1 = if hasItem (itemId item) st then st else dropItem (itemId item) st
        in case findVehicleTarget st of
            Just vId ->
                let amount = fromMaybe 1 (Map.lookup "fuel" . itemProps =<< Map.lookup (itemId item) (itemStates (save st)))
                in case refuelVehicle vId amount st1 of
                    Nothing -> (st1, "The " ++ entityStr ++ " doesn't need fuel.")
                    Just (st2, msg) ->
                        (consumeItem (itemId item) st2, "You use the " ++ itemName item ++ ". " ++ msg)
            Nothing -> (st1, "Nothing happens.")
    findVehicleTarget st = case currentVehicle (save st) of
        Just vId | isJust (lookupVehicle vId st) -> Just vId
        _ -> case findVehicle entityStr st of
            Just v -> Just (vehicleId v)
            Nothing -> Nothing

executeCommand (InteractWith _ _ _) state = (state, "Nothing happens.")

executeCommand Restart _ = (emptyGameState, "")
executeCommand ListSaves state = (state, "")

executeCommand Help state = (state, helpText)
executeCommand Quit state = (endGame (Custom "quit") state, "Goodbye!")
executeCommand (Unknown cmd) state = (state, "I don't understand '" ++ cmd ++ "'. Type 'help' for available commands.")
executeCommand (Save _) state = (state, "")
executeCommand (Load _) state = (state, "")

-- ---------------------------------------------------------------------
-- Vehicles (Phase 3)
-- ---------------------------------------------------------------------

executeCommand (EnterVehicleCmd targetStr) state =
    case findVehicle targetStr state of
        Nothing -> (state, "You don't see '" ++ targetStr ++ "' here to enter.")
        Just v -> case enterVehicle (vehicleId v) state of
            Left err -> (state, err)
            Right (st', msg) -> (st', msg)

executeCommand ExitVehicleCmd state =
    case exitVehicle state of
        Left err -> (state, err)
        Right (st', msg) -> (st', msg)

executeCommand (DriveToCmd targetStr) state =
    case currentVehicle (save state) of
        Nothing -> (state, "You are not in a vehicle.")
        Just vId -> case driveVehicle vId targetStr state of
            Left err -> (state, err)
            Right (st', msg) -> (st', msg)

executeCommand WaitCmd state =
    case advanceVehicleRoute state of
        Left err -> (state, err)
        Right (st', msg) -> (st', msg)

-- | Refuel: `refuel` or `refuel <vehicle>`. Actual fuelling happens via
--   `use <fuel item> on <vehicle>`; plain `refuel` reports the status.
executeCommand (RefuelCmd targetStr) state =
    let v = if null targetStr
            then currentVehicle (save state) >>= \vId -> lookupVehicle vId state
            else findVehicle targetStr state
    in case v of
        Nothing -> (state, "There is no vehicle to refuel.")
        Just veh ->
            let vId = vehicleId veh
                vState = getVehicleState vId state
                fuelStatus = case (vehicleFuelProp veh, vsFuel vState) of
                    (Nothing, _) -> "The " ++ vehicleName veh ++ " doesn't need fuel."
                    (Just (fname, maxF), Just f) ->
                        vehicleName veh ++ " fuel (" ++ fname ++ "): " ++ show f ++ "/" ++ show maxF
                    (Just (fname, maxF), Nothing) ->
                        vehicleName veh ++ " fuel (" ++ fname ++ "): 0/" ++ show maxF
            in (state, fuelStatus)

-- | Repair: `repair <condition>` clears a matching vehicle condition on the
--   current vehicle.
executeCommand (RepairCmd targetStr) state =
    case currentVehicle (save state) of
        Nothing -> (state, "You are not in a vehicle.")
        Just vId ->
            let vState = getVehicleState vId state
                target = normalizeText targetStr
                v = lookupVehicle vId state
                matches = [ c | c <- Set.toList (vsActiveConditions vState)
                          , target `elem` [map toLower c, "the " ++ map toLower c] ]
                vName = maybe vId vehicleName v
            in if null matches
               then (state, "There is nothing broken about the " ++ vName
                            ++ " that matches '" ++ targetStr ++ "'."
                            ++ (if Set.null (vsActiveConditions vState)
                                then "" else " Problems: " ++ intercalate ", " (Set.toList (vsActiveConditions vState)) ++ "."))
               else let c = head matches
                        st' = clearVehicleCondition vId c state
                    in (st', "You repair the " ++ vName ++ " (" ++ c ++ ").")

-- | Resolve a vehicle by name/keyword among all vehicles in the world
findVehicle :: String -> GameState -> Maybe VehicleDef
findVehicle targetStr state =
    let target = normalizeText targetStr
        aliases v = nub (map normalizeText (vehicleId v : vehicleName v : vehicleKeywords v))
    in find (\v -> target `elem` aliases v) (Map.elems (vehicleDefs (world state)))

-- ---------------------------------------------------------------------------
-- Helpers used by executeCommand
-- ---------------------------------------------------------------------------

-- | Look up an item by target string in inventory or current room
findMatchingItem :: String -> GameState -> Maybe ItemDef
findMatchingItem targetStr state =
    let roomItems = getItemsInLocation (currentRoom (save state)) state
        invItems = getItemsInLocation "inventory" state
    in find (matchesItemTarget targetStr) (invItems ++ roomItems)

-- | Is the room dark?
--   A room tagged "dark" stays dark unless the player carries a light source
--   or the room's `lightFlag` has been switched on (e.g. by a `search` outcome).
isDark :: Room -> GameState -> Bool
isDark room state =
    Set.member "dark" (roomTags room)
        && not (playerHasTaggedItem "lightsource" state)
        && not litByFlag
  where
    litByFlag = case roomLightFlag room of
        Nothing  -> False
        Just flg -> getFlag flg state == Just "true"

-- | Pick the room description: an alternative one if its flag is set
resolveDescription :: Room -> GameState -> String
resolveDescription room state =
    let alts = roomAltDescriptions room
        matching = [d | (flag, d) <- Map.toList alts, getFlag flag state == Just "true"]
    in case matching of
        (d:_) -> d
        []    -> roomDescription room

-- | Run a room hook (onEnter / onLook / onExit) if one is defined
runRoomHook :: (Room -> Maybe ActionOutcome) -> RoomID -> GameState -> (GameState, String)
runRoomHook hook rId state =
    case Map.lookup rId (rooms (world state)) >>= hook of
        Nothing -> (state, "")
        Just outcome -> applyOutcome outcome "" state

-- | `search` — reveal hidden items and run the room's search outcome
searchRoom :: GameState -> CommandResult
searchRoom state =
    let roomId = currentRoom (save state)
        -- hidden items currently in this room
        hidden = [ iId
                 | (iId, st) <- Map.toList (itemStates (save state))
                 , itemLocation st == roomId
                 , Just def <- [Map.lookup iId (itemDefs (world state))]
                 , itemHidden def
                 , not (itemDiscovered st)
                 ]
        stateAfterReveal = foldr discoverItem state hidden
        discoveredMsgs = [ maybe ("You find the " ++ iId ++ ".") id
                             (Map.lookup iId (itemDefs (world state)) >>= itemDiscoverText)
                         | iId <- hidden ]
        (stateFinal, hookMsg) =
            case Map.lookup roomId (rooms (world state)) >>= roomSearchOutcome of
                Nothing -> (stateAfterReveal, "")
                Just outcome -> applyOutcome outcome "" stateAfterReveal
        full = intercalate "\n" (filter (not . null) (discoveredMsgs ++ [hookMsg]))
    in (stateFinal, if null full then "You find nothing of interest." else full)

-- | Item-on-item interaction (crafting).
--   Returns Nothing if no interaction is defined, so callers can keep their
--   own "nothing here" message.
tryItemOnItem :: ItemID -> String -> GameState -> Maybe CommandResult
tryItemOnItem usedId targetStr state =
    case findMatchingItem targetStr state of
        Nothing -> Nothing
        Just target ->
            let key = (usedId, itemId target)
                altKey = (itemId target, usedId)
            in case Map.lookup key (itemInteractions (world state)) <|>
                    Map.lookup altKey (itemInteractions (world state)) of
                Just outcome -> Just (applyOutcome outcome (itemId target) state)
                Nothing -> Nothing

-- | Dialogue: use the tree if present, otherwise fall back to the legacy single line
talkTo :: NPCDef -> Maybe NPCState -> GameState -> CommandResult
talkTo npc maybeNpcState state =
    let status = maybe "alive" npcStatus maybeNpcState
    in case Map.lookup status (npcDialogueTrees npc) of
        Just tree -> renderDialogue npc tree maybeNpcState state
        Nothing -> case Map.lookup status (npcDialogue npc) of
            Just speech -> (clearActiveDialogue state, npcName npc ++ " says: \"" ++ speech ++ "\"")
            Nothing -> (clearActiveDialogue state, npcName npc ++ " has nothing to say.")

-- | Render the current node of a dialogue tree and list its choices
renderDialogue :: NPCDef -> DialogueTree -> Maybe NPCState -> GameState -> CommandResult
renderDialogue npc tree maybeNpcState state =
    let nodeId = fromMaybe (dtEntry tree) (maybeNpcState >>= npcDialogueNode)
        maybeNode = Map.lookup nodeId (dtNodes tree)
    in case maybeNode of
        Nothing -> (clearActiveDialogue state, npcName npc ++ " has nothing to say.")
        Just node ->
            let header = npcName npc ++ ": \"" ++ dnText node ++ "\""
                choices = dnChoices node
                body = if null choices
                       then header
                       else header ++ "\n\n" ++ unlines
                            [ "  [" ++ show i ++ "] " ++ dcText c
                            | (i, c) <- zip [1 :: Int ..] choices ]
                -- store the node so a follow-up `choose N` can resolve it
                stateWithNode = setDialogueNode (npcId npc) (Just nodeId) state
                state' = if null choices
                         then clearActiveDialogue stateWithNode
                         else setActiveDialogue (Just (npcId npc)) stateWithNode
            in (state', body)

-- ---------------------------------------------------------------------------
-- Combat
-- ---------------------------------------------------------------------------

-- | Combat logic extracted for reuse and correctness
executeAttack :: NPCDef -> Maybe NPCState -> String -> GameState -> CommandResult
executeAttack npc maybeNpcState targetStr state =
    case maybeNpcState >>= npcHealth of
        Nothing -> (state, "You can't attack the " ++ npcName npc ++ ".")
        Just hp ->
            let nId = npcId npc
                playerDmg = max 1 (effectiveAttack state - npcDefenseBase npc)
                newHp = hp - playerDmg
            in if newHp <= 0
               then (killNPC nId state, "You attack the " ++ targetStr ++ " and kill it!")
               else
                   let npcDmg = max 0 (npcAttackBase npc - effectiveDefense state)
                       state' = case maybeNpcState of
                           Just npcState -> updateNPCState nId (npcState { npcHealth = Just newHp }) state
                           Nothing       -> state
                       state'' = updatePlayerHealth (\h -> h - npcDmg) state'
                   in if isPlayerDead state''
                      then (endGame Death state'', "The " ++ targetStr ++ " strikes back and kills you!")
                      else (state'', "You hit for " ++ show playerDmg ++ ", it hits you for " ++ show npcDmg ++ ".")

-- ---------------------------------------------------------------------------
-- Help text
-- ---------------------------------------------------------------------------

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
    , "  search                     - Search the room for hidden things"
    , "  take / get / grab <item>   - Pick up an item"
    , "  take all                   - Pick up all items in the room"
    , "  drop <item>                - Drop an item"
    , "  drop all                   - Drop everything you're carrying"
    , "  use <item>                 - Use an item from inventory"
    , "  use <item> on <target>     - Use an item on something"
    , "  talk to / speak with <npc> - Talk to a character"
    , "  choose <n> / <n>           - Select a dialogue option"
    , "  attack / hit <npc>         - Attack an enemy"
    , ""
    , "Equipment:"
    , "  equip / wear / wield <item>- Equip an item"
    , "  unequip / remove <item>    - Unequip an item"
    , "  unequip all                - Remove all equipment"
    , "  stats                      - Show health, attack, defense and equipment"
    , ""
    , "Vehicles:"
    , "  enter / board <vehicle>    - Board a vehicle at your stop"
    , "  exit / disembark           - Leave the current vehicle"
    , "  drive to <station>         - Steer (PlayerControlled, from the cockpit)"
    , "  wait                       - Advance to the next stop (AutomaticRoute)"
    , "  refuel [vehicle]           - Show fuel status"
    , "  repair <problem>           - Fix a vehicle condition"
    , ""
    , "Multi-item:"
    , "  take <item> and <item>     - Take multiple items"
    , ""
    , "System:"
    , "  inventory / inv / i        - Check what you're carrying"
    , "  undo                       - Restore the previous game state (up to 50)"
    , "  save [name]                - Save game (default: savegame)"
    , "  load [name]                - Load a saved game"
    , "  saves                      - List all saved games"
    , "  restart                    - Start a new game"
    , "  help                       - Show this help"
    , "  quit / exit / q            - Exit the game"
    , ""
    , "Tip: Press Tab to auto-complete commands and targets."
    ]
