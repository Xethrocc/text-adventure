{-# LANGUAGE TupleSections #-}

-- | Command parsing and processing for the text adventure engine
module Parser where

import Types
import Game
import Combat (CombatActor (..), CombatTarget (..), ShipSystems (..), resolveCombat, targetShipSystems)
import Control.Applicative ((<|>))
import Data.Char (toLower, isDigit)
import Data.List (find, intercalate, nub, foldl')
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import Verbs (resolveVerb)

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
    | WatchCmd (Maybe String)    -- ^ `watch [target]`: play animation frames (Phase D)
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

-- | Parse an input verb word against the unified registry (core + custom).
--   Backwards-compatible alias for Verbs.resolveVerb with no custom defs.
parseVerb :: String -> Maybe Verb
parseVerb = resolveVerb Map.empty

-- | Resolve an input verb word against the registry: core verbs first, then
--   adventure-declared custom verbs.  Delegates to Verbs.resolveVerb.
parseVerbWith :: Map.Map String VerbDef -> String -> Maybe Verb
parseVerbWith = resolveVerb

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

-- | Parse user input into a command (no custom verbs)
parseCommand :: String -> Command
parseCommand = parseCommandWith Map.empty

-- | Parse user input with the adventure's verb registry
parseCommandWith :: Map.Map String VerbDef -> String -> Command
parseCommandWith defs input =
    let tokens = words (map toLower input)
    in case tokens of
        [] -> Unknown ""
        _ | isCompoundCandidateWith defs tokens -> parseCompoundCommandWith defs tokens
          | otherwise -> parseSimpleCommandWith defs tokens input

-- | Check if this looks like a compound command (has "and" after a verb)
isCompoundCandidateWith :: Map.Map String VerbDef -> [String] -> Bool
isCompoundCandidateWith defs (v : rest) = case parseVerbWith defs v of
    Just _ -> "and" `elem` rest
    Nothing -> case v of
        "pick" -> "and" `elem` rest
        _      -> False
isCompoundCandidateWith _ _ = False

-- | Parse a compound command by splitting on last "and"
parseCompoundCommandWith :: Map.Map String VerbDef -> [String] -> Command
parseCompoundCommandWith defs tokens@(v : rest) =
    let targetParts = case rest of
            ("up" : parts) -> parts  -- "pick up X and Y"
            _              -> rest
    in case splitOnLastAnd targetParts of
        Just (_, after) ->
            let cmd1 = parseSimpleCommandWith defs (v : rest `takeWhileNotLast` "and") (unwords tokens)
                cmd2 = parseSimpleCommandWith defs (v : after) (unwords (v : after))
            in CompoundCommand [cmd1, cmd2]
        Nothing -> parseSimpleCommandWith defs tokens (unwords tokens)
  where
    takeWhileNotLast :: [String] -> String -> [String]
    takeWhileNotLast ts target =
        let indices = [i | (i, t) <- zip [0..] ts, t == target]
        in case indices of
            [] -> ts
            _  -> take (last indices) ts
parseCompoundCommandWith _ [] = Unknown ""

-- | Parse a single (non-compound) command with the verb registry
parseSimpleCommandWith :: Map.Map String VerbDef -> [String] -> String -> Command
parseSimpleCommandWith defs tokens input = case tokens of
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
    ["southeast"]          -> Go Southeast
    ["se"]                 -> Go Southeast
    ["southwest"]          -> Go Southwest
    ["sw"]                 -> Go Southwest
    ["northeast"]          -> Go Northeast
    ["ne"]                 -> Go Northeast
    ["northwest"]          -> Go Northwest
    ["nw"]                 -> Go Northwest
    ["look"]               -> Look
    ["inventory"]          -> Inventory
    ["inv"]                -> Inventory
    ["i"]                  -> Inventory
    ["stats"]              -> StatsCmd
    ["journal"]            -> JournalCmd
    ["quests"]             -> JournalCmd
    ["undo"]               -> Undo
    -- Dialogue choice (Phase 4.6).
    -- `pick` is also a `take` alias (src/Verbs.hs) and `pick up <item>` is the
    -- take verb below (:206). A *numeric* `pick` therefore means "choose" and
    -- can never mean "take item <n>" — the keyword list is matched before
    -- `parseVerbWith`. Intended, and pinned by
    -- `testDialoguePickKeywordAlias` in test/Tests.hs.
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
    ["watch"]             -> WatchCmd Nothing
    "watch" : targetParts | not (null targetParts) ->
        let t = unwords (safeStripStopWords targetParts)
        in WatchCmd (if t `elem` ["room", "area", "here", "around"] then Nothing else Just t)
    ["help"]               -> Help
    ["quit"]               -> Quit
    ["exit"]               -> Quit
    ["q"]                  -> Quit
    ["restart"]            -> Restart
    ["saves"]              -> ListSaves
    ["list", "saves"]      -> ListSaves
    ["unequip", "all"]     -> UnequipAllCmd
    ["unequip"]            -> UnequipAllCmd
    ["save"]               -> Save "savegame"
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
    -- Tactical combat (Phase 7f-3, steps A2/A3)
    ["defend"]             -> Interact (VCustom "defend") ""
    ["flee"]               -> Interact (VCustom "flee") ""
    "use-ability" : abParts | not (null abParts) ->
        Interact (VCustom "use-ability") (unwords (safeStripStopWords abParts))
    "use" : "ability" : abParts | not (null abParts) ->
        Interact (VCustom "use-ability") (unwords (safeStripStopWords abParts))
    "ability" : abParts | not (null abParts) ->
        Interact (VCustom "use-ability") (unwords (safeStripStopWords abParts))
    "use"   : useParts -> parseUse useParts input
    -- Generic verb-noun parsing: resolve against registry (core + custom)
    v : targetParts | not (null targetParts) -> case parseVerbWith defs v of
        Just verb -> Interact verb (unwords (safeStripStopWords targetParts))
        Nothing   -> Unknown input
    -- Bare custom verb with no object (e.g. "align", "pray", "accuse")
    [v] | Just verb <- parseVerbWith defs v -> Interact verb ""
    _ -> Unknown input

-- | Keep the original name for backward compatibility in tests
parseSimpleCommand :: [String] -> String -> Command
parseSimpleCommand = parseSimpleCommandWith Map.empty

parseDirection :: String -> String -> Command
parseDirection dir input = case dir of
    "north" -> Go North
    "south" -> Go South
    "east"  -> Go East
    "west"  -> Go West
    "up"    -> Go Up
    "down"    -> Go Down
    "southeast" -> Go Southeast
    "se"      -> Go Southeast
    "southwest" -> Go Southwest
    "sw"      -> Go Southwest
    "northeast" -> Go Northeast
    "ne"      -> Go Northeast
    "northwest" -> Go Northwest
    "nw"      -> Go Northwest
    _         -> Unknown input

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
        roomItems = getItemsInLocation (InRoom currentRoomId) state
        roomNPCs = getNPCsInRoom currentRoomId state
        exitEntities = reachableExitEntities state
        doorAliases = if null exitEntities then [] else ["door", "locked door"]
    in nub (concatMap itemAliases roomItems ++ concatMap npcAliases roomNPCs ++ exitEntities ++ doorAliases)

resolveEntityCandidates :: String -> GameState -> [String]
resolveEntityCandidates entityStr state =
    let target = normalizeText entityStr
        currentRoomId = currentRoom (save state)
        roomItems = getItemsInLocation (InRoom currentRoomId) state
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
-- Command execution
-- ---------------------------------------------------------------------------

-- | Execute a command and return updated game state and message
executeCommand :: Command -> GameState -> CommandResult

executeCommand (Go dir) state
    | canMove dir state = case getExitInDirection dir state of
        Just (Open destinationRoom) ->
            let (st', hookMsg) = transitionToRoom destinationRoom (clearActiveDialogue state)
                fullMsg = intercalate "\n" (filter (not . null) ["You move " ++ show dir ++ ".", hookMsg])
            in (st', fullMsg)
        Just (Locked destinationRoom entityTarget)
            | getEntityState entityTarget state == Just "unlocked" ->
                let (st', hookMsg) = transitionToRoom destinationRoom (clearActiveDialogue state)
                    fullMsg = intercalate "\n" (filter (not . null) ["You move " ++ show dir ++ ".", hookMsg])
                in (st', fullMsg)
            | otherwise -> (state, "The door is locked.")
        Nothing -> (state, "There's nothing in that direction.")
    | otherwise = (state, "You can't go that way.")

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
                itemsInRoom = getItemsInLocation (InRoom (currentRoom (save state))) state
                npcsInRoom = getNPCsInRoom (currentRoom (save state)) state
                itemDesc = if null itemsInRoom
                           then "\nYou see nothing of interest."
                           else "\nYou see: " ++ intercalate ", " (map itemName itemsInRoom) ++ "."
                npcDesc = if null npcsInRoom
                          then ""
                          else "\nAlso here: " ++ intercalate ", " (map npcName npcsInRoom) ++ "."
                (state', hookMsg) = runRoomHook roomOnLook (currentRoom (save state)) state
                vehicleMsg = vehicleLookAddon state'
                asciiArt = resolveAsciiArt (roomAscii room) state
                full = intercalate "\n" (filter (not . null)
                        [asciiArt, desc, itemDesc, npcDesc, hookMsg, fromMaybe "" vehicleMsg])
            in (state', full)

executeCommand Inventory state =
    let invItems = getItemsInLocation (CarriedBy "player") state
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
                                let choices = visibleChoices state node
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
    let roomItems = getItemsInLocation (InRoom (currentRoom (save state))) state
    in if null roomItems
       then (state, "There's nothing here to take.")
       else let (finalState, msgs) = foldl' (\(s, ms) item ->
                    let (s', m) = executeCommand (Interact VTake (itemName item)) s
                    in (s', ms ++ [m])) (state, []) roomItems
            in (finalState, intercalate "\n" msgs)

executeCommand DropAll state =
    let invItems = getItemsInLocation (CarriedBy "player") state
    in if null invItems
       then (state, "You're not carrying anything to drop.")
       else let (finalState, msgs) = foldl' (\(s, ms) item ->
                    let (s', m) = executeCommand (Interact VDrop (itemName item)) s
                    in (s', ms ++ [m])) (state, []) invItems
            in (finalState, intercalate "\n" msgs)

executeCommand (CompoundCommand cmds) state =
    foldl' (\(s, msgs) cmd ->
        let (s', msg) = executeCommand cmd s
        in (s', if null msgs then msg else msgs ++ "\n" ++ msg)
    ) (state, "") cmds

executeCommand (SearchCmd maybeTarget) state =
    case maybeTarget of
        Nothing -> searchRoom state
        Just targetStr ->
            -- `search <thing>`: try a matching item/NPC's VSearch verb first
            let roomItems = getItemsInLocation (InRoom (currentRoom (save state))) state
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

executeCommand (WatchCmd maybeTarget) state = case getCurrentRoom state of
    Nothing -> (state, "You're in a void. There's nothing to watch.")
    Just room
        | isDark room state -> (state, "It's pitch black. You can't watch anything.")
        | otherwise -> case maybeTarget of
            Nothing -> watchArt (roomAscii room) "the room"
            Just targetStr ->
                case find (matchesItemTarget targetStr) allReachableItems of
                    Just item -> watchArt (itemAscii item) (itemName item)
                    Nothing -> case find (matchesNPCTarget targetStr) roomNPCs of
                        Just npc -> watchArt (npcAscii npc) (npcName npc)
                        Nothing  -> (state, "You don't see '" ++ targetStr ++ "' here.")
  where
    allReachableItems = getItemsInLocation (InRoom (currentRoom (save state))) state
                        ++ getItemsInLocation (CarriedBy "player") state
    roomNPCs = getNPCsInRoom (currentRoom (save state)) state
    watchArt art label = case asciiFrames art state of
        []     -> (state, "There is nothing to watch about " ++ label ++ ".")
        frames -> (state { pendingAnimation = Just frames },
                   "Watching " ++ label ++ "...")

executeCommand (Interact verb targetStr) state =
    let roomItems = getItemsInLocation (InRoom (currentRoom (save state))) state
        invItems = getItemsInLocation (CarriedBy "player") state
        allReachableItems = roomItems ++ invItems
        roomNPCs = getNPCsInRoom (currentRoom (save state)) state
        targetItem = find (matchesItemTarget targetStr) allReachableItems
        targetNPC = find (matchesNPCTarget targetStr) roomNPCs
    in case (targetItem, targetNPC) of
        (Just item, _) ->
            let iId = itemId item
                maybeItemState = Map.lookup iId (itemStates (save state))
                currentStatus = maybe "unknown" itemStatus maybeItemState
                notCarried = maybe True (\loc -> loc /= CarriedBy "player") (fmap itemLocation maybeItemState)
                vmLookup = Map.lookup (verb, currentStatus) (itemVerbMap item)
            in case (verb, vmLookup) of
                -- Taking: enforce portability, then pick up AND run on_take.
                (VTake, _)
                    | not notCarried ->
                        (state, "You already have the " ++ itemName item ++ ".")
                    | otherwise ->
                        case itemPortable item of
                            False -> (state, fromMaybe ("You can't take the " ++ itemName item ++ ".")
                                                     (itemTakeFailure item))
                            True ->
                                let (st', extra) = case vmLookup of
                                        Just outcome -> applyOutcome outcome iId state
                                        Nothing      -> (state, "")
                                    takeMsg = "You take the " ++ itemName item ++ "."
                                in (pickupItem iId st',
                                    if null extra then takeMsg else takeMsg ++ "\n" ++ extra)
                _ -> case vmLookup of
                    Just outcome -> applyOutcome outcome iId state
                    Nothing ->
                        if verb == VDrop && hasItem iId state
                        then (dropItem iId state, "You drop the " ++ itemName item ++ ".")
                        else if verb == VLookAt
                        then (state, withAscii (resolveAsciiArt (itemAscii item) state)
                                              (resolveCondText (itemDescription item) state))
                        else case if verb == VAttack then tryAttackVehicle targetStr state else Nothing of
                            Just res -> res
                            Nothing  -> (state, "You can't do that to the " ++ itemName item ++ " right now.")

        (Nothing, Just npc) ->
            let nId = npcId npc
                maybeNpcState = Map.lookup nId (npcStates (save state))
                currentStatus = maybe "unknown" npcStatus maybeNpcState
            in case Map.lookup (verb, currentStatus) (npcVerbMap npc) of
                Just outcome -> applyOutcome outcome nId state
                Nothing
                    | verb == VTalk -> talkTo npc maybeNpcState state
                    | verb == VAttack -> executeAttack npc maybeNpcState targetStr state
                    | verb == VLookAt -> (state, withAscii (resolveAsciiArt (npcAscii npc) state)
                                                          (resolveCondText (npcDescription npc) state))
                    | otherwise -> (state, "You can't do that to " ++ npcName npc ++ ".")

        (Nothing, Nothing)
            -- Phase 7f-3, A2: bare `defend` / `flee` during a tactical fight
            -- route through the combat resolver with the corresponding action.
            | null targetStr, VCustom vn <- verb
            , CombatTactical _ <- combatProfile (world state)
            , isCombatEngaged state
            , Just ca <- tacticalVerbAction vn
            -> executeTacticalAction ca state
            -- Phase 7f-3, A3: `use-ability <id>` during a tactical fight
            | not (null targetStr), VCustom vn <- verb
            , vn `elem` ["use-ability", "ability"]
            , CombatTactical _ <- combatProfile (world state)
            -> executeTacticalAction (CAAbility targetStr) state
            | null targetStr, VCustom vn <- verb
            , vn `elem` ["defend", "flee"]
            -> (state, "You are not in combat.")
            | null targetStr -> (state, "")   -- bare verb (e.g. custom command); triggers carry the message
            | verb == VAttack, Just res <- tryAttackVehicle targetStr state -> res
            | otherwise -> (state, "You don't see '" ++ targetStr ++ "' here.")

-- | Handle "use <item> on <entity>" with weapon→attack fallback
executeCommand (InteractWith VUseOn itemStr entityStr) state =
    let itemTarget = normalizeText itemStr
        entityTarget = normalizeText entityStr
        inventoryItems = getItemsInLocation (CarriedBy "player") state
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

executeCommand Restart state = (state, "")
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
                    (Just fs, Just f) ->
                        vehicleName veh ++ " fuel (" ++ fsItem fs ++ "): " ++ show f ++ "/" ++ show (fsMax fs)
                    (Just fs, Nothing) ->
                        vehicleName veh ++ " fuel (" ++ fsItem fs ++ "): 0/" ++ show (fsMax fs)
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
    let roomItems = getItemsInLocation (InRoom (currentRoom (save state))) state
        invItems = getItemsInLocation (CarriedBy "player") state
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

-- | Pick the room description: resolve CondText variants against game state.
resolveDescription :: Room -> GameState -> String
resolveDescription room state = resolveCondText (roomDescription room) state

-- | Prepend state-resolved ASCII art (when present) above a message.
withAscii :: String -> String -> String
withAscii art msg
    | null art  = msg
    | otherwise = art ++ "\n" ++ msg

-- | `search` — reveal hidden items and run the room's search outcome
searchRoom :: GameState -> CommandResult
searchRoom state =
    let rId = currentRoom (save state)
        -- hidden items currently in this room
        hidden = [ iId
                 | (iId, st) <- Map.toList (itemStates (save state))
                 , itemLocation st == InRoom rId
                 , Just def <- [Map.lookup iId (itemDefs (world state))]
                 , itemHidden def
                 , not (itemDiscovered st)
                 ]
        stateAfterReveal = foldr discoverItem state hidden
        discoveredMsgs = [ maybe ("You find the " ++ iId ++ ".") id
                             (Map.lookup iId (itemDefs (world state)) >>= itemDiscoverText)
                         | iId <- hidden ]
        (stateFinal, hookMsg) =
            case Map.lookup rId (rooms (world state)) >>= roomSearchOutcome of
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
        Nothing -> (clearActiveDialogue state, npcName npc ++ " has nothing to say.")

-- | Choices of a node that pass their optional `visible_when` predicate.
--   Used by both rendering and `choose N` so numbering stays consistent.
visibleChoices :: GameState -> DialogueNode -> [DialogueChoice]
visibleChoices state node =
    [ c | c <- dnChoices node
        , maybe True (\p -> evalPredicate p state) (dcVisible c) ]

-- | The choices currently offered by the active dialogue, if any. Mirrors the
--   node resolution in `executeCommand (ChooseCmd …)`; `Nothing` covers "not in
--   a conversation", "npc gone" and "nothing more to say".
activeChoices :: GameState -> Maybe [DialogueChoice]
activeChoices state = case activeDialogue (save state) of
    Nothing -> Nothing
    Just nId -> case Map.lookup nId (npcDefs (world state)) of
        Nothing -> Nothing
        Just npc ->
            let st = Map.lookup nId (npcStates (save state))
                status = maybe "alive" npcStatus st
            in case Map.lookup status (npcDialogueTrees npc) of
                Nothing -> Nothing
                Just tree ->
                    let nodeId = fromMaybe (dtEntry tree) (st >>= npcDialogueNode)
                    in case Map.lookup nodeId (dtNodes tree) of
                        Nothing -> Nothing
                        Just node -> Just (visibleChoices state node)

-- | Whether `choose N` refers to an option that is actually offered right now.
--   P1-16: an invalid choice is a typo, not a game action, and must not cost a
--   turn (conditions/vehicle ticks, `on: turn`, undo history).
isValidChoice :: Int -> GameState -> Bool
isValidChoice idx state = case activeChoices state of
    Just choices -> idx >= 1 && idx <= length choices
    Nothing      -> False

-- | Render the current node of a dialogue tree and list its choices
renderDialogue :: NPCDef -> DialogueTree -> Maybe NPCState -> GameState -> CommandResult
renderDialogue npc tree maybeNpcState state =
    let nodeId = fromMaybe (dtEntry tree) (maybeNpcState >>= npcDialogueNode)
        maybeNode = Map.lookup nodeId (dtNodes tree)
    in case maybeNode of
        Nothing -> (clearActiveDialogue state, npcName npc ++ " has nothing to say.")
        Just node ->
            let header = npcName npc ++ ": \"" ++ dnText node ++ "\""
                choices = visibleChoices state node
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

-- | Combat logic delegated to the pure resolver (Phase 7f). The parser only
--   wires profile + actors + target and applies the returned effects through
--   the single outcome interpreter. Messages come from the resolver.
executeAttack :: NPCDef -> Maybe NPCState -> String -> GameState -> CommandResult
executeAttack npc _ targetStr state =
    let nId = npcId npc
        profile = combatProfile (world state)
        -- Phase 7g/7h: party members standing here and the ship the player is
        -- aboard fight alongside the player.
        actors = [PlayerActor]
                 ++ map CompanionActor (partyMembersInRoom state)
                 ++ [ShipActor vId | Just vId <- [currentVehicle (save state)]]
        (effects, msgs) = resolveCombat profile actors (TargetNPC nId targetStr) CAAttack state
        -- Apply the whole effect list through the shared interpreter: it
        -- threads the RNG salt and joins every effect message instead of
        -- discarding all but the last (killNPCWithMsg / OnStateChange rules
        -- produce text that must survive trailing companion/ship effects).
        (st', effectMsg) = applyOutcomes effects nId state
        body = combineMsgs (effectMsg : msgs)
    in if null body then (st', "") else (st', body)

-- | Combat logic for attacking a target ship (Phase 7h-2).
executeAttackShip :: VehicleDef -> String -> GameState -> CommandResult
executeAttackShip veh targetStr state =
    let vId = vehicleId veh
        profile = combatProfile (world state)
        actors = [PlayerActor]
                 ++ map CompanionActor (partyMembersInRoom state)
                 ++ [ShipActor pvId | Just pvId <- [currentVehicle (save state)]]
        (effects, msgs) = resolveCombat profile actors (TargetShip vId targetStr) CAAttack state
        (st', effectMsg) = applyOutcomes effects vId state
        body = combineMsgs (effectMsg : msgs)
    in if null body then (st', "") else (st', body)

-- | Check if the target names a vehicle at the player's stop, and if so,
--   refuse attacking an ordinary vehicle or execute ship-to-ship combat.
tryAttackVehicle :: String -> GameState -> Maybe CommandResult
tryAttackVehicle targetStr state = case findVehicle targetStr state of
    Just veh ->
        let vId = vehicleId veh
            outsideStop = case currentVehicle (save state) of
                Just pvId -> vsCurrentStop (getVehicleState pvId state)
                Nothing   -> currentRoom (save state)
            vehStop = vsCurrentStop (getVehicleState vId state)
            isAboardTarget = currentVehicle (save state) == Just vId
        in if isAboardTarget || vehStop /= outsideStop
           then Just (state, "You don't see '" ++ targetStr ++ "' here.")
           else case targetShipSystems (TargetShip vId targetStr) state of
               Nothing -> Just (state, "You can't attack the " ++ targetStr ++ ".")
               Just _  -> Just (executeAttackShip veh targetStr state)
    Nothing -> Nothing

-- | Concatenate non-empty combat messages.
combineMsgs :: [String] -> String
combineMsgs = unlines . filter (not . null)

-- | Map a custom verb name to the tactical combat action it represents.
--   Returns Nothing for verbs that are not tactical combat verbs.
tacticalVerbAction :: String -> Maybe CombatAction
tacticalVerbAction "defend" = Just CADefend
tacticalVerbAction "flee"   = Just CAFlee
tacticalVerbAction _        = Nothing

-- | Execute a tactical combat action against the first living NPC in the
--   room (the same heuristic as `attack <target>`), or against a ship at
--   the current stop. Used for bare verbs like `defend` and `flee` that
--   have no explicit target.
executeTacticalAction :: CombatAction -> GameState -> CommandResult
executeTacticalAction action state =
    let roomNPCs = getNPCsInRoom (currentRoom (save state)) state
        livingNPCs = [ npc | npc <- roomNPCs
                     , let ns = Map.lookup (npcId npc) (npcStates (save state))
                     , maybe True (\s -> npcStatus s /= "dead") ns ]
    in case livingNPCs of
        (npc : _) ->
            let nId = npcId npc
                profile = combatProfile (world state)
                actors = [PlayerActor]
                         ++ map CompanionActor (partyMembersInRoom state)
                         ++ [ShipActor vId | Just vId <- [currentVehicle (save state)]]
                (effects, msgs) = resolveCombat profile actors (TargetNPC nId (npcName npc)) action state
                (st', effectMsg) = applyOutcomes effects nId state
                body = combineMsgs (effectMsg : msgs)
            in if null body then (st', "") else (st', body)
        [] ->
            let outsideStop = case currentVehicle (save state) of
                    Just pvId -> vsCurrentStop (getVehicleState pvId state)
                    Nothing   -> currentRoom (save state)
                targetShips = [ v | v <- Map.elems (vehicleDefs (world state))
                              , Just (vehicleId v) /= currentVehicle (save state)
                              , vsCurrentStop (getVehicleState (vehicleId v) state) == outsideStop
                              , isJust (targetShipSystems (TargetShip (vehicleId v) "") state)
                              , maybe True (> 0) (targetShipSystems (TargetShip (vehicleId v) "") state >>= ssHull)
                              ]
            in case targetShips of
                (veh : _) ->
                    let vId = vehicleId veh
                        profile = combatProfile (world state)
                        actors = [PlayerActor]
                                 ++ map CompanionActor (partyMembersInRoom state)
                                 ++ [ShipActor pvId | Just pvId <- [currentVehicle (save state)]]
                        (effects, msgs) = resolveCombat profile actors (TargetShip vId (vehicleName veh)) action state
                        (st', effectMsg) = applyOutcomes effects vId state
                        body = combineMsgs (effectMsg : msgs)
                    in if null body then (st', "") else (st', body)
                [] -> (state, "There's nothing to fight here.")

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
    , "  watch [target]             - Play an item's/NPC's animation frames"
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
    , "  disembark                  - Leave the current vehicle ('exit' quits!)"
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
