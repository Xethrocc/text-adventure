{-# LANGUAGE TupleSections, PatternSynonyms #-}

-- | Command parsing and processing for the text adventure engine
--
--   The export list is explicit (Phase 0.7): everything not listed here is an
--   internal helper. This is the only way @-Wall@ can report unused top-level
--   bindings in this module — while everything was exported, dead code was
--   structurally invisible.
module Parser
    ( -- * Command and target types
      Command (..)
    , InteractTarget (..)
    , TargetResolution (..)
      -- * Pattern synonyms for 'TargetResolution'
    , pattern TargetItem
    , pattern TargetVehicle
    , pattern TargetAmbiguous
    , pattern TargetNotFound
    , pattern TargetBare
      -- * Parsing
    , parseCommand
    , parseCommandWith
    , parseVerbWith
    , preferInventoryTarget
      -- * Target resolution
    , resolveTarget
    , resolveInteractTarget
    , reachableExitEntities
      -- * Disambiguation (Phase 2.3)
    , disambiguableCommand
    , resolveDisambiguationAnswer
      -- * Execution
    , executeCommand
    , executeCommandEv
    , dispatchCommandEv
    , checkBeforeVeto
    , joinBeforeAndCmd
    , resolveCmdTarget
    , executeAttack
    , interactItem
    , bindCommandVars
      -- * Messages, darkness and help
    , defaultDarkMessage
    , isCurrentRoomDark
    , helpText
    , isValidChoice
    ) where

import Types
import Game
import Messages (renderMsg, evMsg)
import Vehicles
import Effects
import Quests
import Cards
import Combat (CombatActor (..), CombatTarget (..), ShipSystems (..), combatScreenLines, resolveCombatEv, targetShipSystems)
import Control.Applicative ((<|>))
import Data.Char (toLower, isDigit)
import Data.List (find, intercalate, nub, foldl', dropWhileEnd, isPrefixOf, isSuffixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, catMaybes)
import qualified Data.Set as Set
import Verbs (resolveVerb, verbCanonicalName)

-- | Parsed command structure
data Command
    = Go Direction
    | Look
    | Inventory
    | Interact Verb String
    | InteractWith Verb String String
    | ChooseCmd Int              -- ^ select a dialogue option (Phase 4.6)
    | AskCmd String String       -- ^ 4.5: `ask X about Y` (topic table)
    | TellCmd String String      -- ^ 4.5: `tell X about Y` (topic table)
    | TakeAll
    | DropAll
    | CompoundCommand [Command]
    | EquipCmd String
    | UnequipCmd String
    | UnequipAllCmd
    | StatsCmd
    | SearchCmd (Maybe String)   -- ^ `search` or `search <target>`
    | WatchCmd (Maybe String)    -- ^ `watch [target]`: play animation frames (Phase D)
    | MapCmd                    -- ^ `map`/`legend`: art with numbered hotspots (Phase E)
    | JournalCmd                 -- ^ show active/completed quests
    | Undo                       -- ^ restore the previous game state
    | EnterVehicleCmd String     -- ^ enter a vehicle
    | ExitVehicleCmd             -- ^ exit the current vehicle
    | OpenCmd String             -- ^ 4.4: open a container
    | CloseCmd String            -- ^ 4.4: close a container
    | LockCmd String             -- ^ 4.4: lock a container
    | UnlockCmd String           -- ^ 4.4: unlock a container
    | TakeFromCmd String String  -- ^ 4.4: take X from Y
    | PutInCmd String String     -- ^ 4.4: put X in Y
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
    | ActionWithArgs Verb [String]  -- ^ Parameterized command (Phase 1C)
    | PlayCardCmd Int (Maybe String) -- ^ Play card by 1-based index with optional target
    | HandCmd                        -- ^ Show hand
    | DeckCmd                        -- ^ Show draw pile / deck
    | DiscardCmd                     -- ^ Show discard pile
    | EndTurnCmd                     -- ^ End combat turn
    | Unknown String
    deriving (Show, Eq)


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
-- | 4.4: split a word list at the first occurrence of one of the prepositions
--   (`take X from Y` / `put X in Y`). Both sides must be non-empty.
splitPrep :: [String] -> [String] -> Maybe ([String], [String])
splitPrep preps ws =
    case break (`elem` preps) ws of
        (x, _ : y) | not (null x) && not (null y) -> Just (x, y)
        _ -> Nothing

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
    -- Card & Deck commands (Phase 2B)
    ["hand"]               -> HandCmd
    ["karten"]             -> HandCmd
    ["cards"]              -> HandCmd
    ["deck"]               -> DeckCmd
    ["discard"]            -> DiscardCmd
    ["ablage"]             -> DiscardCmd
    ["end", "turn"]        -> EndTurnCmd
    ["endturn"]            -> EndTurnCmd
    ["zug", "beenden"]     -> EndTurnCmd
    ["pass"]               -> EndTurnCmd
    ["passe"]              -> EndTurnCmd
    "play" : nStr : rest | all isDigit nStr && not (null nStr) ->
        let target = unwords (safeStripStopWords rest)
        in PlayCardCmd (read nStr) (if null target then Nothing else Just target)
    ["play", nStr] | all isDigit nStr && not (null nStr) ->
        PlayCardCmd (read nStr) Nothing
    "spiele" : nStr : "auf" : rest | all isDigit nStr && not (null nStr) ->
        let target = unwords (safeStripStopWords rest)
        in PlayCardCmd (read nStr) (if null target then Nothing else Just target)
    "spiele" : nStr : rest | all isDigit nStr && not (null nStr) ->
        let target = unwords (safeStripStopWords rest)
        in PlayCardCmd (read nStr) (if null target then Nothing else Just target)
    ["spiele", nStr] | all isDigit nStr && not (null nStr) ->
        PlayCardCmd (read nStr) Nothing
    -- Dialogue choice (Phase 4.6).
    -- `pick` is also a `take` alias (src/Verbs.hs) and `pick up <item>` is the
    -- take verb below (:206). A *numeric* `pick` therefore means "choose" and
    -- can never mean "take item <n>" — the keyword list is matched before
    -- `parseVerbWith`. Intended, and pinned by
    -- `testDialoguePickKeywordAlias` in test/Tests.hs.
    ("ask" : rest) | (whoParts@(_:_), "about" : whatParts@(_:_)) <- break (== "about") rest ->
        AskCmd (unwords (safeStripStopWords whoParts)) (unwords whatParts)
    ("tell" : rest) | (whoParts@(_:_), "about" : whatParts@(_:_)) <- break (== "about") rest ->
        TellCmd (unwords (safeStripStopWords whoParts)) (unwords whatParts)
    ("ask" : rest) | (whoParts@(_:_), "nach" : whatParts@(_:_)) <- break (== "nach") rest ->
        AskCmd (unwords (safeStripStopWords whoParts)) (unwords whatParts)
    ("frag" : rest) | (whoParts@(_:_), "nach" : whatParts@(_:_)) <- break (== "nach") rest ->
        AskCmd (unwords (safeStripStopWords whoParts)) (unwords whatParts)
    ("erzaehl" : rest) | (whoParts@(_:_), "von" : whatParts@(_:_)) <- break (== "von") rest ->
        TellCmd (unwords (safeStripStopWords whoParts)) (unwords whatParts)
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
    ["map"]               -> MapCmd
    ["legend"]            -> MapCmd
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
    "remove"  : targetParts
        | not (null targetParts)
        , "from" `notElem` targetParts
        , "aus" `notElem` targetParts -> UnequipCmd (unwords (safeStripStopWords targetParts))
    -- 4.4: container verbs (open/close/lock/unlock + take X from Y / put X in Y)
    "open"       : targetParts | not (null targetParts) -> OpenCmd (unwords (safeStripStopWords targetParts))
    "oeffne"     : targetParts | not (null targetParts) -> OpenCmd (unwords (safeStripStopWords targetParts))
    "close"      : targetParts | not (null targetParts) -> CloseCmd (unwords (safeStripStopWords targetParts))
    "schliesse"  : targetParts | not (null targetParts) -> CloseCmd (unwords (safeStripStopWords targetParts))
    "lock"       : targetParts | not (null targetParts) -> LockCmd (unwords (safeStripStopWords targetParts))
    "verschliesse" : targetParts | not (null targetParts) -> LockCmd (unwords (safeStripStopWords targetParts))
    "unlock"     : targetParts | not (null targetParts) -> UnlockCmd (unwords (safeStripStopWords targetParts))
    "entsperre"  : targetParts | not (null targetParts) -> UnlockCmd (unwords (safeStripStopWords targetParts))
    "take" : rest | Just (x, y) <- splitPrep ["from", "aus"] rest ->
        TakeFromCmd (unwords (safeStripStopWords x)) (unwords (safeStripStopWords y))
    "get"  : rest | Just (x, y) <- splitPrep ["from", "aus"] rest ->
        TakeFromCmd (unwords (safeStripStopWords x)) (unwords (safeStripStopWords y))
    "nimm" : rest | Just (x, y) <- splitPrep ["from", "aus"] rest ->
        TakeFromCmd (unwords (safeStripStopWords x)) (unwords (safeStripStopWords y))
    "put"  : rest | Just (x, y) <- splitPrep ["in"] rest ->
        PutInCmd (unwords (safeStripStopWords x)) (unwords (safeStripStopWords y))
    "lege" : rest | Just (x, y) <- splitPrep ["in"] rest ->
        PutInCmd (unwords (safeStripStopWords x)) (unwords (safeStripStopWords y))
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
        Just verb ->
            let cleanParts = safeStripStopWords targetParts
            in case (isCustomVerb verb, cleanParts) of
                (True, [single]) -> Interact verb single
                (True, parts)    -> ActionWithArgs verb parts
                (False, _)       -> Interact verb (unwords cleanParts)
        Nothing   -> Unknown input
    -- Bare custom verb with no object (e.g. "align", "pray", "accuse")
    [v] | Just verb <- parseVerbWith defs v -> Interact verb ""
    _ -> Unknown input


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


-- | Rogue Phase 3: locked doors reachable from the current room — via the
--   central 'effectiveConnections', so a dynamically set (`set_exit` with
--   `locked_by`) door is unlockable exactly like a static one.
reachableExitEntities :: GameState -> [String]
reachableExitEntities state = case getCurrentRoom state of
    Just room -> [normalizeText entity | Locked _ entity <- Map.elems (effectiveConnections state (roomId room))]
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
-- Command argument binding & parameterized verbs (Phase 1C)
-- ---------------------------------------------------------------------------

-- | Check if a verb is custom or genre-specific
isCustomVerb :: Verb -> Bool
isCustomVerb (VCustom _) = True
isCustomVerb _           = False

-- | Check if the world defines an OnCommand trigger for this verb.
hasOnCommandTrigger :: Verb -> GameState -> Bool
hasOnCommandTrigger verb state =
    let vName = verbCanonicalName verb
    in any (\td -> trEvent td == OnCommand vName) (triggerDefs (world state))

-- | Resolve target ID and target kind for command variables (Phase 2.2).
resolveCmdTarget :: Command -> GameState -> (String, String)
resolveCmdTarget cmd st = case cmd of
    Go dir ->
        case getExitInDirection dir st of
            Just exit -> (exitRoomID exit, "room")
            Nothing   -> (map toLower (show dir), "none")
    Interact verb target ->
        resolveTargetToPair verb target st
    InteractWith verb target _ ->
        resolveTargetToPair verb target st
    ActionWithArgs verb args ->
        if null args
        then ("", "none")
        else resolveTargetToPair verb (unwords args) st
    EquipCmd target ->
        resolveTargetToPair (VCustom "equip") target st
    UnequipCmd target ->
        resolveTargetToPair (VCustom "unequip") target st
    SearchCmd (Just target) ->
        resolveTargetToPair VSearch target st
    SearchCmd Nothing ->
        ("", "none")
    WatchCmd (Just target) ->
        resolveTargetToPair (VCustom "watch") target st
    WatchCmd Nothing ->
        ("", "none")
    EnterVehicleCmd target ->
        resolveTargetToPair (VCustom "enter") target st
    DriveToCmd target ->
        (target, "station")
    RefuelCmd target ->
        resolveTargetToPair (VCustom "refuel") target st
    RepairCmd target ->
        resolveTargetToPair (VCustom "repair") target st
    PlayCardCmd _ (Just target) ->
        resolveTargetToPair (VCustom "play") target st
    PlayCardCmd _ Nothing ->
        ("", "none")
    TakeAll ->
        ("all", "all")
    DropAll ->
        ("all", "all")
    UnequipAllCmd ->
        ("all", "all")
    ChooseCmd n ->
        (show n, "choice")
    Save s -> (s, "save")
    Load s -> (s, "save")
    _ -> ("", "none")
  where
    resolveTargetToPair v tgt s =
        case resolveTarget v tgt s of
            ResolvedItem iid    -> (iid, "item")
            ResolvedNPC nid     -> (nid, "npc")
            ResolvedVehicle vid -> (vid, "vehicle")
            ResolvedDevice did  -> (did, "device")
            Ambiguous _         -> (tgt, "ambiguous")
            NotFound _          -> (tgt, "none")
            BareVerb            -> ("", "none")

-- | Bind command arguments to cmd.* variables in GameState before trigger execution.
--   Sets:
--     cmd.verb        - String: canonical verb name
--     cmd.count       - Int: number of arguments
--     cmd.raw_args    - String: unparsed argument string
--     cmd.target      - String: resolved target ID or input
--     cmd.target_kind - String: "item", "npc", "vehicle", "room", "ambiguous", "none", etc.
--     cmd.arg1..N     - VVInt if parseable as Int, otherwise VVText
bindCommandVars :: Command -> GameState -> GameState
bindCommandVars cmd st =
    let (vName, rawArgs, argTokens) = extractCommandArgs cmd
        (targetVal, targetKindVal) = resolveCmdTarget cmd st
        countVal = length argTokens
        baseVars = [ ("cmd.verb", VVText vName)
                   , ("cmd.count", VVInt countVal)
                   , ("cmd.raw_args", VVText rawArgs)
                   , ("cmd.target", VVText targetVal)
                   , ("cmd.target_kind", VVText targetKindVal)
                   ]
        argVars = [ ("cmd.arg" ++ show i, parseArgVal tok)
                  | (i, tok) <- zip [1 :: Int ..] argTokens
                  ]
        allNewCmdVars = baseVars ++ argVars
        cleanedVars = Map.filterWithKey (\k _ -> not ("cmd.arg" `isPrefixOf` k)) (variables (save st))
        finalVars = foldl' (\vm (k, v) -> Map.insert k v vm) cleanedVars allNewCmdVars
    in st { save = (save st) { variables = finalVars } }
  where
    parseArgVal s = case reads s of
        [(n, [])] -> VVInt n
        _         -> VVText s

-- | Extract canonical verb name, raw argument string, and token list from a command.
extractCommandArgs :: Command -> (String, String, [String])
extractCommandArgs cmd = case cmd of
    ActionWithArgs v args -> (verbCanonicalName v, unwords args, args)
    Interact v target     -> (verbCanonicalName v, target, words target)
    InteractWith v t1 t2  -> (verbCanonicalName v, t1 ++ " " ++ t2, words (t1 ++ " " ++ t2))
    Go dir                -> ("go", show dir, [show dir])
    Look                  -> ("look", "", [])
    Inventory             -> ("inventory", "", [])
    StatsCmd              -> ("stats", "", [])
    JournalCmd            -> ("journal", "", [])
    SearchCmd Nothing     -> ("search", "", [])
    SearchCmd (Just t)    -> ("search", t, words t)
    WatchCmd Nothing      -> ("watch", "", [])
    WatchCmd (Just t)     -> ("watch", t, words t)
    MapCmd                -> ("map", "", [])
    TakeAll               -> ("take", "all", ["all"])
    DropAll               -> ("drop", "all", ["all"])
    OpenCmd t             -> ("open", t, words t)
    CloseCmd t            -> ("close", t, words t)
    LockCmd t             -> ("lock", t, words t)
    UnlockCmd t           -> ("unlock", t, words t)
    TakeFromCmd x y       -> ("take", x ++ " " ++ y, words (x ++ " " ++ y))
    PutInCmd x y          -> ("put", x ++ " " ++ y, words (x ++ " " ++ y))
    EquipCmd t            -> ("equip", t, words t)
    UnequipCmd t          -> ("unequip", t, words t)
    UnequipAllCmd         -> ("unequip", "all", ["all"])
    Save s                -> ("save", s, words s)
    Load s                -> ("load", s, words s)
    ListSaves             -> ("saves", "", [])
    Help                  -> ("help", "", [])
    Quit                  -> ("quit", "", [])
    Restart               -> ("restart", "", [])
    Undo                  -> ("undo", "", [])
    EnterVehicleCmd s     -> ("enter", s, words s)
    ExitVehicleCmd        -> ("exit", "", [])
    DriveToCmd s          -> ("drive", s, words s)
    WaitCmd               -> ("wait", "", [])
    RefuelCmd s           -> ("refuel", s, words s)
    RepairCmd s           -> ("repair", s, words s)
    AskCmd who what       -> ("ask", who, [who, what])
    TellCmd who what      -> ("tell", who, [who, what])
    ChooseCmd n           -> ("choose", show n, [show n])
    PlayCardCmd idx mT    -> ("play", show idx ++ maybe "" (" " ++) mT, show idx : maybe [] words mT)
    HandCmd               -> ("hand", "", [])
    DeckCmd               -> ("deck", "", [])
    DiscardCmd            -> ("discard", "", [])
    EndTurnCmd            -> ("end_turn", "", [])
    CompoundCommand _     -> ("compound", "", [])
    Unknown s             -> ("unknown", s, words s)

-- | Check if a command is vetoed by OnBefore triggers (Phase 2.2).
--   Returns Left (state, messages, consumesTurn) if blocked,
--   or Right (state, messages) to proceed.
checkBeforeVeto :: Command -> GameState -> Either (GameState, [OutputEvent], Bool) (GameState, [OutputEvent])
checkBeforeVeto cmd st =
    let stClean = st { lastVeto = Nothing }
        stWithVars = bindCommandVars cmd stClean
        (vName, _, _) = extractCommandArgs cmd
        (stAfterBefore, beforeMsgs) = fireTriggers (OnBefore vName) stWithVars
    in case lastVeto stAfterBefore of
        Just consumesTurn ->
            Left (stAfterBefore { lastVeto = Nothing }, beforeMsgs, consumesTurn)
        Nothing ->
            Right (stAfterBefore, beforeMsgs)

-- | Join before-trigger messages with command messages cleanly.
joinBeforeAndCmd :: [OutputEvent] -> [OutputEvent] -> [OutputEvent]
joinBeforeAndCmd before cmd
    | null (renderEvents before) = cmd
    | null (renderEvents cmd)    = before
    | otherwise =
        if lastIsNl before
        then before ++ cmd
        else joinEv before cmd
  where
    lastIsNl evs = case reverse evs of
        (EvText st : _) -> "\n" `isSuffixOf` stText st
        _               -> False

-- ---------------------------------------------------------------------------
-- Command execution
-- ---------------------------------------------------------------------------

-- | Execute a command and return updated game state and message (Phase 1.2).
--   Phase 2.2: checks OnBefore triggers first; if vetoed, command action is stopped.
executeCommandEv :: Command -> GameState -> CommandResultEv
executeCommandEv cmd state =
    case checkBeforeVeto cmd state of
        Left (stBlocked, msgs, _turn) ->
            (stBlocked, msgs)
        Right (stAfterBefore, beforeMsgs) ->
            let (stFinal, cmdMsgs) = dispatchCommandEv cmd stAfterBefore
            in (stFinal, joinBeforeAndCmd beforeMsgs cmdMsgs)

-- | Dispatch command execution without the OnBefore veto phase.


-- | 4.4: the display name of a container (item name, else the containers: name).
containerName :: String -> GameState -> String
containerName cid state =
    case lookupItem cid state of
        Just i  -> itemName i
        Nothing -> maybe cid conName (Map.lookup cid (containerDefs (world state)))

-- | 4.4: resolve a container target — an item with `capacity` (portable) or
--   a `containers:` entry (stationary).
findContainerRef :: String -> GameState -> Maybe String
findContainerRef targetStr state =
    case [ itemId i | i <- scopeItems, matchesItemTarget targetStr i ] of
        (iId : _) -> Just iId
        [] ->
            let ents = [ conId c
                       | c <- Map.elems (containerDefs (world state))
                       , conLocation c == currentRoom (save state)
                       , conName c == targetStr || conId c == targetStr ]
            in case ents of
                (eId : _) -> Just eId
                []        -> Nothing
  where
    scopeItems = visibleItemsAt (InRoom (currentRoom (save state))) state
                ++ getItemsInLocation (CarriedBy ActorPlayer) state

-- | 4.4: a scope item (visible through open containers) matching the target.
findScopeItem :: String -> GameState -> Maybe ItemID
findScopeItem targetStr state =
    case [ itemId i | i <- scopeItems, matchesItemTarget targetStr i ] of
        (iId : _) -> Just iId
        []        -> Nothing
  where
    scopeItems = visibleItemsAt (InRoom (currentRoom (save state))) state
                ++ getItemsInLocation (CarriedBy ActorPlayer) state

-- | 4.4: the container holds as many items as its capacity allows.
containerFull :: String -> GameState -> Bool
containerFull cid state =
    case containerCapacityOf cid state of
        Nothing -> False
        Just n  -> length (itemsInContainer cid state) >= n

-- | 4.4: the player carries as many items as the inventory limit allows
--   (`inventory.limit` in the VarMap — set by the world or `set_inventory_limit:`).
inventoryFull :: GameState -> Bool
inventoryFull state =
    case getVariable "inventory.limit" state of
        Just (VVInt n) | n >= 0 ->
            length (inventory (save state)) + Map.size (equipment (save state)) >= n
        _ -> False




dispatchCommandEv :: Command -> GameState -> CommandResultEv

dispatchCommandEv (Go dir) state
    | canMove dir state = case getExitInDirection dir state of
        Just (Open destinationRoom) ->
            let (st', hookMsg) = transitionToRoom destinationRoom (clearActiveDialogue state)
                fullMsg = joinAllEv [evMsg "move.ok" [("dir", show dir)], hookMsg]
            in (st', fullMsg)
        Just (Locked destinationRoom entityTarget)
            | getEntityState entityTarget state == Just "unlocked" ->
                let (st', hookMsg) = transitionToRoom destinationRoom (clearActiveDialogue state)
                    fullMsg = joinAllEv [evMsg "move.ok" [("dir", show dir)], hookMsg]
                in (st', fullMsg)
            | otherwise -> (state, evMsg "move.door_locked" [])
        Just (Guarded destinationRoom exitCond maybeMsg)
            | evalPredicate exitCond state ->
                let (st', hookMsg) = transitionToRoom destinationRoom (clearActiveDialogue state)
                    fullMsg = joinAllEv [evMsg "move.ok" [("dir", show dir)], hookMsg]
                in (st', fullMsg)
            | otherwise ->
                case maybeMsg of
                    Just msg -> (state, evRaw (formatWithVars msg state))
                    Nothing  -> (state, evMsg "move.blocked" [])
        Nothing -> (state, evMsg "move.no_exit" [])
    | otherwise = (state, evMsg "move.blocked" [])

dispatchCommandEv Look state = case getCurrentRoom state of
    Nothing -> (state, evMsg "look.void" [])
    Just room
        | isDark room state ->
            (state, darkRoomEv room)
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
                npcsHere = getNPCsInRoom (currentRoom (save state)) state
                livingHere = [ n | n <- npcsHere, not (isDeadNPC (npcId n) state) ]
                corpsesHere = [ n | n <- npcsHere, isDeadNPC (npcId n) state ]
                itemDesc = if null itemsInRoom
                           then evMsg "look.see_nothing" []
                           else evMsg "look.items" [("names", intercalate ", " (map itemName itemsInRoom))]
                -- 4.4: open containers show their contents (recursively through
                --   further open containers).
                openContainers =
                    [ (itemId i, itemName i)
                    | i <- itemsInRoom
                    , isContainer (itemId i) state
                    , containerStateOf (itemId i) state == "open" ]
                    ++
                    [ (conId c, conName c)
                    | c <- Map.elems (containerDefs (world state))
                    , conLocation c == currentRoom (save state)
                    , containerStateOf (conId c) state == "open" ]
                containerDesc = concat
                    [ case itemsInContainer cid state of
                        [] -> evMsg "container.empty" [("name", cname)]
                        contents -> evMsg "container.contains"
                            [("name", cname), ("items", intercalate ", " (map itemName contents))]
                    | (cid, cname) <- openContainers ]
                npcDesc = if null livingHere
                          then []
                          else evMsg "look.npcs" [("names", intercalate ", " (map npcName livingHere))]
                -- A body is nobody to talk to, but it is still lying there: it
                -- gets its own line, so `look at <name>` has something to point at.
                corpseDesc = case map npcName corpsesHere of
                    []  -> []
                    [c] -> evMsg "look.corpse_one" [("name", c)]
                    cs  -> evMsg "look.corpse_many" [("names", intercalate ", " cs)]
                (state', hookMsg) = runRoomHook roomOnLook (currentRoom (save state)) state
                vehicleMsg = vehicleLookAddon state'
                asciiArt = renderArtForLook (roomAscii room) state
                -- Phase 1.2: the room's art travels as a structured payload
                -- (hotspots for graphical frontends), the prose as messages.
                artFrags = if null asciiArt
                           then []
                           else [EvArt (ArtPayload asciiArt
                                   [ ArtHotspot i (hsGlyph h) (hsTarget h)
                                   | (i, h) <- zip [1 :: Int ..] (aaHotspots (roomAscii room)) ])]
                full = joinAllEv
                        [ artFrags, evRaw desc, itemDesc, containerDesc, npcDesc, corpseDesc, hookMsg,
                          maybe [] evRaw vehicleMsg ]
            in (state', full)

dispatchCommandEv Inventory state =
    let invItems = getItemsInLocation (CarriedBy ActorPlayer) state
    in if null invItems
       then (state, evMsg "inv.empty" [])
       else (state, evMsg "inv.header" [("items", intercalate ", " (map itemName invItems))])

dispatchCommandEv StatsCmd state =
    let p = player (save state)
        condList = filter (not . condHidden) (Map.elems (conditions (save state)))
        skillList = Map.toList (playerSkills p)
        skillDesc = if null skillList
                    then []
                    else evMsg "stats.skills" [("skills", intercalate ", " [n ++ " " ++ show v | (n, v) <- skillList])]
        condDesc = if null condList
                   then []
                   else evMsg "stats.conditions" [("conds", intercalate ", "
                        [ condName c ++ " (" ++ show (condRemaining c) ++ " turns)"
                        | c <- condList ])]
        progDesc = case progressionDef (world state) of
            Nothing   -> []
            Just prog ->
                let curLvl = getLevel state
                    curXp = getXp state
                    mCurDef = find (\l -> lvlNumber l == curLvl) (progLevels prog)
                    lvlTitle = maybe ("Level " ++ show curLvl) lvlName mCurDef
                    mNextDef = find (\l -> lvlNumber l == curLvl + 1) (progLevels prog)
                in case mNextDef of
                    Just nextLvl ->
                        evMsg "stats.progression"
                            [ ("level", show curLvl)
                            , ("name", lvlTitle)
                            , ("xp", show curXp)
                            , ("next", show (lvlXp nextLvl))
                            ]
                    Nothing ->
                        evMsg "stats.progression_max"
                            [ ("level", show curLvl)
                            , ("name", lvlTitle)
                            , ("xp", show curXp)
                            ]
        -- Byte-identical to the former unlines: every line gets its "\n" —
        -- including the last, and the 4th piece concatenates without one.
        msg = unlinesEv $ catMaybes
            [ if null progDesc then Nothing else Just progDesc
            , Just (evMsg "stats.health" [("hp", show (playerHealth p)), ("max", show (effectiveMaxHealth state))])
            , Just (evMsg "stats.attack" [("atk", show (effectiveAttack state)), ("base", show (playerAttack p))])
            , Just (evMsg "stats.defense" [("def", show (effectiveDefense state)), ("base", show (playerDefense p))])
            , Just (joinAllEv [skillDesc, condDesc, evRaw (equipmentSummary state)])
            ]
    in (state, msg)

dispatchCommandEv JournalCmd state = (state, journalTextEv state)
dispatchCommandEv Undo state = (state, evMsg "undo.nothing" [])

-- Card & Deck commands (Phase 2B)
dispatchCommandEv (PlayCardCmd idx target) state = playCard idx target state
dispatchCommandEv HandCmd state = showHand state
dispatchCommandEv DeckCmd state = showDeck state
dispatchCommandEv DiscardCmd state = showDiscard state
dispatchCommandEv EndTurnCmd state = endTurn state

dispatchCommandEv (AskCmd who what) state = talkTopic who what state
dispatchCommandEv (TellCmd who what) state = talkTopic who what state

dispatchCommandEv (ChooseCmd idx) state =
    case activeDialogue (save state) of
        Nothing -> (state, evMsg "dialogue.none_active" [])
        Just nId -> case Map.lookup nId (npcDefs (world state)) of
            Nothing -> (clearActiveDialogue state, evMsg "dialogue.partner_gone" [])
            Just npc ->
                let st = Map.lookup nId (npcStates (save state))
                    status = maybe "alive" npcStatus st
                in case Map.lookup status (npcDialogueTrees npc) of
                    Nothing -> (clearActiveDialogue state, evMsg "dialogue.nothing_more" [("name", npcName npc)])
                    Just tree ->
                        let nodeId = fromMaybe (dtEntry tree) (st >>= npcDialogueNode)
                        in case Map.lookup nodeId (dtNodes tree) of
                            Nothing -> (clearActiveDialogue state, evMsg "dialogue.nothing_more" [("name", npcName npc)])
                            Just node ->
                                let choices = visibleChoices state node
                                in if idx < 1 || idx > length choices
                                   then (state, evMsg "choice.invalid" [("max", show (length choices))])
                                   else
                                       let choice = choices !! (idx - 1)
                                           outcome = dcOutcome choice
                                           (stateAfterOutcome, outcomeMsg) = applyOutcomeEv outcome nId state
                                       in case dcNextNode choice of
                                           Nothing ->
                                               -- Dialogue ends
                                               let stateFinal = clearActiveDialogue (setDialogueNode nId Nothing stateAfterOutcome)
                                                   msg = if null (renderEvents outcomeMsg)
                                                         then evMsg "dialogue.ended" []
                                                         else outcomeMsg
                                               in (stateFinal, msg)
                                           Just nextNodeId ->
                                               let stateWithNext = setDialogueNode nId (Just nextNodeId) stateAfterOutcome
                                                   st' = Map.lookup nId (npcStates (save stateWithNext))
                                                   (stateFinal, nextDialogue) = renderDialogue npc tree st' stateWithNext
                                                   fullMsg = if null (renderEvents outcomeMsg)
                                                             then nextDialogue
                                                             else outcomeMsg ++ nl2 ++ nextDialogue
                                               in (stateFinal, fullMsg)

dispatchCommandEv (EquipCmd targetStr) state =
    let stateWithVars = bindCommandVars (EquipCmd targetStr) state
    in case resolveTarget (VCustom "equip") targetStr stateWithVars of
        TargetItem iid ->
            case Map.lookup iid (itemDefs (world stateWithVars)) of
                Just item ->
                    case equipItem iid stateWithVars of
                        Left err     -> (stateWithVars, evRaw err)
                        Right state' -> (state', evMsg "equip.ok" [("item", itemName item)])
                Nothing -> (stateWithVars, evMsg "target.not_carried" [("target", targetStr)])
        TargetAmbiguous ids ->
            let equippableIds = filter (\i -> maybe False (isJust . itemEquipSlot) (Map.lookup i (itemDefs (world stateWithVars)))) ids
            in case equippableIds of
                [singleEquippable] ->
                    case Map.lookup singleEquippable (itemDefs (world stateWithVars)) of
                        Just item ->
                            case equipItem singleEquippable stateWithVars of
                                Left err     -> (stateWithVars, evRaw err)
                                Right state' -> (state', evMsg "equip.ok" [("item", itemName item)])
                        Nothing -> interactAmbiguous ids stateWithVars
                (e1:e2:es) -> interactAmbiguous (e1:e2:es) stateWithVars
                []         -> interactAmbiguous ids stateWithVars
        _                   -> (stateWithVars, evMsg "target.not_carried" [("target", targetStr)])

dispatchCommandEv (UnequipCmd targetStr) state =
    let stateWithVars = bindCommandVars (UnequipCmd targetStr) state
    in case resolveTarget (VCustom "unequip") targetStr stateWithVars of
        TargetItem iid ->
            case Map.lookup iid (itemDefs (world stateWithVars)) of
                Just item
                    | isEquipped iid stateWithVars -> (unequipItem iid stateWithVars, evMsg "unequip.ok" [("item", itemName item)])
                    | otherwise                    -> (stateWithVars, evMsg "unequip.not_equipped" [("item", itemName item)])
                Nothing -> (stateWithVars, evMsg "target.not_carried" [("target", targetStr)])
        TargetAmbiguous ids ->
            let equippedIds = filter (`isEquipped` stateWithVars) ids
            in case equippedIds of
                [singleEquipped] ->
                    case Map.lookup singleEquipped (itemDefs (world stateWithVars)) of
                        Just item -> (unequipItem singleEquipped stateWithVars, evMsg "unequip.ok" [("item", itemName item)])
                        Nothing   -> interactAmbiguous ids stateWithVars
                (e1:e2:es)       -> interactAmbiguous (e1:e2:es) stateWithVars
                []               -> interactAmbiguous ids stateWithVars
        _                   -> (stateWithVars, evMsg "target.not_carried" [("target", targetStr)])

dispatchCommandEv UnequipAllCmd state
    | Map.null (equipment (save state)) = (state, evMsg "equip.nothing" [])
    | otherwise = (state { save = (save state) { equipment = Map.empty } }, evMsg "unequip.all" [])

dispatchCommandEv TakeAll state = case getCurrentRoom state of
    Nothing -> (state, evMsg "take.none_here" [])
    Just room ->
        let inRoom = getItemsInLocation (InRoom (currentRoom (save state))) state
            -- Phase 0.3 (B3): in the dark only feelable items can be picked up.
            roomItems = if isDark room state then filter itemIsFeelable inRoom else inRoom
        in if null roomItems
           then (state, if isDark room state
                        then darkRoomEv room
                        else evMsg "take.none_here" [])
           else let (finalState, msgs) = foldl' (\(s, ms) item ->
                            let (s', m) = executeCommandEv (Interact VTake (itemId item)) s
                            in (s', ms ++ [m])) (state, []) roomItems
                    in (finalState, evIntercalate msgs)

dispatchCommandEv DropAll state =
    let invItems = getItemsInLocation (CarriedBy ActorPlayer) state
    in if null invItems
       then (state, evMsg "drop.nothing" [])
       else let (finalState, msgs) = foldl' (\(s, ms) item ->
                    let (s', m) = executeCommandEv (Interact VDrop (itemId item)) s
                    in (s', ms ++ [m])) (state, []) invItems
            in (finalState, evIntercalate msgs)

dispatchCommandEv (CompoundCommand cmds) state =
    foldl' (\(s, msgs) cmd ->
        let (s', msg) = executeCommandEv cmd s
        in (s', joinEv msgs msg)
    ) (state, []) cmds

dispatchCommandEv (SearchCmd maybeTarget) state = case getCurrentRoom state of
    Nothing -> (state, evMsg "search.void" [])
    Just room
        | isDark room state
        , not (maybe False (targetIsFeelable state) maybeTarget) ->
            (state, darkRoomEv room)
        | otherwise ->
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
                                Just outcome -> applyOutcomeEv outcome iId state
                                Nothing -> (state, evMsg "search.nothing_item" [("item", itemName item)])
                        Nothing -> case find (matchesNPCTarget targetStr) roomNPCs of
                            Just npc ->
                                let nId = npcId npc
                                    currentStatus = maybe "unknown" npcStatus (Map.lookup nId (npcStates (save state)))
                                in case Map.lookup (VSearch, currentStatus) (npcVerbMap npc) of
                                    Just outcome -> applyOutcomeEv outcome nId state
                                    Nothing -> (state, evMsg "search.nothing_npc" [("npc", npcName npc)])
                            Nothing -> (state, evMsg "target.not_seen" [("target", targetStr)])

dispatchCommandEv (WatchCmd maybeTarget) state = case getCurrentRoom state of
    Nothing -> (state, evMsg "watch.void" [])
    Just room
        | isDark room state -> (state, darkRoomEv room)
        | otherwise -> case maybeTarget of
            Nothing -> watchArt (roomAscii room) "the room"
            Just targetStr ->
                case find (matchesItemTarget targetStr) allReachableItems of
                    Just item -> watchArt (itemAscii item) (itemName item)
                    Nothing -> case find (matchesNPCTarget targetStr) roomNPCs of
                        Just npc -> watchArt (npcAscii npc) (npcName npc)
                        Nothing  -> (state, evMsg "target.not_seen" [("target", targetStr)])
  where
    allReachableItems = getItemsInLocation (InRoom (currentRoom (save state))) state
                        ++ getItemsInLocation (CarriedBy ActorPlayer) state
    roomNPCs = getNPCsInRoom (currentRoom (save state)) state
    watchArt art label = case asciiPlayback art state of
        ([], _)            -> (state, evMsg "watch.nothing" [("label", label)])
        (frames, micros) ->
            (state { pendingAnimation = Just (frames, micros) },
             evMsg "watch.start" [("label", label)])

dispatchCommandEv MapCmd state = case getCurrentRoom state of
    Nothing -> (state, evMsg "map.void" [])
    Just room
        | isDark room state -> (state, darkRoomEv room)
        | otherwise ->
            let art = roomAscii room
                spots = aaHotspots art
            in if null spots
               then (state, evMsg "map.no_marks" [])
               else
                   let numbered = foldl' (\s (i, h) -> replaceChar (hsGlyph h) (show i) s)
                                          (resolveAsciiArt art state) (zip [1 :: Int ..] spots)
                       legendLines = [ evMsg "map.legend_line" [("i", show i), ("label", hotspotLabel state h)]
                                     | (i, h) <- zip [1 :: Int ..] spots ]
                       -- Byte-identical: numbered ++ "\n\nLegend:\n" ++ unlines legend
                       mapFrags = [EvArt (ArtPayload numbered
                                      [ ArtHotspot i (hsGlyph h) (hsTarget h)
                                      | (i, h) <- zip [1 :: Int ..] spots ])]
                                  ++ nl2 ++ evRaw "Legend:\n" ++ unlinesEv legendLines
                   in (state, mapFrags)

dispatchCommandEv (ActionWithArgs verb args) state =
    let stateWithVars = bindCommandVars (ActionWithArgs verb args) state
    in dispatchCommandEv (Interact verb (unwords args)) stateWithVars

-- | 4.4: container verbs — open / close / lock / unlock.
dispatchCommandEv (OpenCmd t) state =
    case findContainerRef t state of
        Nothing -> (state, evMsg "container.not_a_container" [("target", t)])
        Just cid -> case containerStateOf cid state of
            "locked" -> (state, evMsg "container.is_locked" [("name", containerName cid state)])
            "open"   -> (state, evMsg "container.already_open" [("name", containerName cid state)])
            _        -> (setEntityState cid "open" state
                        , evMsg "container.opened" [("name", containerName cid state)])

dispatchCommandEv (CloseCmd t) state =
    case findContainerRef t state of
        Nothing -> (state, evMsg "container.not_a_container" [("target", t)])
        Just cid -> case containerStateOf cid state of
            "locked" -> (state, evMsg "container.is_locked" [("name", containerName cid state)])
            "closed" -> (state, evMsg "container.already_closed" [("name", containerName cid state)])
            _        -> (setEntityState cid "closed" state
                        , evMsg "container.closed" [("name", containerName cid state)])

dispatchCommandEv (LockCmd t) state =
    case findContainerRef t state of
        Nothing -> (state, evMsg "container.not_a_container" [("target", t)])
        Just cid -> case containerStateOf cid state of
            "locked" -> (state, evMsg "container.is_locked" [("name", containerName cid state)])
            _        -> (setEntityState cid "locked" state
                        , evMsg "container.locked" [("name", containerName cid state)])

dispatchCommandEv (UnlockCmd t) state =
    case findContainerRef t state of
        Nothing -> (state, evMsg "container.not_a_container" [("target", t)])
        Just cid -> case containerStateOf cid state of
            "locked" -> (setEntityState cid "closed" state
                        , evMsg "container.unlocked" [("name", containerName cid state)])
            _        -> (state, evMsg "container.not_locked" [("name", containerName cid state)])

-- | 4.4: `take X from Y` — one item out of an open container (the item is
--   looked up inside Y, so a closed container reports "closed", not "missing").
dispatchCommandEv (TakeFromCmd x y) state =
    case findContainerRef y state of
        Nothing -> (state, evMsg "container.not_a_container" [("target", y)])
        Just cid
            | not (containerChainOpen cid state) ->
                (state, evMsg "container.is_locked" [("name", containerName cid state)])
            | containerStateOf cid state /= "open" ->
                (state, evMsg "container.is_closed" [("name", containerName cid state)])
            | otherwise ->
                case [ itemId i | i <- itemsInContainer cid state, matchesItemTarget x i ] of
                    [] -> (state, evMsg "container.no_item" [("item", x), ("name", containerName cid state)])
                    (iId : _)
                        | inventoryFull state -> (state, evMsg "inventory.full" [])
                        | otherwise ->
                            ( relocateItem iId (CarriedBy ActorPlayer) state
                            , evMsg "container.took_from"
                                [ ("item", x), ("name", containerName cid state) ] )

-- | 4.4: `put X in Y` — one item into an open container (capacity checked).
dispatchCommandEv (PutInCmd x y) state =
    case findContainerRef y state of
        Nothing -> (state, evMsg "container.not_a_container" [("target", y)])
        Just cid
            | not (containerChainOpen cid state) ->
                (state, evMsg "container.is_locked" [("name", containerName cid state)])
            | containerStateOf cid state /= "open" ->
                (state, evMsg "container.is_closed" [("name", containerName cid state)])
            | containerFull cid state ->
                (state, evMsg "container.full" [("name", containerName cid state)])
            | otherwise ->
                case findScopeItem x state of
                    Nothing -> (state, evMsg "container.no_item" [("item", x), ("name", y)])
                    Just iId ->
                        ( relocateItem iId (InContainer cid) state
                        , evMsg "container.put"
                            [ ("item", x), ("name", containerName cid state) ] )

dispatchCommandEv (Interact verb targetStr) state =
    let stateWithVars = bindCommandVars (Interact verb targetStr) state
    in case resolveInteractTarget verb targetStr stateWithVars of
        ITItem item mSt
            | isDarkRestricted verb
            , Just room <- getCurrentRoom stateWithVars
            , isDark room stateWithVars
            , not (hasItem (itemId item) stateWithVars)
            , not (itemIsFeelable item) ->
                (stateWithVars, darkRoomEv room)
            | otherwise ->
                interactItem verb item mSt targetStr stateWithVars
        ITNpc npc mSt
            | isDarkRestricted verb
            , Just room <- getCurrentRoom stateWithVars
            , isDark room stateWithVars ->
                (stateWithVars, darkRoomEv room)
            | otherwise ->
                interactNpc verb npc mSt targetStr stateWithVars
        ITVehicle veh
            | isDarkRestricted verb
            , Just room <- getCurrentRoom stateWithVars
            , isDark room stateWithVars ->
                (stateWithVars, darkRoomEv room)
            | otherwise ->
                interactVehicle verb veh targetStr stateWithVars
        ITDevice dev
            | isDarkRestricted verb
            , Just room <- getCurrentRoom stateWithVars
            , isDark room stateWithVars ->
                (stateWithVars, darkRoomEv room)
            | otherwise ->
                interactDevice verb dev targetStr stateWithVars
        ITBareVerb
            | isDarkRestricted verb
            , Just room <- getCurrentRoom stateWithVars
            , isDark room stateWithVars ->
                (stateWithVars, darkRoomEv room)
            | otherwise ->
                interactBare verb stateWithVars
        ITNotFound str
            | isDarkRestricted verb
            , Just room <- getCurrentRoom stateWithVars
            , isDark room stateWithVars ->
                (stateWithVars, darkRoomEv room)
            | otherwise ->
                interactNotFound verb str stateWithVars
        ITAmbiguous ids
            | isDarkRestricted verb
            , Just room <- getCurrentRoom stateWithVars
            , isDark room stateWithVars
            , not (all (itemReachableInDark stateWithVars) ids) ->
                (stateWithVars, darkRoomEv room)
            | otherwise ->
                interactAmbiguous ids stateWithVars

dispatchCommandEv (InteractWith VUseOn itemStr entityStr) state =
    let itemTarget = normalizeText itemStr
        entityTarget = normalizeText entityStr
        inventoryItems = getItemsInLocation (CarriedBy ActorPlayer) state
        maybeItem = find (matchesItemTarget itemTarget) inventoryItems
        maybeVehicle = findVehicle entityStr state
        entityInInventory = any (matchesItemTarget entityTarget) inventoryItems
        -- Phase 0.3 (B3): a feelable room entity stays usable in the dark.
        entityIsFeelable = any (\i -> matchesItemTarget entityTarget i && itemIsFeelable i)
                               (getItemsInLocation (InRoom (currentRoom (save state))) state)
    in case maybeItem of
        Nothing -> (state, evMsg "use.not_carried" [("item", itemStr)])
        Just item
            | Just room <- getCurrentRoom state
            , isDark room state
            , not entityInInventory
            , not entityIsFeelable ->
                (state, darkRoomEv room)
            | otherwise ->
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
                        in (state', evRaw msg)
                    Nothing
                        | isLivingNPCInRoom entityTarget state ->
                            dispatchCommandEv (Interact VAttack entityTarget) state
                        | otherwise ->
                            case tryItemOnItem (itemId item) entityTarget state of
                                Just result -> result
                                Nothing -> tryRefuelByItem item state
            else
                if isLivingNPCInRoom entityTarget state
                then dispatchCommandEv (Interact VAttack entityTarget) state
                else case tryItemOnItem (itemId item) entityTarget state of
                        Just result -> result
                        Nothing ->
                            case maybeVehicle of
                                Just _ -> tryRefuelByItem item state
                                Nothing -> (state, evMsg "use.unreachable" [("entity", entityStr)])
  where
    -- Vehicle refuelling: `use <fuel item> on <vehicle>` adds the item's
    -- "fuel" prop value (default 1) to the vehicle's tank, consuming the item.
    tryRefuelByItem item st =
        let st1 = if hasItem (itemId item) st then st else dropItem (itemId item) st
        in case findVehicleTarget st of
            Just vId ->
                let amount = fromMaybe 1 (Map.lookup "fuel" . itemProps =<< Map.lookup (itemId item) (itemStates (save st)))
                in case refuelVehicle vId amount st1 of
                    Nothing -> (st1, evMsg "refuel.not_needed" [("vehicle", entityStr)])
                    Just (st2, fuelMsgs) ->
                        (consumeItem (itemId item) st2,
                         joinEv (evMsg "use.ok" [("item", itemName item)]) fuelMsgs)
            Nothing -> (st1, evMsg "use.nothing" [])
    findVehicleTarget st = case currentVehicle (save st) of
        Just vId | isJust (lookupVehicle vId st) -> Just vId
        _ -> case findVehicle entityStr st of
            Just v -> Just (vehicleId v)
            Nothing -> Nothing

dispatchCommandEv (InteractWith _ _ _) state = (state, evMsg "use.nothing" [])

dispatchCommandEv Restart state = (state, [])
dispatchCommandEv ListSaves state = (state, [])

dispatchCommandEv Help state = (state, evMsg "help.text" [])
dispatchCommandEv Quit state = (endGame (Custom "quit") state, evMsg "quit.bye" [])
dispatchCommandEv (Unknown cmd) state = (state, evMsg "parse.unknown" [("input", cmd)])
dispatchCommandEv (Save _) state = (state, [])
dispatchCommandEv (Load _) state = (state, [])


-- ---------------------------------------------------------------------
-- Vehicles (Phase 3)
-- ---------------------------------------------------------------------

dispatchCommandEv (EnterVehicleCmd targetStr) state =
    case findVehicle targetStr state of
        Nothing -> (state, evMsg "enter.not_seen" [("target", targetStr)])
        Just v -> case enterVehicle (vehicleId v) state of
            Left err -> (state, evRaw err)
            Right (st', msg) -> (st', msg)

dispatchCommandEv ExitVehicleCmd state =
    case exitVehicle state of
        Left err -> (state, evRaw err)
        Right (st', msg) -> (st', msg)

dispatchCommandEv (DriveToCmd targetStr) state =
    case currentVehicle (save state) of
        Nothing -> (state, evMsg "vehicle.not_in" [])
        Just vId -> case driveVehicle vId targetStr state of
            Left err -> (state, evRaw err)
            Right (st', msg) -> (st', msg)

dispatchCommandEv WaitCmd state =
    case advanceVehicleRoute state of
        Left err -> (state, evRaw err)
        Right (st', msg) -> (st', msg)

-- | Refuel: `refuel` or `refuel <vehicle>`. Actual fuelling happens via
--   `use <fuel item> on <vehicle>`; plain `refuel` reports the status.
dispatchCommandEv (RefuelCmd targetStr) state =
    let v = if null targetStr
            then currentVehicle (save state) >>= \vId -> lookupVehicle vId state
            else findVehicle targetStr state
    in case v of
        Nothing -> (state, evMsg "refuel.no_vehicle" [])
        Just veh ->
            let vId = vehicleId veh
                vState = getVehicleState vId state
                fuelStatus = case (vehicleFuelProp veh, vsFuel vState) of
                    (Nothing, _) -> evMsg "refuel.not_needed" [("vehicle", vehicleName veh)]
                    (Just fs, Just f) ->
                        evMsg "fuel.status" [("vehicle", vehicleName veh), ("item", fsItem fs), ("f", show f), ("max", show (fsMax fs))]
                    (Just fs, Nothing) ->
                        evMsg "fuel.status_zero" [("vehicle", vehicleName veh), ("item", fsItem fs), ("max", show (fsMax fs))]
            in (state, fuelStatus)

-- | Repair: `repair <condition>` clears a matching vehicle condition on the
--   current vehicle.
dispatchCommandEv (RepairCmd targetStr) state =
    case currentVehicle (save state) of
        Nothing -> (state, evMsg "vehicle.not_in" [])
        Just vId ->
            let vState = getVehicleState vId state
                target = normalizeText targetStr
                v = lookupVehicle vId state
                matches = [ c | c <- Set.toList (vsActiveConditions vState)
                          , target `elem` [map toLower c, "the " ++ map toLower c] ]
                vName = maybe vId vehicleName v
            in if null matches
               then (state, joinAllEv
                            [ evMsg "repair.nothing_broken" [("vehicle", vName), ("target", targetStr)]
                            , if Set.null (vsActiveConditions vState)
                                then [] else evMsg "repair.problems" [("list", intercalate ", " (Set.toList (vsActiveConditions vState)))] ])
               else let c = head matches
                        st' = clearVehicleCondition vId c state
                    in (st', evMsg "repair.ok" [("vehicle", vName), ("problem", c)])

-- | Resolve a vehicle by name/keyword among all vehicles in the world
findVehicle :: String -> GameState -> Maybe VehicleDef
findVehicle targetStr state =
    let target = normalizeText targetStr
        aliases v = nub (map normalizeText (vehicleId v : vehicleName v : vehicleKeywords v))
    in find (\v -> target `elem` aliases v) (Map.elems (vehicleDefs (world state)))

-- ---------------------------------------------------------------------------
-- Helpers used by executeCommand
-- ---------------------------------------------------------------------------

-- | Target resolved for an interaction command (Phase R3).
data InteractTarget
    = ITItem    ItemDef    (Maybe ItemState)  -- ^ Item in current room or inventory
    | ITNpc     NPCDef     (Maybe NPCState)   -- ^ NPC in current room
    | ITVehicle VehicleDef                    -- ^ Vehicle attack candidate
    | ITDevice  DeviceDef                     -- ^ Device in current room (W4)
    | ITBareVerb                              -- ^ Bare verb with no target (e.g. defend, flee, custom command)
    | ITNotFound String                       -- ^ Target not found
    | ITAmbiguous [String]                    -- ^ Target is ambiguous between multiple candidates
    deriving (Show, Eq)

-- | Result of central target resolution (Phase 0.1).
data TargetResolution
    = ResolvedItem String       -- ^ Item ID
    | ResolvedNPC String        -- ^ NPC ID
    | ResolvedVehicle String    -- ^ Vehicle ID
    | ResolvedDevice String     -- ^ Device ID (W4)
    | Ambiguous [String]        -- ^ Candidate IDs when ambiguous
    | NotFound String           -- ^ Target string not found
    | BareVerb                  -- ^ Bare verb without target
    deriving (Show, Eq)

pattern TargetItem :: String -> TargetResolution
pattern TargetItem x = ResolvedItem x

pattern TargetVehicle :: String -> TargetResolution
pattern TargetVehicle x = ResolvedVehicle x

pattern TargetAmbiguous :: [String] -> TargetResolution
pattern TargetAmbiguous xs = Ambiguous xs

pattern TargetNotFound :: String -> TargetResolution
pattern TargetNotFound s = NotFound s

pattern TargetBare :: TargetResolution
pattern TargetBare = BareVerb

-- | Check if a target string matches a device definition by ID, name, or keywords (W4).
matchesDeviceTarget :: String -> DeviceDef -> Bool
matchesDeviceTarget tgt d
    | null (words tgt) = False
    | otherwise        = normalizeText tgt `elem` aliases
  where
    aliases = nub (map normalizeText (devId d : devName d : devKeys d))

-- | Check if a target string matches a vehicle definition by ID, name, or keywords.
matchesVehicleTarget :: String -> VehicleDef -> Bool
matchesVehicleTarget tgt v
    | null (words tgt) = False
    | otherwise        = normalizeText tgt `elem` aliases
  where
    aliases = nub (map normalizeText (vehicleId v : vehicleName v : vehicleKeywords v))

-- | Check if a verb prioritizes inventory over room entities during target resolution (Phase 0.2, Bug B2).
preferInventoryTarget :: Verb -> Bool
preferInventoryTarget verb = case verbCanonicalName verb of
    "drop"    -> True
    "use"     -> True
    "equip"   -> True
    "wear"    -> True
    "wield"   -> True
    "unequip" -> True
    "remove"  -> True
    _         -> False

-- | Central target resolution (Phase 0.1, Phase 0.2).
--   Resolves an interaction verb's target string against reachable entities
--   with verb-dependent search order (Phase 0.2, Bug B2):
--   - 'drop', 'use', 'equip' prioritize inventory items first, falling back to room.
--   - 'take' prioritizes room items first, falling back to inventory.
--   - other verbs prioritize room entities first, falling back to inventory.
--   Phase 2.3: when 'chosenTarget' is set (the player answered a disambiguation
--   question), that entity id wins — the answer selects an entity, not a string,
--   so a candidate whose id is also another candidate's keyword resolves too.
resolveTarget :: Verb -> String -> GameState -> TargetResolution
resolveTarget verb targetStr state
    | Just chosen <- chosenTarget state = chosenResolution chosen state
    | null (words targetStr) = BareVerb
    | otherwise =
        let resolved = resolveHotspotTarget targetStr state
            roomItems = visibleItemsAt (InRoom (currentRoom (save state))) state
            invItems  = getItemsInLocation (CarriedBy ActorPlayer) state
            roomNPCs  = getNPCsInRoom (currentRoom (save state)) state
            roomDevices = filter (\d -> devLocation d == currentRoom (save state))
                                 (Map.elems (deviceDefs (world state)))

            matchingRoomItems = filter (matchesItemTarget resolved) roomItems
            matchingInvItems  = filter (matchesItemTarget resolved) invItems
            matchingNPCs      = filter (matchesNPCTarget resolved) roomNPCs
            matchingVehicles  = if verb == VAttack
                                then filter (\v -> matchesVehicleTarget resolved v || matchesVehicleTarget targetStr v)
                                            (Map.elems (vehicleDefs (world state)))
                                else []
            matchingDevices   = filter (matchesDeviceTarget resolved) roomDevices

            roomCandidateIds = nub (map itemId matchingRoomItems ++ map npcId matchingNPCs ++ map vehicleId matchingVehicles ++ map devId matchingDevices)
            invCandidateIds  = nub (map itemId matchingInvItems)

            (primaryCandidates, secondaryCandidates) =
                if preferInventoryTarget verb || (isCurrentRoomDark state && verbCanonicalName verb == "examine")
                then (invCandidateIds, roomCandidateIds)
                else (roomCandidateIds, invCandidateIds)

            allCandidates = if null primaryCandidates
                            then secondaryCandidates
                            else primaryCandidates

            allVehIds = map vehicleId matchingVehicles
            allNpcIds = map npcId matchingNPCs
            allDevIds = map devId matchingDevices
        in case allCandidates of
            [] -> NotFound targetStr
            [singleId]
                | singleId `elem` allVehIds -> ResolvedVehicle singleId
                | singleId `elem` allNpcIds -> ResolvedNPC singleId
                | singleId `elem` allDevIds -> ResolvedDevice singleId
                | otherwise                 -> ResolvedItem singleId
            _  -> Ambiguous allCandidates

-- | Resolve what entity an interaction verb is targeting.
--   Thin adapter delegating to central 'resolveTarget'.
resolveInteractTarget :: Verb -> String -> GameState -> InteractTarget
resolveInteractTarget verb targetStr state = case resolveTarget verb targetStr state of
    ResolvedItem iid ->
        case Map.lookup iid (itemDefs (world state)) of
            Just item ->
                let mSt = Map.lookup iid (itemStates (save state))
                in ITItem item mSt
            Nothing   -> ITNotFound targetStr
    ResolvedNPC nid ->
        case Map.lookup nid (npcDefs (world state)) of
            Just npc ->
                let mSt = Map.lookup nid (npcStates (save state))
                in ITNpc npc mSt
            Nothing  -> ITNotFound targetStr
    ResolvedVehicle vid ->
        case Map.lookup vid (vehicleDefs (world state)) of
            Just veh -> ITVehicle veh
            Nothing  -> ITNotFound targetStr
    ResolvedDevice did ->
        case Map.lookup did (deviceDefs (world state)) of
            Just dev -> ITDevice dev
            Nothing  -> ITNotFound targetStr
    Ambiguous ids -> ITAmbiguous ids
    NotFound s    -> ITNotFound s
    BareVerb      -> ITBareVerb

-- | Execute interaction on an item.
interactItem :: Verb -> ItemDef -> Maybe ItemState -> String -> GameState -> (GameState, [OutputEvent])
interactItem verb item maybeItemState targetStr state =
    let iId = itemId item
        currentStatus = maybe "unknown" itemStatus maybeItemState
        notCarried = maybe True (\loc -> loc /= CarriedBy ActorPlayer) (fmap itemLocation maybeItemState)
        vmLookup = Map.lookup (verb, currentStatus) (itemVerbMap item)
    in case (verb, vmLookup) of
        -- Taking: enforce portability, then pick up AND run on_take.
        (VTake, _)
            | not notCarried ->
                (state, evMsg "take.already" [("item", itemName item)])
            | otherwise ->
                case itemPortable item of
                    False -> (state, fromMaybe (evMsg "take.not_portable" [("item", itemName item)])
                                             (fmap evRaw (itemTakeFailure item)))
                    True
                        | inventoryFull state ->
                            (state, evMsg "inventory.full" [])
                        | otherwise ->
                        let (st', extra) = case vmLookup of
                                Just outcome -> applyOutcomeEv outcome iId state
                                Nothing      -> (state, [])
                            takeMsg = evMsg "take.ok" [("item", itemName item)]
                        in (pickupItem iId st',
                            joinEv takeMsg extra)
        _ -> case vmLookup of
            Just outcome -> applyOutcomeEv outcome iId state
            Nothing ->
                if verb == VDrop && hasItem iId state
                then (dropItem iId state, evMsg "drop.ok" [("item", itemName item)])
                else if verb == VLookAt
                then (state, lookWithArtEv (itemAscii item) state (resolveCondText (itemDescription item) state))
                else case if verb == VAttack then tryAttackVehicle targetStr state else Nothing of
                    Just res -> res
                    Nothing
                        | hasOnCommandTrigger verb state -> (state, [])
                        | otherwise -> (state, evMsg "item.cant_do" [("item", itemName item)])

-- | Execute interaction on an NPC.
interactNpc :: Verb -> NPCDef -> Maybe NPCState -> String -> GameState -> (GameState, [OutputEvent])
interactNpc verb npc maybeNpcState targetStr state =
    let nId = npcId npc
        currentStatus = maybe "unknown" npcStatus maybeNpcState
        isCorpse = isDeadNPC nId state
    in case Map.lookup (verb, currentStatus) (npcVerbMap npc) of
        Just outcome -> applyOutcomeEv outcome nId state
        Nothing
            -- A body can be looked at, searched and targeted by authored
            -- verbs, but it neither fights nor talks.
            | isCorpse, verb == VAttack ->
                (state, evMsg "npc.already_dead" [("npc", npcName npc)])
            | isCorpse, verb == VTalk ->
                (state, evMsg "npc.dead_silent" [("npc", npcName npc)])
            | verb == VTalk -> talkTo npc maybeNpcState state
            | verb == VAttack -> executeAttack npc maybeNpcState targetStr state
            | verb == VLookAt -> (state, lookWithArtEv (npcAscii npc) state (resolveCondText (npcDescription npc) state))
            | hasOnCommandTrigger verb state -> (state, [])
            | otherwise -> (state, evMsg "npc.cant_do" [("npc", npcName npc)])

-- | Execute interaction on a vehicle.
interactVehicle :: Verb -> VehicleDef -> String -> GameState -> (GameState, [OutputEvent])
interactVehicle verb _veh targetStr state
    | hasOnCommandTrigger verb state = (state, [])
    | verb == VAttack, Just res <- tryAttackVehicle targetStr state = res
    | otherwise = (state, evMsg "target.not_seen" [("target", targetStr)])

-- | W4: Execute interaction on a device / fixture.
interactDevice :: Verb -> DeviceDef -> String -> GameState -> (GameState, [OutputEvent])
interactDevice verb dev _targetStr state
    | verb == VLookAt =
        let baseDesc = case devDescription dev of
                Just d  -> d
                Nothing -> devName dev
            mounted = [ item
                      | item <- Map.elems (itemDefs (world state))
                      , case Map.lookup (itemId item) (itemStates (save state)) of
                          Just is -> itemLocation is == CarriedBy (ActorEntity (devId dev))
                          Nothing -> False
                      ]
            mountedEv = case mounted of
                (m:_) -> evMsg "device.examine_mounted" [("device", devName dev), ("item", itemName m)]
                []    -> []
            descEv = evRaw baseDesc
        in (state, joinEv descEv mountedEv)
    | hasOnCommandTrigger verb state = (state, [])
    | otherwise = (state, evMsg "item.cant_do" [("item", devName dev)])

-- | Execute a bare interaction command without a target string.
interactBare :: Verb -> GameState -> (GameState, [OutputEvent])
interactBare verb state
    -- Phase 7f-3, A2: bare `defend` / `flee` during a tactical fight
    -- route through the combat resolver with the corresponding action.
    | VCustom vn <- verb
    , CombatTactical _ <- combatProfile (world state)
    , isCombatEngaged state
    , Just ca <- tacticalVerbAction vn
    = executeTacticalAction ca state
    | VCustom vn <- verb
    , vn `elem` ["defend", "flee"]
    = (state, evMsg "combat.not_engaged" [])
    | otherwise = (state, [])   -- bare verb (e.g. custom command); triggers carry the message

-- | Fallback interaction when target was not found.
interactNotFound :: Verb -> String -> GameState -> (GameState, [OutputEvent])
interactNotFound verb targetStr state
    -- Phase 7f-3, A3: `use-ability <id>` during a tactical fight
    | not (null targetStr), VCustom vn <- verb
    , vn `elem` ["use-ability", "ability"]
    , CombatTactical _ <- combatProfile (world state)
    = executeTacticalAction (CAAbility targetStr) state
    | hasOnCommandTrigger verb state = (state, [])
    | verb == VAttack, Just res <- tryAttackVehicle targetStr state = res
    | otherwise = (state, evMsg "target.not_seen" [("target", targetStr)])

-- | Fallback interaction when multiple entities match the target.
--   Phase 2.3: this is a *question*, not a terminal message — the stream
--   carries 'EvDisambiguate' with the candidate ids, and the numbered form of
--   the catalog prompt names every candidate. The loop remembers the pending
--   question and turns the player's next input into the chosen candidate.
interactAmbiguous :: [String] -> GameState -> (GameState, [OutputEvent])
interactAmbiguous ids state =
    let names = map (entityDisplayName state) ids
        options = zipWith (\n name -> renderMsg "disambiguate.option" [("n", show (n :: Int)), ("name", name)])
                           [1 ..] names
    in (state, EvDisambiguate ids : evMsg "disambiguate.prompt" [("names", intercalate ", " options)])

-- | Phase 2.3: the resolution of an explicitly chosen entity id (see
--   'chosenTarget'). Unknown ids fall back to 'NotFound'.
chosenResolution :: String -> GameState -> TargetResolution
chosenResolution chosen state
    | Map.member chosen (itemDefs (world state))    = ResolvedItem chosen
    | Map.member chosen (npcDefs (world state))     = ResolvedNPC chosen
    | Map.member chosen (vehicleDefs (world state)) = ResolvedVehicle chosen
    | Map.member chosen (deviceDefs (world state))  = ResolvedDevice chosen
    | otherwise = NotFound chosen

-- | Phase 2.3: can this command be replayed once the player has chosen one of
--   the ambiguous candidates? Only the command shapes whose dispatch resolves a
--   target through 'resolveTarget' qualify; for any other command no question is
--   recorded.
disambiguableCommand :: Command -> Bool
disambiguableCommand cmd = case cmd of
    Interact _ _       -> True
    ActionWithArgs _ _ -> True
    EquipCmd _         -> True
    UnequipCmd _       -> True
    _                  -> False

-- | Phase 2.3: interpret the player's answer to a disambiguation question.
--   A plain number picks the candidate at that 1-based position; a word picks
--   the candidate it distinguishes, provided exactly one candidate is described
--   by it (its id, display name or keyword words). 'Nothing' means "not an
--   answer" — the caller then treats the input as a normal command.
resolveDisambiguationAnswer :: [String] -> String -> GameState -> Maybe String
resolveDisambiguationAnswer ids raw state
    | not (null answerWords), all isDigit (unwords answerWords) =
        let n = read (unwords answerWords) :: Int
        in if n >= 1 && n <= length ids then Just (ids !! (n - 1)) else Nothing
    | otherwise = case filter (describesAnswer answerWords) ids of
        [single] -> Just single
        _        -> Nothing
  where
    answerWords = stripStopWords (words (map toLower raw))
    describesAnswer ws eid = any (matches ws) (candidateWordSource state eid)
    matches ws src = let srcN = normalizeText src
                     in unwords ws == srcN || case ws of
                            [singleWord] -> singleWord `elem` words srcN
                            _            -> False

-- | Phase 2.3: the strings a disambiguation answer may distinguish a candidate by.
candidateWordSource :: GameState -> String -> [String]
candidateWordSource state eid =
    let itemWords = maybe [] itemKeywords (Map.lookup eid (itemDefs (world state)))
        npcWords  = maybe [] npcKeywords  (Map.lookup eid (npcDefs (world state)))
        vehWords  = maybe [] vehicleKeywords (Map.lookup eid (vehicleDefs (world state)))
        devWords  = maybe [] devKeys (Map.lookup eid (deviceDefs (world state)))
    in eid : entityDisplayName state eid : itemWords ++ npcWords ++ vehWords ++ devWords

-- | Display name of an entity (item name, NPC name, or vehicle name).
entityDisplayName :: GameState -> String -> String
entityDisplayName state eid =
    case Map.lookup eid (itemDefs (world state)) of
        Just item -> itemName item
        Nothing -> case Map.lookup eid (npcDefs (world state)) of
            Just npc -> npcName npc
            Nothing  -> case Map.lookup eid (vehicleDefs (world state)) of
                Just veh -> vehicleName veh
                Nothing  -> case Map.lookup eid (deviceDefs (world state)) of
                    Just dev -> devName dev
                    Nothing  -> eid

-- | Display name of a hotspot target (item first, then NPC, else the id).
hotspotLabel :: GameState -> Hotspot -> String
hotspotLabel state h = case Map.lookup (hsTarget h) (itemDefs (world state)) of
    Just it -> itemName it
    Nothing -> case Map.lookup (hsTarget h) (npcDefs (world state)) of
        Just np -> npcName np
        Nothing -> hsTarget h

-- | Look up an item by target string in inventory or current room
findMatchingItem :: String -> GameState -> Maybe ItemDef
findMatchingItem targetStr state =
    let roomItems = getItemsInLocation (InRoom (currentRoom (save state))) state
        invItems = getItemsInLocation (CarriedBy ActorPlayer) state
    in find (matchesItemTarget targetStr) (invItems ++ roomItems)

-- | Default message when an action cannot be performed in darkness.
defaultDarkMessage :: String
defaultDarkMessage = renderMsg "dark.default" []

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

-- | Is the player's current room dark?
isCurrentRoomDark :: GameState -> Bool
isCurrentRoomDark state = case getCurrentRoom state of
    Just room -> isDark room state
    Nothing   -> False

-- | Message when darkness prevents seeing or interacting with non-carried entities.
--   Uses the room's custom dark message if configured, falling back to 'defaultDarkMessage'.
-- | Phase 1.2: the darkness refusal as event fragments — authored
--   `dark_msg` stays raw prose, the default keeps its catalog key.
darkRoomEv :: Room -> [OutputEvent]
darkRoomEv room = case roomDarkMsg room of
    Just authored -> evRaw authored
    Nothing       -> evMsg "dark.default" []

-- | Phase 1.2: an NPC's combat art as an art payload (empty -> no event).
combatArtMsgEv :: NPCID -> GameState -> [OutputEvent]
combatArtMsgEv nId st = case combatArtMsg nId st of
    "" -> []
    art -> [EvArt (ArtPayload art [])]

-- | Phase 1.2: `withAscii` as events — the art as structured payload (with
--   hotspot anchors), the description as raw prose. Byte-identical:
--   @art ++ "\\n" ++ desc@, just the description when the art is empty.
lookWithArtEv :: AsciiArt -> GameState -> String -> [OutputEvent]
lookWithArtEv art state desc =
    let rendered = renderArtForLook art state
        artFrags | null rendered = []
                 | otherwise = [ EvArt (ArtPayload rendered
                                   [ ArtHotspot i (hsGlyph h) (hsTarget h)
                                   | (i, h) <- zip [1 :: Int ..] (aaHotspots art) ]) ] ++ nl
    in artFrags ++ evRaw desc

-- | Verbs that cannot be performed on non-carried targets in darkness (Phase 0.3, Bug B3).
isDarkRestricted :: Verb -> Bool
isDarkRestricted v = case verbCanonicalName v of
    "take"    -> True
    "examine" -> True
    "search"  -> True
    "use"     -> True
    _         -> False

-- | Phase 0.3 (B3): an item tagged @feelable@ can be found and handled by touch,
--   so the darkness restriction does not apply to it. The author decides per item
--   what is reachable in an unlit room — a torch, a key, a lever, anything.
itemIsFeelable :: ItemDef -> Bool
itemIsFeelable item = Set.member "feelable" (itemTags item)

-- | Item is reachable in darkness: carried, or its definition is tagged @feelable@.
itemReachableInDark :: GameState -> String -> Bool
itemReachableInDark st iId =
    hasItem iId st
        || maybe False itemIsFeelable (Map.lookup iId (itemDefs (world st)))

-- | Does the target string match a @feelable@ item in the current room?
targetIsFeelable :: GameState -> String -> Bool
targetIsFeelable st t =
    any (\i -> matchesItemTarget t i && itemIsFeelable i)
        (getItemsInLocation (InRoom (currentRoom (save st))) st)

-- | Pick the room description: resolve CondText variants against game state.
resolveDescription :: Room -> GameState -> String
resolveDescription room state = resolveCondText (roomDescription room) state

-- | An enemy's art as a message, empty when it has none. It is rendered *after*
--   the round's effects have been applied on purpose: a lethal hit has already
--   set the status by then, so the killing round shows the body's variant.
--   Trailing newlines are dropped so the art does not add a blank line to the
--   round's message.
combatArtMsg :: NPCID -> GameState -> String
combatArtMsg nId st = case Map.lookup nId (npcDefs (world st)) of
    Just npc | not (isEmptyAscii (npcAscii npc)) ->
        dropWhileEnd (== '\n') (resolveAsciiArt (npcAscii npc) st)
    _ -> ""

-- | `search` — reveal hidden items and run the room's search outcome
searchRoom :: GameState -> (GameState, [OutputEvent])
searchRoom state = case getCurrentRoom state of
    Just room | isDark room state -> (state, darkRoomEv room)
    _ ->
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
            discoveredMsgs = [ maybe (evMsg "search.reveal" [("item", iId)]) evRaw
                                 (Map.lookup iId (itemDefs (world state)) >>= itemDiscoverText)
                             | iId <- hidden ]
            (stateFinal, hookMsg) =
                case lookupRoom rId state >>= roomSearchOutcome of
                    Nothing -> (stateAfterReveal, [])
                    Just outcome -> applyOutcomeEv outcome "" stateAfterReveal
            full = joinAllEv (discoveredMsgs ++ [hookMsg])
        in (stateFinal, if null (renderEvents full) then evMsg "search.nothing" [] else full)

-- | Item-on-item interaction (crafting).
--   Returns Nothing if no interaction is defined, so callers can keep their
--   own "nothing here" message.
tryItemOnItem :: ItemID -> String -> GameState -> Maybe (GameState, [OutputEvent])
tryItemOnItem usedId targetStr state =
    case findMatchingItem targetStr state of
        Nothing -> Nothing
        Just target ->
            let key = (usedId, itemId target)
                altKey = (itemId target, usedId)
            in case Map.lookup key (itemInteractions (world state)) <|>
                    Map.lookup altKey (itemInteractions (world state)) of
                Just outcome -> Just (applyOutcomeEv outcome (itemId target) state)
                Nothing -> Nothing

-- | Dialogue: use the tree if present, otherwise fall back to the legacy single line
talkTo :: NPCDef -> Maybe NPCState -> GameState -> (GameState, [OutputEvent])
talkTo npc maybeNpcState state =
    let status = maybe "alive" npcStatus maybeNpcState
    in case Map.lookup status (npcDialogueTrees npc) of
        Just tree -> renderDialogue npc tree maybeNpcState state
        Nothing -> (clearActiveDialogue state, evMsg "dialogue.nothing_to_say" [("name", npcName npc)])

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
-- | 4.5: run one topic's effect (the topic table lives on the NPC).
talkTopic :: String -> String -> GameState -> (GameState, [OutputEvent])
talkTopic who what state =
    case [ n | n <- Map.elems (npcDefs (world state))
             , let w = map toLower who
             , npcId n == who || w `elem` map (map toLower) (npcKeywords n) ] of
        (npc : _) ->
            let topicEff = case Map.lookup (map toLower what) (npcTopics npc) of
                    Just eff -> applyOutcomeEv eff (npcId npc) state
                    Nothing -> (state, evMsg "dialog.no_topic" [("npc", npcName npc), ("topic", what)])
                (st1, evs1) = topicEff
                -- 4.5: `on: talk` triggers see the npc and the topic in the VarMap
                st2 = st1 { save = (save st1) { variables =
                            Map.insert "talk.npc" (VVText (npcId npc))
                              (Map.insert "talk.topic" (VVText what) (variables (save st1))) } }
                (st3, evs2) = fireTriggers (OnTalk (npcId npc) what) st2
            in (st3, evs1 ++ evs2)
        [] -> (state, evMsg "dialog.no_npc" [])

renderDialogue :: NPCDef -> DialogueTree -> Maybe NPCState -> GameState -> (GameState, [OutputEvent])
renderDialogue npc tree maybeNpcState state =
    let nodeId = fromMaybe (dtEntry tree) (maybeNpcState >>= npcDialogueNode)
        maybeNode = Map.lookup nodeId (dtNodes tree)
    in case maybeNode of
        Nothing -> (clearActiveDialogue state, evMsg "dialogue.nothing_to_say" [("name", npcName npc)])
        Just node ->
            let header = evMsg "dialogue.line" [("name", npcName npc), ("text", formatWithVars (dnText node) state)]
                choices = visibleChoices state node
                choiceLines = [ evMsg "dialogue.choice_line" [("i", show i), ("text", formatWithVars (dcText c) state)]
                              | (i, c) <- zip [1 :: Int ..] choices ]
                -- Byte-identical: header ++ "\\n\\n" ++ unlines choiceLines
                body = if null choices
                       then header
                       else header ++ nl2 ++ unlinesEv choiceLines
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
executeAttack :: NPCDef -> Maybe NPCState -> String -> GameState -> (GameState, [OutputEvent])
executeAttack npc mNpcState targetStr state =
    let nId = npcId npc
        profile = combatProfile (world state)
        -- Phase 7g/7h: party members standing here and the ship the player is
        -- aboard fight alongside the player.
        actors = [PlayerActor]
                 ++ map CompanionActor (partyMembersInRoom state)
                 ++ [ShipActor vId | Just vId <- [currentVehicle (save state)]]
        (effects, msgs) = resolveCombatEv profile actors (TargetNPC nId targetStr) CAAttack state
        -- Apply the whole effect list through the shared interpreter: it
        -- threads the RNG salt and joins every effect message instead of
        -- discarding all but the last (killNPCWithMsg / OnStateChange rules
        -- produce text that must survive trailing companion/ship effects).
        (st', effectMsg) = applyOutcomes effects nId state
        -- The authored combat screen (classic only) is rendered from the state
        -- *before* the round resolves — screen first, then the strike's
        -- outcome, exactly like the original fight loop.
        screenMsgs = case (profile, mNpcState >>= npcHealth) of
            (CombatClassic (Just scr), Just _) -> combatScreenLines scr nId state
            _                                  -> []
        body = combineMsgsEv (map evRaw screenMsgs ++ effectMsg : msgs)
        -- The enemy is rendered every round (after the effects, so the killing
        -- round shows the body — see `combatArtMsg`).
        body' = combineMsgsEv [combatArtMsgEv nId st', body]
    in if null (renderEvents body') then (st', []) else (st', body')

-- | Combat logic for attacking a target ship (Phase 7h-2).
executeAttackShip :: VehicleDef -> String -> GameState -> (GameState, [OutputEvent])
executeAttackShip veh targetStr state =
    let vId = vehicleId veh
        profile = combatProfile (world state)
        actors = [PlayerActor]
                 ++ map CompanionActor (partyMembersInRoom state)
                 ++ [ShipActor pvId | Just pvId <- [currentVehicle (save state)]]
        (effects, msgs) = resolveCombatEv profile actors (TargetShip vId targetStr) CAAttack state
        (st', effectMsg) = applyOutcomes effects vId state
        body = combineMsgsEv (effectMsg : msgs)
    in if null (renderEvents body) then (st', []) else (st', body)

-- | Check if the target names a vehicle at the player's stop, and if so,
--   refuse attacking an ordinary vehicle or execute ship-to-ship combat.
tryAttackVehicle :: String -> GameState -> Maybe (GameState, [OutputEvent])
tryAttackVehicle targetStr state = case findVehicle targetStr state of
    Just veh ->
        let vId = vehicleId veh
            outsideStop = case currentVehicle (save state) of
                Just pvId -> vsCurrentStop (getVehicleState pvId state)
                Nothing   -> currentRoom (save state)
            vehStop = vsCurrentStop (getVehicleState vId state)
            isAboardTarget = currentVehicle (save state) == Just vId
        in if isAboardTarget || vehStop /= outsideStop
           then Just (state, evMsg "target.not_seen" [("target", targetStr)])
           else case targetShipSystems (TargetShip vId targetStr) state of
               Nothing -> Just (state, evMsg "attack.cant_target" [("target", targetStr)])
               Just _  -> Just (executeAttackShip veh targetStr state)
    Nothing -> Nothing

-- | Concatenate non-empty combat messages.
-- | Phase 1.2: fragment form of the former `combineMsgs` (unlines with
--   empty-piece filter) — byte-identical.
combineMsgsEv :: [[OutputEvent]] -> [OutputEvent]
combineMsgsEv = unlinesEv . filter (not . null . renderEvents)

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
executeTacticalAction :: CombatAction -> GameState -> (GameState, [OutputEvent])
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
                (effects, msgs) = resolveCombatEv profile actors (TargetNPC nId (npcName npc)) action state
                (st', effectMsg) = applyOutcomes effects nId state
                body = combineMsgsEv (effectMsg : msgs)
                body' = combineMsgsEv [combatArtMsgEv nId st', body]
            in if null (renderEvents body') then (st', []) else (st', body')
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
                        (effects, msgs) = resolveCombatEv profile actors (TargetShip vId (vehicleName veh)) action state
                        (st', effectMsg) = applyOutcomes effects vId state
                        body = combineMsgsEv (effectMsg : msgs)
                    in if null (renderEvents body) then (st', []) else (st', body)
                [] -> (state, evMsg "attack.none_here" [])

-- | Compatibility form (Phase 1.2; used by the tests, the aux paths of the
--   game loop and the TUI hand-over): the same command, its event stream
--   rendered back to the flat CLI text — byte-identical to the pre-1.2
--   string pipeline.
executeCommand :: Command -> GameState -> CommandResult
executeCommand cmd state =
    let (st', evs) = executeCommandEv cmd state
    in (st', renderEvents evs)

-- ---------------------------------------------------------------------------
-- Help text
-- ---------------------------------------------------------------------------

-- | Formatted help text (Phase 1.1: template lebt im Message-Katalog,
--   Key help.text — byte-identisch zum bisherigen intercalate-Block).
helpText :: String
helpText = renderMsg "help.text" []
