-- | World validation before starting the game.
--   Checks consistency of IDs, exits, references and reachability.
module Validate (ValidationError(..), validateWorld, validateGameState, setFlagsInWorld) where

import Types
import Data.List (nub, stripPrefix, foldl')
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- ---------------------------------------------------------------------------
-- Validation error types
-- ---------------------------------------------------------------------------

data ValidationError
    = MissingRoom      RoomID                       -- ^ Referenced room does not exist
    | MissingItem      ItemID                       -- ^ Referenced item def is missing
    | MissingNPC       NPCID                        -- ^ Referenced NPC def is missing
    | MissingQuest     QuestID                      -- ^ Referenced quest is missing
    | MissingVehicle   VehicleID                   -- ^ Referenced vehicle is missing
    | MissingItemState ItemID                       -- ^ ItemDef exists but is placed nowhere (P2-8)
    | DanglingExit     RoomID Direction RoomID      -- ^ Exit points to a non-existent room
    | UnreachableRoom  RoomID                       -- ^ Room cannot be reached from start
    | DuplicateID      String String String         -- ^ (id, type1, type2)
    | MissingSetFlag   FlagID String                -- ^ Flag referenced in CheckFlag but never set
    | MissingDialogueNode NPCID String String      -- ^ (npc, status, entryNode) entry node missing
    | DanglingDialogueChoice NPCID String String String -- ^ (npc, status, sourceNode, targetNode)
    -- Phase 2a: SaveState / reference validation
    | InvalidStartRoom    RoomID                    -- ^ start_room / currentRoom does not exist
    | InvalidItemLocation ItemID String             -- ^ (itemId, roomId or sentinel) room missing
    | InvalidNPCLocation  NPCID String              -- ^ (npcId, roomId) room missing
    | InvalidVehicleRoom  VehicleID String RoomID   -- ^ (vId, "entry"/"cockpit"/"stop:<label>", missingRoom)
    | EmptyQuestStages    QuestID                   -- ^ Quest has zero stages
    | UnknownQuestPrereq  QuestID FlagID            -- ^ Quest prereq flag is never set anywhere
    | MissingEntity      String String              -- ^ (entityId, typeContext) VRProperty ref not in itemDefs or npcDefs
    | InvalidContainer   ItemID ItemID             -- ^ (itemId, missing container item id)
    deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Main validation entry point
-- ---------------------------------------------------------------------------

-- | Validate a complete GameWorld, returning a list of errors (empty = valid).
validateWorld :: GameWorld -> [ValidationError]
validateWorld gw =
    concat
        [ checkDanglingExits gw
        , checkDuplicateIDs gw
        , checkDialogueTrees gw
        , checkMissingItemsInDefs gw
        , checkMissingNPCsInDefs gw
        , checkMissingEntitiesInDefs gw
        , checkMissingQuestsInDefs gw
        , checkMissingVehiclesInDefs gw
        , checkFlags gw
        ]

-- ---------------------------------------------------------------------------
-- Room existence
-- ---------------------------------------------------------------------------

-- | Every exit destination must be a key in the rooms map
checkDanglingExits :: GameWorld -> [ValidationError]
checkDanglingExits gw =
    [ DanglingExit rId dir target
    | (rId, room) <- Map.toList (rooms gw)
    , (dir, exit) <- Map.toList (roomConnections room)
    , let target = exitRoomID exit
    , target `notElem` Map.keys (rooms gw)
    ]

exitRoomID :: Exit -> RoomID
exitRoomID (Open r) = r
exitRoomID (Locked r _) = r

-- ---------------------------------------------------------------------------
-- Reachability
-- ---------------------------------------------------------------------------

-- | Rooms unreachable from the given start room via Open/Locked exits, plus the
--   vehicle stops and interiors (reached by boarding/driving, not by room
--   exits). The caller supplies the start room: `validateGameState` uses
--   `currentRoom` from the SaveState, which is the only place the real
--   `start_room` is known. `validateWorld` has no start room and therefore does
--   not guess one.
--
--   A start room that does not exist is reported as `InvalidStartRoom` by
--   `validateGameState`; reachability is skipped in that case to avoid
--   flagging every room as unreachable on top of it.
checkUnreachableFrom :: RoomID -> GameWorld -> [ValidationError]
checkUnreachableFrom start gw
    | Map.null (rooms gw) = []
    | not (Map.member start (rooms gw)) = []
    | otherwise =
        let -- Vehicle stops and interiors are reached by boarding/driving, not by
            -- room exits, so they must not be flagged as unreachable.
            vehRoomSet = Set.fromList
                ( concat [ (stopExternalRoom <$> Map.elems (vehicleStops vd))
                           ++ vehicleRooms vd
                         | vd <- Map.elems (vehicleDefs gw) ] )
            reachable = Set.fromList
                ( concat [ reachableRooms s gw
                         | s <- start : Set.toList vehRoomSet
                         , Map.member s (rooms gw) ] )
            allRooms = [ rId | rId <- Map.keys (rooms gw)
                             , let room = rooms gw Map.! rId
                             , not ("vehicle" `Set.member` roomTags room) ]
        in [UnreachableRoom rId | rId <- allRooms, not (Set.member rId reachable)]

-- | BFS from a starting room, following both open and locked exits.
reachableRooms :: RoomID -> GameWorld -> [RoomID]
reachableRooms start gw = go Set.empty [start]
  where
    allRoomIds = Set.fromList (Map.keys (rooms gw))
    go visited [] = Set.toList visited
    go visited (r:rest)
        | Set.member r visited = go visited rest
        | otherwise =
            let exits = case Map.lookup r (rooms gw) of
                    Just room ->
                        [ target
                        | exit <- Map.elems (roomConnections room)
                        , let target = exitRoomID exit
                        , Set.member target allRoomIds
                        ]
                    Nothing -> []
                newQueue = rest ++ [e | e <- exits, not (Set.member e visited)]
            in go (Set.insert r visited) newQueue

-- ---------------------------------------------------------------------------
-- Duplicate IDs
-- ---------------------------------------------------------------------------

-- | Check that no ID is used across two different categories
checkDuplicateIDs :: GameWorld -> [ValidationError]
checkDuplicateIDs gw =
    let idSets =
            [ ("room",      Map.keys (rooms gw))
            , ("item",      Map.keys (itemDefs gw))
            , ("npc",       Map.keys (npcDefs gw))
            , ("quest",     Map.keys (questDefs gw))
            -- Vehicles intentionally share IDs with items (e.g. "carriage")
            , ("vehicle",   Map.keys (vehicleDefs gw))
            ]
        pairs = [(i, t1, t2) | (t1, ids1) <- idSets, (t2, ids2) <- idSets, t1 < t2, i <- ids1, i `elem` ids2]
        -- Allow vehicle-item pairings by convention
        allowedPair (_, "vehicle", "item") = True
        allowedPair (_, "item", "vehicle") = True
        allowedPair _ = False
    in [DuplicateID i t1 t2 | (i, t1, t2) <- nub pairs, not (allowedPair (i, t1, t2))]

-- ---------------------------------------------------------------------------
-- Dialogue trees
-- ---------------------------------------------------------------------------

-- | Validate dialogue tree structure: valid entry points and non-dangling next-node links
checkDialogueTrees :: GameWorld -> [ValidationError]
checkDialogueTrees gw =
    [ err
    | npc <- Map.elems (npcDefs gw)
    , (status, tree) <- Map.toList (npcDialogueTrees npc)
    , let nId = npcId npc
    , let nodeKeys = Map.keysSet (dtNodes tree)
    , err <- checkTree nId status tree nodeKeys
    ]
  where
    checkTree nId status tree nodeKeys =
        let entryErr = [ MissingDialogueNode nId status (dtEntry tree)
                       | not (Set.member (dtEntry tree) nodeKeys) ]
            danglingChoiceErrs =
                [ DanglingDialogueChoice nId status nodeId target
                | (nodeId, node) <- Map.toList (dtNodes tree)
                , choice <- dnChoices node
                , Just target <- [dcNextNode choice]
                , not (Set.member target nodeKeys)
                ]
        in entryErr ++ danglingChoiceErrs

-- ---------------------------------------------------------------------------
-- Missing items/NPCs/Quests/Vehicles referenced in definitions
-- ---------------------------------------------------------------------------

checkMissingItemsInDefs :: GameWorld -> [ValidationError]
checkMissingItemsInDefs gw =
    let itemRefs = Set.fromList (Map.keys (itemDefs gw))
        -- MoveEntity is ambiguous (items AND NPCs); NPC refs are validated by
        -- checkMissingNPCsInDefs, so exclude them here.
        npcRefs = Set.fromList (Map.keys (npcDefs gw))
        allRefs =
            concatMap idsFromOutcomeItem (allOutcomes gw)
            ++ [i1 | (i1, _) <- Map.keys (itemInteractions gw)]
            ++ [i2 | (_, i2) <- Map.keys (itemInteractions gw)]
    in [MissingItem iId | iId <- nub allRefs, not (Set.member iId itemRefs), not (Set.member iId npcRefs)]

checkMissingNPCsInDefs :: GameWorld -> [ValidationError]
checkMissingNPCsInDefs gw =
    let npcRefs = Set.fromList (Map.keys (npcDefs gw))
        itemRefs = Set.fromList (Map.keys (itemDefs gw))
        -- MoveEntity is ambiguous (items AND NPCs); VRProperty is ambiguous
        -- (item states vs. NPC states). Only flag refs that are clearly NPC
        -- references: those already known to be NPCs are checked elsewhere;
        -- anything else is treated as an entity check (see checkMissingEntities).
        allRefs = concatMap idsFromOutcomeNPC (allOutcomes gw)
    in [MissingNPC nId | nId <- nub allRefs, not (Set.member nId npcRefs), not (Set.member nId itemRefs)]

checkMissingEntitiesInDefs :: GameWorld -> [ValidationError]
checkMissingEntitiesInDefs gw =
    let -- "player" is a built-in entity valid for VRProperty refs
        -- (e.g. VRProperty "player" "room" teleports the player).
        -- Exit lock keys (`locked_by: <id>`) are entities too: `set_state`
        -- compiles to `VRProperty <lockId> "state"`, which the engine stores in
        -- `entityStates` and reads back for the locked exit. Without them, every
        -- `set_state` on a lock would read as a missing entity.
        lockEntities = [ e | room <- Map.elems (rooms gw)
                           , Locked _ e <- Map.elems (roomConnections room) ]
        entityRefs = Set.insert "player"
            (Set.fromList (Map.keys (itemDefs gw) ++ Map.keys (npcDefs gw) ++ lockEntities))
        allRefs = concatMap idsFromOutcomeEntity (allOutcomes gw)
    in [MissingEntity eId "property" | eId <- nub allRefs, not (Set.member eId entityRefs)]

checkMissingQuestsInDefs :: GameWorld -> [ValidationError]
checkMissingQuestsInDefs gw =
    let questRefs = Set.fromList (Map.keys (questDefs gw))
        allRefs = concatMap idsFromOutcomeQuest (allOutcomes gw)
    in [MissingQuest qId | qId <- nub allRefs, not (Set.member qId questRefs)]

-- | Vehicle IDs referenced from the world: `ship.<vehicleId>.<system>`
--   variable reads/writes in effects and predicates. An undeclared vehicle id
--   in such a reference is a typo the engine would silently evaluate against a
--   missing variable, so it is a real validation error.
checkMissingVehiclesInDefs :: GameWorld -> [ValidationError]
checkMissingVehiclesInDefs gw =
    let vehicleRefs = Set.fromList (Map.keys (vehicleDefs gw))
        allRefs = concatMap idsFromOutcomeVehicle (allOutcomes gw)
               ++ concatMap idsFromPredicateVehicle (allPredicates gw)
    in [MissingVehicle vId | vId <- nub allRefs, not (Set.member vId vehicleRefs)]

-- ---------------------------------------------------------------------------
-- Flag consistency
-- ---------------------------------------------------------------------------

-- | Find flags that are checked in a predicate (HasFlag / Compare on VRFlag)
--   but never set (SetFlag outcome) anywhere in the world definitions.
checkFlags :: GameWorld -> [ValidationError]
checkFlags gw =
    let setFlags = foldl' scanSetFlags Set.empty (allOutcomes gw)
        checked  = Set.fromList (concatMap flagsInPredicate (allPredicates gw))
        missing  = Set.toList (Set.difference checked setFlags)
    in [MissingSetFlag flg "checked but never set in any outcome" | flg <- missing]

-- ---------------------------------------------------------------------------
-- Collect all ActionOutcomes / predicates defined in a GameWorld
-- ---------------------------------------------------------------------------

allOutcomes :: GameWorld -> [Effect]
allOutcomes gw = concat
    [ concatMap (\r -> catMaybes [roomOnEnter r, roomOnLook r, roomOnExit r, roomSearchOutcome r])
        (Map.elems (rooms gw))
    , concatMap (\(_, o) -> [o]) (concatMap (Map.toList . itemVerbMap) (Map.elems (itemDefs gw)))
    , Map.elems (itemInteractions gw)
    , concatMap (\(_, o) -> [o]) (concatMap (Map.toList . npcVerbMap) (Map.elems (npcDefs gw)))
    , catMaybes (map questReward (Map.elems (questDefs gw)))
    , concatMap (\(_, o) -> [o])
        (concatMap (Map.toList . vehicleConditionEffects) (Map.elems (vehicleDefs gw)))
    , map dcOutcome (worldDialogueChoices gw)
    -- Trigger rules carry the bulk of the gameplay logic since Phase 3f/7; a
    -- validator that ignores them is blind to `give:`, `start_quest:` and flag
    -- references inside `rules:`.
    , concatMap trEffects (triggerDefs gw)
    ]

-- | Every predicate tree reachable from a GameWorld: trigger conditions,
--   dialogue-choice gates and conditional-text variants. Mirrors the
--   worldbuilder's `allWorldPredicates` so both packages see one reference set.
allPredicates :: GameWorld -> [Predicate]
allPredicates gw = concat
    [ [ p | TriggerDef { trCondition = Just p } <- triggerDefs gw ]
    , catMaybes (map dcVisible (worldDialogueChoices gw))
    , concatMap (map tvWhen . ctVariants) (map roomDescription (Map.elems (rooms gw)))
    , concatMap (map tvWhen . ctVariants) (map itemDescription (Map.elems (itemDefs gw)))
    , concatMap (map tvWhen . ctVariants) (map npcDescription (Map.elems (npcDefs gw)))
    ]

-- | Every dialogue choice in every NPC dialogue tree.
worldDialogueChoices :: GameWorld -> [DialogueChoice]
worldDialogueChoices gw =
    [ choice
    | npc <- Map.elems (npcDefs gw)
    , tree <- Map.elems (npcDialogueTrees npc)
    , node <- Map.elems (dtNodes tree)
    , choice <- dnChoices node
    ]

catMaybes :: [Maybe a] -> [a]
catMaybes xs = [x | Just x <- xs]

-- ---------------------------------------------------------------------------
-- Type-specific ID collectors from Outcome trees
-- ---------------------------------------------------------------------------

idsFromOutcomeItem :: Effect -> [String]
idsFromOutcomeItem outcome = case outcome of
    MoveEntity iId _             -> [iId]
    SetValue (VRItemProp iId _) _ -> [iId]
    ModifyValue (VRItemProp iId _) _ -> [iId]
    Sequence os                  -> concatMap idsFromOutcomeItem os
    RandomChoice os              -> concatMap (idsFromOutcomeItem . snd) os
    Conditional _ t e            -> idsFromOutcomeItem t ++ idsFromOutcomeItem e
    _                            -> []

idsFromOutcomeNPC :: Effect -> [String]
idsFromOutcomeNPC outcome = case outcome of
    MoveEntity nId _             -> [nId]
    Sequence os                  -> concatMap idsFromOutcomeNPC os
    RandomChoice os              -> concatMap (idsFromOutcomeNPC . snd) os
    Conditional _ t e            -> idsFromOutcomeNPC t ++ idsFromOutcomeNPC e
    _                            -> []

-- | Collect entity IDs referenced via VRProperty (item state / NPC state /
--   generic property writes). These are checked against itemDefs ∪ npcDefs.
idsFromOutcomeEntity :: Effect -> [String]
idsFromOutcomeEntity outcome = case outcome of
    SetValue (VRProperty eId _) _   -> [eId]
    ModifyValue (VRProperty eId _) _ -> [eId]
    Sequence os                     -> concatMap idsFromOutcomeEntity os
    RandomChoice os                 -> concatMap (idsFromOutcomeEntity . snd) os
    Conditional _ t e               -> idsFromOutcomeEntity t ++ idsFromOutcomeEntity e
    Narrative _ followUp            -> idsFromOutcomeEntity followUp
    _                               -> []

idsFromOutcomeQuest :: Effect -> [String]
idsFromOutcomeQuest outcome = case outcome of
    QuestOp StartQuest qId       -> [qId]
    QuestOp AdvanceQuest qId     -> [qId]
    QuestOp CompleteQuest qId    -> [qId]
    Sequence os                  -> concatMap idsFromOutcomeQuest os
    RandomChoice os              -> concatMap (idsFromOutcomeQuest . snd) os
    Conditional _ t e            -> idsFromOutcomeQuest t ++ idsFromOutcomeQuest e
    _                            -> []

-- | Vehicle IDs referenced via `ship.<id>.<system>` variables in an Effect.
idsFromOutcomeVehicle :: Effect -> [String]
idsFromOutcomeVehicle outcome = case outcome of
    SetValue (VRVariable n) _      -> shipFromVar n
    ModifyValue (VRVariable n) _   -> shipFromVar n
    Sequence os                    -> concatMap idsFromOutcomeVehicle os
    RandomChoice os                -> concatMap (idsFromOutcomeVehicle . snd) os
    Conditional p t e              -> idsFromPredicateVehicle p
                                   ++ idsFromOutcomeVehicle t
                                   ++ idsFromOutcomeVehicle e
    Narrative _ followUp           -> idsFromOutcomeVehicle followUp
    ApplyCondition _ _ mt me       -> concatMap idsFromOutcomeVehicle (catMaybes [mt, me])
    _                              -> []

-- | Vehicle IDs referenced via `ship.<id>.<system>` variables in a Predicate.
idsFromPredicateVehicle :: Predicate -> [String]
idsFromPredicateVehicle p = case p of
    PNot q              -> idsFromPredicateVehicle q
    PAll qs             -> concatMap idsFromPredicateVehicle qs
    PAny qs             -> concatMap idsFromPredicateVehicle qs
    CompareVar n _ _    -> shipFromVar n
    Compare lhs _ rhs   -> shipFromRef lhs ++ shipFromRef rhs
    _                   -> []
  where
    shipFromRef (VRVariable n) = shipFromVar n
    shipFromRef _              = []

-- | `"ship.<vehicleId>.<system>"` -> `Just "<vehicleId>"`; anything else ->
--   Nothing. Requires a non-empty vehicle id and a following ".system" part,
--   so a bare `"ship."` or `"ship.x"` is not treated as a reference.
shipFromVar :: String -> [String]
shipFromVar n = case stripPrefix "ship." n of
    Just rest -> case break (== '.') rest of
        (vId, '.':_) | not (null vId) -> [vId]
        _                             -> []
    Nothing -> []

-- ---------------------------------------------------------------------------
-- Phase 2a: SaveState / reference validation
-- ---------------------------------------------------------------------------

-- | Validate a GameWorld + SaveState pair: all room/id references must exist.
validateGameState :: GameWorld -> SaveState -> [ValidationError]
validateGameState gw st = concat
    [ checkStartRoom
    , checkItemLocs
    , checkItemStateCoverage
    , checkContainerRefs
    , checkNPCLocs
    , checkVehicleRefs
    , checkQuestStageCounts
    , checkQuestPrereqFlags
    -- Reachability needs the real start room, which only the SaveState knows.
    , checkUnreachableFrom (currentRoom st) gw
    ]
  where
    roomKeys = Map.keys (rooms gw)
    itemKeys = Map.keys (itemDefs gw)

    checkStartRoom =
        [ InvalidStartRoom (currentRoom st)
        | currentRoom st `notElem` roomKeys ]

    checkItemLocs =
        [ InvalidItemLocation iId rId
        | (iId, is) <- Map.toList (itemStates st)
        , let loc = itemLocation is
        , (rId) <- case loc of { InRoom r -> [r]; _ -> [] }
        , rId `notElem` roomKeys ]

    -- P2-8: item lookup scans `itemStates`, so an `ItemDef` that is placed
    -- nowhere is invisible at runtime (`take`/`look at` never see it). Fail
    -- loudly instead of letting the item silently not exist.
    checkItemStateCoverage =
        [ MissingItemState iId
        | iId <- itemKeys
        , not (Map.member iId (itemStates st)) ]

    -- Items that start inside a container must reference an existing item id.
    checkContainerRefs =
        [ InvalidContainer iId cid
        | (iId, is) <- Map.toList (itemStates st)
        , cid <- case itemLocation is of { InContainer c -> [c]; _ -> [] }
        , cid `notElem` itemKeys ]

    checkNPCLocs =
        [ InvalidNPCLocation nId rId
        | (nId, ns) <- Map.toList (npcStates st)
        , let loc = npcLocation ns
        , (rId) <- case loc of { InRoom r -> [r]; _ -> [] }
        , rId `notElem` roomKeys ]

    checkVehicleRefs =
        concat [ checkVehicle vId (vehicleDefs gw Map.! vId)
               | vId <- Map.keys (vehicleDefs gw) ]
      where
        checkVehicle vId v =
            let entryBad = vehicleEntryRoom v `notElem` roomKeys
                badStops = [ (label, stopExternalRoom stop)
                           | (label, stop) <- Map.toList (vehicleStops v)
                           , stopExternalRoom stop `notElem` roomKeys ]
                cockpitErrs = case vehicleCockpitRoom v of
                    Just r | r `notElem` roomKeys -> [InvalidVehicleRoom vId "cockpit" r]
                    _ -> []
            in concat
                [ [InvalidVehicleRoom vId "entry" (vehicleEntryRoom v) | entryBad]
                , cockpitErrs
                , [InvalidVehicleRoom vId ("stop:" ++ label) rId | (label, rId) <- badStops]
                ]

    checkQuestStageCounts =
        [ EmptyQuestStages qId
        | (qId, q) <- Map.toList (questDefs gw)
        , null (questStages q) ]

    checkQuestPrereqFlags =
        let knownSetFlags = setFlagsInWorld gw
        in [ UnknownQuestPrereq qId flag
           | (qId, q) <- Map.toList (questDefs gw)
           , (flag, _) <- Map.toList (questPrereqs q)
           , flag `Set.notMember` knownSetFlags ]

-- | Collect all flag IDs that are ever set in the world (SetFlag outcomes,
--   room alt-description keys, light-flag references).
setFlagsInWorld :: GameWorld -> Set.Set FlagID
setFlagsInWorld gw =
    let setFromOutcomes = foldl' scanSetFlags Set.empty (allOutcomes gw)
        setFromPredicates = Set.fromList
            [ f | p <- allPredicates gw
                , f <- flagsInPredicate p
                , not (null f) ]
        setFromTexts    = Set.fromList
            [ f | r <- Map.elems (rooms gw)
                , f <- flagsInCondText (roomDescription r)
                , not (null f) ]
            `Set.union`
            Set.fromList
            [ f | i <- Map.elems (itemDefs gw)
                , f <- flagsInCondText (itemDescription i)
                , not (null f) ]
            `Set.union`
            Set.fromList
            [ f | n <- Map.elems (npcDefs gw)
                , f <- flagsInCondText (npcDescription n)
                , not (null f) ]
        setFromLight    = Set.fromList
            [ f | r <- Map.elems (rooms gw)
                , Just f <- [roomLightFlag r]
                , not (null f) ]
    in setFromOutcomes `Set.union` setFromPredicates `Set.union` setFromTexts `Set.union` setFromLight

-- | Collect flags referenced by a predicate (HasFlag / Compare on VRFlag).
flagsInPredicate :: Predicate -> [FlagID]
flagsInPredicate p = case p of
    PTrue               -> []
    PNot q              -> flagsInPredicate q
    PAll qs             -> concatMap flagsInPredicate qs
    PAny qs             -> concatMap flagsInPredicate qs
    HasFlag f           -> [f]
    Compare lhs _ rhs   -> flagsInRef lhs ++ flagsInRef rhs
    _                   -> []
  where
    flagsInRef (VRFlag f) = [f]
    flagsInRef _          = []

-- | Collect flags referenced by a CondText's variant predicates.
flagsInCondText :: CondText -> [FlagID]
flagsInCondText ct = concatMap (flagsInPredicate . tvWhen) (ctVariants ct)

scanSetFlags :: Set.Set FlagID -> Effect -> Set.Set FlagID
scanSetFlags acc outcome = case outcome of
    SetValue (VRFlag n) _         -> Set.insert n acc
    Sequence os                   -> foldl' scanSetFlags acc os
    RandomChoice os               -> foldl' scanSetFlags acc (map snd os)
    Conditional _ t e             -> scanSetFlags (scanSetFlags acc t) e
    _                             -> acc
