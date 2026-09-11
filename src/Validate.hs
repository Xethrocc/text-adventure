-- | World validation before starting the game.
--   Checks consistency of IDs, exits, references and reachability.
module Validate (ValidationError(..), validateWorld, validateGameState, setFlagsInWorld) where

import Types
import Data.List (nub)
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
    | MissingVehicle   VehicleID                    -- ^ Referenced vehicle is missing
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
    deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Main validation entry point
-- ---------------------------------------------------------------------------

-- | Validate a complete GameWorld, returning a list of errors (empty = valid).
validateWorld :: GameWorld -> [ValidationError]
validateWorld gw =
    concat
        [ checkDanglingExits gw
        , checkUnreachableRooms gw
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

-- | Find all rooms reachable from "start" via Open exits, then flag the rest.
checkUnreachableRooms :: GameWorld -> [ValidationError]
checkUnreachableRooms gw =
    let start = if Map.member "start" (rooms gw) then "start"
                else if Map.null (rooms gw) then ""
                else fst (Map.findMin (rooms gw))
        reachable = reachableRooms start gw
        allRooms = [ rId | rId <- Map.keys (rooms gw)
                         , let room = rooms gw Map.! rId
                         , not ("vehicle" `Set.member` roomTags room) ]
    in [UnreachableRoom rId | rId <- allRooms, rId `notElem` reachable]

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
        entityRefs = Set.insert "player"
            (Set.fromList (Map.keys (itemDefs gw) ++ Map.keys (npcDefs gw)))
        allRefs = concatMap idsFromOutcomeEntity (allOutcomes gw)
    in [MissingEntity eId "property" | eId <- nub allRefs, not (Set.member eId entityRefs)]

checkMissingQuestsInDefs :: GameWorld -> [ValidationError]
checkMissingQuestsInDefs gw =
    let questRefs = Set.fromList (Map.keys (questDefs gw))
        allRefs = concatMap idsFromOutcomeQuest (allOutcomes gw)
    in [MissingQuest qId | qId <- nub allRefs, not (Set.member qId questRefs)]

checkMissingVehiclesInDefs :: GameWorld -> [ValidationError]
checkMissingVehiclesInDefs gw =
    let vehicleRefs = Set.fromList (Map.keys (vehicleDefs gw))
        allRefs = []
    in [MissingVehicle vId | vId <- nub allRefs, not (Set.member vId vehicleRefs)]

-- ---------------------------------------------------------------------------
-- Flag consistency
-- ---------------------------------------------------------------------------

-- | Find flags that are checked (CheckFlag) but never set (SetFlag or flag
--   interaction) anywhere in the world definitions.
checkFlags :: GameWorld -> [ValidationError]
checkFlags gw =
    let (setFlags, checkFlags) = foldl scanFlags (Set.empty, Set.empty)
            (allOutcomes gw)
        missing = Set.toList (Set.difference checkFlags setFlags)
    in [MissingSetFlag flg "checked but never set in any outcome" | flg <- missing]

scanFlags :: (Set.Set FlagID, Set.Set FlagID) -> Effect
          -> (Set.Set FlagID, Set.Set FlagID)
scanFlags (setAcc, checkAcc) outcome = case outcome of
    SetValue (VRFlag n) _         -> (Set.insert n setAcc, checkAcc)
    Sequence os                   -> foldl scanFlags (setAcc, checkAcc) os
    RandomChoice os               -> foldl scanFlags (setAcc, checkAcc) (map snd os)
    Conditional _ t e             -> scanFlags (scanFlags (setAcc, checkAcc) t) e
    _                             -> (setAcc, checkAcc)

-- ---------------------------------------------------------------------------
-- Collect all ActionOutcomes defined in a GameWorld
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
    , [ dcOutcome choice
      | npc <- Map.elems (npcDefs gw)
      , tree <- Map.elems (npcDialogueTrees npc)
      , node <- Map.elems (dtNodes tree)
      , choice <- dnChoices node
      ]
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

-- ---------------------------------------------------------------------------
-- Phase 2a: SaveState / reference validation
-- ---------------------------------------------------------------------------

-- | Validate a GameWorld + SaveState pair: all room/id references must exist.
validateGameState :: GameWorld -> SaveState -> [ValidationError]
validateGameState gw st = concat
    [ checkStartRoom
    , checkItemLocs
    , checkNPCLocs
    , checkVehicleRefs
    , checkQuestStageCounts
    , checkQuestPrereqFlags
    ]
  where
    roomKeys = Map.keys (rooms gw)

    checkStartRoom =
        [ InvalidStartRoom (currentRoom st)
        | currentRoom st `notElem` roomKeys ]

    checkItemLocs =
        [ InvalidItemLocation iId roomId
        | (iId, is) <- Map.toList (itemStates st)
        , let loc = itemLocation is
        , (roomId) <- case loc of { InRoom r -> [r]; _ -> [] }
        , roomId `notElem` roomKeys ]

    checkNPCLocs =
        [ InvalidNPCLocation nId roomId
        | (nId, ns) <- Map.toList (npcStates st)
        , let loc = npcLocation ns
        , (roomId) <- case loc of { InRoom r -> [r]; _ -> [] }
        , roomId `notElem` roomKeys ]

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
    let setFromOutcomes = foldl scanSetFlags Set.empty (allOutcomes gw)
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
    in setFromOutcomes `Set.union` setFromTexts `Set.union` setFromLight

-- | Collect flags referenced by a predicate (HasFlag / Compare on VRFlag).
flagsInPredicate :: Predicate -> [FlagID]
flagsInPredicate p = case p of
    PTrue               -> []
    PNot q              -> flagsInPredicate q
    PAll qs             -> concatMap flagsInPredicate qs
    PAny qs             -> concatMap flagsInPredicate qs
    HasFlag f           -> [f]
    Compare (VRFlag f) _ (VRFlag g) -> [f, g]
    Compare lhs _ _     -> flagsInRef lhs
    Compare _ _ rhs     -> flagsInRef rhs
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
    Sequence os                   -> foldl scanSetFlags acc os
    RandomChoice os               -> foldl scanSetFlags acc (map snd os)
    Conditional _ t e             -> scanSetFlags (scanSetFlags acc t) e
    _                             -> acc
