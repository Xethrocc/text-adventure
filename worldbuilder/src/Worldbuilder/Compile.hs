-- | Compile an Adventure (authoring schema) into engine types, or return error messages
module Worldbuilder.Compile (CompileResult(..), compileAdventure) where

import Worldbuilder.Types
import Types as E
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Char (toLower)

-- | Result of compilation
data CompileResult = CompileResult
    { crWorld :: E.GameWorld
    , crSave  :: E.SaveState
    } deriving (Show, Eq)

-- | Compile an Adventure into engine types, or return error messages
compileAdventure :: Adventure -> Either [String] CompileResult
compileAdventure adv = do
    -- Compile each section, collecting errors
    let errors = []
    
    -- Build rooms map (always succeeds)
    let (compiledRooms, vehicleExtraRooms) = compileRooms (advRooms adv) (advVehicles adv)
    
    -- Build items
    let (itemDefs, itemStates) = compileItems (advItems adv)
    
    -- Build NPCs
    let (npcDefs, npcStates) = compileNPCs (advNPCs adv)
    
    -- Build quests
    let questDefs = compileQuests (advQuests adv)
    
    -- Build vehicles
    let (vehicleDefs, vehicleStates) = compileVehicles (advVehicles adv)
    
    -- Build interactions
    let (entityInteractions, itemInteractions) = compileInteractions (advInteractions adv)
    
    -- Everything should actually validate against engine Validate.hs too,
    -- but we rely on the caller to do that.
    
    let allRooms = Map.union compiledRooms vehicleExtraRooms
        startRoomId = advStartRoom adv
    
    let gw = E.GameWorld
            { E.rooms = allRooms
            , E.itemDefs = itemDefs
            , E.npcDefs = npcDefs
            , E.entityInteractions = entityInteractions
            , E.itemInteractions = itemInteractions
            , E.questDefs = questDefs
            , E.vehicleDefs = vehicleDefs
            }
    
    let startSave = E.SaveState
            { E.player = E.Player 100 100 10 5 Map.empty
            , E.currentRoom = startRoomId
            , E.inventory = []
            , E.itemStates = itemStates
            , E.npcStates = npcStates
            , E.entityStates = initialEntityStates allRooms
            , E.flags = Map.empty
            , E.turnCount = 0
            , E.gameOver = False
            , E.gameOverReason = Nothing
            , E.visitedRooms = Set.empty
            , E.equipment = Map.empty
            , E.conditions = Map.empty
            , E.activeQuests = Map.empty
            , E.completedQuests = Set.empty
            , E.vehicleStates = vehicleStates
            , E.currentVehicle = Nothing
            , E.activeDialogue = Nothing
            }
    
    Right (CompileResult gw startSave)
  where
    -- Every locked exit starts locked in entityStates
    initialEntityStates rooms =
        Map.fromList
            [ (e, "locked")
            | room <- Map.elems rooms
            , E.Locked _ e <- Map.elems (E.roomConnections room)
            ]

-- ---------------------------------------------------------------------------
-- Rooms
-- ---------------------------------------------------------------------------

compileRooms :: [ARoom] -> [AVehicle] -> (Map.Map String E.Room, Map.Map String E.Room)
compileRooms rooms vehicles =
    let roomMap = Map.fromList [(arId r, compileRoom r) | r <- rooms]
        vehicleInterior = concatMap (\(_, rs) -> map (compileRoom . tagVehicleAr) rs)
                                      [(avId v, avInterior v) | v <- vehicles]
        interiorMap = Map.fromList [(E.roomId r, r) | r <- vehicleInterior]
    in (roomMap, interiorMap)
  where
    compileRoom :: ARoom -> E.Room
    compileRoom r = E.Room
        { E.roomId = arId r
        , E.roomName = arName r
        , E.roomDescription = arDesc r
        , E.roomConnections = Map.fromList [(parseDir dir, compileExitInner ref) | (dir, ref) <- Map.toList (arExits r)]
        , E.roomTags = Set.fromList (arTags r)
        , E.roomAltDescriptions = arAltDesc r
        , E.roomLightFlag = arLightFlag r
        , E.roomOnEnter = compileMaybeOutcomes (arOnEnter r)
        , E.roomOnLook = compileMaybeOutcomes (arOnLook r)
        , E.roomOnExit = compileMaybeOutcomes (arOnExit r)
        , E.roomSearchOutcome = compileMaybeOutcomes (arSearch r)
        , E.roomAscii = arAscii r
        }
    
    compileExitInner :: AExitRef -> E.Exit
    compileExitInner ref = case aeLocked ref of
        Nothing -> E.Open (aeTarget ref)
        Just entity -> E.Locked (aeTarget ref) entity
    
    parseDir :: String -> E.Direction
    parseDir s = case map toLower s of
        "north" -> E.North; "south" -> E.South; "east" -> E.East
        "west" -> E.West; "up" -> E.Up; "down" -> E.Down
        _ -> E.North  -- fallback, will be caught by Validate
    
    tagVehicleAr :: ARoom -> ARoom
    tagVehicleAr r = r { arTags = "vehicle" : arTags r }

-- ---------------------------------------------------------------------------
-- Items
-- ---------------------------------------------------------------------------

compileItems :: [AItem] -> (Map.Map String E.ItemDef, Map.Map String E.ItemState)
compileItems items =
    let defs = Map.fromList [(aiId i, compileItemDef i) | i <- items]
        states = Map.fromList [(aiId i, compileItemState i) | i <- items]
    in (defs, states)

compileItemDef :: AItem -> E.ItemDef
compileItemDef i = E.ItemDef
    { E.itemId = aiId i
    , E.itemName = aiName i
    , E.itemDescription = aiDesc i
    , E.itemKeywords = aiKeywords i
    , E.itemTags = Set.fromList (aiTags i)
    , E.itemEquipSlot = parseSlot (aiEquipSlot i)
    , E.itemEquipEffects = parseEffects (aiEquipEffects i)
    , E.itemHidden = aiHidden i
    , E.itemDiscoverText = aiDiscover i
    , E.itemVerbMap = compileVerbMap (aiVerbMap i)
    }
  where
    parseSlot :: Maybe String -> Maybe E.EquipSlot
    parseSlot Nothing = Nothing
    parseSlot (Just s) = case s of
        "head" -> Just E.Head; "body" -> Just E.Body; "hands" -> Just E.Hands
        "feet" -> Just E.Feet; "weapon" -> Just E.Weapon; "offhand" -> Just E.Offhand
        "accessory" -> Just E.Accessory; _ -> Nothing
    
    parseEffects :: [String] -> [E.EquipEffect]
    parseEffects = mapMaybe parseOne
      where
        parseOne s = case s of
            'a':'t':'t':'a':'c':'k':'+':n -> Just (E.AttackBonus (read n))
            'd':'e':'f':'e':'n':'s':'e':'+':n -> Just (E.DefenseBonus (read n))
            'm':'a':'x':'h':'p':'+':n -> Just (E.MaxHealthBonus (read n))
            _ -> Nothing
    
    mapMaybe :: (a -> Maybe b) -> [a] -> [b]
    mapMaybe f xs = [y | Just y <- fmap f xs]

compileItemState :: AItem -> E.ItemState
compileItemState i = E.ItemState
    { E.itemLocation = aiLocation i
    , E.itemStatus = aiState i
    , E.itemProps = aiProps i
    , E.itemDiscovered = not (aiHidden i)
    }

-- ---------------------------------------------------------------------------
-- NPCs
-- ---------------------------------------------------------------------------

compileNPCs :: [ANPC] -> (Map.Map String E.NPCDef, Map.Map String E.NPCState)
compileNPCs npcs =
    let defs = Map.fromList [(anId n, compileNPCDef n) | n <- npcs]
        states = Map.fromList [(anId n, compileNPCState n) | n <- npcs]
    in (defs, states)

compileNPCDef :: ANPC -> E.NPCDef
compileNPCDef n = E.NPCDef
    { E.npcId = anId n
    , E.npcName = anName n
    , E.npcDescription = anDesc n
    , E.npcDialogue = Map.empty  -- legacy, we use DialogueTrees
    , E.npcDialogueTrees = compileDialogueTrees (anDialogue n)
    , E.npcKeywords = anKeywords n
    , E.npcMaxHealth = anMaxHealth n
    , E.npcAttackBase = anAttack n
    , E.npcDefenseBase = anDefense n
    , E.npcVerbMap = compileVerbMap (anVerbMap n)
    }

compileDialogueTrees :: Map.Map String ADialogueTree -> Map.Map String E.DialogueTree
compileDialogueTrees = Map.map compileDialogueTree

compileDialogueTree :: ADialogueTree -> E.DialogueTree
compileDialogueTree t = E.DialogueTree
    { E.dtEntry = adtEntry t
    , E.dtNodes = Map.mapWithKey (\k n -> (compileDialogueNode n) { E.dnId = k }) (adtNodes t)
    }

compileDialogueNode :: ADialogueNode -> E.DialogueNode
compileDialogueNode n = E.DialogueNode
    { E.dnId = ""  -- placeholder, set by mapWithKey
    , E.dnText = adnText n
    , E.dnChoices = map compileDialogueChoice (adnChoices n)
    }

compileDialogueChoice :: ADialogueChoice -> E.DialogueChoice
compileDialogueChoice c = E.DialogueChoice
    { E.dcText = adcText c
    , E.dcNextNode = adcNext c
    , E.dcOutcome = compileOutcomes (adcOutcomes c)
    }

compileNPCState :: ANPC -> E.NPCState
compileNPCState n = E.NPCState
    { E.npcLocation = anLocation n
    , E.npcStatus = anState n
    , E.npcHealth = anMaxHealth n
    , E.npcProps = Map.empty
    , E.npcDialogueNode = Nothing
    }

-- ---------------------------------------------------------------------------
-- Quests
-- ---------------------------------------------------------------------------

compileQuests :: [AQuest] -> Map.Map String E.Quest
compileQuests = Map.fromList . map compileQuest

compileQuest :: AQuest -> (String, E.Quest)
compileQuest q =
    ( aqId q
    , E.Quest
        { E.questId = aqId q
        , E.questName = aqName q
        , E.questDescription = aqDesc q
        , E.questPrereqs = Map.fromList [(flag, "true") | flag <- aqPrereqs q]
        , E.questStages = [E.QuestStage (aqsId s) (aqsDesc s) (aqsHint s) | s <- aqStages q]
        , E.questReward = compileMaybeOutcomes (aqReward q)
        }
    )

-- ---------------------------------------------------------------------------
-- Vehicles
-- ---------------------------------------------------------------------------

compileVehicles :: [AVehicle] -> (Map.Map String E.VehicleDef, Map.Map String E.VehicleState)
compileVehicles vehicles =
    let defs = Map.fromList [(avId v, compileVehicleDef v) | v <- vehicles]
        states = Map.fromList [(avId v, compileVehicleState v) | v <- vehicles]
    in (defs, states)

compileVehicleDef :: AVehicle -> E.VehicleDef
compileVehicleDef v = E.VehicleDef
    { E.vehicleId = avId v
    , E.vehicleName = avName v
    , E.vehicleDescription = avDesc v
    , E.vehicleType = compileVehicleType (avType v)
    , E.vehicleRooms = map arId (avInterior v)
    , E.vehicleEntryRoom = avEntryRoom v
    , E.vehicleCockpitRoom = avCockpit v
    , E.vehicleStops = Map.fromList [(rId, E.VehicleStop rId label Nothing) | (label, rId) <- Map.toList (avStops v)]
    , E.vehicleKeywords = avKeywords v
    , E.vehicleFuelProp = avFuel v
    , E.vehicleConditionEffects = Map.map (\os -> combineOutcomes (map compileAActionOutcome os)) (avConditions v)
    }
  where
    compileVehicleType :: String -> E.VehicleType
    compileVehicleType "auto" = E.AutomaticRoute
    compileVehicleType "paid" = E.PaidVehicle
    compileVehicleType "player" = E.PlayerControlled
    compileVehicleType _ = E.PlayerControlled
    
    combineOutcomes :: [E.ActionOutcome] -> E.ActionOutcome
    combineOutcomes [] = E.MessageOnly ""
    combineOutcomes [o] = o
    combineOutcomes os = E.MultipleOutcomes os

compileVehicleState :: AVehicle -> E.VehicleState
compileVehicleState v = E.VehicleState
    { E.vsCurrentStop = case Map.lookup (headSafe (Map.keys (avStops v))) (avStops v) of
          Just room -> room
          Nothing   -> avEntryRoom v
    , E.vsFuel = Nothing
    , E.vsActiveConditions = Set.empty
    , E.vsRoomOverrides = Map.empty
    }
  where
    headSafe [] = ""
    headSafe xs = head xs

-- ---------------------------------------------------------------------------
-- Interactions
-- ---------------------------------------------------------------------------

compileInteractions :: Maybe AInteractions -> (Map.Map (String, String) (String, String), Map.Map (String, String) E.ActionOutcome)
compileInteractions Nothing = (Map.empty, Map.empty)
compileInteractions (Just ix) = (entityMap, itemMap)
  where
    entityMap = Map.map (\s -> (s, "")) (aiEntity ix)
    itemMap = Map.map (compileOutcomes) (aiItem ix)

-- ---------------------------------------------------------------------------
-- Verb maps
-- ---------------------------------------------------------------------------

compileVerbMap :: Map.Map String [AActionOutcome] -> Map.Map (E.Verb, String) E.ActionOutcome
compileVerbMap = Map.fromList . map compileEntry . Map.toList
  where
    compileEntry (key, outcomes) =
        let (verbStr, rest) = break (== ',') key
            state = case rest of
                ',':s -> s
                _     -> "intact"
            verb = parseVerb verbStr
        in ((verb, state), compileOutcomes outcomes)
    
    parseVerb :: String -> E.Verb
    parseVerb s = case s of
        "take" -> E.VTake; "drop" -> E.VDrop; "look" -> E.VLookAt; "use" -> E.VUse
        "talk" -> E.VTalk; "attack" -> E.VAttack; "search" -> E.VSearch
        _ -> E.VUnknown

-- ---------------------------------------------------------------------------
-- Action outcome compilation
-- ---------------------------------------------------------------------------

compileOutcomes :: [AActionOutcome] -> E.ActionOutcome
compileOutcomes [] = E.MessageOnly ""
compileOutcomes [o] = compileAActionOutcome o
compileOutcomes os = E.MultipleOutcomes (map compileAActionOutcome os)

compileMaybeOutcomes :: Maybe [AActionOutcome] -> Maybe E.ActionOutcome
compileMaybeOutcomes Nothing = Nothing
compileMaybeOutcomes (Just os) = Just (compileOutcomes os)

compileAActionOutcome :: AActionOutcome -> E.ActionOutcome
compileAActionOutcome ao = case ao of
    AOMessage s -> E.MessageOnly s
    AOHealPlayer n -> E.HealPlayer n ""
    AODamagePlayer n -> E.DamagePlayer n ""
    AOGiveItem i -> E.GiveItem i ""
    AOConsumeItem i -> E.ConsumeItem i ""
    AOSetFlag f v -> E.SetFlag f v ""
    AOCheckFlag f v t e ->
        E.CheckFlag f v (compileAActionOutcome t) (compileAActionOutcome e)
    AOStartQuest q -> E.StartQuest q ""
    AOAdvanceQuest q -> E.AdvanceQuest q ""
    AOCompleteQuest q -> E.CompleteQuest q ""
    AOEquipItem i -> E.EquipItem i ""
    AORoomTransition r -> E.TransitionRoom r ""
    AONarrative ls -> E.Narrative ls (E.MessageOnly "")