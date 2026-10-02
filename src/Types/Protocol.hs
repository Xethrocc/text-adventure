{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Protocol v1 types and codec for the text adventure engine (Plan 1.4).
--
--   This module defines the transport-agnostic JSON protocol between clients
--   (CLI, TUI, WebUI, server, WASM host) and the game engine:
--
--   * 'ClientMsg': command, choose, continue, load_world, save, load.
--   * 'ServerMsg': events (transports '[OutputEvent]' 1:1), snapshot (HUD, map,
--     quests, dialogue, combat, game over), and error.
--   * 'Snapshot': comprehensive presentation-tier snapshot extracted purely
--     from 'GameState' via 'makeSnapshot'.
--   * Deterministic encoding ('encodeSorted'): lexicographically sorted keys
--     at all nesting depths via 'Data.Aeson.Encode.Pretty'.
--   * Explicit protocol versioning: every message in both directions carries
--     a @version@ field ('currentProtocolVersion' = 1).
--
--   Export list is explicit (Phase 0.7 project convention).
module Types.Protocol
    ( -- * Protocol Version
      ProtocolVersion
    , currentProtocolVersion

      -- * Client Messages
    , ClientMsg (..)
    , cmdCommand
    , cmdChoose
    , cmdContinue
    , cmdLoadWorld
    , cmdSave
    , cmdLoad

      -- * Server Messages
    , ServerMsg (..)
    , msgEvents
    , msgSnapshot
    , msgError

      -- * Protocol Errors
    , ProtocolErrorCode (..)
    , ProtocolError (..)

      -- * State Snapshots
    , Snapshot (..)
    , PlayerSnapshot (..)
    , RoomSnapshot (..)
    , ExitSnapshot (..)
    , ItemSummary (..)
    , NpcSummary (..)
    , ConditionSnapshot (..)
    , QuestSnapshot (..)
    , ActiveQuestSnapshot (..)
    , CompletedQuestSnapshot (..)
    , DialogueSnapshot (..)
    , ChoiceSnapshot (..)
    , CombatSnapshot (..)
    , GameOverSnapshot (..)

      -- * Construction and Codec
    , makeSnapshot
    , sessionLinesToEvents
    , encodeSorted
    , decodeClientMsg
    , decodeServerMsg
    ) where

import Control.Applicative ((<|>))
import qualified Data.Aeson as Aeson
import Data.Aeson
    ( ToJSON (..)
    , FromJSON (..)
    , Value (..)
    , object
    , (.=)
    , (.:)
    , (.:?)
    , (.!=)
    , withObject
    , withText
    )
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.Generics (Generic)

import Types.Core
    ( GameState (..)
    , SaveState (..)
    , GameWorld (..)
    , Player (..)
    , Room (..)
    , Exit (..)
    , ItemDef (..)
    , ItemState (..)
    , Location (..)
    , ActorRef (..)
    , EquipEffect (..)
    , CondText (..)
    , Predicate (..)
    , Condition (..)
    , Quest (..)
    , QuestStage (..)
    , DialogueChoice (..)
    , DialogueNode (..)
    , DialogueTree (..)
    , NPCDef (..)
    , NPCState (..)
    , GameOverReason (..)
    , VariableValue (..)
    , RoomID
    , ItemID
    , NPCID
    , QuestID
    )
import Types.Output
    ( OutputEvent (..)
    , styledText
    )

-- ---------------------------------------------------------------------------
-- Protocol Version
-- ---------------------------------------------------------------------------

-- | Protocol version number.
type ProtocolVersion = Int

-- | Current protocol version (v1).
currentProtocolVersion :: ProtocolVersion
currentProtocolVersion = 1

-- ---------------------------------------------------------------------------
-- Protocol Errors
-- ---------------------------------------------------------------------------

-- | Error code indicating why a message was rejected.
data ProtocolErrorCode
    = ErrVersionMismatch
    | ErrUnknownType
    | ErrMalformedPayload
    | ErrSessionError
    deriving (Show, Eq, Generic)

instance ToJSON ProtocolErrorCode where
    toJSON c = case c of
        ErrVersionMismatch  -> "version_mismatch"
        ErrUnknownType      -> "unknown_type"
        ErrMalformedPayload -> "malformed_payload"
        ErrSessionError     -> "session_error"

instance FromJSON ProtocolErrorCode where
    parseJSON = withText "ProtocolErrorCode" $ \t -> case t of
        "version_mismatch"  -> pure ErrVersionMismatch
        "unknown_type"      -> pure ErrUnknownType
        "malformed_payload" -> pure ErrMalformedPayload
        "session_error"     -> pure ErrSessionError
        _                   -> pure ErrMalformedPayload

-- | Structured error payload sent on protocol failure.
data ProtocolError = ProtocolError
    { peCode    :: ProtocolErrorCode
    , peMessage :: String
    } deriving (Show, Eq, Generic)

instance ToJSON ProtocolError where
    toJSON pe = object
        [ "code"    .= peCode pe
        , "message" .= peMessage pe
        ]

instance FromJSON ProtocolError where
    parseJSON = withObject "ProtocolError" $ \o -> ProtocolError
        <$> o .: "code"
        <*> o .: "message"

-- ---------------------------------------------------------------------------
-- Client Messages
-- ---------------------------------------------------------------------------

-- | Messages sent from client to engine.
data ClientMsg
    = ClientCommand
        { cmVersion :: ProtocolVersion
        , cmCommand :: String
        }
    | ClientChoose
        { cmVersion :: ProtocolVersion
        , cmChoice  :: Int
        }
    | ClientContinue
        { cmVersion :: ProtocolVersion
        }
    | ClientLoadWorld
        { cmVersion   :: ProtocolVersion
        , cmWorldPath :: FilePath
        }
    | ClientSave
        { cmVersion :: ProtocolVersion
        , cmSlot    :: String
        }
    | ClientLoad
        { cmVersion :: ProtocolVersion
        , cmSlot    :: String
        }
    deriving (Show, Eq, Generic)

-- | Construct a 'ClientCommand' with current protocol version.
cmdCommand :: String -> ClientMsg
cmdCommand = ClientCommand currentProtocolVersion

-- | Construct a 'ClientChoose' with current protocol version.
cmdChoose :: Int -> ClientMsg
cmdChoose = ClientChoose currentProtocolVersion

-- | Construct a 'ClientContinue' with current protocol version.
cmdContinue :: ClientMsg
cmdContinue = ClientContinue currentProtocolVersion

-- | Construct a 'ClientLoadWorld' with current protocol version.
cmdLoadWorld :: FilePath -> ClientMsg
cmdLoadWorld = ClientLoadWorld currentProtocolVersion

-- | Construct a 'ClientSave' with current protocol version.
cmdSave :: String -> ClientMsg
cmdSave = ClientSave currentProtocolVersion

-- | Construct a 'ClientLoad' with current protocol version.
cmdLoad :: String -> ClientMsg
cmdLoad = ClientLoad currentProtocolVersion

instance ToJSON ClientMsg where
    toJSON msg = case msg of
        ClientCommand v cmd     -> object [ "version" .= v, "type" .= ("command" :: String), "command" .= cmd ]
        ClientChoose v choice   -> object [ "version" .= v, "type" .= ("choose" :: String), "choice" .= choice ]
        ClientContinue v        -> object [ "version" .= v, "type" .= ("continue" :: String) ]
        ClientLoadWorld v path  -> object [ "version" .= v, "type" .= ("load_world" :: String), "world_path" .= path ]
        ClientSave v slot       -> object [ "version" .= v, "type" .= ("save" :: String), "slot" .= slot ]
        ClientLoad v slot       -> object [ "version" .= v, "type" .= ("load" :: String), "slot" .= slot ]

instance FromJSON ClientMsg where
    parseJSON = withObject "ClientMsg" $ \o -> do
        v <- o .: "version"
        if v /= currentProtocolVersion
            then fail ("Unsupported protocol version " ++ show (v :: Int) ++ ", expected " ++ show currentProtocolVersion)
            else do
                t <- o .: "type"
                case (t :: String) of
                    "command"    -> ClientCommand v <$> o .: "command"
                    "choose"     -> ClientChoose v <$> o .: "choice"
                    "continue"   -> pure (ClientContinue v)
                    "load_world" -> ClientLoadWorld v <$> (o .: "world_path" <|> o .: "worldPath")
                    "save"       -> ClientSave v <$> o .: "slot"
                    "load"       -> ClientLoad v <$> o .: "slot"
                    other        -> fail ("Unknown ClientMsg type: " ++ other)

-- ---------------------------------------------------------------------------
-- State Snapshots
-- ---------------------------------------------------------------------------

-- | One exit connection from the current room.
data ExitSnapshot = ExitSnapshot
    { esDirection  :: String
    , esTargetRoom :: RoomID
    , esLocked     :: Bool
    } deriving (Show, Eq, Generic)

instance ToJSON ExitSnapshot where
    toJSON es = object
        [ "direction" .= esDirection es
        , "target"    .= esTargetRoom es
        , "locked"    .= esLocked es
        ]

instance FromJSON ExitSnapshot where
    parseJSON = withObject "ExitSnapshot" $ \o -> ExitSnapshot
        <$> o .: "direction"
        <*> o .: "target"
        <*> o .:? "locked" .!= False

-- | Item summary for inventory or room listing.
data ItemSummary = ItemSummary
    { isId          :: ItemID
    , isName        :: String
    , isDescription :: String
    } deriving (Show, Eq, Generic)

instance ToJSON ItemSummary where
    toJSON is = object
        [ "id"          .= isId is
        , "name"        .= isName is
        , "description" .= isDescription is
        ]

instance FromJSON ItemSummary where
    parseJSON = withObject "ItemSummary" $ \o -> ItemSummary
        <$> o .: "id"
        <*> o .: "name"
        <*> o .:? "description" .!= ""

-- | NPC summary in the current room.
data NpcSummary = NpcSummary
    { nsId      :: NPCID
    , nsName    :: String
    , nsAlive   :: Bool
    , nsCarried :: [ItemSummary]  -- ^ B7: carried items (omitted in JSON when empty)
    } deriving (Show, Eq, Generic)

instance ToJSON NpcSummary where
    toJSON ns = object $
        [ "id"    .= nsId ns
        , "name"  .= nsName ns
        , "alive" .= nsAlive ns
        ] ++ [ "carried" .= nsCarried ns | not (null (nsCarried ns)) ]

instance FromJSON NpcSummary where
    parseJSON = withObject "NpcSummary" $ \o -> NpcSummary
        <$> o .: "id"
        <*> o .: "name"
        <*> o .:? "alive" .!= True
        <*> o .:? "carried" .!= []

-- | Condition (status effect) summary.
data ConditionSnapshot = ConditionSnapshot
    { csName      :: String
    , csRemaining :: Int
    } deriving (Show, Eq, Generic)

instance ToJSON ConditionSnapshot where
    toJSON cs = object
        [ "name"            .= csName cs
        , "remaining_turns" .= csRemaining cs
        ]

instance FromJSON ConditionSnapshot where
    parseJSON = withObject "ConditionSnapshot" $ \o -> ConditionSnapshot
        <$> o .: "name"
        <*> (o .: "remaining_turns" <|> o .: "remainingTurns")

-- | Player statistics and gear for HUD rendering.
data PlayerSnapshot = PlayerSnapshot
    { psHealth     :: Int
    , psMaxHealth  :: Int
    , psAttack     :: Int
    , psDefense    :: Int
    , psGold       :: Maybe Int
    , psConditions :: [ConditionSnapshot]
    , psEquipment  :: [(String, ItemID)]
    , psInventory  :: [ItemSummary]
    } deriving (Show, Eq, Generic)

instance ToJSON PlayerSnapshot where
    toJSON ps = object
        [ "health"     .= psHealth ps
        , "max_health" .= psMaxHealth ps
        , "attack"     .= psAttack ps
        , "defense"    .= psDefense ps
        , "gold"       .= psGold ps
        , "conditions" .= psConditions ps
        , "equipment"  .= [ object ["slot" .= slot, "item" .= iId] | (slot, iId) <- psEquipment ps ]
        , "inventory"  .= psInventory ps
        ]

instance FromJSON PlayerSnapshot where
    parseJSON = withObject "PlayerSnapshot" $ \o -> do
        h <- o .: "health"
        mh <- o .: "max_health" <|> o .: "maxHealth"
        atk <- o .: "attack"
        def <- o .: "defense"
        g <- o .:? "gold"
        conds <- o .:? "conditions" .!= []
        eqPairs <- o .:? "equipment" .!= []
        eq <- mapM parseEqPair eqPairs
        inv <- o .:? "inventory" .!= []
        pure (PlayerSnapshot h mh atk def g conds eq inv)
      where
        parseEqPair (Object obj) = (,) <$> obj .: "slot" <*> obj .: "item"
        parseEqPair (Array arr) | [String s, String i] <- F.toList arr = pure (T.unpack s, T.unpack i)
        parseEqPair _ = fail "Invalid equipment pair in PlayerSnapshot"

-- | Room and surrounding map snapshot.
data RoomSnapshot = RoomSnapshot
    { rsId          :: RoomID
    , rsName        :: String
    , rsDescription :: String
    , rsExits       :: [ExitSnapshot]
    , rsItems       :: [ItemSummary]
    , rsNpcs        :: [NpcSummary]
    , rsVehicle     :: Maybe String
    } deriving (Show, Eq, Generic)

instance ToJSON RoomSnapshot where
    toJSON rs = object
        [ "id"          .= rsId rs
        , "name"        .= rsName rs
        , "description" .= rsDescription rs
        , "exits"       .= rsExits rs
        , "items"       .= rsItems rs
        , "npcs"        .= rsNpcs rs
        , "vehicle"     .= rsVehicle rs
        ]

instance FromJSON RoomSnapshot where
    parseJSON = withObject "RoomSnapshot" $ \o -> RoomSnapshot
        <$> o .: "id"
        <*> o .: "name"
        <*> o .:? "description" .!= ""
        <*> o .:? "exits" .!= []
        <*> o .:? "items" .!= []
        <*> o .:? "npcs" .!= []
        <*> o .:? "vehicle"

-- | Active quest snapshot.
data ActiveQuestSnapshot = ActiveQuestSnapshot
    { activeQuestId               :: QuestID
    , activeQuestName             :: String
    , activeQuestStageIndex       :: Int
    , activeQuestStageDescription :: String
    } deriving (Show, Eq, Generic)

instance ToJSON ActiveQuestSnapshot where
    toJSON aq = object
        [ "id"                .= activeQuestId aq
        , "name"              .= activeQuestName aq
        , "stage_index"       .= activeQuestStageIndex aq
        , "stage_description" .= activeQuestStageDescription aq
        ]

instance FromJSON ActiveQuestSnapshot where
    parseJSON = withObject "ActiveQuestSnapshot" $ \o -> ActiveQuestSnapshot
        <$> o .: "id"
        <*> o .: "name"
        <*> (o .: "stage_index" <|> o .: "stageIndex")
        <*> (o .: "stage_description" <|> o .: "stageDescription")

-- | Completed quest snapshot.
data CompletedQuestSnapshot = CompletedQuestSnapshot
    { completedQuestId   :: QuestID
    , completedQuestName :: String
    } deriving (Show, Eq, Generic)

instance ToJSON CompletedQuestSnapshot where
    toJSON cq = object
        [ "id"   .= completedQuestId cq
        , "name" .= completedQuestName cq
        ]

instance FromJSON CompletedQuestSnapshot where
    parseJSON = withObject "CompletedQuestSnapshot" $ \o -> CompletedQuestSnapshot
        <$> o .: "id"
        <*> o .: "name"

-- | Quest state for HUD journal.
data QuestSnapshot = QuestSnapshot
    { qsActive    :: [ActiveQuestSnapshot]
    , qsCompleted :: [CompletedQuestSnapshot]
    } deriving (Show, Eq, Generic)

instance ToJSON QuestSnapshot where
    toJSON qs = object
        [ "active"    .= qsActive qs
        , "completed" .= qsCompleted qs
        ]

instance FromJSON QuestSnapshot where
    parseJSON = withObject "QuestSnapshot" $ \o -> QuestSnapshot
        <$> o .:? "active" .!= []
        <*> o .:? "completed" .!= []

-- | One selectable dialogue choice.
data ChoiceSnapshot = ChoiceSnapshot
    { csIndex    :: Int
    , csText     :: String
    , csNextNode :: Maybe String
    } deriving (Show, Eq, Generic)

instance ToJSON ChoiceSnapshot where
    toJSON cs = object
        [ "index"     .= csIndex cs
        , "text"      .= csText cs
        , "next_node" .= csNextNode cs
        ]

instance FromJSON ChoiceSnapshot where
    parseJSON = withObject "ChoiceSnapshot" $ \o -> ChoiceSnapshot
        <$> o .: "index"
        <*> o .: "text"
        <*> (o .:? "next_node" <|> o .:? "nextNode")

-- | Active dialogue state (choices and prompt).
data DialogueSnapshot = DialogueSnapshot
    { dsNpcId   :: NPCID
    , dsNpcName :: String
    , dsNodeId  :: String
    , dsText    :: String
    , dsChoices :: [ChoiceSnapshot]
    } deriving (Show, Eq, Generic)

instance ToJSON DialogueSnapshot where
    toJSON ds = object
        [ "npc_id"   .= dsNpcId ds
        , "npc_name" .= dsNpcName ds
        , "node_id"  .= dsNodeId ds
        , "text"     .= dsText ds
        , "choices"  .= dsChoices ds
        ]

instance FromJSON DialogueSnapshot where
    parseJSON = withObject "DialogueSnapshot" $ \o -> DialogueSnapshot
        <$> (o .: "npc_id" <|> o .: "npcId")
        <*> (o .: "npc_name" <|> o .: "npcName")
        <*> (o .: "node_id" <|> o .: "nodeId")
        <*> o .: "text"
        <*> o .:? "choices" .!= []

-- | Combat state summary.
data CombatSnapshot = CombatSnapshot
    { csEngaged :: Bool
    } deriving (Show, Eq, Generic)

instance ToJSON CombatSnapshot where
    toJSON cs = object
        [ "engaged" .= csEngaged cs
        ]

instance FromJSON CombatSnapshot where
    parseJSON = withObject "CombatSnapshot" $ \o -> CombatSnapshot
        <$> o .: "engaged"

-- | Game over reason and menu options.
data GameOverSnapshot = GameOverSnapshot
    { goReason :: String
    , goMenu   :: [String]
    } deriving (Show, Eq, Generic)

instance ToJSON GameOverSnapshot where
    toJSON go = object
        [ "reason" .= goReason go
        , "menu"   .= goMenu go
        ]

instance FromJSON GameOverSnapshot where
    parseJSON = withObject "GameOverSnapshot" $ \o -> GameOverSnapshot
        <$> o .: "reason"
        <*> o .:? "menu" .!= []

-- | Comprehensive game snapshot for frontend presentation.
data Snapshot = Snapshot
    { snapTurn         :: Int
    , snapPlayer       :: PlayerSnapshot
    , snapRoom         :: RoomSnapshot
    , snapQuests       :: QuestSnapshot
    , snapDialogue     :: Maybe DialogueSnapshot
    , snapCombat       :: CombatSnapshot
    , snapGameOver     :: Maybe GameOverSnapshot
    , snapVisitedRooms :: [RoomID]
    } deriving (Show, Eq, Generic)

instance ToJSON Snapshot where
    toJSON s = object
        [ "turn"          .= snapTurn s
        , "player"        .= snapPlayer s
        , "room"          .= snapRoom s
        , "quests"        .= snapQuests s
        , "dialogue"      .= snapDialogue s
        , "combat"        .= snapCombat s
        , "game_over"     .= snapGameOver s
        , "visited_rooms" .= snapVisitedRooms s
        ]

instance FromJSON Snapshot where
    parseJSON = withObject "Snapshot" $ \o -> do
        turn <- o .: "turn"
        p <- o .: "player"
        r <- o .: "room"
        q <- o .: "quests"
        d <- o .:? "dialogue"
        c <- o .: "combat"
        mGo1 <- o .:? "game_over"
        mGo2 <- o .:? "gameOver"
        let go = mGo1 <|> mGo2
        mVisited1 <- o .:? "visited_rooms"
        mVisited2 <- o .:? "visitedRooms"
        let visited = fromMaybe [] (mVisited1 <|> mVisited2)
        pure (Snapshot turn p r q d c go visited)

-- ---------------------------------------------------------------------------
-- Server Messages
-- ---------------------------------------------------------------------------

-- | Messages sent from engine to client.
data ServerMsg
    = ServerEvents
        { smVersion :: ProtocolVersion
        , smEvents  :: [OutputEvent]
        }
    | ServerSnapshot
        { smVersion  :: ProtocolVersion
        , smSnapshot :: Snapshot
        }
    | ServerError
        { smVersion :: ProtocolVersion
        , smError   :: ProtocolError
        }
    deriving (Show, Eq, Generic)

-- | Construct a 'ServerEvents' message with current protocol version.
msgEvents :: [OutputEvent] -> ServerMsg
msgEvents = ServerEvents currentProtocolVersion

-- | Construct a 'ServerSnapshot' message with current protocol version.
msgSnapshot :: Snapshot -> ServerMsg
msgSnapshot = ServerSnapshot currentProtocolVersion

-- | Construct a 'ServerError' message with current protocol version.
msgError :: ProtocolErrorCode -> String -> ServerMsg
msgError code msg = ServerError currentProtocolVersion (ProtocolError code msg)

instance ToJSON ServerMsg where
    toJSON msg = case msg of
        ServerEvents v evs    -> object [ "version" .= v, "type" .= ("events" :: String), "events" .= evs ]
        ServerSnapshot v snap -> object [ "version" .= v, "type" .= ("snapshot" :: String), "snapshot" .= snap ]
        ServerError v err     -> object [ "version" .= v, "type" .= ("error" :: String), "error" .= err ]

instance FromJSON ServerMsg where
    parseJSON = withObject "ServerMsg" $ \o -> do
        v <- o .: "version"
        if v /= currentProtocolVersion
            then fail ("Unsupported protocol version " ++ show (v :: Int) ++ ", expected " ++ show currentProtocolVersion)
            else do
                t <- o .: "type"
                case (t :: String) of
                    "events"   -> ServerEvents v <$> o .: "events"
                    "snapshot" -> ServerSnapshot v <$> o .: "snapshot"
                    "error"    -> ServerError v <$> o .: "error"
                    other      -> fail ("Unknown ServerMsg type: " ++ other)

-- ---------------------------------------------------------------------------
-- Codec & Construction Helpers
-- ---------------------------------------------------------------------------

-- | Canonical deterministic JSON encoding with lexicographically sorted keys.
--   Guarantees stable, reproducible bytes across runs and architectures.
encodeSorted :: ToJSON a => a -> BLC.ByteString
encodeSorted = Pretty.encodePretty' (Pretty.defConfig { Pretty.confCompare = compare })

-- | Message type discriminators this protocol version understands, per direction.
clientMsgTypes, serverMsgTypes :: [T.Text]
clientMsgTypes = ["command", "choose", "continue", "load_world", "save", "load"]
serverMsgTypes = ["events", "snapshot", "error"]

-- | Classify a payload that failed to parse. An unrecognized @type@
--   discriminator gets its own code, so a client built against a newer protocol
--   hears *why* it was rejected instead of a generic 'ErrMalformedPayload'
--   (forward compatibility; spec section 2.3).
payloadErrorCode :: [T.Text] -> KeyMap.KeyMap Aeson.Value -> ProtocolErrorCode
payloadErrorCode known obj = case KeyMap.lookup "type" obj of
    Just (Aeson.String t) | t `notElem` known -> ErrUnknownType
    _                                         -> ErrMalformedPayload

-- | Decode a client message from JSON bytes, enforcing the protocol version.
decodeClientMsg :: BL.ByteString -> Either ProtocolError ClientMsg
decodeClientMsg bs = case Aeson.eitherDecode bs of
    Right msg -> Right msg
    Left err  -> case Aeson.decode bs of
        Just (Object obj) -> case KeyMap.lookup "version" obj of
            Just vVal -> case Aeson.fromJSON vVal of
                Aeson.Success v
                    | v /= currentProtocolVersion ->
                        Left (ProtocolError ErrVersionMismatch
                                ("Unsupported protocol version " ++ show (v :: Int)
                                 ++ ", expected " ++ show currentProtocolVersion))
                    | otherwise -> Left (ProtocolError (payloadErrorCode clientMsgTypes obj) err)
                Aeson.Error _ ->
                    Left (ProtocolError ErrMalformedPayload "Invalid 'version' field format")
            Nothing -> Left (ProtocolError ErrMalformedPayload "Missing required 'version' field")
        _ -> Left (ProtocolError ErrMalformedPayload err)

-- | Decode a server message from JSON bytes, enforcing the protocol version.
decodeServerMsg :: BL.ByteString -> Either ProtocolError ServerMsg
decodeServerMsg bs = case Aeson.eitherDecode bs of
    Right msg -> Right msg
    Left err  -> case Aeson.decode bs of
        Just (Object obj) -> case KeyMap.lookup "version" obj of
            Just vVal -> case Aeson.fromJSON vVal of
                Aeson.Success v
                    | v /= currentProtocolVersion ->
                        Left (ProtocolError ErrVersionMismatch
                                ("Unsupported protocol version " ++ show (v :: Int)
                                 ++ ", expected " ++ show currentProtocolVersion))
                    | otherwise -> Left (ProtocolError (payloadErrorCode serverMsgTypes obj) err)
                Aeson.Error _ ->
                    Left (ProtocolError ErrMalformedPayload "Invalid 'version' field format")
            Nothing -> Left (ProtocolError ErrMalformedPayload "Missing required 'version' field")
        _ -> Left (ProtocolError ErrMalformedPayload err)

-- | Bridge session transition text lines into protocol wire events (Plan 1.4, constraint 6).
--   Wraps each plain line into an 'EvText' event without styling spans.
--   Phase 4.3: the caller renders these lines with the world's effective
--   catalog ('Messages.renderMsgFor') — the wire carries final text. Keyed
--   events in a 'ServerMsg' must likewise pass 'Messages.localizeEventsFor'
--   at the server boundary before encoding.
sessionLinesToEvents :: [String] -> [OutputEvent]
sessionLinesToEvents = map (EvText . styledText)

-- | Construct a complete snapshot from a 'GameState'.
makeSnapshot :: GameState -> Snapshot
makeSnapshot state = Snapshot
    { snapTurn         = turnCount ss
    , snapPlayer       = pSnap
    , snapRoom         = rSnap
    , snapQuests       = qSnap
    , snapDialogue     = dSnap
    , snapCombat       = cSnap
    , snapGameOver     = goSnap
    , snapVisitedRooms = Set.toList (visitedRooms ss)
    }
  where
    gw = world state
    ss = save state
    p  = player ss
    curRoomId = currentRoom ss
    mRoom = Map.lookup curRoomId (dynamicRooms ss) <|> Map.lookup curRoomId (rooms gw)

    -- Player snapshot
    pSnap = PlayerSnapshot
        { psHealth     = playerHealth p
        , psMaxHealth  = calcMaxHealth
        , psAttack     = calcAttack
        , psDefense    = calcDefense
        , psGold       = case Map.lookup "gold" (variables ss) of
            Just (VVInt g) -> Just g
            _              -> Nothing
        , psConditions = [ ConditionSnapshot (condName c) (condRemaining c)
                         | c <- Map.elems (conditions ss)
                         , not (condHidden c) ]
        , psEquipment  = [ (show slot, iId) | (slot, iId) <- Map.toList (equipment ss) ]
        , psInventory  = [ ItemSummary (itemId def) (itemName def) (ctDefault (itemDescription def))
                         | (iId, st) <- Map.toList (itemStates ss)
                         , itemLocation st == CarriedBy ActorPlayer
                         , Just def <- [Map.lookup iId (itemDefs gw)] ]
        }

    sumEquip f = sum
        [ v
        | iId <- Map.elems (equipment ss)
        , Just def <- [Map.lookup iId (itemDefs gw)]
        , eff <- itemEquipEffects def
        , Just v <- [f eff]
        ]

    calcAttack = playerAttack p + sumEquip atkBonus
      where
        atkBonus (AttackBonus n) = Just n
        atkBonus _               = Nothing

    calcDefense = playerDefense p + sumEquip defBonus
      where
        defBonus (DefenseBonus n) = Just n
        defBonus _                = Nothing

    calcMaxHealth = playerMaxHealth p + sumEquip hpBonus
      where
        hpBonus (MaxHealthBonus n) = Just n
        hpBonus _                  = Nothing

    -- Room snapshot
    effectiveConns = case mRoom of
        Nothing   -> []
        Just r    ->
            let baseConns = roomConnections r
                overrides = Map.filterWithKey (\(roomKey, _) _ -> roomKey == curRoomId) (exitOverrides ss)
                applied = Map.foldlWithKey step baseConns overrides
            in Map.toList applied
      where
        step acc (_, dir) (Just exit) = Map.insert dir exit acc
        step acc (_, dir) Nothing     = Map.delete dir acc

    exitSnapshot (dir, Open dest)        = ExitSnapshot (show dir) dest False
    exitSnapshot (dir, Locked dest _)    = ExitSnapshot (show dir) dest True
    exitSnapshot (dir, Guarded dest _ _) = ExitSnapshot (show dir) dest True

    rSnap = RoomSnapshot
        { rsId          = curRoomId
        , rsName        = maybe curRoomId roomName mRoom
        , rsDescription = maybe "" (ctDefault . roomDescription) mRoom
        , rsExits       = map exitSnapshot effectiveConns
        , rsItems       = [ ItemSummary (itemId def) (itemName def) (ctDefault (itemDescription def))
                          | (iId, st) <- Map.toList (itemStates ss)
                          , itemLocation st == InRoom curRoomId
                          , Just def <- [Map.lookup iId (itemDefs gw)]
                          , not (itemHidden def) || itemDiscovered st ]
        , rsNpcs        = [ NpcSummary (npcId def) (npcName def) (npcAlive st) (carriedOf nId)
                          | (nId, st) <- Map.toList (npcStates ss)
                          , npcLocation st == InRoom curRoomId
                          , Just def <- [Map.lookup nId (npcDefs gw)] ]
        , rsVehicle     = currentVehicle ss
        }
    npcAlive st = maybe "alive" npcStatus (Just st) /= "dead"
    -- B7: items an NPC carries (hidden ones need discovery, like everywhere)
    carriedOf nId =
        [ ItemSummary (itemId d) (itemName d) (ctDefault (itemDescription d))
        | (iId, ist) <- Map.toList (itemStates ss)
        , itemLocation ist == CarriedBy (ActorNPC nId)
        , Just d <- [Map.lookup iId (itemDefs gw)]
        , not (itemHidden d) || itemDiscovered ist ]

    -- Quest snapshot
    qSnap = QuestSnapshot
        { qsActive    = [ ActiveQuestSnapshot qId (questName q) idx (stageDesc q idx)
                        | (qId, idx) <- Map.toList (activeQuests ss)
                        , Just q <- [Map.lookup qId (questDefs gw)] ]
        , qsCompleted = [ CompletedQuestSnapshot qId (maybe qId questName (Map.lookup qId (questDefs gw)))
                        | qId <- Set.toList (completedQuests ss) ]
        }
    stageDesc q idx
        | idx >= 0 && idx < length (questStages q) = qsText (questStages q !! idx)
        | otherwise = ""

    -- Dialogue snapshot
    dSnap = case activeDialogue ss of
        Nothing -> Nothing
        Just nId -> case Map.lookup nId (npcDefs gw) of
            Nothing -> Nothing
            Just npc ->
                let ns = Map.lookup nId (npcStates ss)
                    status = maybe "alive" npcStatus ns
                in case Map.lookup status (npcDialogueTrees npc) of
                    Nothing -> Nothing
                    Just tree ->
                        let nodeId = fromMaybe (dtEntry tree) (ns >>= npcDialogueNode)
                        in case Map.lookup nodeId (dtNodes tree) of
                            Nothing -> Nothing
                            Just node ->
                                let choices = filter isChoiceVisible (dnChoices node)
                                    choiceSnaps = [ ChoiceSnapshot idx (dcText c) (dcNextNode c)
                                                  | (idx, c) <- zip [1..] choices ]
                                in Just (DialogueSnapshot nId (npcName npc) nodeId (dnText node) choiceSnaps)
    isChoiceVisible c = case dcVisible c of
        Nothing              -> True
        Just PTrue           -> True
        Just (HasFlag f)     -> Map.lookup f (flags ss) == Just "true"
        Just (PlayerHas iId) -> any (\st -> itemLocation st == CarriedBy ActorPlayer) (Map.lookup iId (itemStates ss))
        Just (VarIs v val)   -> case Map.lookup v (variables ss) of
            Just (VVText s) -> s == val
            _               -> False
        Just (HasCondition cond) -> Map.member cond (conditions ss)
        Just _               -> True

    -- Combat snapshot
    cSnap = CombatSnapshot
        { csEngaged = case Map.lookup "combat.engaged" (variables ss) of
            Just (VVInt n) -> n /= 0
            _              -> False
        }

    -- GameOver snapshot
    goSnap = if not (gameOver ss)
        then Nothing
        else Just $ case gameOverReason ss of
            Just Death        -> GameOverSnapshot "death" ["u", "l", "r", "q"]
            Just Victory      -> GameOverSnapshot "victory" ["r", "q"]
            Just (Custom msg) -> GameOverSnapshot ("custom: " ++ msg) ["r", "q"]
            Nothing           -> GameOverSnapshot "ended" ["r", "q"]
