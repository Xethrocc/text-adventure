-- | The bundled sample adventure.
--   Lives outside the library so that `text-adventure` ships a framework,
--   not a hardcoded world.
module Sample (initSampleGame) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Types

-- | Initialize a sample game with rooms, items, NPCs, equipment and a quest
initSampleGame :: GameState
initSampleGame = GameState
    { world = GameWorld
        { rooms = Map.fromList
            [ ("start", Room
                { roomId = "start"
                , roomName = "Starting Room"
                , roomDescription = plainText "You are in a small stone chamber with torches on the walls. There are exits to the north and east. The east door looks sturdy and has a keyhole."
                , roomConnections = Map.fromList [(North, Open "hallway"), (East, Locked "treasure" "treasure_door"), (South, Open "meadow")]
                , roomTags = Set.empty
                , roomLightFlag = Nothing
                , roomOnEnter = Nothing
                , roomOnLook = Nothing
                , roomOnExit = Nothing
                , roomSearchOutcome = Nothing
                , roomAscii = emptyAscii
        , roomIntro = Nothing
                })
            , ("hallway", Room
                { roomId = "hallway"
                , roomName = "Dark Hallway"
                , roomDescription = CondText "A long, dark hallway stretches before you. The air is damp and cold. There's an exit to the south."
                    [TextVariant (HasFlag "torch_lit") "The torches along the wall sputter to life, pushing the darkness back."]
                , roomConnections = Map.fromList [(South, Open "start")]
                , roomTags = Set.fromList ["dark"]
                , roomLightFlag = Just "torch_lit"
                , roomOnEnter = Nothing
                , roomOnLook = Nothing
                , roomOnExit = Nothing
                , roomSearchOutcome = Just (Sequence
                    [ SetValue (VRFlag "torch_lit") (EVString "true")
                    , SendMessage "Faded runes cover the eastern wall." ])
                , roomAscii = emptyAscii
        , roomIntro = Nothing
                })
            , ("treasure", Room
                { roomId = "treasure"
                , roomName = "Treasure Room"
                , roomDescription = plainText "You've entered a magnificent treasure room! Gold coins and jewels are scattered everywhere. There's an exit to the west."
                , roomConnections = Map.fromList [(West, Open "start")]
                , roomTags = Set.empty
                , roomLightFlag = Nothing
                , roomOnEnter = Nothing
                , roomOnLook = Nothing
                , roomOnExit = Nothing
                , roomSearchOutcome = Nothing
                , roomAscii = emptyAscii
        , roomIntro = Nothing
                })
            -- Vehicle demo (Phase 3): a horse-drawn carriage
            , ("meadow", Room
                { roomId = "meadow"
                , roomName = "Sunny Meadow"
                , roomDescription = plainText "A wide meadow stretches to the horizon. Wildflowers sway in the breeze. Your carriage is parked here."
                , roomConnections = Map.fromList [(North, Open "start")]
                , roomTags = Set.empty
                , roomLightFlag = Nothing
                , roomOnEnter = Just (Sequence
                    (map SendMessage ["A gentle breeze rustles the grass.",
                     "Somewhere in the distance, birds sing.",
                     "The carriage horse stamps its foot impatiently."]
                     ++ [SendMessage "You feel at peace here."]))
                , roomOnLook = Nothing
                , roomOnExit = Nothing
                , roomSearchOutcome = Nothing
                , roomAscii = emptyAscii
        , roomIntro = Nothing
                })
            , ("carriage_cabin", Room
                { roomId = "carriage_cabin"
                , roomName = "Carriage Cabin"
                , roomDescription = plainText "You sit inside a comfortably worn carriage. A small window lets you watch the road. The reins are within reach."
                , roomConnections = Map.empty
                , roomTags = Set.fromList ["vehicle"]
                , roomLightFlag = Nothing
                , roomOnEnter = Nothing
                , roomOnLook = Nothing
                , roomOnExit = Nothing
                , roomSearchOutcome = Nothing
                , roomAscii = emptyAscii
        , roomIntro = Nothing
                })
            ]
        , itemDefs = Map.fromList
            [ ("torch", ItemDef
                { itemId = "torch"
                , itemName = "torch"
                , itemDescription = plainText "A burning torch that provides light."
                , itemKeywords = ["torch", "burning torch"]
                , itemTags = Set.fromList ["lightsource"]
                , itemEquipSlot = Nothing
                , itemEquipEffects = []
                , itemHidden = False
                , itemDiscoverText = Nothing
                , itemPortable = True
                , itemTakeFailure = Nothing
                , itemVerbMap = Map.empty
                , itemAscii = emptyAscii
                })
            , ("key", ItemDef
                { itemId = "key"
                , itemName = "key"
                , itemDescription = plainText "A small brass key."
                , itemKeywords = ["key", "brass key"]
                , itemTags = Set.empty
                , itemEquipSlot = Nothing
                , itemEquipEffects = []
                , itemHidden = False
                , itemDiscoverText = Nothing
                , itemPortable = True
                , itemTakeFailure = Nothing
                , itemVerbMap = Map.singleton (VTake, "intact")
                    (Sequence
                        [ QuestOp StartQuest "find_treasure"
                        , SetValue (VRFlag "quest_started") (EVString "true") ])
                , itemAscii = emptyAscii
                })
            , ("gold", ItemDef
                { itemId = "gold"
                , itemName = "gold"
                , itemDescription = plainText "A pile of shiny gold coins."
                , itemKeywords = ["gold", "coins", "gold coins"]
                , itemTags = Set.empty
                , itemEquipSlot = Nothing
                , itemEquipEffects = []
                , itemHidden = False
                , itemDiscoverText = Nothing
                , itemPortable = True
                , itemTakeFailure = Nothing
                , itemVerbMap = Map.empty
                , itemAscii = emptyAscii
                })
            , ("jewel", ItemDef
                { itemId = "jewel"
                , itemName = "jewel"
                , itemDescription = plainText "A sparkling ruby that catches the light."
                , itemKeywords = ["jewel", "ruby", "sparkling ruby"]
                , itemTags = Set.empty
                , itemEquipSlot = Nothing
                , itemEquipEffects = []
                , itemHidden = False
                , itemDiscoverText = Nothing
                , itemPortable = True
                , itemTakeFailure = Nothing
                , itemVerbMap = Map.empty
                , itemAscii = emptyAscii
                })
            , ("potion_healing", ItemDef
                { itemId = "potion_healing"
                , itemName = "healing potion"
                , itemDescription = plainText "A small vial filled with a bubbling red liquid."
                , itemKeywords = ["potion", "red potion", "healing potion"]
                , itemTags = Set.empty
                , itemEquipSlot = Nothing
                , itemEquipEffects = []
                , itemHidden = False
                , itemDiscoverText = Nothing
                , itemPortable = True
                , itemTakeFailure = Nothing
                , itemVerbMap = Map.singleton (VUse, "intact")
                    (Sequence
                        [ ModifyValue VRPlayerHealth 50
                        , SetValue (VRItemProp "potion_healing" "uses") (EVInt (-1))
                        , SetValue (VRActorProp (ActorEntity "potion_healing") PState) (EVString "empty") ])
                , itemAscii = emptyAscii
                })
            -- Equipment examples
            , ("sword_rusty", ItemDef
                { itemId = "sword_rusty"
                , itemName = "rusty sword"
                , itemDescription = plainText "A pitted blade, but it will do."
                , itemKeywords = ["sword", "rusty sword", "blade"]
                , itemTags = Set.fromList ["weapon"]
                , itemEquipSlot = Just Weapon
                , itemEquipEffects = [AttackBonus 5]
                , itemHidden = False
                , itemDiscoverText = Nothing
                , itemPortable = True
                , itemTakeFailure = Nothing
                , itemVerbMap = Map.empty
                , itemAscii = emptyAscii
                })
            , ("leather_armor", ItemDef
                { itemId = "leather_armor"
                , itemName = "leather armor"
                , itemDescription = plainText "Supple boiled leather, well worn."
                , itemKeywords = ["armor", "leather armor"]
                , itemTags = Set.empty
                , itemEquipSlot = Just Body
                , itemEquipEffects = [DefenseBonus 3]
                , itemHidden = False
                , itemDiscoverText = Nothing
                , itemPortable = True
                , itemTakeFailure = Nothing
                , itemVerbMap = Map.empty
                , itemAscii = emptyAscii
                })
            , ("ring_vigor", ItemDef
                { itemId = "ring_vigor"
                , itemName = "ring of vigor"
                , itemDescription = plainText "A plain bronze band that feels warm."
                , itemKeywords = ["ring", "ring of vigor"]
                , itemTags = Set.empty
                , itemEquipSlot = Just Accessory
                , itemEquipEffects = [MaxHealthBonus 20]
                , itemHidden = False
                , itemDiscoverText = Nothing
                , itemPortable = True
                , itemTakeFailure = Nothing
                , itemVerbMap = Map.empty
                , itemAscii = emptyAscii
                })
            -- Hidden item, found via `search`
            , ("note_old", ItemDef
                { itemId = "note_old"
                , itemName = "old note"
                , itemDescription = plainText "A folded scrap of parchment, brittle with age."
                , itemKeywords = ["note", "old note"]
                , itemTags = Set.empty
                , itemEquipSlot = Nothing
                , itemEquipEffects = []
                , itemHidden = True
                , itemDiscoverText = Just "Wedged behind a loose brick you find an old note."
                , itemPortable = True
                , itemTakeFailure = Nothing
                , itemVerbMap = Map.empty
                , itemAscii = emptyAscii
                })
            -- Vehicle demo (Phase 3)
            , ("carriage", ItemDef
                { itemId = "carriage"
                , itemName = "carriage"
                , itemDescription = plainText "A sturdy horse-drawn carriage with polished wood panels."
                , itemKeywords = ["carriage", "wagon", "coach"]
                , itemTags = Set.fromList ["vehicle"]
                , itemEquipSlot = Nothing
                , itemEquipEffects = []
                , itemHidden = False
                , itemDiscoverText = Nothing
                , itemPortable = True
                , itemTakeFailure = Nothing
                , itemVerbMap = Map.empty
                , itemAscii = emptyAscii
                })
            , ("hay", ItemDef
                { itemId = "hay"
                , itemName = "bale of hay"
                , itemDescription = plainText "A fragrant bale of hay — prime horse fuel."
                , itemKeywords = ["hay", "bale", "bale of hay"]
                , itemTags = Set.empty
                , itemEquipSlot = Nothing
                , itemEquipEffects = []
                , itemHidden = False
                , itemDiscoverText = Nothing
                , itemPortable = True
                , itemTakeFailure = Nothing
                , itemVerbMap = Map.empty
                , itemAscii = emptyAscii
                })
            ]
        , npcDefs = Map.fromList
            [ ("oldman", NPCDef
                { npcId = "oldman"
                , npcName = "old man"
                , npcDescription = plainText "A withered old man in robes."
                , npcDialogueTrees = Map.singleton "alive" (DialogueTree "greeting" (Map.fromList
                    [ ("greeting", DialogueNode "greeting" "Greetings, traveler! What brings you into this dark place?"
                        [ DialogueChoice "Who are you?" (Just "who") Nothing (Sequence [])
                        , DialogueChoice "Tell me about the treasure." (Just "rumor") Nothing (Sequence [])
                        , DialogueChoice "Farewell." Nothing Nothing (SendMessage "Stay safe, friend.")
                        ])
                    , ("who", DialogueNode "who" "I am just an old hermit who watches over these ruins."
                        [ DialogueChoice "What do you know about the treasure?" (Just "rumor") Nothing (Sequence [])
                        , DialogueChoice "Goodbye." Nothing Nothing (SendMessage "May the light guide your steps.")
                        ])
                    , ("rumor", DialogueNode "rumor" "The treasure room lies beyond the eastern door, but it is locked with a brass key lost in the hallway."
                        [ DialogueChoice "Thank you for the advice!" Nothing Nothing (SetValue (VRFlag "met_oldman") (EVString "true"))
                        ])
                    ]))
                , npcKeywords = ["man", "old man"]
                , npcMaxHealth = Nothing
                , npcAttackBase = 0
                , npcDefenseBase = 0
                , npcVerbMap = Map.empty
                , npcAscii = emptyAscii
                })
            , ("goblin", NPCDef
                { npcId = "goblin"
                , npcName = "goblin"
                , npcDescription = plainText "A nasty little green goblin."
                , npcDialogueTrees = Map.empty
                , npcKeywords = ["goblin", "monster"]
                , npcMaxHealth = Just 30
                , npcAttackBase = 8
                , npcDefenseBase = 2
                , npcVerbMap = Map.empty
                , npcAscii = emptyAscii
                })
            ]
        , entityInteractions = Map.fromList
            [ (("key", "door"), ("unlocked", "You insert the brass key into the door. It clicks open!"))
            , (("key", "treasure_door"), ("unlocked", "You insert the brass key into the door. It clicks open!"))
            ]
        , itemInteractions = Map.empty
        , questDefs = Map.fromList
            [ ("find_treasure", Quest
                "find_treasure"
                "The Lost Treasure"
                "Find the treasure rumoured to rest beyond the locked door."
                Map.empty   -- no prereqs
                [ QuestStage "explore"  "Explore the dark hallway."       (Just "Try searching when you have light.")
                , QuestStage "find_key" "Find the brass key."              Nothing
                , QuestStage "open_up"  "Unlock the treasure room door."   Nothing
                ]
                (Just (SendMessage "The treasure is yours! Well, what's left of it after the goblin.")))
            , ("gated_quest", Quest
                "gated_quest"
                "A Favor for the Old Man"
                "The old man asked for help — once you've actually talked to him."
                (Map.singleton "met_oldman" "true")
                [ QuestStage "do_thing" "Do the thing." Nothing ]
                Nothing)
            ]
        , vehicleDefs = Map.fromList
            [ ("carriage", VehicleDef
                "carriage"
                "carriage"
                "A sturdy horse-drawn carriage. The reins hang from the driver's bench."
                PlayerControlled
                ["carriage_cabin"]
                "carriage_cabin"
                (Just "carriage_cabin")        -- cockpit = the cabin itself (single-room vehicle)
                (Map.fromList
                    [ ("meadow", VehicleStop "meadow" "the sunny meadow" Nothing)
                    , ("start",  VehicleStop "start"  "the stone chamber's entrance" Nothing)
                    ])
                []                          -- ^ vehicleRoute (authored stop order)
                ["carriage", "wagon", "coach"]  -- ^ vehicleKeywords
                (Just (FuelSpec "hay" 10))      -- ^ vehicleFuelProp
                Map.empty                       -- ^ vehicleConditionEffects (Phase 3)
            )
            ]
        , verbDefs = Map.empty
        , varDefs = Map.empty
        , triggerDefs = []
        , combatProfile = CombatClassic
        , worldName = "Sample Adventure"
        , abilities = Map.empty
        , worldEndArt = Map.empty
        , worldTitleArt = emptyAscii
        , worldClips = Map.empty
        }
    , save = SaveState
        { player = Player 100 100 10 5 (Map.singleton "lockpick" 2)
        , currentRoom = "start"
        , inventory = []
        , itemStates = Map.fromList
            [ ("torch", ItemState (InRoom "start") "burning" Map.empty False)
            , ("key", ItemState (InRoom "hallway") "intact" Map.empty False)
            , ("gold", ItemState (InRoom "treasure") "intact" Map.empty False)
            , ("jewel", ItemState (InRoom "treasure") "intact" Map.empty False)
            , ("potion_healing", ItemState (InRoom "start") "intact" (Map.singleton "uses" 3) False)
            , ("sword_rusty", ItemState (InRoom "start") "intact" Map.empty False)
            , ("leather_armor", ItemState (InRoom "start") "intact" Map.empty False)
            , ("ring_vigor", ItemState (InRoom "start") "intact" Map.empty False)
            , ("note_old", ItemState (InRoom "hallway") "intact" Map.empty False)
            , ("carriage", ItemState (InRoom "meadow") "intact" Map.empty False)
            , ("hay", ItemState (InRoom "meadow") "intact" Map.empty False)
            ]
        , npcStates = Map.fromList
            [ ("oldman", NPCState (InRoom "start") "alive" Nothing Map.empty Nothing)
            , ("goblin", NPCState (InRoom "hallway") "alive" (Just 30) Map.empty Nothing)
            ]
        , entityStates = Map.singleton "treasure_door" "locked"
        , flags = Map.empty
        , turnCount = 0
        , gameOver = False
        , gameOverReason = Nothing
        , visitedRooms = Set.empty
        , equipment = Map.empty
        , conditions = Map.empty
        , activeQuests = Map.empty
        , completedQuests = Set.empty
        , vehicleStates = Map.singleton "carriage"
            (VehicleState "meadow" (Just 10) Set.empty Map.empty)
        , currentVehicle = Nothing
        , activeDialogue = Nothing
        , rngState       = initialRngState
        , variables      = Map.empty
        , triggerStates = Map.empty
        }
    , pendingNarrative = Nothing
    , pendingAnimation = Nothing
    , pendingCutscene = Nothing
    , diagnostics = []
    }