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
                "start"
                "Starting Room"
                "You are in a small stone chamber with torches on the walls. There are exits to the north and east. The east door looks sturdy and has a keyhole."
                (Map.fromList [(North, Open "hallway"), (East, Locked "treasure" "treasure_door"), (South, Open "meadow")])
                Set.empty
                Map.empty
                Nothing
                Nothing Nothing Nothing Nothing Nothing)
            , ("hallway", Room
                "hallway"
                "Dark Hallway"
                "A long, dark hallway stretches before you. The air is damp and cold. There's an exit to the south."
                (Map.fromList [(South, Open "start")])
                (Set.fromList ["dark"])
                (Map.singleton "torch_lit" "The torches along the wall sputter to life, pushing the darkness back.")
                (Just "torch_lit")
                Nothing Nothing Nothing
                (Just (Sequence
                    [ SetValue (VRFlag "torch_lit") (EVString "true")
                    , SendMessage "Faded runes cover the eastern wall." ]))
                Nothing)
            , ("treasure", Room
                "treasure"
                "Treasure Room"
                "You've entered a magnificent treasure room! Gold coins and jewels are scattered everywhere. There's an exit to the west."
                (Map.fromList [(West, Open "start")])
                Set.empty
                Map.empty
                Nothing
                Nothing Nothing Nothing Nothing Nothing)
            -- Vehicle demo (Phase 3): a horse-drawn carriage
            , ("meadow", Room
                "meadow"
                "Sunny Meadow"
                "A wide meadow stretches to the horizon. Wildflowers sway in the breeze. Your carriage is parked here."
                (Map.fromList [(North, Open "start")])
                Set.empty
                Map.empty
                Nothing
                (Just (Sequence
                    (map SendMessage ["A gentle breeze rustles the grass.",
                     "Somewhere in the distance, birds sing.",
                     "The carriage horse stamps its foot impatiently."]
                     ++ [SendMessage "You feel at peace here."])))
                Nothing Nothing Nothing Nothing)
            , ("carriage_cabin", Room
                "carriage_cabin"
                "Carriage Cabin"
                "You sit inside a comfortably worn carriage. A small window lets you watch the road. The reins are within reach."
                Map.empty
                (Set.fromList ["vehicle"])
                Map.empty
                Nothing
                Nothing Nothing Nothing Nothing Nothing)
            ]
        , itemDefs = Map.fromList
            [ ("torch", ItemDef "torch" "torch" "A burning torch that provides light."
                ["torch", "burning torch"] (Set.fromList ["lightsource"])
                Nothing [] False Nothing True Nothing Map.empty)
            , ("key", ItemDef "key" "key" "A small brass key."
                ["key", "brass key"] Set.empty Nothing [] False Nothing True Nothing
                (Map.singleton (VTake, "intact")
                    (Sequence
                        [ QuestOp StartQuest "find_treasure"
                        , SetValue (VRFlag "quest_started") (EVString "true") ])))
            , ("gold", ItemDef "gold" "gold" "A pile of shiny gold coins."
                ["gold", "coins", "gold coins"] Set.empty Nothing [] False Nothing True Nothing Map.empty)
            , ("jewel", ItemDef "jewel" "jewel" "A sparkling ruby that catches the light."
                ["jewel", "ruby", "sparkling ruby"] Set.empty Nothing [] False Nothing True Nothing Map.empty)
            , ("potion_healing", ItemDef "potion_healing" "healing potion" "A small vial filled with a bubbling red liquid."
                ["potion", "red potion", "healing potion"] Set.empty Nothing [] False Nothing True Nothing
                (Map.singleton (VUse, "intact")
                    (Sequence
                        [ ModifyValue VRPlayerHealth 50
                        , SetValue (VRItemProp "potion_healing" "uses") (EVInt (-1))
                        , SetValue (VRProperty "potion_healing" "state") (EVString "empty") ])))
            -- Equipment examples
            , ("sword_rusty", ItemDef "sword_rusty" "rusty sword" "A pitted blade, but it will do."
                ["sword", "rusty sword", "blade"] (Set.fromList ["weapon"])
                (Just Weapon) [AttackBonus 5] False Nothing True Nothing Map.empty)
            , ("leather_armor", ItemDef "leather_armor" "leather armor" "Supple boiled leather, well worn."
                ["armor", "leather armor"] Set.empty
                (Just Body) [DefenseBonus 3] False Nothing True Nothing Map.empty)
            , ("ring_vigor", ItemDef "ring_vigor" "ring of vigor" "A plain bronze band that feels warm."
                ["ring", "ring of vigor"] Set.empty
                (Just Accessory) [MaxHealthBonus 20] False Nothing True Nothing Map.empty)
            -- Hidden item, found via `search`
            , ("note_old", ItemDef "note_old" "old note" "A folded scrap of parchment, brittle with age."
                ["note", "old note"] Set.empty Nothing []
                True (Just "Wedged behind a loose brick you find an old note.") True Nothing Map.empty)
            -- Vehicle demo (Phase 3)
            , ("carriage", ItemDef "carriage" "carriage" "A sturdy horse-drawn carriage with polished wood panels."
                ["carriage", "wagon", "coach"] (Set.fromList ["vehicle"]) Nothing [] False Nothing True Nothing Map.empty)
            , ("hay", ItemDef "hay" "bale of hay" "A fragrant bale of hay — prime horse fuel."
                ["hay", "bale", "bale of hay"] Set.empty Nothing [] False Nothing True Nothing Map.empty)
            ]
        , npcDefs = Map.fromList
            [ ("oldman", NPCDef "oldman" "old man" "A withered old man in robes."
                (Map.singleton "alive" "It's dangerous to go alone! Take... well, I don't have anything actually.")
                (Map.singleton "alive" (DialogueTree "greeting" (Map.fromList
                    [ ("greeting", DialogueNode "greeting" "Greetings, traveler! What brings you into this dark place?"
                        [ DialogueChoice "Who are you?" (Just "who") (Sequence [])
                        , DialogueChoice "Tell me about the treasure." (Just "rumor") (Sequence [])
                        , DialogueChoice "Farewell." Nothing (SendMessage "Stay safe, friend.")
                        ])
                    , ("who", DialogueNode "who" "I am just an old hermit who watches over these ruins."
                        [ DialogueChoice "What do you know about the treasure?" (Just "rumor") (Sequence [])
                        , DialogueChoice "Goodbye." Nothing (SendMessage "May the light guide your steps.")
                        ])
                    , ("rumor", DialogueNode "rumor" "The treasure room lies beyond the eastern door, but it is locked with a brass key lost in the hallway."
                        [ DialogueChoice "Thank you for the advice!" Nothing (SetValue (VRFlag "met_oldman") (EVString "true"))
                        ])
                    ])))
                ["man", "old man"] Nothing 0 0 Map.empty)
            , ("goblin", NPCDef "goblin" "goblin" "A nasty little green goblin."
                (Map.singleton "alive" "Grrr!! I will eat you!")
                Map.empty
                ["goblin", "monster"] (Just 30) 8 2 Map.empty)
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
                ["carriage", "wagon", "coach"]
                (Just ("hay", 10))
                Map.empty)
            ]
        , verbDefs = Map.empty
        , varDefs = Map.empty
        , triggerDefs = []
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
        , containers     = Map.empty
        , triggerStates = Map.empty
        }
    , pendingNarrative = Nothing
    }
