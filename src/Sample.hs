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
                (Map.fromList [(North, Open "hallway"), (East, Locked "treasure" "treasure_door")])
                Set.empty
                Map.empty
                Nothing
                Nothing Nothing Nothing Nothing)
            , ("hallway", Room
                "hallway"
                "Dark Hallway"
                "A long, dark hallway stretches before you. The air is damp and cold. There's an exit to the south."
                (Map.fromList [(South, Open "start")])
                (Set.fromList ["dark"])
                (Map.singleton "torch_lit" "The torches along the wall sputter to life, pushing the darkness back.")
                (Just "torch_lit")
                Nothing Nothing Nothing
                (Just (MultipleOutcomes
                    [ SetFlag "torch_lit" "true" "You find a wall sconce and light it."
                    , MessageOnly "Faded runes cover the eastern wall." ])))
            , ("treasure", Room
                "treasure"
                "Treasure Room"
                "You've entered a magnificent treasure room! Gold coins and jewels are scattered everywhere. There's an exit to the west."
                (Map.fromList [(West, Open "start")])
                Set.empty
                Map.empty
                Nothing
                Nothing Nothing Nothing Nothing)
            ]
        , itemDefs = Map.fromList
            [ ("torch", ItemDef "torch" "torch" "A burning torch that provides light."
                ["torch", "burning torch"] (Set.fromList ["lightsource"])
                Nothing [] False Nothing Map.empty)
            , ("key", ItemDef "key" "key" "A small brass key."
                ["key", "brass key"] Set.empty Nothing [] False Nothing
                (Map.singleton (VTake, "intact")
                    (MultipleOutcomes
                        [ StartQuest "find_treasure" "Journal updated: The Lost Treasure."
                        , SetFlag "quest_started" "true" "" ])))
            , ("gold", ItemDef "gold" "gold" "A pile of shiny gold coins."
                ["gold", "coins", "gold coins"] Set.empty Nothing [] False Nothing Map.empty)
            , ("jewel", ItemDef "jewel" "jewel" "A sparkling ruby that catches the light."
                ["jewel", "ruby", "sparkling ruby"] Set.empty Nothing [] False Nothing Map.empty)
            , ("potion_healing", ItemDef "potion_healing" "healing potion" "A small vial filled with a bubbling red liquid."
                ["potion", "red potion", "healing potion"] Set.empty Nothing [] False Nothing
                (Map.singleton (VUse, "intact")
                    (MultipleOutcomes
                        [ HealPlayer 50 "You drink the potion and feel your wounds closing!"
                        , ModifyItemProp "potion_healing" "uses" (-1) "The potion has less liquid now."
                        , ChangeItemState "empty" "The vial is now empty." ])))
            -- Equipment examples
            , ("sword_rusty", ItemDef "sword_rusty" "rusty sword" "A pitted blade, but it will do."
                ["sword", "rusty sword", "blade"] (Set.fromList ["weapon"])
                (Just Weapon) [AttackBonus 5] False Nothing Map.empty)
            , ("leather_armor", ItemDef "leather_armor" "leather armor" "Supple boiled leather, well worn."
                ["armor", "leather armor"] Set.empty
                (Just Body) [DefenseBonus 3] False Nothing Map.empty)
            , ("ring_vigor", ItemDef "ring_vigor" "ring of vigor" "A plain bronze band that feels warm."
                ["ring", "ring of vigor"] Set.empty
                (Just Accessory) [MaxHealthBonus 20] False Nothing Map.empty)
            -- Hidden item, found via `search`
            , ("note_old", ItemDef "note_old" "old note" "A folded scrap of parchment, brittle with age."
                ["note", "old note"] Set.empty Nothing []
                True (Just "Wedged behind a loose brick you find an old note.") Map.empty)
            ]
        , npcDefs = Map.fromList
            [ ("oldman", NPCDef "oldman" "old man" "A withered old man in robes."
                (Map.singleton "alive" "It's dangerous to go alone! Take... well, I don't have anything actually.")
                Map.empty
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
                (Just (MessageOnly "The treasure is yours! Well, what's left of it after the goblin.")))
            , ("gated_quest", Quest
                "gated_quest"
                "A Favor for the Old Man"
                "The old man asked for help — once you've actually talked to him."
                (Map.singleton "met_oldman" "true")
                [ QuestStage "do_thing" "Do the thing." Nothing ]
                Nothing)
            ]
        }
    , save = SaveState
        { player = Player 100 100 10 5 (Map.singleton "lockpick" 2)
        , currentRoom = "start"
        , inventory = []
        , itemStates = Map.fromList
            [ ("torch", ItemState "start" "burning" Map.empty False)
            , ("key", ItemState "hallway" "intact" Map.empty False)
            , ("gold", ItemState "treasure" "intact" Map.empty False)
            , ("jewel", ItemState "treasure" "intact" Map.empty False)
            , ("potion_healing", ItemState "start" "intact" (Map.singleton "uses" 3) False)
            , ("sword_rusty", ItemState "start" "intact" Map.empty False)
            , ("leather_armor", ItemState "start" "intact" Map.empty False)
            , ("ring_vigor", ItemState "start" "intact" Map.empty False)
            , ("note_old", ItemState "hallway" "intact" Map.empty False)
            ]
        , npcStates = Map.fromList
            [ ("oldman", NPCState "start" "alive" Nothing Map.empty Nothing)
            , ("goblin", NPCState "hallway" "alive" (Just 30) Map.empty Nothing)
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
        }
    }
