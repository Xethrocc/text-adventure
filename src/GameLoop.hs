-- | Main game loop and user interaction for the text adventure engine
module GameLoop where

import Types
import Parser
import System.IO
import Control.Exception (try, SomeException)
import qualified Data.Map as Map
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as BL

-- | Main game loop function

runGame :: GameState -> IO ()
runGame state = do
    let (newState, message) = executeCommand Look state
    putStrLn message
    gameLoop newState
    
-- | Interactive game loop

gameLoop :: GameState -> IO ()
gameLoop state
    | gameOver (save state) = return ()
    | otherwise      = do
        putStr "> "
        hFlush stdout
        input <- getLine
        let command = parseCommand input
        case command of
            Save name -> do
                let filepath = name ++ ".json"
                BL.writeFile filepath (Aeson.encodePretty (save state))
                putStrLn $ "Game saved to " ++ filepath ++ "."
                gameLoop state
            Load name -> do
                let filepath = name ++ ".json"
                result <- try (BL.readFile filepath) :: IO (Either SomeException BL.ByteString)
                case result of
                    Left _ -> do
                        putStrLn $ "Error: Could not read file '" ++ filepath ++ "'."
                        gameLoop state
                    Right contents -> case Aeson.decode contents of
                        Just loadedSave -> do
                            putStrLn $ "Game loaded from " ++ filepath ++ "."
                            let loadedState = state { save = loadedSave }
                            let (s', msg) = executeCommand Look loadedState
                            putStrLn msg
                            gameLoop s'
                        Nothing -> do
                            putStrLn "Error: Save file is corrupted or incompatible."
                            gameLoop state
            _ -> do
                let (newState, message) = executeCommand command state
                putStrLn message
                gameLoop newState

-- | Initialize a sample game with rooms and items
initSampleGame :: GameState
initSampleGame = GameState
    { world = GameWorld
        { rooms = Map.fromList
            [ ("start", Room "start" "Starting Room" "You are in a small stone chamber with torches on the walls. There are exits to the north and east. The east door looks sturdy and has a keyhole."
                (Map.fromList [(North, Open "hallway"), (East, Locked "treasure" "treasure_door")]) True)
            , ("hallway", Room "hallway" "Dark Hallway" "A long, dark hallway stretches before you. The air is damp and cold. There's an exit to the south."
                (Map.fromList [(South, Open "start")]) False)
            , ("treasure", Room "treasure" "Treasure Room" "You've entered a magnificent treasure room! Gold coins and jewels are scattered everywhere. There's an exit to the west."
                (Map.fromList [(West, Open "start")]) False)
            ]
        , itemDefs = Map.fromList
            [ ("torch", ItemDef "torch" "torch" "A burning torch that provides light." ["torch", "burning torch"] Map.empty)
            , ("key", ItemDef "key" "key" "A small brass key." ["key", "brass key"] Map.empty)
            , ("gold", ItemDef "gold" "gold" "A pile of shiny gold coins." ["gold", "coins", "gold coins"] Map.empty)
            , ("jewel", ItemDef "jewel" "jewel" "A sparkling ruby that catches the light." ["jewel", "ruby", "sparkling ruby"] Map.empty)
            , ("potion_healing", ItemDef "potion_healing" "healing potion" "A small vial filled with a bubbling red liquid." ["potion", "red potion", "healing potion"] (Map.singleton (VUse, "intact") (MultipleOutcomes [HealPlayer 50 "You drink the potion and feel your wounds closing!", ModifyItemProp "potion_healing" "uses" (-1) "The potion has less liquid now.", ChangeItemState "empty" "The vial is now empty."])))
            ]
        , npcDefs = Map.fromList
            [ ("oldman", NPCDef "oldman" "old man" "A withered old man in robes." (Map.singleton "alive" "It's dangerous to go alone! Take... well, I don't have anything actually.") ["man", "old man"] Nothing 0 0 Map.empty)
            , ("goblin", NPCDef "goblin" "goblin" "A nasty little green goblin." (Map.singleton "alive" "Grrr!! I will eat you!") ["goblin", "monster"] (Just 30) 8 2 Map.empty)
            ]
        , entityInteractions = Map.fromList 
            [ (("key", "door"), ("unlocked", "You insert the brass key into the door. It clicks open!"))
            , (("key", "treasure_door"), ("unlocked", "You insert the brass key into the door. It clicks open!"))
            ]
        }
    , save = SaveState
        { player = Player 100 100 10 5
        , currentRoom = "start"
        , inventory = []
        , itemStates = Map.fromList 
            [ ("torch", ItemState "start" "burning" Map.empty)
            , ("key", ItemState "hallway" "intact" Map.empty)
            , ("gold", ItemState "treasure" "intact" Map.empty)
            , ("jewel", ItemState "treasure" "intact" Map.empty)
            , ("potion_healing", ItemState "start" "intact" (Map.singleton "uses" 3))
            ]
        , npcStates = Map.fromList
            [ ("oldman", NPCState "start" "alive" Nothing Map.empty)
            , ("goblin", NPCState "hallway" "alive" (Just 30) Map.empty)
            ]
        , entityStates = Map.singleton "treasure_door" "locked"
        , gameOver = False
        }
    }

-- | Main entry point for the game

main :: IO ()
main = do
    putStrLn "=== Text Adventure Game ==="
    putStrLn "Type 'help' for available commands."
    putStrLn "----------------------------"
    runGame initSampleGame