-- | Main game loop and user interaction for the text adventure engine
module GameLoop where

import Types
import Game
import Parser
import System.IO
import Control.Monad (unless)
import qualified Data.Map as Map

-- | Main game loop function

runGame :: GameState -> IO ()
runGame state = do
    let (newState, message) = executeCommand Look state
    putStrLn message
    gameLoop newState
    
-- | Interactive game loop

gameLoop :: GameState -> IO ()
gameLoop state
    | gameOver state = return ()
    | otherwise      = do
        putStr "> "
        hFlush stdout
        input <- getLine
        let command = parseCommand input
        let (newState, message) = executeCommand command state
        putStrLn message
        gameLoop newState

-- | Initialize a sample game with rooms and items

initSampleGame :: GameState
initSampleGame = GameState
    { rooms = Map.fromList
        [ ("start", Room
            { roomName = "Starting Room"
            , roomDescription = "You are in a small stone chamber with torches on the walls. There are exits to the north and east. The east door looks sturdy and has a keyhole."
            , roomItems = [Item "torch" "A burning torch that provides light." ["torch", "burning torch"] ["take", "examine", "drop"] True]
            , roomNPCs = [NPC "old man" "A withered old man in robes." "It's dangerous to go alone! Take... well, I don't have anything actually." ["man", "old man"] Nothing 0 0]
            , roomConnections = Map.fromList [(North, Open "hallway"), (East, Locked "treasure" "treasure_door")]
            , roomVisited = True
            })
        , ("hallway", Room
            { roomName = "Dark Hallway"
            , roomDescription = "A long, dark hallway stretches before you. The air is damp and cold. There's an exit to the south."
            , roomItems = [Item "key" "A small brass key." ["key", "brass key"] ["take", "examine", "use", "drop"] True]
            , roomNPCs = [NPC "goblin" "A nasty little green goblin." "Grrr!! I will eat you!" ["goblin", "monster"] (Just 30) 8 2]
            , roomConnections = Map.fromList [(South, Open "start")]
            , roomVisited = False
            })
        , ("treasure", Room
            { roomName = "Treasure Room"
            , roomDescription = "You've entered a magnificent treasure room! Gold coins and jewels are scattered everywhere. There's an exit to the west."
            , roomItems = [Item "gold" "A pile of shiny gold coins." ["gold", "coins", "gold coins"] ["take", "examine", "drop"] True,
                          Item "jewel" "A sparkling ruby that catches the light." ["jewel", "ruby", "sparkling ruby"] ["take", "examine", "drop"] True]
            , roomNPCs = []
            , roomConnections = Map.fromList [(West, Open "start")]
            , roomVisited = False
            })
        ]
    , player = Player 100 100 10 5
    , currentRoom = "start"
    , inventory = []
    , entityStates = Map.singleton "treasure_door" "locked"
    , entityInteractions = Map.fromList 
        [ (("key", "door"), ("unlocked", "You insert the brass key into the door. It clicks open!"))
        , (("key", "treasure_door"), ("unlocked", "You insert the brass key into the door. It clicks open!"))
        ]
    , gameOver = False
    }

-- | Main entry point for the game

main :: IO ()
main = do
    putStrLn "=== Text Adventure Game ==="
    putStrLn "Type 'help' for available commands."
    putStrLn "----------------------------"
    runGame initSampleGame