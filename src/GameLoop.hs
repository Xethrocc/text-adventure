-- | Main game loop and user interaction for the text adventure engine
module GameLoop where

import Types
import Game
import Parser hiding (reachableExitEntities)
import Control.Exception (try, SomeException)
import Data.Char (toLower)
import Data.List (isPrefixOf, nub)
import qualified Data.Map as Map
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as BL
import System.Console.Haskeline

commandWords :: [String]
commandWords =
    [ "go", "move", "walk", "look", "examine", "inspect", "read", "take", "pick", "drop", "put"
    , "inventory", "inv", "i", "use", "talk", "speak", "attack", "hit", "kill"
    , "save", "load", "help", "quit", "exit", "q"
    ]

directionWords :: [String]
directionWords = ["north", "south", "east", "west", "up", "down"]

completionItems :: [String] -> String -> [Completion]
completionItems options prefix =
    let loweredPrefix = map toLower prefix
    in map simpleCompletion (filter (\opt -> loweredPrefix `isPrefixOf` map toLower opt) (nub options))

itemCompletionTerms :: [ItemDef] -> [String]
itemCompletionTerms items = nub (concatMap (\i -> itemName i : itemKeywords i) items)

npcCompletionTerms :: [NPCDef] -> [String]
npcCompletionTerms npcs = nub (concatMap (\n -> npcName n : npcKeywords n) npcs)

roomTargets :: GameState -> [String]
roomTargets state =
    let currentRoomId = currentRoom (save state)
        roomItems = getItemsInLocation currentRoomId state
        roomNpcs = getNPCsInRoom currentRoomId state
    in nub (itemCompletionTerms roomItems ++ npcCompletionTerms roomNpcs)

inventoryTargets :: GameState -> [String]
inventoryTargets state = itemCompletionTerms (getItemsInLocation "inventory" state)

reachableExitEntities :: GameState -> [String]
reachableExitEntities state = case getCurrentRoom state of
    Just room -> [entity | Locked _ entity <- Map.elems (roomConnections room)]
    Nothing   -> []

entityTargets :: GameState -> [String]
entityTargets state =
    let exits = reachableExitEntities state
        doorAliases = if null exits then [] else ["door", "locked door"]
    in nub (roomTargets state ++ exits ++ doorAliases)

contextualSuggestions :: GameState -> [String] -> [String]
contextualSuggestions state prevWords = case prevWords of
    [] -> commandWords ++ directionWords
    ("go" : _) -> directionWords
    ("move" : _) -> directionWords
    ("walk" : _) -> directionWords
    ("look" : "at" : _) -> roomTargets state
    ("talk" : "to" : _) -> npcCompletionTerms (getNPCsInRoom (currentRoom (save state)) state)
    ("speak" : "with" : _) -> npcCompletionTerms (getNPCsInRoom (currentRoom (save state)) state)
    ("pick" : "up" : _) -> roomTargets state
    ("put" : "down" : _) -> inventoryTargets state
    ("use" : _)
        | "on" `elem` prevWords || "with" `elem` prevWords -> entityTargets state
        | otherwise -> inventoryTargets state
    (verb : _)
        | verb `elem` ["take", "drop", "attack", "hit", "kill", "examine", "inspect", "read"] ->
            roomTargets state ++ inventoryTargets state
        | otherwise -> commandWords ++ directionWords ++ roomTargets state ++ entityTargets state ++ inventoryTargets state

commandCompletion :: GameState -> CompletionFunc IO
commandCompletion state (left, _) = do
    let loweredLeft = map toLower left
        tokens = words loweredLeft
        (prevWords, currentWord)
            | not (null loweredLeft) && last loweredLeft /= ' ' && not (null tokens) =
                (init tokens, last tokens)
            | otherwise = (tokens, "")
        suggestions = contextualSuggestions state prevWords
    pure (currentWord, completionItems suggestions currentWord)

haskelineSettings :: GameState -> Settings IO
haskelineSettings state =
    (defaultSettings :: Settings IO)
        { autoAddHistory = True
        , complete = commandCompletion state
        }

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
        inputResult <- runInputT (haskelineSettings state) (getInputLine "> ")
        case inputResult of
            Nothing -> do
                let (newState, message) = executeCommand Quit state
                putStrLn message
                gameLoop newState
            Just input -> do
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
                                    let loadedState = state { save = syncInventory loadedSave }
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
