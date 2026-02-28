-- | Core game state and manipulation for the text adventure engine
module Game where

import Types
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.List (intercalate)

-- | Default empty game state
emptyGameState :: GameState
emptyGameState = GameState
    { world = GameWorld
        { rooms              = Map.empty
        , itemDefs           = Map.empty
        , npcDefs            = Map.empty
        , entityInteractions = Map.empty
        }
    , save = SaveState
        { player             = Player 100 100 10 5
        , currentRoom        = "start"
        , inventory          = []
        , itemStates         = Map.empty
        , npcStates          = Map.empty
        , entityStates       = Map.empty
        , gameOver           = False
        }
    }

-- | Helper to get current room from game state
getCurrentRoom :: GameState -> Maybe Room
getCurrentRoom state = Map.lookup (currentRoom (save state)) (rooms (world state))

-- | Get all items currently in a specific location (e.g., room ID or "inventory")
getItemsInLocation :: RoomID -> GameState -> [ItemDef]
getItemsInLocation loc state = 
    let itemIds = Map.keys $ Map.filter (\s -> itemLocation s == loc) (itemStates (save state))
    in [def | iId <- itemIds, Just def <- [Map.lookup iId (itemDefs (world state))]]

-- | Check if player has an item in inventory
hasItem :: ItemID -> GameState -> Bool
hasItem iId state = iId `elem` inventory (save state)

-- | Move player to a different room
moveToRoom :: RoomID -> GameState -> GameState
moveToRoom roomName state = state { save = (save state) { currentRoom = roomName } }

-- | Add item to player's inventory
pickupItem :: ItemID -> GameState -> GameState
pickupItem iId state = state 
    { save = (save state) 
        { inventory = iId : inventory (save state) 
        , itemStates = Map.adjust (\s -> s { itemLocation = "inventory" }) iId (itemStates (save state))
        }
    }

-- | Remove item from player's inventory to current room
dropItem :: ItemID -> GameState -> GameState
dropItem iId state = state 
    { save = (save state)
        { inventory = filter (/= iId) (inventory (save state)) 
        , itemStates = Map.adjust (\s -> s { itemLocation = currentRoom (save state) }) iId (itemStates (save state))
        }
    }

-- | Check if a direction is valid from current room
canMove :: Direction -> GameState -> Bool
canMove dir state = case getCurrentRoom state of
    Just room -> dir `Map.member` roomConnections room
    Nothing   -> False

-- | Get exit in a given direction
getExitInDirection :: Direction -> GameState -> Maybe Exit
getExitInDirection dir state = case getCurrentRoom state of
    Just room -> Map.lookup dir (roomConnections room)
    Nothing   -> Nothing

-- | Get entity state
getEntityState :: String -> GameState -> Maybe String
getEntityState entity state = Map.lookup entity (entityStates (save state))

-- | Set entity state
setEntityState :: String -> String -> GameState -> GameState
setEntityState entity val state = state { save = (save state) { entityStates = Map.insert entity val (entityStates (save state)) } }

-- | Update player health
updatePlayerHealth :: (Int -> Int) -> GameState -> GameState
updatePlayerHealth f state = 
    let p = player (save state)
        newHealth = max 0 (min (playerMaxHealth p) (f (playerHealth p)))
    in state { save = (save state) { player = p { playerHealth = newHealth } } }

-- | Check if player is dead
isPlayerDead :: GameState -> Bool
isPlayerDead state = playerHealth (player (save state)) <= 0

-- | Get all NPCs in a specific room
getNPCsInRoom :: RoomID -> GameState -> [NPCDef]
getNPCsInRoom rId state = 
    let npcIds = Map.keys $ Map.filter (\s -> npcLocation s == rId) (npcStates (save state))
    in [def | nId <- npcIds, Just def <- [Map.lookup nId (npcDefs (world state))]]

-- | Update NPC state (health, status, etc)
updateNPCState :: String -> NPCState -> GameState -> GameState
updateNPCState targetNpcId newNpcState state = state
    { save = (save state) { npcStates = Map.insert targetNpcId newNpcState (npcStates (save state)) } }

-- | Move NPC to void (dead)
killNPC :: String -> GameState -> GameState
killNPC targetNpcId state = state
    { save = (save state) { npcStates = Map.adjust (\s -> s { npcLocation = "void", npcStatus = "dead" }) targetNpcId (npcStates (save state)) } }

-- | Modify an item's property
modifyItemProp :: String -> String -> Int -> GameState -> GameState
modifyItemProp iId prop delta state = state
    { save = (save state) { itemStates = Map.adjust (\s -> 
        let currentVal = Map.findWithDefault 0 prop (itemProps s)
        in s { itemProps = Map.insert prop (currentVal + delta) (itemProps s) }
        ) iId (itemStates (save state)) } }

-- | Modify an NPC's property
modifyNPCProp :: String -> String -> Int -> GameState -> GameState
modifyNPCProp nId prop delta state = state
    { save = (save state) { npcStates = Map.adjust (\s -> 
        let currentVal = Map.findWithDefault 0 prop (npcProps s)
        in s { npcProps = Map.insert prop (currentVal + delta) (npcProps s) }
        ) nId (npcStates (save state)) } }