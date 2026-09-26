-- | Vehicle subsystem for the text adventure engine
module Vehicles
    ( lookupVehicle
    , getVehicleState
    , setVehicleState
    , vehicleForRoom
    , isInsideVehicle
    , inVehicleRoom
    , vehicleStopList
    , nextVehicleStop
    , enterVehicle
    , exitVehicle
    , moveVehicleToStop
    , payStopCost
    , driveVehicle
    , advanceVehicleRoute
    , refuelVehicle
    , clearVehicleCondition
    , vehicleConditionTickWith
    , vehicleConditionTick
    , vehicleLookAddon
    ) where

import Types
import Game (followParty, hasItem, consumeItem, applyOutcome)
import Data.List (intercalate, find, elemIndex, foldl')
import Data.Char (toLower)
import Data.Maybe (listToMaybe, fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Look up a vehicle definition
lookupVehicle :: VehicleID -> GameState -> Maybe VehicleDef
lookupVehicle vId state = Map.lookup vId (vehicleDefs (world state))

-- | Look up a vehicle's dynamic state (defaults if unknown)
getVehicleState :: VehicleID -> GameState -> VehicleState
getVehicleState vId state =
    Map.findWithDefault (VehicleState "" Nothing Set.empty Map.empty) vId (vehicleStates (save state))

-- | Set a vehicle's dynamic state
setVehicleState :: VehicleID -> VehicleState -> GameState -> GameState
setVehicleState vId vs state = state
    { save = (save state) { vehicleStates = Map.insert vId vs (vehicleStates (save state)) } }

-- | Which vehicle (if any) owns this room id as an interior room?
vehicleForRoom :: RoomID -> GameState -> Maybe VehicleID
vehicleForRoom rId state =
    fmap fst (find (\(_, v) -> rId `elem` vehicleRooms v) (Map.toList (vehicleDefs (world state))))

-- | Is the player currently inside a vehicle?
isInsideVehicle :: GameState -> Bool
isInsideVehicle state = currentVehicle (save state) /= Nothing

-- | Is the current room one of the vehicle's interior rooms?
inVehicleRoom :: GameState -> Bool
inVehicleRoom state = case currentVehicle (save state) of
    Just vId -> case lookupVehicle vId state of
        Just v -> currentRoom (save state) `elem` vehicleRooms v
        Nothing -> False
    Nothing -> False

-- | All stops of a vehicle in route order (Map order = key order)
vehicleStopList :: VehicleDef -> [(RoomID, VehicleStop)]
vehicleStopList v
    | null (vehicleRoute v) = Map.toList (vehicleStops v)
    | otherwise =
        [ (rId, stop)
        | rId <- vehicleRoute v
        , Just stop <- [Map.lookup rId (vehicleStops v)] ]

-- | The next stop after the current one (wrapping around the route)
nextVehicleStop :: VehicleDef -> RoomID -> Maybe (RoomID, VehicleStop)
nextVehicleStop v cur =
    let stops = vehicleStopList v
        curIdx = elemIndex cur (map fst stops)
    in case curIdx of
        Just i  -> let n = (i + 1) `mod` length stops in Just (stops !! n)
        Nothing -> listToMaybe stops

-- | Enter a vehicle. Fails with a message if that isn't possible here.
enterVehicle :: VehicleID -> GameState -> Either String (GameState, String)
enterVehicle vId state = case lookupVehicle vId state of
    Nothing -> Left ("There is no '" ++ vId ++ "' here to enter.")
    Just v ->
        let vState = getVehicleState vId state
            stopRoom = vsCurrentStop vState
        in if currentRoom (save state) /= stopRoom
           then Left ("The " ++ vehicleName v ++ " is not here.")
           else Right
                ( followParty (vehicleEntryRoom v)
                    ( state { save = (save state)
                        { currentVehicle = Just vId
                        , currentRoom = vehicleEntryRoom v
                        , visitedRooms = Set.insert (vehicleEntryRoom v) (visitedRooms (save state)) } } )
                , "You board the " ++ vehicleName v ++ "." )

-- | Exit the current vehicle back to its current stop's outside room
exitVehicle :: GameState -> Either String (GameState, String)
exitVehicle state = case currentVehicle (save state) of
    Nothing -> Left "You are not in a vehicle."
    Just vId -> case lookupVehicle vId state of
        Nothing -> Left "You are not in a vehicle."
        Just v ->
            let vState = getVehicleState vId state
                outside = vsCurrentStop vState
            in Right
                ( followParty outside
                    ( state { save = (save state)
                        { currentVehicle = Nothing
                        , currentRoom = outside
                        , visitedRooms = Set.insert outside (visitedRooms (save state)) } } )
                , "You disembark from the " ++ vehicleName v ++ "." )

-- | Move a vehicle to a stop's outside room (updates its position).
--   Returns updated state; the caller decides whether the player travels with it.
moveVehicleToStop :: VehicleID -> RoomID -> GameState -> GameState
moveVehicleToStop vId stopRoom state
    | currentVehicle (save state) == Just vId =
        -- Player is aboard: travel with the vehicle
        followParty stopRoom $
            setVehicleState vId ((getVehicleState vId state) { vsCurrentStop = stopRoom })
                state { save = (save state) { currentRoom = stopRoom } }
    | otherwise =
        setVehicleState vId ((getVehicleState vId state) { vsCurrentStop = stopRoom }) state

-- | Consume the fare for a PaidVehicle stop, if one is required.
--   Returns Left with the error message when the player cannot pay.
payStopCost :: VehicleStop -> GameState -> Either String GameState
payStopCost stop state = case stopCost stop of
    Nothing -> Right state
    Just (costItem, errMsg) ->
        if hasItem costItem state
        then Right (consumeItem costItem state)
        else Left errMsg

-- | PlayerControlled: drive to a station by name (matched against stop labels
--   and outside-room ids). Must be done from the cockpit.
driveVehicle :: VehicleID -> String -> GameState -> Either String (GameState, String)
driveVehicle vId targetStr state = case lookupVehicle vId state of
    Nothing -> Left ("There is no '" ++ vId ++ "'.")
    Just v
        | vehicleType v /= PlayerControlled ->
            Left ("You can't steer the " ++ vehicleName v ++ "; it follows its own route.")
        | otherwise ->
            let target = map toLower targetStr
                matches = [ (rId, stop)
                          | (rId, stop) <- vehicleStopList v
                          , target `elem` [map toLower (stopLabel stop), map toLower rId]
                          , rId /= vsCurrentStop (getVehicleState vId state) ]
            in case matches of
                [] -> Left ("You can't drive there from here. Stations: "
                            ++ intercalate ", " (map (stopLabel . snd) (vehicleStopList v)))
                ((destId, stop):_) ->
                    if currentVehicle (save state) == Just vId
                       && Just (currentRoom (save state)) /= vehicleCockpitRoom v
                    then Left "You need to be at the controls to drive."
                    else case payStopCost stop state of
                        Left err -> Left err
                        Right st ->
                            let st' = moveVehicleToStop vId destId st
                            in Right (st', "You drive to " ++ stopLabel stop ++ ".")

-- | AutomaticRoute/PaidVehicle: advance to the next stop (`wait`).
--   Only while aboard; pays the fare for PaidVehicles.
advanceVehicleRoute :: GameState -> Either String (GameState, String)
advanceVehicleRoute state = case currentVehicle (save state) of
    Nothing -> Left "You are not on a vehicle."
    Just vId -> case lookupVehicle vId state of
        Nothing -> Left "You are not on a vehicle."
        Just v
            | vehicleType v == PlayerControlled ->
                Left "This vehicle only moves when you drive it."
            | otherwise ->
                let vState = getVehicleState vId state
                in case nextVehicleStop v (vsCurrentStop vState) of
                    Nothing -> Left "The route has no further stops."
                    Just (destId, stop) -> case payStopCost stop state of
                        Left err -> Left err
                        Right st ->
                            let st' = moveVehicleToStop vId destId st
                            in Right (st', "You travel on to " ++ stopLabel stop ++ ".")

-- | Refuel: add fuel units (capped at max) via an item interaction.
--   Returns Nothing if the vehicle takes no fuel.
refuelVehicle :: VehicleID -> Int -> GameState -> Maybe (GameState, String)
refuelVehicle vId amount state = case lookupVehicle vId state >>= vehicleFuelProp of
    Nothing -> Nothing
    Just fs ->
        let vs = getVehicleState vId state
            maxFuel = fsMax fs
            cur = fromMaybe 0 (vsFuel vs)
            newFuel = min maxFuel (cur + amount)
        in Just (setVehicleState vId (vs { vsFuel = Just newFuel }) state,
                 "The " ++ maybe vId vehicleName (lookupVehicle vId state) ++ " is fuelled ("
                    ++ show newFuel ++ "/" ++ show maxFuel ++ ").")

-- | Clear a vehicle condition (e.g. after `repair`)
clearVehicleCondition :: VehicleID -> String -> GameState -> GameState
clearVehicleCondition vId condId state =
    let vs = getVehicleState vId state
    in setVehicleState vId (vs { vsActiveConditions = Set.delete condId (vsActiveConditions vs) }) state

-- | Fire a vehicle-wide condition effect with an outcome runner.
--   Returns the (possibly updated) state plus a message ("" if nothing fired).
vehicleConditionTickWith :: (Effect -> EntityID -> GameState -> (GameState, String))
                         -> GameState -> (GameState, String)
vehicleConditionTickWith runOutcome state = case currentVehicle (save state) of
    Nothing -> (state, "")
    Just vId -> case lookupVehicle vId state of
        Nothing -> (state, "")
        Just v ->
            let vState = getVehicleState vId state
                activeConds = Set.toList (vsActiveConditions vState)
                outcomes = [ o
                           | c <- activeConds
                           , Just o <- [Map.lookup c (vehicleConditionEffects v)] ]
            in if null outcomes
               then (state, "")
               else
                   let (st', msgs) = foldl' (\(s, ms) o ->
                            let (s2, m2) = runOutcome o "" s
                            in (s2, if null m2 then ms else ms ++ [m2]))
                            (state, []) outcomes
                   in (st', intercalate "\n" msgs)

-- | Fire a vehicle-wide condition effect using the engine's applyOutcome.
vehicleConditionTick :: GameState -> (GameState, String)
vehicleConditionTick = vehicleConditionTickWith applyOutcome

-- | Vehicle flavour for `look`: room override + active conditions + fuel
vehicleLookAddon :: GameState -> Maybe String
vehicleLookAddon state = case currentVehicle (save state) of
    Nothing -> Nothing
    Just vId -> case lookupVehicle vId state of
        Nothing -> Nothing
        Just v ->
            let vState = getVehicleState vId state
                condLine = if Set.null (vsActiveConditions vState)
                           then ""
                           else "Warning: " ++ intercalate ", " (Set.toList (vsActiveConditions vState))
                                ++ "!"
                fuelLine = case (vehicleFuelProp v, vsFuel vState) of
                    (Just fs, Just f) ->
                        if f <= 0
                            then "\nOut of " ++ fsItem fs ++ " (0/" ++ show (fsMax fs) ++ ")"
                            else "\nFuel (" ++ fsItem fs ++ ": " ++ show f ++ "/" ++ show (fsMax fs) ++ ")"
                    _ -> ""
                statusLine = unlines (filter (not . null) [condLine]) ++
                             (if null condLine then "" else "\n") ++ fuelLine
            in if null (trim statusLine) then Nothing else Just (trim statusLine)
  where
    trim = f . f where f = reverse . dropWhile (== '\n')
