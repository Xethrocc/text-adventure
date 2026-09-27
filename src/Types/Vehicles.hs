{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Vehicles and route traversal data types
module Types.Vehicles
    ( VehicleType (..)
    , VehicleStop (..)
    , FuelSpec (..)
    , VehicleDef (..)
    , VehicleState (..)
    ) where

import Control.Applicative ((<|>))
import GHC.Generics (Generic)
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import {-# SOURCE #-} Types (RoomID, ItemID, VehicleID, Effect)

-- | How a vehicle moves between its stops
data VehicleType
    = PlayerControlled   -- ^ The player steers it (from the cockpit, `drive to`)
    | AutomaticRoute     -- ^ Follows its route (`wait` advances to next stop)
    | PaidVehicle        -- ^ AutomaticRoute, but each stop costs an item
    deriving (Show, Eq, Generic)

instance ToJSON VehicleType
instance FromJSON VehicleType

-- | One stop on a vehicle's route: the outside room it docks at
data VehicleStop = VehicleStop
    { stopExternalRoom :: RoomID                     -- ^ Outside room at this stop
    , stopLabel        :: String                     -- ^ e.g. "Köln Hbf, Gleis 3"
    , stopCost         :: Maybe (ItemID, String)     -- ^ (item consumed per stop, error msg) for PaidVehicle
    } deriving (Show, Eq, Generic)

instance ToJSON VehicleStop
instance FromJSON VehicleStop

-- | Fuel specification for a vehicle (P2-21).
--   Replaces a bare `(String, Int)` tuple; decoded from both the current
--   object form `{"item": …, "max": …}` and the legacy two-element array.
data FuelSpec = FuelSpec
    { fsItem :: ItemID   -- ^ item that refuels this vehicle
    , fsMax  :: Int      -- ^ tank capacity
    } deriving (Show, Eq, Generic)

instance ToJSON FuelSpec where
    toJSON fs = object [ "item" .= fsItem fs, "max" .= fsMax fs ]

instance FromJSON FuelSpec where
    parseJSON v =
        (do xs <- parseJSON v :: Parser [Value]
            case xs of
                [i, m] -> FuelSpec <$> parseJSON i <*> parseJSON m
                _      -> fail "fuel: expected [item, max] or {item, max}")
        <|> withObject "FuelSpec" (\o -> FuelSpec <$> o .: "item" <*> o .: "max") v

-- | Static vehicle definition. The vehicle's interior rooms live in the
--   world's global `rooms` map (so hooks, tags and lighting work there too);
--   `vehicleRooms` lists which room ids belong to this vehicle.
data VehicleDef = VehicleDef
    { vehicleId               :: VehicleID
    , vehicleName             :: String
    , vehicleDescription      :: String
    , vehicleType             :: VehicleType
    , vehicleRooms            :: [RoomID]                       -- ^ interior room ids
    , vehicleEntryRoom        :: RoomID                         -- ^ where `enter` puts the player
    , vehicleCockpitRoom      :: Maybe RoomID                   -- ^ required for `drive` (PlayerControlled)
    , vehicleStops            :: Map.Map RoomID VehicleStop     -- ^ outside room -> stop
    , vehicleRoute            :: [RoomID]                       -- ^ AUTHORED stop order; empty = Map key order (backward compat)
    , vehicleKeywords         :: [String]
    , vehicleFuelProp         :: Maybe FuelSpec                -- ^ fuel item + tank capacity
    , vehicleConditionEffects :: Map.Map String Effect   -- ^ condition -> outcome fired vehicle-wide
    } deriving (Show, Eq)

instance ToJSON VehicleDef where
    toJSON v = object
        [ "vehicleId"               .= vehicleId v
        , "vehicleName"             .= vehicleName v
        , "vehicleDescription"      .= vehicleDescription v
        , "vehicleType"             .= vehicleType v
        , "vehicleRooms"            .= vehicleRooms v
        , "vehicleEntryRoom"        .= vehicleEntryRoom v
        , "vehicleCockpitRoom"      .= vehicleCockpitRoom v
        , "vehicleStops"            .= vehicleStops v
        , "vehicleRoute"            .= vehicleRoute v
        , "vehicleKeywords"         .= vehicleKeywords v
        , "vehicleFuelProp"         .= vehicleFuelProp v
        , "vehicleConditionEffects" .= vehicleConditionEffects v
        ]

instance FromJSON VehicleDef where
    parseJSON = withObject "VehicleDef" $ \o -> VehicleDef
        <$> o .:  "vehicleId"
        <*> o .:  "vehicleName"
        <*> o .:  "vehicleDescription"
        <*> o .:  "vehicleType"
        <*> o .:? "vehicleRooms"            .!= []
        <*> o .:  "vehicleEntryRoom"
        <*> o .:? "vehicleCockpitRoom"      .!= Nothing
        <*> o .:? "vehicleStops"            .!= Map.empty
        <*> o .:? "vehicleRoute"            .!= []
        <*> o .:? "vehicleKeywords"         .!= []
        <*> o .:? "vehicleFuelProp"         .!= Nothing
        <*> o .:? "vehicleConditionEffects" .!= Map.empty

-- | Dynamic vehicle state
data VehicleState = VehicleState
    { vsCurrentStop      :: RoomID                    -- ^ outside room the vehicle is currently at
    , vsFuel             :: Maybe Int                 -- ^ remaining fuel units (if fuelled)
    , vsActiveConditions :: Set.Set String            -- ^ e.g. "hull_breach", "derailed"
    , vsRoomOverrides    :: Map.Map RoomID String     -- ^ condition-flavoured room descriptions
    } deriving (Show, Eq, Generic)

instance ToJSON VehicleState
instance FromJSON VehicleState where
    parseJSON = withObject "VehicleState" $ \o -> VehicleState
        <$> o .:  "vsCurrentStop"
        <*> o .:? "vsFuel"             .!= Nothing
        <*> o .:? "vsActiveConditions" .!= Set.empty
        <*> o .:? "vsRoomOverrides"    .!= Map.empty
