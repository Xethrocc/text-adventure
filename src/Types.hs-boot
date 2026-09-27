module Types where

import Data.Aeson (ToJSON, FromJSON)

type CardID = String
type RoomID = String
type ItemID = String
type VehicleID = String

data Effect
instance Show Effect
instance Eq Effect
instance ToJSON Effect
instance FromJSON Effect

noopEffect :: Effect

data AsciiArt
instance Show AsciiArt
instance Eq AsciiArt
instance ToJSON AsciiArt
instance FromJSON AsciiArt

emptyAscii :: AsciiArt
isEmptyAscii :: AsciiArt -> Bool
