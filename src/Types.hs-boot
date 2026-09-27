module Types where

import Data.Aeson (ToJSON, FromJSON)

type CardID = String
data Effect
instance Show Effect
instance Eq Effect
instance ToJSON Effect
instance FromJSON Effect
