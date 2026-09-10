-- | File format auto-detection and parsing for the worldbuilder CLI.
--   Supports JSON (.json) and YAML (.yaml/.yml).
module Worldbuilder.ParseFile (parseAdventureFile) where

import Worldbuilder.Types (Adventure)

import Data.Aeson (decode)
import Data.YAML.Aeson (decode1)
import qualified Data.ByteString.Lazy as BL
import System.FilePath (takeExtension)
import Data.Char (toLower)

-- | Parse an adventure from a JSON or YAML file.
--   Returns Nothing if the file can't be read or parsed.
parseAdventureFile :: FilePath -> IO (Maybe Adventure)
parseAdventureFile path = do
    bytes <- BL.readFile path
    pure $ case map toLower (takeExtension path) of
        ".yaml" -> case decode1 bytes of
                       Right adv -> Just adv
                       Left _    -> Nothing
        ".yml"  -> case decode1 bytes of
                       Right adv -> Just adv
                       Left _    -> Nothing
        _       -> decode bytes