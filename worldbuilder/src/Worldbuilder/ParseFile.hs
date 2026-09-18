-- | File format auto-detection and parsing for the worldbuilder CLI.
--   Supports JSON (.json) and YAML (.yaml/.yml).
module Worldbuilder.ParseFile (parseAdventureFile) where

import Worldbuilder.Types (Adventure)

import Data.Aeson (eitherDecode)
import Data.YAML.Aeson (decode1)
import Data.YAML (Pos (..))
import qualified Data.ByteString.Lazy as BL
import Control.Exception (IOException, try)
import System.FilePath (takeExtension)
import Data.Char (toLower)

-- | Parse an adventure from a JSON or YAML file.
--
--   P2-15: this used to return `Nothing` for every failure, so the CLI could
--   only say \"Failed to parse adventure file: <path>\" — the most common
--   authoring mistake was also the worst-diagnosed one. Now the reason (and for
--   YAML the line/column) comes back to the caller, and an unreadable file is
--   reported instead of throwing.
parseAdventureFile :: FilePath -> IO (Either String Adventure)
parseAdventureFile path = do
    readResult <- try (BL.readFile path) :: IO (Either IOException BL.ByteString)
    pure $ case readResult of
        Left ioErr  -> Left ("Cannot read " ++ path ++ ": " ++ show ioErr)
        Right bytes -> case map toLower (takeExtension path) of
            ".yaml" -> fromYaml bytes
            ".yml"  -> fromYaml bytes
            _       -> case eitherDecode bytes of
                Right adv -> Right adv
                Left err  -> Left (path ++ ": JSON parse error: " ++ err)
  where
    fromYaml bytes = case decode1 bytes of
        Right adv          -> Right adv
        Left (pos, reason) -> Left (path ++ ": YAML parse error at line "
                                    ++ show (posLine pos) ++ ", column "
                                    ++ show (posColumn pos) ++ ": " ++ reason)
