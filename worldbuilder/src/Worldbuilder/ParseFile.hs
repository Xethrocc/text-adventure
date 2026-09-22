-- | File format auto-detection and parsing for the worldbuilder CLI.
--   Supports JSON (.json) and YAML (.yaml/.yml).
module Worldbuilder.ParseFile (parseAdventureFile) where

import Worldbuilder.Types (Adventure (..), AClip (..))

import Data.Aeson (eitherDecode)
import Data.YAML.Aeson (decode1)
import Data.YAML (Pos (..))
import qualified Data.ByteString.Lazy as BL
import Control.Exception (IOException, try)
import System.FilePath (takeExtension, takeDirectory, (</>))
import Data.Char (toLower)

-- | Parse an adventure from a JSON or YAML file.
--
--   P2-15: this used to return `Nothing` for every failure, so the CLI could
--   only say \"Failed to parse adventure file: <path>\" — the most common
--   authoring mistake was also the worst-diagnosed one. Now the reason (and for
--   YAML the line/column) comes back to the caller, and an unreadable file is
--   reported instead of throwing.
--
--   D14: after a successful parse, clips declared with `file:` get their
--   frames embedded from the companion file (paths relative to the
--   adventure), so the compiled world needs no extra file access at runtime.
parseAdventureFile :: FilePath -> IO (Either String Adventure)
parseAdventureFile path = do
    readResult <- try (BL.readFile path) :: IO (Either IOException BL.ByteString)
    case readResult of
        Left ioErr -> pure (Left ("Cannot read " ++ path ++ ": " ++ show ioErr))
        Right bytes ->
            let parsed = case map toLower (takeExtension path) of
                    ".yaml" -> fromYaml bytes
                    ".yml"  -> fromYaml bytes
                    _       -> case eitherDecode bytes of
                        Right adv -> Right adv
                        Left err  -> Left (path ++ ": JSON parse error: " ++ err)
            in case parsed of
                Left err   -> pure (Left err)
                Right adv  -> embedClipFiles path adv
  where
    fromYaml bytes = case decode1 bytes of
        Right adv          -> Right adv
        Left (pos, reason) -> Left (path ++ ": YAML parse error at line "
                                    ++ show (posLine pos) ++ ", column "
                                    ++ show (posColumn pos) ++ ": " ++ reason)

-- | Embed companion-file frames of declared clips (D14). Only clips that
--   declare `file:` and no inline `frames:` are touched; failures surface as
--   parse errors with the offending path and clip id.
embedClipFiles :: FilePath -> Adventure -> IO (Either String Adventure)
embedClipFiles adventurePath adv
    | null (advClips adv) = pure (Right adv)
    | otherwise           = do
        results <- mapM loadOne (advClips adv)
        pure $ case sequence' results of
            Left err      -> Left err
            Right clips'  -> Right adv { advClips = clips' }
  where
    dir = takeDirectory adventurePath
    loadOne c = case acFile c of
        Nothing -> pure (Right c)
        Just rel -> do
            r <- try (BL.readFile (dir </> rel)) :: IO (Either IOException BL.ByteString)
            pure $ case r of
                Left ioErr -> Left (adventurePath ++ ": clip '" ++ acId c
                                    ++ "': cannot read " ++ rel ++ ": " ++ show ioErr)
                Right bytes -> case eitherDecode bytes of
                    Left err  -> Left (adventurePath ++ ": clip '" ++ acId c
                                       ++ "': " ++ rel ++ ": JSON parse error: " ++ err)
                    Right frames -> Right c { acFrames = frames, acFile = Nothing }
    sequence' xs = go xs []
      where
        go [] acc = Right (reverse acc)
        go (Left e : _) _ = Left e
        go (Right x : rest) acc = go rest (x : acc)
