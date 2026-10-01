{-# LANGUAGE OverloadedStrings #-}

-- | B6: game export — bundle a compiled adventure together with its referenced
--   asset files and a launcher, so a finished game leaves the repo as one
--   directory (optionally zipped, optionally self-contained).
--
--   Contracts:
--
--   * @world.json@/@save.json@ are written with the exact same encoding as
--     @worldbuilder compile@ — an exported bundle is byte-identical to a plain
--     compile, the export only *adds* files around it.
--   * Asset paths (`sfx:`/`music:` effects and the `assets:` manifest) are
--     **game-root-relative**: resolved against the main adventure file's
--     directory at export time and against the bundle root (the launcher's
--     working directory) at runtime. Relative paths are copied verbatim, so the
--     strings inside world.json never change.
--   * Missing assets are warnings, not errors — the bundle is still written.
module Worldbuilder.Export
    ( collectAssetRefs
    , bundleFiles
    , launcherSh
    , launcherBat
    , exportBundle
    , makeZip
    ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Set as Set
import System.Directory
    ( copyFile, createDirectoryIfMissing, doesFileExist, findExecutable
    , getPermissions, setPermissions, setOwnerExecutable )
import System.Exit (ExitCode (..))
import System.FilePath
    ( (</>), (<.>), dropTrailingPathSeparator, isAbsolute, normalise
    , splitDirectories, takeDirectory, takeFileName )
import System.Process (CreateProcess (cwd), createProcess, proc, waitForProcess)

import Worldbuilder.Compile (allAOutcomes)
import Worldbuilder.Types
import qualified Types as E

-- | Every asset file the bundle needs: the paths referenced by @sfx:@/@music:@
--   outcomes (recursively, over every surface) plus the explicit @assets:@
--   manifest — sorted and deduplicated (ordered container, reproducible).
collectAssetRefs :: Adventure -> [String]
collectAssetRefs adv =
    Set.toList (Set.fromList (concatMap outcomeAssets (allAOutcomes adv) ++ advAssets adv))
  where
    outcomeAssets o = case o of
        AOPlaySfx p   -> [p]
        AOPlayMusic p -> [p]
        _             -> []

-- | The files every bundle contains (relative paths), assets included. The
--   engine binary is optional and handled in 'exportBundle'.
bundleFiles :: [String] -> [FilePath]
bundleFiles assets = ["world.json", "save.json", "play.sh", "play.bat"] ++ assets

-- | Unix/macOS launcher: cd into the bundle, prefer a bundled engine in @bin/@,
--   auto-detect a bundled audio helper, then run the game. Extra arguments are
--   passed through (e.g. @--tui@).
launcherSh :: String
launcherSh = unlines
    [ "#!/usr/bin/env bash"
    , "# play.sh -- start this game bundle."
    , "set -e"
    , "cd \"$(dirname \"$0\")\""
    , ""
    , "# prefer a bundled engine in bin/, fall back to one on PATH"
    , "if [ -x \"bin/text-adventure\" ]; then"
    , "  ENGINE=\"bin/text-adventure\""
    , "else"
    , "  ENGINE=\"$(command -v text-adventure || true)\""
    , "fi"
    , "if [ -z \"$ENGINE\" ]; then"
    , "  echo \"Engine 'text-adventure' not found on PATH (install it or re-export with --with-engine).\" >&2"
    , "  exit 1"
    , "fi"
    , ""
    , "# bundled audio helper auto-detect (same convention as the engine release)"
    , "if [ -z \"${TEXT_ADVENTURE_AUDIO_HELPER:-}\" ]; then"
    , "  for cand in bin/mpv bin/ffplay bin/audio-helper; do"
    , "    if [ -x \"$cand\" ]; then"
    , "      TEXT_ADVENTURE_AUDIO_HELPER=\"$cand\""
    , "      export TEXT_ADVENTURE_AUDIO_HELPER"
    , "      break"
    , "    fi"
    , "  done"
    , "fi"
    , ""
    , "exec \"$ENGINE\" --world world.json --save save.json \"$@\""
    ]

-- | Windows launcher (same shape as the release ZIP's play.bat).
launcherBat :: String
launcherBat = unlines
    [ "@echo off"
    , "rem play.bat -- start this game bundle."
    , "setlocal enabledelayedexpansion"
    , "chcp 65001 >nul 2>&1"
    , "cd /d \"%~dp0\""
    , ""
    , "if not defined TEXT_ADVENTURE_AUDIO_HELPER ("
    , "    if exist \"%~dp0bin\\mpv.exe\" ("
    , "        set \"TEXT_ADVENTURE_AUDIO_HELPER=%~dp0bin\\mpv.exe\""
    , "    ) else if exist \"%~dp0bin\\ffplay.exe\" ("
    , "        set \"TEXT_ADVENTURE_AUDIO_HELPER=%~dp0bin\\ffplay.exe\""
    , "    ) else if exist \"%~dp0bin\\audio-helper.exe\" ("
    , "        set \"TEXT_ADVENTURE_AUDIO_HELPER=%~dp0bin\\audio-helper.exe\""
    , "    )"
    , ")"
    , ""
    , "if exist \"%~dp0bin\\text-adventure.exe\" ("
    , "    \"%~dp0bin\\text-adventure.exe\" --world \"%~dp0world.json\" --save \"%~dp0save.json\" %*"
    , ") else ("
    , "    text-adventure --world \"%~dp0world.json\" --save \"%~dp0save.json\" %*"
    , ")"
    ]

-- | Write the bundle into @outDir@: world/save (byte-identical to
--   @worldbuilder compile@), the launchers and every referenced asset
--   (relative layout preserved). Returns the warnings (missing assets, missing
--   engine, rejected paths) — never throws for content problems.
exportBundle :: FilePath -> FilePath -> E.GameWorld -> E.SaveState -> [String] -> Bool
             -> IO [String]
exportBundle srcDir outDir world save assets withEngine = do
    createDirectoryIfMissing True outDir
    BL.writeFile (outDir </> "world.json") (Aeson.encode world)
    BL.writeFile (outDir </> "save.json") (Aeson.encode save)
    writeFile (outDir </> "play.sh") launcherSh
    shPath <- getPermissions (outDir </> "play.sh")
    setPermissions (outDir </> "play.sh") (setOwnerExecutable True shPath)
    writeFile (outDir </> "play.bat") launcherBat
    assetWarnings <- mapM copyAsset (Set.toList (Set.fromList assets))
    engineWarning <- copyEngine
    pure (concat assetWarnings ++ maybe [] pure engineWarning)
  where
    copyAsset rel
        | isAbsolute rel || ".." `elem` splitDirectories (normalise rel) =
            pure ["asset '" ++ rel ++ "' is not game-root-relative — skipped"]
        | otherwise = do
            let src = srcDir </> rel
                dst = outDir </> rel
            exists <- doesFileExist src
            if exists
                then do
                    createDirectoryIfMissing True (takeDirectory dst)
                    copyFile src dst
                    pure []
                else pure ["asset '" ++ rel ++ "' not found (looked at " ++ src ++ ") — skipped"]

    copyEngine
        | not withEngine = pure Nothing
        | otherwise = do
            mExe <- findExecutable "text-adventure"
            case mExe of
                Nothing -> pure (Just "--with-engine given, but no 'text-adventure' executable was found on PATH")
                Just exe -> do
                    createDirectoryIfMissing True (outDir </> "bin")
                    copyFile exe (outDir </> "bin" </> takeFileName exe)
                    pure Nothing

-- | Zip the bundle directory (the archive lands next to it: @<dir>.zip@).
--   Uses @zip@, then @7z@ — the same candidate chain as the release script.
makeZip :: FilePath -> IO (Either String FilePath)
makeZip outDir = do
    let parent = takeDirectory outDir
        base   = takeFileName (dropTrailingPathSeparator outDir)
        zipPath = parent </> base <.> "zip"
    mZip <- findExecutable "zip"
    m7z  <- findExecutable "7z"
    case (mZip, m7z) of
        (Just z, _) -> runIn parent z ["-r", zipPath, base] zipPath
        (_, Just s) -> runIn parent s ["a", "-tzip", zipPath, base] zipPath
        _           -> pure (Left "no zip tool found (install 'zip' or '7z') — bundle directory was written")
  where
    runIn dir tool args out = do
        (_, _, _, ph) <- createProcess (proc tool args) { cwd = Just dir }
        code <- waitForProcess ph
        pure $ case code of
            ExitSuccess   -> Right out
            ExitFailure n -> Left (tool ++ " exited with " ++ show n)
