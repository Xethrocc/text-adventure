-- | Worldbuilder CLI: validate, compile, check
module Worldbuilder.CLI (runCLI) where

import Worldbuilder.Types ()
import Worldbuilder.Compile (CompileResult(..), compileAdventure, CompileIssue(..), Severity(..))
import Worldbuilder.ParseFile (parseAdventureFile)
import Worldbuilder.Generate (parseTemplate, validateTemplate, generateDungeon, dtSeed, GenerateError (..))
import Worldbuilder.Rng (deriveRuntimeSeed)
import Worldbuilder.Run (RunConfig (..), runRunner)

-- JSON encoding (output only)
import Data.Aeson (encode)
import qualified Data.YAML.Aeson (decode1)
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word64)

-- Engine
import Validate (validateWorld, validateGameState)
import qualified Types as E

-- System
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (hSetEncoding, stdout, stderr, stdin, utf8)
import Control.Monad (unless)
import Worldbuilder.Locate (lineForPath)
import Control.Exception (try, SomeException)
import Data.YAML (posLine, posColumn)
import qualified Data.Map.Strict as Map

-- | Entry point for the worldbuilder CLI
runCLI :: IO ()
runCLI = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    hSetEncoding stdin utf8
    args <- getArgs
    case args of
        ("validate" : path : _)    -> validate path
        ("compile" : path : rest)  -> compile path rest
        ("generate" : path : rest) -> generateCmd path rest
        ("run" : path : rest)      -> runCmd path rest
        ("check" : path : _)       -> checkStats path
        _                          -> putStrLn usage

usage :: String
usage = unlines
    [ "worldbuilder - text-adventure authoring toolchain"
    , ""
    , "Usage:"
    , "  worldbuilder validate <adventure.json>      Check for consistency errors (exit 1 on errors)"
    , "  worldbuilder compile <adventure.json> -o <dir> [--force]  Emit world.json + save.json"
    , "                                              --force writes even if validation has issues"
    , "  worldbuilder check <adventure.json>         Print content statistics"
    , ""
    , "  worldbuilder generate <template.yaml> --seed N -o <dir> [--force]"
    , "                                              Generate a dungeon from a template and emit"
    , "                                              world.json + save.json (Rogue Phase 4; the seed"
    , "                                              is required — same seed, bit-identical world)"
    , ""
    , "  worldbuilder run <template.yaml> [--seed N] [--saves-dir <dir>] [--keep-runs N]"
    , "                                   [--no-launch] [--force] [--tui] [--no-color]"
    , "                                              Generate next run from template (seed derived from"
    , "                                              slug + meta.runs) in <saves-dir>/<slug>/run_<n>/"
    , "                                              and start the engine on it (Rogue Phase 4c)."
    , ""
    , "Supports .json, .yaml and .yml files."
    ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Render a list of compile issues in a readable format. The source file is
--   searched for the issue's path so the author gets a line number (W5, Stufe 1):
--   @rooms.cave.ascii@ becomes @rooms.cave.ascii (line 47)@ when the file
--   contains that nesting, and stays unchanged when it does not (heuristik —
--   see `Worldbuilder.Locate.lineForPath`).
printCompileIssues :: FilePath -> [CompileIssue] -> IO ()
printCompileIssues srcFile issues = do
    content <- readFileUtf8Safe srcFile
    mapM_ (putStrLn . showIssue content) issues

-- | Read the source file for locating, tolerating encoding issues.
readFileUtf8Safe :: FilePath -> IO String
readFileUtf8Safe path = do
    r <- try (readFile path) :: IO (Either SomeException String)
    pure $ case r of
        Left _  -> ""
        Right c -> c

showIssue :: String -> CompileIssue -> String
showIssue content i =
    let sev = case ciSeverity i of
            SError  -> "error"
            SWarning -> "warning"
        at = case lineForPath content (ciPath i) of
            Just (n, _) -> " (line " ++ show n ++ ")"
            Nothing     -> ""
        hint = repairHint (ciCode i)
    in "  [" ++ sev ++ "] " ++ ciCode i ++ " at " ++ ciPath i ++ at
       ++ ": " ++ ciMessage i
       ++ if null hint then "" else "\n        -> " ++ hint

-- | One-sentence repair hint for the most common authoring mistakes (W6
--   material, decision DW5b): shown as a second line under the issue. Codes
--   without an entry just print the message — the list is deliberately small,
--   so hints stay true and never generic boilerplate.
repairHint :: String -> String
repairHint code = case code of
    "UnknownDirection" ->
        "Use one of: north, south, east, west, up, down, in, out (or their short forms)."
    "HotspotGlyphMissing" ->
        "Put the marker glyph (e.g. '*') into the ascii art itself, or change the hotspot's glyph."
    "UnknownHotspotTarget" ->
        "The hotspot target must be the id of a declared item or NPC."
    "UnknownRoom" ->
        "Check the id for a typo — it must match a room's id exactly (ids are case-sensitive)."
    "UnknownNPC" ->
        "Check the npc id for a typo — it must match an npc's id exactly."
    "UnknownItem" ->
        "Check the item id for a typo — it must match an item's id exactly."
    "DuplicateRoomID" ->
        "Give one of the rooms a different id; every room id must be unique."
    "DuplicateDirection" ->
        "A room can only have one exit per direction — remove the duplicate key."
    "InvalidVariableType" ->
        "type must be int, text or bool."
    "CombatVariableClash" ->
        "Names starting with 'combat.' belong to the engine — rename the variable."
    "CooldownConditionClash" ->
        "Condition names starting with 'cooldown_' belong to the engine — rename the condition."
    "UnknownCommandVerb" ->
        "Use a core command (look, take, drop, use, ...) or declare the verb under verbs:."
    _ -> ""

-- | Render validation errors (ValidationError derives Show)
showValidationError :: Show a => a -> String
showValidationError = show

-- ---------------------------------------------------------------------------
-- Validate
-- ---------------------------------------------------------------------------

validate :: FilePath -> IO ()
validate path = do
    advResult <- parseAdventureFile path
    case advResult of
        Left err -> do
            putStrLn $ "Failed to parse adventure file: " ++ err
            exitFailure
        Right adv -> do
            case compileAdventure adv of
                Left errs -> do
                    putStrLn "Compilation errors:"
                    printCompileIssues path errs
                    putStrLn ""
                    putStrLn $ show (length errs) ++ " hard error(s); adventure cannot be compiled."
                    exitFailure
                Right cr -> do
                    unless (null (crWarnings cr)) $ do
                        putStrLn "Compiler warnings:"
                        printCompileIssues path (crWarnings cr)
                        putStrLn ""
                    let worldErrs = validateWorld (crWorld cr)
                        stateErrs = validateGameState (crWorld cr) (crSave cr)
                        allErrs = worldErrs ++ stateErrs
                    if null allErrs
                    then do
                        putStrLn "Adventure is valid! (no issues found)"
                        exitSuccess
                    else do
                        putStrLn "Validation found issues:"
                        mapM_ (\e -> putStrLn ("  - " ++ showValidationError e)) allErrs
                        putStrLn ""
                        putStrLn "These issues may cause problems during play."
                        exitFailure

-- ---------------------------------------------------------------------------
-- Compile
-- ---------------------------------------------------------------------------

compile :: FilePath -> [String] -> IO ()
compile path rest = do
    let outDir = case rest of
            "-o" : d : _ -> d
            "--output" : d : _ -> d
            _            -> "."
        force = "--force" `elem` rest
    advResult <- parseAdventureFile path
    case advResult of
        Left err -> do
            putStrLn $ "Failed to parse adventure file: " ++ err
            exitFailure
        Right adv -> case compileAdventure adv of
            Left errs -> do
                putStrLn "Compilation errors:"
                printCompileIssues path errs
                putStrLn ""
                putStrLn $ show (length errs) ++ " hard error(s); output not written."
                exitFailure
            Right cr -> do
                -- Rogue Phase 1: non-fatal compiler diagnostics (e.g.
                -- IronmanWithoutSavezones) are shown, never block.
                unless (null (crWarnings cr)) $ do
                    putStrLn "Compiler warnings:"
                    printCompileIssues path (crWarnings cr)
                    putStrLn ""
                let errors = validateWorld (crWorld cr) ++ validateGameState (crWorld cr) (crSave cr)
                if not (null errors) && not force
                then do
                    putStrLn "Validation found issues (use --force to write anyway):"
                    mapM_ (\e -> putStrLn ("  - " ++ showValidationError e)) errors
                    exitFailure
                else do
                    createDirectoryIfMissing True outDir
                    let worldPath = outDir </> "world.json"
                    BL.writeFile worldPath (encode (crWorld cr))
                    putStrLn $ "Wrote " ++ worldPath
                    let savePath = outDir </> "save.json"
                    BL.writeFile savePath (encode (crSave cr))
                    putStrLn $ "Wrote " ++ savePath
                    if null errors
                    then putStrLn "No validation issues found."
                    else do
                        putStrLn "Validation warnings (written with --force):"
                        mapM_ (\e -> putStrLn ("  - " ++ showValidationError e)) errors
                    exitSuccess

-- ---------------------------------------------------------------------------
-- Check stats
-- ---------------------------------------------------------------------------

checkStats :: FilePath -> IO ()
checkStats path = do
    advResult <- parseAdventureFile path
    case advResult of
        Left err -> do
            putStrLn $ "Failed to parse adventure file: " ++ err
            exitFailure
        Right adv -> case compileAdventure adv of
            Left errs -> do
                putStrLn "Compilation errors:"
                printCompileIssues path errs
                exitFailure
            Right cr -> do
                let gw = crWorld cr
                putStrLn "=== Adventure Statistics ==="
                putStrLn $ "Rooms:          " ++ show (length (E.rooms gw))
                putStrLn $ "Items:          " ++ show (length (E.itemDefs gw))
                putStrLn $ "NPCs:           " ++ show (length (E.npcDefs gw))
                putStrLn $ "Quests:         " ++ show (length (E.questDefs gw))
                putStrLn $ "Vehicles:       " ++ show (length (E.vehicleDefs gw))
                putStrLn $ "Exits:          " ++ show (sum [length (E.roomConnections r) | r <- Map.elems (E.rooms gw)])
                let totalDlg = sum [length (E.dtNodes t) | npc <- Map.elems (E.npcDefs gw), t <- Map.elems (E.npcDialogueTrees npc)]
                putStrLn $ "Dialogue nodes: " ++ show totalDlg
                putStrLn $ "Entity ints:    " ++ show (length (E.entityInteractions gw))
                putStrLn $ "Item ints:      " ++ show (length (E.itemInteractions gw))
                let validationErrors = validateWorld gw ++ validateGameState gw (crSave cr)
                if null validationErrors
                then putStrLn "Validation:     CLEAN"
                else putStrLn $ "Validation:     " ++ show (length validationErrors) ++ " issue(s)"
                exitSuccess
-- ---------------------------------------------------------------------------
-- Generate (Rogue Phase 4d): template -> dungeon -> world.json + save.json
-- ---------------------------------------------------------------------------

-- | @generate <template.yaml> --seed N -o <dir> [--force]@
--
--   Reads the dungeon template (YAML/JSON like an adventure), validates its
--   schema (TemplateIssue-style diagnostics rendered against the template
--   file, with Locate line numbers), runs the pure generator, feeds the
--   resulting Adventure through the regular compileAdventure pipeline,
--   validates the compiled world + save and writes world.json + save.json.
--   The save's runtime rngState is derived from the generation seed
--   (detail plan section 2), so runtime randomness inside a generated
--   dungeon is deterministic per seed as well.
--
--   Exit codes mirror compile: 0 ok, 1 on template/generation/compile/
--   validation errors; --force writes despite validation issues.
generateCmd :: FilePath -> [String] -> IO ()
generateCmd path rest = do
    let outDir = case lookupFlag "-o" rest of
            Just d  -> d
            Nothing -> case lookupFlag "--output" rest of
                Just d  -> d
                Nothing -> "."
        force = "--force" `elem` rest
        seedFromCli = case rest of
            ("--seed" : s : _) -> readMaybeW64 s
            _                  -> Nothing
    rawResult <- try (BL.readFile path) :: IO (Either SomeException BL.ByteString)
    templateValue <- case rawResult of
        Left ioErr -> do
            putStrLn $ "Failed to read template file: " ++ show ioErr
            exitFailure
        Right bytes -> case Data.YAML.Aeson.decode1 bytes of
            Left (pos, err) -> do
                putStrLn $ "Failed to parse template file: YAML parse error at line "
                    ++ show (posLine pos) ++ ", column " ++ show (posColumn pos) ++ ": " ++ err
                exitFailure
            Right v -> pure v
    case parseTemplate templateValue of
        Left err -> do
            putStrLn $ "Failed to decode template: " ++ err
            exitFailure
        Right tmpl -> do
            -- schema validation first: diagnostics point into the template
            case validateTemplate tmpl of
                errs@(i : _)
                    | any ((== SError) . ciSeverity) errs -> do
                        putStrLn "Template errors:"
                        printCompileIssues path errs
                        putStrLn ""
                        putStrLn $ show (length errs) ++ " template error(s) (first: " ++ ciCode i ++ ")."
                        exitFailure
                _ -> pure ()
            -- seed: CLI wins over template seed; absence is an error (no
            -- hidden time-based seed — determinism beats convenience)
            seed <- case (seedFromCli, dtSeed tmpl) of
                (Just s, _)  -> pure s
                (Nothing, Just s) -> pure s
                (Nothing, Nothing) -> do
                    putStrLn "Error: no seed given. Pass --seed N (or seed: N in the template)."
                    exitFailure
            case generateDungeon tmpl seed of
                Left err -> do
                    putStrLn "Generation failed:"
                    putStrLn $ "  - " ++ showGenerateError err
                    exitFailure
                Right (adv, genWarns) -> case compileAdventure adv of
                    Left cerrs -> do
                        putStrLn "Generated adventure failed to compile (generator bug — please report):"
                        printCompileIssues path cerrs
                        exitFailure
                    Right cr0 -> do
                        -- runtime rngState derived from the generation seed
                        let cr = cr0 { crSave = (crSave cr0)
                            { E.rngState = deriveRuntimeSeed seed } }
                        unless (null genWarns) $ do
                            putStrLn "Generator warnings:"
                            printCompileIssues path genWarns
                            putStrLn ""
                        unless (null (crWarnings cr)) $ do
                            putStrLn "Compiler warnings:"
                            printCompileIssues path (crWarnings cr)
                            putStrLn ""
                        let errors = validateWorld (crWorld cr)
                                     ++ validateGameState (crWorld cr) (crSave cr)
                        if not (null errors) && not force
                            then do
                                putStrLn "Validation found issues (use --force to write anyway):"
                                mapM_ (\e -> putStrLn ("  - " ++ showValidationError e)) errors
                                exitFailure
                            else do
                                createDirectoryIfMissing True outDir
                                let worldPath = outDir </> "world.json"
                                BL.writeFile worldPath (encode (crWorld cr))
                                putStrLn $ "Wrote " ++ worldPath
                                let savePath = outDir </> "save.json"
                                BL.writeFile savePath (encode (crSave cr))
                                putStrLn $ "Wrote " ++ savePath
                                putStrLn $ "Seed: " ++ show seed
                                    ++ ", rooms: " ++ show (length (crWorldRooms cr))
                                if null errors
                                    then putStrLn "No validation issues found."
                                    else do
                                        putStrLn "Validation warnings (written with --force):"
                                        mapM_ (\e -> putStrLn ("  - " ++ showValidationError e)) errors
                                exitSuccess
  where
    crWorldRooms = Map.keys . E.rooms . crWorld

-- | Parse a decimal Word64 seed, Nothing on garbage.
readMaybeW64 :: String -> Maybe Word64
readMaybeW64 s = case reads s of
    [(n, "")] | n >= (0 :: Integer) -> Just (fromIntegral n)
    _ -> Nothing

-- | Value of @-x <value>@ anywhere in the argument list.
lookupFlag :: String -> [String] -> Maybe String
lookupFlag k (x : y : ys) | x == k    = Just y
                          | otherwise = lookupFlag k (y : ys)
lookupFlag _ _ = Nothing

-- | Generation errors are parameter problems, not template locations
--   (detail plan section 4) — reported without line numbers.
showGenerateError :: GenerateError -> String
showGenerateError (GETemplate iss) =
    "template schema violation: " ++ unwords (map ciCode iss)
showGenerateError (GEUnreachable cells) =
    "cells unreachable after docking pass: " ++ show cells
    ++ " (generator bug — please report)"
showGenerateError GENoSpace =
    "grid too small to place even layout.rooms.min cells —"
    ++ " widen the grid budget (rooms.min) or reduce depth"

-- | Execute `worldbuilder run <template.yaml>` (Rogue Phase 4c).
runCmd :: FilePath -> [String] -> IO ()
runCmd path rest = do
    let seedFromCli = case lookupFlag "--seed" rest of
            Just s  -> readMaybeW64 s
            Nothing -> Nothing
        savesDirFromCli = lookupFlag "--saves-dir" rest
        keepRunsFromCli = case lookupFlag "--keep-runs" rest of
            Just s  -> readMaybeInt s
            Nothing -> Nothing
        noLaunch = "--no-launch" `elem` rest || "--dry-run" `elem` rest
        force = "--force" `elem` rest
        tui = "--tui" `elem` rest
        noColor = "--no-color" `elem` rest
        cfg = RunConfig
            { rcTemplatePath = path
            , rcSeedOverride = seedFromCli
            , rcSavesDir     = savesDirFromCli
            , rcKeepRuns     = keepRunsFromCli
            , rcNoLaunch     = noLaunch
            , rcForce        = force
            , rcTui          = tui
            , rcNoColor      = noColor
            , rcExtraArgs    = filter (`notElem` [ "--no-launch", "--dry-run", "--force", "--tui", "--no-color" ]) rest
            }
    runRunner cfg
  where
    readMaybeInt s = case reads s of
        [(n, "")] | n >= (0 :: Integer) -> Just (fromIntegral n :: Int)
        _                               -> Nothing
