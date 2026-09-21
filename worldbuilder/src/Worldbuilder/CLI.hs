-- | Worldbuilder CLI: validate, compile, check
module Worldbuilder.CLI (runCLI) where

import Worldbuilder.Types ()
import Worldbuilder.Compile (CompileResult(..), compileAdventure, CompileIssue(..), Severity(..))
import Worldbuilder.ParseFile (parseAdventureFile)

-- JSON encoding (output only)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL

-- Engine
import Validate (validateWorld, validateGameState)
import qualified Types as E

-- System
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (hSetEncoding, stdout, stderr, stdin, utf8)
import Worldbuilder.Locate (lineForPath)
import Control.Exception (try, SomeException)
import qualified Data.Map.Strict as Map

-- | Entry point for the worldbuilder CLI
runCLI :: IO ()
runCLI = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    hSetEncoding stdin utf8
    args <- getArgs
    case args of
        ("validate" : path : _)  -> validate path
        ("compile" : path : rest) -> compile path rest
        ("check" : path : _)     -> checkStats path
        _                        -> putStrLn usage

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