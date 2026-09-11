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
import qualified Data.Map.Strict as Map

-- | Entry point for the worldbuilder CLI
runCLI :: IO ()
runCLI = do
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

-- | Render a list of compile issues in a readable format
showIssue :: CompileIssue -> String
showIssue i =
    let sev = case ciSeverity i of
            SError  -> "error"
            SWarning -> "warning"
    in "  [" ++ sev ++ "] " ++ ciCode i ++ " at " ++ ciPath i ++ ": " ++ ciMessage i

printCompileIssues :: [CompileIssue] -> IO ()
printCompileIssues = mapM_ (putStrLn . showIssue)

-- | Render validation errors (ValidationError derives Show)
showValidationError :: Show a => a -> String
showValidationError = show

-- ---------------------------------------------------------------------------
-- Validate
-- ---------------------------------------------------------------------------

validate :: FilePath -> IO ()
validate path = do
    mbAdv <- parseAdventureFile path
    case mbAdv of
        Nothing -> do
            putStrLn $ "Failed to parse adventure file: " ++ path
            exitFailure
        Just adv -> do
            case compileAdventure adv of
                Left errs -> do
                    putStrLn "Compilation errors:"
                    printCompileIssues errs
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
    mbAdv <- parseAdventureFile path
    case mbAdv of
        Nothing -> do
            putStrLn $ "Failed to parse adventure file: " ++ path
            exitFailure
        Just adv -> case compileAdventure adv of
            Left errs -> do
                putStrLn "Compilation errors:"
                printCompileIssues errs
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
    mbAdv <- parseAdventureFile path
    case mbAdv of
        Nothing -> do
            putStrLn $ "Failed to parse adventure file: " ++ path
            exitFailure
        Just adv -> case compileAdventure adv of
            Left errs -> do
                putStrLn "Compilation errors:"
                printCompileIssues errs
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