-- | Worldbuilder CLI: validate, compile, check
module Worldbuilder.CLI (runCLI) where

import Worldbuilder.Types ()
import Worldbuilder.Compile (CompileResult(..), compileAdventure, CompileIssue(..), Severity(..))
import Worldbuilder.ParseFile (parseAdventureFile)
import Worldbuilder.Generate (parseTemplate, validateTemplate, generateDungeon, dtSeed, GenerateError (..))
import Worldbuilder.Rng (deriveRuntimeSeed)
import Worldbuilder.Run (RunConfig (..), runRunner)
import Worldbuilder.Test (runContentTests)
import Worldbuilder.Fuzz (FuzzConfig (..), defaultFuzzConfig, runFuzzer)
import Worldbuilder.Export (collectAssetRefs, bundleFiles, exportBundle, makeZip)

-- JSON encoding (output only)
import Data.Aeson (encode)
import qualified Data.YAML.Aeson (decode1)
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word64)

-- Engine
import Validate (validateWorldWithFlags, validateGameState)
import qualified Types as E

-- System
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeBaseName, takeDirectory)
import System.IO (hSetEncoding, stdout, stderr, stdin, utf8)
import Control.Monad (unless)
import Worldbuilder.Locate (lineForPath)
import Worldbuilder.YamlDoc (YamlDoc, parseYamlDoc, ydResolveIssuePath, ydScalarSpan,
                             setScalarAt, renderYEditError, ssText)
import Data.YAML (Pos (posLine, posColumn))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.ByteString.Lazy.Char8 as BLC
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
        ("validate" : path : _)    -> validate path
        ("compile" : path : rest)  -> compile path rest
        ("export" : path : rest)   -> exportCmd path rest
        ("generate" : path : rest) -> generateCmd path rest
        ("run" : path : rest)      -> runCmd path rest
        ("check" : path : _)       -> checkStats path
        ("yaml-pos" : path : rest) -> yamlPos path rest
        ("yaml-set" : path : rest) -> yamlSet path rest
        ("test" : path : rest)     -> testCmd path rest
        ("fuzz" : path : rest)     -> fuzzCmd path rest
        _                          -> putStrLn usage

-- | B1: run the authored `tests:` content tests of an adventure. An
--   optional name argument (without leading '-') filters the test names.
testCmd :: FilePath -> [String] -> IO ()
testCmd path rest = do
    let mFilter = case rest of
            (f : _) | take 1 f /= "-" -> Just f
            _                         -> Nothing
    failures <- runContentTests path mFilter
    unless (failures == 0) exitFailure

-- | B5: run the content fuzzer against an adventure. Deterministic per seed;
--   every finding is reproducible via --seed/--runs/--steps or --replay.
fuzzCmd :: FilePath -> [String] -> IO ()
fuzzCmd path rest = do
    let defaults = defaultFuzzConfig path
        intFlag k d = maybe d id (lookupFlag k rest >>= readIntArg)
        cfg = defaults
            { fcSeed      = maybe (fcSeed defaults) id (lookupFlag "--seed" rest >>= readMaybeW64)
            , fcRuns      = intFlag "--runs" (fcRuns defaults)
            , fcSteps     = intFlag "--steps" (fcSteps defaults)
            , fcTimeoutMs = intFlag "--timeout-ms" (fcTimeoutMs defaults)
            , fcWindow    = intFlag "--window" (fcWindow defaults)
            , fcReplay    = lookupFlag "--replay" rest
            }
    findings <- runFuzzer cfg
    unless (findings == 0) exitFailure
  where
    readIntArg s = case reads s of
        [(n, "")] | n >= (0 :: Integer) -> Just (fromIntegral n :: Int)
        _                               -> Nothing

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
    , "  worldbuilder yaml-pos <file.yaml> <dotted.path>   Exact line:column of a YAML node"
    , "                                              (W5 Stufe 2; reads the file as a node tree"
    , "                                              with positions, nothing else is touched)"
    , "  worldbuilder yaml-set <file.yaml> <dotted.path> <value>"
    , "                                              Replace one plain scalar in place; comments,"
    , "                                              key order and formatting stay byte-identical."
    , "                                              Refuses quoted/block/multi-line/non-scalars."
    , "  worldbuilder test <adventure.json> [name]   Run the authored content tests (`tests:` section)"
    , ""
    , "  worldbuilder fuzz <adventure.json> [--seed N] [--runs N] [--steps N]"
    , "                                   [--timeout-ms N] [--window N] [--replay <file>]"
    , "                                              B5: seeded fuzz runs (crashes, non-terminating"
    , "                                              steps, frozen loops). Findings print their exact"
    , "                                              command sequence; --replay feeds one command per"
    , "                                              line from a file instead of generated runs."
    , ""
    , "  worldbuilder export <adventure.json> -o <dir> [--with-engine] [--zip] [--force]"
    , "                                              Bundle a finished game: world.json + save.json"
    , "                                              + referenced assets (sfx:/music: + assets:) +"
    , "                                              play.sh/play.bat. --with-engine copies the engine"
    , "                                              into bin/, --zip wraps the bundle in <dir>.zip."
    , "                                              Default -o: dist/<adventure name>."
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

-- | Render a list of compile issues in a readable format.
--
--   The source line comes from the exact YAML position first (W5, Stufe 2: the
--   file is parsed a second time as @Node Pos@, so every dotted path resolves
--   to the real @line:column@) and only falls back to the nesting heuristic of
--   Stufe 1 ('Worldbuilder.Locate.lineForPath') when the path is not a node —
--   which happens for paths that address a *value* inside a list element or a
--   synthetic outcome field (@outcomes.give.to@), and for parse failures.
--
--   Measured over all shipped adventures (53 859 dotted paths): both resolve
--   for 3 634, of which 179 differ — and in every inspected case the exact
--   line is the object's own line while the heuristic pointed at the section
--   header (129) or an unrelated match (50, e.g. @rooms.garten@ in the German
--   fixture landing on a @topics:@ entry 40 lines later). 497 paths resolve
--   only exactly.
printCompileIssues :: FilePath -> [CompileIssue] -> IO ()
printCompileIssues srcFile issues = do
    raw <- readFileBsSafe srcFile
    let content = decodeLenientUtf8 raw
        mdoc = either (const Nothing) Just (parseYamlDoc raw)
    mapM_ (putStrLn . showIssue content mdoc) issues

-- | Read the source file for locating, tolerating encoding issues.
readFileBsSafe :: FilePath -> IO BL.ByteString
readFileBsSafe path = do
    r <- try (BL.readFile path) :: IO (Either SomeException BL.ByteString)
    pure $ case r of
        Left _  -> BL.empty
        Right c -> c

-- | UTF-8 with replacement characters, so a stray byte can never throw here.
decodeLenientUtf8 :: BL.ByteString -> String
decodeLenientUtf8 raw = T.unpack (TE.decodeUtf8With TEE.lenientDecode (BL.toStrict raw))

showIssue :: String -> Maybe YamlDoc -> CompileIssue -> String
showIssue content mdoc i =
    let sev = case ciSeverity i of
            SError  -> "error"
            SWarning -> "warning"
        at = case exactLine of
            Just n  -> " (line " ++ show n ++ ")"
            Nothing -> heuristic
        exactLine = mdoc >>= \d -> fmap posLine (ydResolveIssuePath d (ciPath i))
        heuristic = case lineForPath content (ciPath i) of
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
    "KeywordCollision" ->
        "Give colliding items or NPCs in the same room distinct keywords in 'keys:'."
    "UnknownPlaceholder" ->
        "Declare the variable under 'variables:' or check for a typo in the placeholder name."
    "DarkRoomDeadEnd" ->
        "Add 'tags: [feelable]' to an item, configure a 'light_flag', or provide a reachable 'lightsource'."
    "ProgressionVariableClash" ->
        "Names starting with 'xp.', 'level.' or 'bonus.' belong to the engine — rename the variable."
    "EmptyLevels" ->
        "progression: must define at least one level in 'levels:'."
    "BadLevelXp" ->
        "The first level must have xp: 0."
    "NonMonotonicXp" ->
        "XP thresholds in 'levels:' must be strictly increasing."
    "GainXpWithoutProgression" ->
        "Add a 'progression:' section to define level thresholds and names."
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
                    let worldErrs = validateWorldWithFlags (crWorld cr) (Map.keysSet (E.flags (crSave cr)))
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
                let errors = validateWorldWithFlags (crWorld cr) (Map.keysSet (E.flags (crSave cr))) ++ validateGameState (crWorld cr) (crSave cr)
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

-- | B6: export a finished game as a playable bundle — world.json/save.json
--   (byte-identical to `compile`), the referenced assets (sfx:/music: effects
--   plus the `assets:` manifest) and play.sh/play.bat launchers.
--   `--with-engine` copies the `text-adventure` binary into bin/, `--zip`
--   wraps the bundle in <dir>.zip.
exportCmd :: FilePath -> [String] -> IO ()
exportCmd path rest = do
    let outDir = case rest of
            ("-o" : d : _)       -> d
            ("--output" : d : _) -> d
            _                    -> "dist" </> takeBaseName path
        withEngine = "--with-engine" `elem` rest
        wantZip    = "--zip" `elem` rest
        force      = "--force" `elem` rest
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
                putStrLn $ show (length errs) ++ " hard error(s); bundle not written."
                exitFailure
            Right cr -> do
                unless (null (crWarnings cr)) $ do
                    putStrLn "Compiler warnings:"
                    printCompileIssues path (crWarnings cr)
                    putStrLn ""
                let errors = validateWorldWithFlags (crWorld cr) (Map.keysSet (E.flags (crSave cr))) ++ validateGameState (crWorld cr) (crSave cr)
                if not (null errors) && not force
                then do
                    putStrLn "Validation found issues (use --force to write anyway):"
                    mapM_ (\e -> putStrLn ("  - " ++ showValidationError e)) errors
                    exitFailure
                else do
                    let assets = collectAssetRefs adv
                    warns <- exportBundle (takeDirectory path) outDir (crWorld cr) (crSave cr)
                                assets withEngine
                    mapM_ (\w -> putStrLn ("Warning: " ++ w)) warns
                    putStrLn $ "Wrote bundle " ++ outDir
                        ++ " (" ++ show (length (bundleFiles assets)) ++ " files, "
                        ++ show (length assets) ++ " assets, "
                        ++ show (length warns) ++ " warnings)"
                    if wantZip
                    then makeZip outDir >>= \z -> case z of
                        Left e  -> do
                            putStrLn $ "Zip failed: " ++ e
                            exitFailure
                        Right zPath -> putStrLn $ "Wrote " ++ zPath
                    else pure ()
                    exitSuccess

-- ---------------------------------------------------------------------------
-- Check stats
-- ---------------------------------------------------------------------------

-- | @yaml-pos <file> <dotted.path>@ (W5, Stufe 2): print the exact position of
--   a YAML node — the read half of the position-true source handling, exposed
--   on its own so it can be checked against 'Worldbuilder.Locate' without
--   provoking a compile error.
yamlPos :: FilePath -> [String] -> IO ()
yamlPos path rest = case rest of
    (dotted:_) -> do
        raw <- readFileBsSafe path
        case parseYamlDoc raw of
            Left err -> do
                putStrLn $ "YAML parse failed: " ++ err
                exitFailure
            Right doc -> case ydResolveIssuePath doc dotted of
                Nothing -> do
                    putStrLn $ "no node at '" ++ dotted ++ "'"
                    exitFailure
                Just p ->
                    putStrLn $ show (posLine p) ++ ":" ++ show (posColumn p)
                                ++ "\t" ++ dotted
    [] -> putStrLn "yaml-pos: missing dotted path"
      where _ = path

-- | @yaml-set <file> <dotted.path> <value>@: replace one plain scalar in place.
--   Everything else in the file — comments, key order, quoting, blank lines — is
--   written back byte-for-byte. Refuses anything it cannot do safely (quoted,
--   block, multi-line, non-scalar) with a named reason and a non-zero exit; the
--   file is only written when the edit succeeded.
yamlSet :: FilePath -> [String] -> IO ()
yamlSet path rest = case rest of
    (dotted:newVal:_) -> do
        raw <- readFileBsSafe path
        case parseYamlDoc raw of
            Left err -> do
                putStrLn $ "YAML parse failed: " ++ err
                exitFailure
            Right doc -> case ydScalarSpan doc dotted of
                Left err -> do
                    putStrLn $ "refused: " ++ renderYEditError err
                    exitFailure
                Right sp -> case setScalarAt doc dotted (BLC.pack newVal) of
                    Left err -> do
                        putStrLn $ "refused: " ++ renderYEditError err
                        exitFailure
                    Right out -> do
                        writeResult <- try (BL.writeFile path out) :: IO (Either SomeException ())
                        case writeResult of
                            Left err -> do
                                putStrLn $ "write failed: " ++ show err
                                exitFailure
                            Right () -> do
                                putStrLn $ "set " ++ dotted ++ ": "
                                            ++ BLC.unpack (ssText sp)
                                            ++ " -> " ++ newVal
    _ -> putStrLn "yaml-set: usage: yaml-set <file> <dotted.path> <value>"
      where _ = path

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
                let validationErrors = validateWorldWithFlags gw (Map.keysSet (E.flags (crSave cr))) ++ validateGameState gw (crSave cr)
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
                        let errors = validateWorldWithFlags (crWorld cr) (Map.keysSet (E.flags (crSave cr)))
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
