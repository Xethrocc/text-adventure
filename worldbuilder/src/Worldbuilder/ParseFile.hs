-- | File format auto-detection and parsing for the worldbuilder CLI.
--   Supports JSON (.json) and YAML (.yaml/.yml).
--
--   5.3 `include:`: an adventure may pull in library files (procedures,
--   verbs, rules, content — \"Pakete\"). The merge contract is fixed because
--   YAML objects have no order and trigger order is semantically live:
--
--     * a file's own sections come **before** its includes' sections (the
--       including file has priority — its first `block:` stops the library);
--     * includes merge in `include:` list order, depth-first, a file's own
--       sections before that file's includes (repeated for every level);
--     * a file reachable twice (diamond) is loaded once, at its first
--       position; an include cycle is an error;
--     * single-value fields (`name`, `start_room`, `player`, `combat`, …)
--       are reserved for the main adventure file — a library that sets one
--       is a hard error naming the file;
--     * duplicate ids across merged sources are a hard error naming both
--       files (the compiler would catch them later, but without the paths).
module Worldbuilder.ParseFile (parseAdventureFile) where

import Worldbuilder.Types

import Data.Aeson (eitherDecode, Value (..))
import Data.YAML.Aeson (decode1)
import Data.YAML (Pos (..))
import qualified Data.ByteString.Lazy as BL
import Control.Exception (IOException, try)
import System.FilePath (takeExtension, takeDirectory, (</>), normalise)
import Data.Char (toLower)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K

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
--
--   5.3: `include:` libraries are resolved and merged afterwards (see the
--   module header for the merge contract).
parseAdventureFile :: FilePath -> IO (Either String Adventure)
parseAdventureFile path = do
    readResult <- try (BL.readFile path) :: IO (Either IOException BL.ByteString)
    case readResult of
        Left ioErr -> pure (Left ("Cannot read " ++ path ++ ": " ++ show ioErr))
        Right bytes ->
            let parsed = parseBytes path bytes
            in case parsed of
                Left err   -> pure (Left err)
                Right adv
                    | null (advStartRoom adv) ->
                        pure (Left (path ++ ": start_room is missing"
                                    ++ " (the main adventure file must declare one)"))
                    | otherwise -> do
                        embedded <- embedClipFiles path adv
                        case embedded of
                            Left err    -> pure (Left err)
                            Right adv'  -> resolveIncludes path adv'

-- | Parse one file's bytes (JSON or YAML) into an Adventure.
parseBytes :: FilePath -> BL.ByteString -> Either String Adventure
parseBytes path bytes = case map toLower (takeExtension path) of
    ".yaml" -> fromYaml bytes
    ".yml"  -> fromYaml bytes
    _       -> case eitherDecode bytes of
        Right adv -> Right adv
        Left err  -> Left (path ++ ": JSON parse error: " ++ err)
  where
    fromYaml b = case decode1 b of
        Right adv          -> Right adv
        Left (pos, reason) -> Left (path ++ ": YAML parse error at line "
                                    ++ show (posLine pos) ++ ", column "
                                    ++ show (posColumn pos) ++ ": " ++ reason)

-- | Keys a library file may not set — single-value fields belong to the
--   main adventure file. Both accepted spellings are listed where the schema
--   allows them.
includeForbiddenKeys :: [String]
includeForbiddenKeys =
    [ "name", "start_room", "interactions", "player", "environment", "stealth"
    , "patrol", "combat", "game", "deck", "hand_limit", "handLimit"
    , "combine_verb", "journal", "progression", "title_art", "titleArt" ]

-- | 5.3: a library may not set single-value fields (hard error, naming the
--   file and the key).
checkIncludeForbidden :: FilePath -> Adventure -> Either String ()
checkIncludeForbidden path adv = case advRawValue adv of
    Just (Object o) -> case [ k | k <- includeForbiddenKeys
                                , KM.member (K.fromString k) o ] of
        []      -> Right ()
        (k : _) -> Left (path ++ ": '" ++ k ++ "' is reserved for the main"
                         ++ " adventure file (a library may not set it)")
    _ -> Right ()

-- | Load and flatten `include:` sources. Each file's own sections precede its
--   includes' sections; includes merge in list order, depth-first. Ancestral
--   cycles are an error; a diamond (same file via two paths) is loaded once,
--   at its first position.
flattenSources
    :: [FilePath]          -- ^ ancestor chain (innermost first), for cycle detection
    -> Set.Set FilePath    -- ^ already-loaded normalised paths
    -> FilePath            -- ^ this file's path (normalised)
    -> Adventure
    -> IO (Either String ([(FilePath, Adventure)], Set.Set FilePath))
flattenSources ancestors seen path adv = do
    results <- go (advInclude adv) seen []
    pure $ case results of
        Left err             -> Left err
        Right (subs, seen')  -> Right ((path, adv) : subs, seen')
  where
    go [] st acc = pure (Right (concat (reverse acc), st))
    go (rel : rest) st acc = do
        let childPath = normalise (takeDirectory path </> rel)
        if childPath `elem` ancestors
            then pure (Left (path ++ ": include cycle: '" ++ rel ++ "' ("
                             ++ childPath ++ ") is already in the include chain"))
            else if childPath `Set.member` st
                then go rest st acc   -- diamond: already loaded at its first position
                else do
                    parsed <- parseLibraryFile childPath
                    case parsed of
                        Left err -> pure (Left err)
                        Right child -> do
                            r <- flattenSources (path : ancestors)
                                    (Set.insert childPath st) childPath child
                            case r of
                                Left err            -> pure (Left err)
                                Right (sub, st')    -> go rest st' (sub : acc)

-- | Parse a library file: read, parse, forbidden-key check, embed its clips
--   (relative to the library file itself).
parseLibraryFile :: FilePath -> IO (Either String Adventure)
parseLibraryFile path = do
    readResult <- try (BL.readFile path) :: IO (Either IOException BL.ByteString)
    case readResult of
        Left ioErr -> pure (Left ("Cannot read " ++ path ++ ": " ++ show ioErr))
        Right bytes -> case parseBytes path bytes of
            Left err  -> pure (Left err)
            Right adv -> case checkIncludeForbidden path adv of
                Left err   -> pure (Left err)
                Right ()   -> embedClipFiles path adv

-- | 5.3 entry point: resolve `include:` libraries (if any) and merge them
--   into the main adventure.
resolveIncludes :: FilePath -> Adventure -> IO (Either String Adventure)
resolveIncludes mainPath adv
    | null (advInclude adv) = pure (Right adv)
    | otherwise = do
        flattened <- flattenSources [normalise mainPath]
                        (Set.singleton (normalise mainPath)) (normalise mainPath) adv
        pure $ case flattened of
            Left err      -> Left err
            Right (sources, _) -> case checkMergedDuplicates sources of
                Left err  -> Left err
                Right ()  -> Right (mergeSources sources)

-- | Sections whose ids must be unique across merged sources. Content tests
--   are deliberately absent: their names are run labels, nothing references
--   them. `combine:` has no id at all (its fact set is the key).
duplicateSections :: Adventure -> [(String, [String])]
duplicateSections adv =
    [ ("rooms", map arId (advRooms adv))
    , ("items", map aiId (advItems adv))
    , ("npcs", map anId (advNPCs adv))
    , ("quests", map aqId (advQuests adv))
    , ("vehicles", map avId (advVehicles adv))
    , ("verbs", map avbName (advVerbs adv))
    , ("variables", map avbVarName (advVariables adv))
    , ("triggers", map atId (advTriggers adv))
    , ("cards", map acdId (advCards adv))
    , ("sandbox_zones", map aszId (advSandboxZones adv))
    , ("procedures", map apId (advProcedures adv))
    , ("chapters", map achId (advChapters adv))
    , ("facts", map afdId (advFacts adv))
    , ("devices", map adId (advDevices adv))
    , ("clips", map acId (advClips adv))
    , ("abilities", map aabId (advAbilities adv))
    , ("encounter_tables", map ertId (advEncounterTables adv))
    , ("factions", map afId (advFactions adv))
    , ("pursuit", map apeNpc (advPursuit adv))
    , ("initial_flags", Map.keys (advInitialFlags adv))
    , ("initial_variables", Map.keys (advInitialVariables adv))
    , ("end_art", Map.keys (advEndArt adv))
    ]

-- | Hard error for duplicate ids across merged sources, naming every file
--   involved (5.3 decision: the paths must be in the message).
checkMergedDuplicates :: [(FilePath, Adventure)] -> Either String ()
checkMergedDuplicates sources = case dupErrors of
    []     -> Right ()
    (e:_)  -> Left e
  where
    dupErrors =
        [ section ++ ": '" ++ i ++ "' defined in " ++ intercalate " and " ps
        | (section, _) <- sectionGetters
        , (i, ps) <- Map.toList (Map.fromListWith (flip (++))
            [ (i, [p]) | (p, adv) <- sources
                       , i <- maybe [] id (lookup section (duplicateSections adv)) ])
        , length ps > 1 ]
    sectionGetters =
        [ (s, ()) | (s, _) <- duplicateSections (snd (head sources)) ]

-- | Merge flattened sources: the first (main) file's own sections first,
--   then every include's sections in order. Single-value fields always keep
--   the main file's values (libraries may not set them — see
--   'checkIncludeForbidden').
mergeSources :: [(FilePath, Adventure)] -> Adventure
mergeSources [] = error "mergeSources: no sources"
mergeSources ((_, main) : incls) = foldl mergeOne main { advInclude = [] } incls
  where
    mergeOne acc (_, inc) = acc
        { advRooms           = advRooms acc ++ advRooms inc
        , advItems           = advItems acc ++ advItems inc
        , advNPCs            = advNPCs acc ++ advNPCs inc
        , advQuests          = advQuests acc ++ advQuests inc
        , advVehicles        = advVehicles acc ++ advVehicles inc
        , advVerbs           = advVerbs acc ++ advVerbs inc
        , advVariables       = advVariables acc ++ advVariables inc
        , advTriggers        = advTriggers acc ++ advTriggers inc
        , advActiveQuests    = advActiveQuests acc ++ advActiveQuests inc
        , advFactions        = advFactions acc ++ advFactions inc
        , advEncounterTables = advEncounterTables acc ++ advEncounterTables inc
        , advAbilities       = advAbilities acc ++ advAbilities inc
        , advClips           = advClips acc ++ advClips inc
        , advCards           = advCards acc ++ advCards inc
        , advSandboxZones    = advSandboxZones acc ++ advSandboxZones inc
        , advProcedures      = advProcedures acc ++ advProcedures inc
        , advChapters        = advChapters acc ++ advChapters inc
        , advPursuit         = advPursuit acc ++ advPursuit inc
        , advFacts           = advFacts acc ++ advFacts inc
        , advCombines        = advCombines acc ++ advCombines inc
        , advDevices         = advDevices acc ++ advDevices inc
        , advTests           = advTests acc ++ advTests inc
        , advAssets          = advAssets acc ++ advAssets inc
        , advInitialVariables = Map.union (advInitialVariables acc) (advInitialVariables inc)
        , advInitialFlags     = Map.union (advInitialFlags acc) (advInitialFlags inc)
        , advEndArt           = Map.union (advEndArt acc) (advEndArt inc)
        }

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
