-- | Runner for pre-run dungeon generation (Rogue Phase 4c).
--
--   Generates a fresh dungeon run deterministically derived from the template
--   slug and the run counter (@meta.runs@):
--
--   @seed = splitmix64(salt(slug) * GOLDEN1 + meta.runs * GOLDEN2)@
--
--   The run artifacts are placed in @<saves-dir>/<slug>/run_<n>/@, binding the
--   checkpoint and emitted world securely. Older runs can optionally be pruned
--   with @--keep-runs N@.
module Worldbuilder.Run
    ( RunConfig (..)
    , RunResult (..)
    , defaultRunConfig
    , prepareRun
    , runRunner
    , pruneOldRuns
    ) where

import Control.Exception (try, SomeException)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import Data.List (isPrefixOf, sortOn)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist,
                         findExecutable, listDirectory, removeDirectoryRecursive)
import System.Environment (lookupEnv, setEnv)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.Process (callProcess)
import qualified Data.YAML.Aeson as YA

import qualified SaveLoad
import qualified Types as E
import Validate (validateWorld, validateGameState)
import Worldbuilder.Compile (compileAdventure, CompileResult (..), CompileIssue (..), Severity (..))
import Worldbuilder.Generate (DTemplate (..), parseTemplate, validateTemplate, generateDungeon)
import Worldbuilder.Rng (deriveRuntimeSeed)
import Ansi (stripAnsi)
import GameLoop (runGameWith)

-- | Configuration passed to the run generator.
data RunConfig = RunConfig
    { rcTemplatePath :: FilePath
    , rcSeedOverride :: Maybe Word64
    , rcSavesDir     :: Maybe FilePath
    , rcKeepRuns     :: Maybe Int
    , rcNoLaunch     :: Bool
    , rcForce        :: Bool
    , rcTui          :: Bool
    , rcNoColor      :: Bool
    , rcExtraArgs    :: [String]
    } deriving (Show, Eq)

-- | Default configuration for a template path.
defaultRunConfig :: FilePath -> RunConfig
defaultRunConfig path = RunConfig
    { rcTemplatePath = path
    , rcSeedOverride = Nothing
    , rcSavesDir     = Nothing
    , rcKeepRuns     = Nothing
    , rcNoLaunch     = False
    , rcForce        = False
    , rcTui          = False
    , rcNoColor      = False
    , rcExtraArgs    = []
    }

-- | Summary of a prepared run.
data RunResult = RunResult
    { rrSlug       :: String
    , rrRunIndex   :: Int
    , rrSeed       :: Word64
    , rrRunDir     :: FilePath
    , rrWorldPath  :: FilePath
    , rrSavePath   :: FilePath
    , rrWorld      :: E.GameWorld
    , rrSave       :: E.SaveState
    } deriving (Show, Eq)

-- | Parse template, read meta, determine seed, generate, compile, and write run artifacts.
prepareRun :: RunConfig -> IO (Either String RunResult)
prepareRun cfg = do
    let path = rcTemplatePath cfg
    rawBytesResult <- try (BL.readFile path) :: IO (Either SomeException BL.ByteString)
    case rawBytesResult of
        Left ioErr -> pure $ Left $ "Failed to read template file: " ++ show ioErr
        Right bytes -> case YA.decode1 bytes of
            Left (_, err) -> pure $ Left $ "YAML parse error: " ++ err
            Right val     -> case parseTemplate val of
                Left err  -> pure $ Left $ "Template decode error: " ++ err
                Right tmpl -> do
                    let tIssues = validateTemplate tmpl
                        tErrors = filter ((== SError) . ciSeverity) tIssues
                    if not (null tErrors) && not (rcForce cfg)
                        then pure $ Left $ "Template validation failed: "
                                    ++ unwords (map ciCode tErrors)
                        else do
                            let slug = E.slugify (dtName tmpl)
                            baseSaves <- maybe SaveLoad.savesDir pure (rcSavesDir cfg)
                            -- Read existing meta file for the slug (scoped to baseSaves)
                            oldMetaDir <- lookupEnv "TA_META_DIR"
                            setEnv "TA_META_DIR" baseSaves
                            metaVarsMap <- SaveLoad.loadMetaForSlug slug
                            case oldMetaDir of
                                Just d  -> setEnv "TA_META_DIR" d
                                Nothing -> pure ()

                            let runIndex = case Map.lookup "meta.runs" metaVarsMap of
                                    Just (E.VVInt n) -> n + 1
                                    _                -> 1
                                seed = case rcSeedOverride cfg of
                                    Just s  -> s
                                    Nothing -> SaveLoad.deriveRunSeedFromSlug slug runIndex

                            case generateDungeon tmpl seed of
                                Left gErr -> pure $ Left $ "Generation failed: " ++ show gErr
                                Right (adv, _warns) -> case compileAdventure adv of
                                    Left cErrs -> pure $ Left $ "Generated adventure failed to compile: "
                                                            ++ unwords (map ciCode cErrs)
                                    Right cr -> do
                                        let gw = crWorld cr
                                            valIssues = validateWorld gw ++ validateGameState gw (crSave cr)
                                        if not (null valIssues) && not (rcForce cfg)
                                            then pure $ Left $ "Validation found issues: "
                                                                ++ unwords (map show valIssues)
                                            else do
                                                let slugDir = baseSaves </> slug
                                                    runDir = slugDir </> ("run_" ++ show runIndex)
                                                    worldFile = runDir </> "world.json"
                                                    saveFile = runDir </> "save.json"
                                                    gwWithMeta = gw
                                                        { E.varDefs = if Map.member "meta.runs" (E.varDefs gw)
                                                                      then E.varDefs gw
                                                                      else Map.insert "meta.runs" (E.VarDef "meta.runs" (E.VTInt Nothing Nothing) (E.VVInt 0)) (E.varDefs gw)
                                                        }
                                                    saveInit = (crSave cr)
                                                        { E.rngState = deriveRuntimeSeed seed
                                                        , E.variables = Map.insert "meta.runs" (E.VVInt (fromIntegral (runIndex - 1))) (E.variables (crSave cr))
                                                        }
                                                createDirectoryIfMissing True runDir
                                                BL.writeFile worldFile (encode gwWithMeta)
                                                BL.writeFile saveFile (encode saveInit)
                                                -- Prune older runs if configured
                                                case rcKeepRuns cfg of
                                                    Just k  -> do
                                                        _ <- pruneOldRuns slugDir k
                                                        pure ()
                                                    Nothing -> pure ()
                                                pure $ Right $ RunResult
                                                    { rrSlug      = slug
                                                    , rrRunIndex  = runIndex
                                                    , rrSeed      = seed
                                                    , rrRunDir    = runDir
                                                    , rrWorldPath = worldFile
                                                    , rrSavePath  = saveFile
                                                    , rrWorld     = gwWithMeta
                                                    , rrSave      = saveInit
                                                    }

-- | Prune old run directories in @slugDir@, keeping only the @keepCount@ newest runs.
pruneOldRuns :: FilePath -> Int -> IO [FilePath]
pruneOldRuns slugDir keepCount
    | keepCount <= 0 = pure []
    | otherwise = do
        exists <- doesDirectoryExist slugDir
        if not exists then pure [] else do
            entries <- listDirectory slugDir
            let runDirs = [ (n, slugDir </> e)
                          | e <- entries
                          , "run_" `isPrefixOf` e
                          , Just n <- [readMaybeInt (drop 4 e)]
                          ]
                sorted = sortOn fst runDirs
                toDelete = if length sorted > keepCount
                           then take (length sorted - keepCount) sorted
                           else []
            mapM_ (\(_, d) -> try (removeDirectoryRecursive d) :: IO (Either SomeException ())) toDelete
            pure (map snd toDelete)
  where
    readMaybeInt s = case reads s of
        [(n, "")] | n >= (0 :: Integer) -> Just (fromIntegral n :: Int)
        _                               -> Nothing

-- | Execute the run command from CLI options: prepare the run, then launch
--   the engine unless @--no-launch@ is given.
runRunner :: RunConfig -> IO ()
runRunner cfg = do
    prepResult <- prepareRun cfg
    case prepResult of
        Left err -> do
            putStrLn $ "Error preparing run: " ++ err
            exitFailure
        Right res -> do
            putStrLn $ "=== Prepared Run " ++ show (rrRunIndex res) ++ " for " ++ rrSlug res ++ " ==="
            putStrLn $ "Run directory: " ++ rrRunDir res
            putStrLn $ "Seed:          " ++ show (rrSeed res)
            putStrLn $ "Rooms:         " ++ show (length (Map.keys (E.rooms (rrWorld res))))
            if rcNoLaunch cfg
                then exitSuccess
                else do
                    baseSaves <- maybe SaveLoad.savesDir pure (rcSavesDir cfg)
                    setEnv "TA_META_DIR" baseSaves
                    setEnv "TA_SAVES_DIR" (rrRunDir res)
                    putStrLn $ "Launching engine..."
                    mExe <- findExecutable "text-adventure"
                    case mExe of
                        Just exePath -> do
                            let args = [ "--world", rrWorldPath res
                                       , "--save", rrSavePath res
                                       , "--saves-dir", rrRunDir res
                                       ] ++ (if rcTui cfg then ["--tui"] else [])
                                         ++ (if rcNoColor cfg then ["--no-color"] else [])
                                         ++ rcExtraArgs cfg
                            callProcess exePath args
                        Nothing -> do
                            let outFilter = if rcNoColor cfg then stripAnsi else id
                                st = E.GameState (rrWorld res) (rrSave res) Nothing Nothing Nothing [] []
                            runGameWith outFilter st
