-- | WASM spike stub (Plan 1.0). NOT production code.
--
--   The real 'SaveLoad' writes save files with System.Directory / Data.Time.
--   GameLoop imports SaveLoad wholesale, but the pure core ('applyLoopCommand')
--   never reaches the IO functions (save/load are handled in 'loopGame').
--   This stub keeps 'GameLoop.hs' byte-identical to the production copy and
--   aborts loudly if anything actually touches the disk layer.
module SaveLoad where

import Types
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map

-- | Stub: saving is a Phase 5.1 concern (browser storage / WASI preopens).
saveGame :: GameState -> String -> IO ()
saveGame _ _ = error "SaveLoad stub: saveGame is not available in the WASM spike"

-- | Stub: loading is a Phase 5.1 concern.
loadGame :: GameState -> String -> IO (Maybe GameState)
loadGame _ _ = error "SaveLoad stub: loadGame is not available in the WASM spike"

-- | Stub: save listing is a Phase 5.1 concern.
listSaves :: GameWorld -> IO ()
listSaves _ = error "SaveLoad stub: listSaves is not available in the WASM spike"

-- | Stub: meta persistence is a Phase 5.1 concern.
saveMeta :: GameWorld -> Map.Map String VariableValue -> IO ()
saveMeta _ _ = error "SaveLoad stub: saveMeta is not available in the WASM spike"

-- | Stub: meta loading is a Phase 5.1 concern.
loadMeta :: GameWorld -> IO (Map.Map String VariableValue)
loadMeta _ = error "SaveLoad stub: loadMeta is not available in the WASM spike"

-- | Ironman checkpoint slot name (pure constant, copied from src/SaveLoad.hs).
ironmanCheckpointSlot :: String
ironmanCheckpointSlot = "checkpoint"

-- | Pure meta merge (copied verbatim from src/SaveLoad.hs — no IO involved).
metaVarPrefix :: String
metaVarPrefix = "meta."

metaVars :: Map.Map String VariableValue -> Map.Map String VariableValue
metaVars = Map.filterWithKey (\k _ -> metaVarPrefix `isPrefixOf` k)

mergeMetaVars :: Map.Map String VariableValue -> Map.Map String VariableValue -> Map.Map String VariableValue
mergeMetaVars incoming base = Map.union (metaVars incoming) base

-- | Stub: deleting an ironman checkpoint is a Phase 5.1 concern.
deleteSaveSlot :: String -> IO ()
deleteSaveSlot _ = error "SaveLoad stub: deleteSaveSlot is not available in the WASM spike"