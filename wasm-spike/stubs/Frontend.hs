-- | WASM spike stub (Plan 1.0). NOT production code.
--
--   The real 'Frontend' module pulls in haskeline, which is not available on
--   wasm32-wasi. GameLoop only needs the 'Frontend' record, the 'OutputFilter'
--   type and 'haskelineFrontend' for 'runGameWithFrontend' — none of which are
--   on the pure path ('applyLoopCommand'). This stub keeps 'GameLoop.hs'
--   byte-identical to the production copy: the record compiles, and any
--   attempt to actually run the terminal loop aborts loudly.
module Frontend
  ( Frontend (..)
  , OutputFilter
  , haskelineFrontend
  , commandCompletion
  ) where

import Types (GameState)
import Completion (completionFor)
import System.IO (hPutStrLn, stderr)

-- | Mapping applied to every player-facing line at the I/O boundary.
type OutputFilter = String -> String

-- | Same shape as the real record in src/Frontend.hs.
data Frontend = Frontend
    { feEmitLine    :: String -> IO ()
    , feEmitRaw     :: String -> IO ()
    , feReadInput   :: GameState -> String -> IO (Maybe String)
    , feReadPlain   :: GameState -> String -> IO (Maybe String)
    , feReadPause   :: IO ()
    , fePlayFrames  :: Int -> [String] -> IO ()
    , feDiagnostics :: [String] -> IO ()
    , fePlaySfx     :: FilePath -> IO ()
    , feStartMusic  :: FilePath -> IO ()
    , feStopMusic   :: IO ()
    }

-- | Stub: the haskeline terminal cannot exist on wasm. Any code that reaches
--   for it has left the pure core — abort loudly so the spike notices.
haskelineFrontend :: OutputFilter -> Frontend
haskelineFrontend _f = Frontend
    { feEmitLine    = \_ -> unsupported "feEmitLine"
    , feEmitRaw     = \_ -> unsupported "feEmitRaw"
    , feReadInput   = \_ _ -> unsupported "feReadInput"
    , feReadPlain   = \_ _ -> unsupported "feReadPlain"
    , feReadPause   = unsupported "feReadPause"
    , fePlayFrames  = \_ _ -> unsupported "fePlayFrames"
    , feDiagnostics = mapM_ (hPutStrLn stderr)
    , fePlaySfx     = \_ -> unsupported "fePlaySfx"
    , feStartMusic  = \_ -> unsupported "feStartMusic"
    , feStopMusic   = unsupported "feStopMusic"
    }
  where
    unsupported what = error ("Frontend stub: " ++ what ++ " is not available in the WASM spike")

-- | Completion exists in pure form; only its wiring needs a frontend.
commandCompletion :: GameState -> (String, String) -> IO (String, [String])
commandCompletion state (left, _) =
    pure (currentWord, map simpleCompletion' options)
  where
    (currentWord, options) = completionFor state left
    simpleCompletion' = id