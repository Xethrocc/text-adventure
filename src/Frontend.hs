-- | The frontend abstraction at the I/O boundary (Phase V).
--
--   All player-facing policy of the game loop — output lines, input with
--   completion and history, the narrative \"press Enter\" pause, animation
--   playback, and the diagnostics channel — is routed through this record.
--   The loop logic in "GameLoop" contains no terminal access of its own; the
--   Haskeline/stdout terminal is one implementation ('haskelineFrontend'), and
--   a future TUI or web backend provides its own without touching the loop.
module Frontend
  ( Frontend (..)
  , OutputFilter
  , haskelineFrontend
  , commandCompletion
  ) where

import Types (GameState)
import Completion (completionFor)
import Control.Concurrent (threadDelay)
import Control.Monad (void)
import System.Console.Haskeline
import System.IO (hPutStrLn, stderr)

-- | Mapping applied to every player-facing line at the I/O boundary. The pure
--   core never inspects it: @app/Main@ passes 'id' on a colour TTY and
--   'Ansi.stripAnsi' when colour is off or stdout is redirected.
type OutputFilter = String -> String

-- | Everything the game loop needs from the presentation layer.
data Frontend = Frontend
    { feEmitLine    :: String -> IO ()
      -- ^ print one player-facing line (with trailing newline)
    , feEmitRaw     :: String -> IO ()
      -- ^ print without a trailing newline
    , feReadInput   :: GameState -> String -> IO (Maybe String)
      -- ^ read a command line for the given game state (completion and
      --   history are the frontend's business); 'Nothing' means end of input
    , feReadPlain   :: String -> IO (Maybe String)
      -- ^ read a bare line without completion/history (game-over menus)
    , feReadPause   :: IO ()
      -- ^ narrative continuation: wait until the player confirms
    , fePlayFrames  :: Int -> [String] -> IO ()
      -- ^ play animation frames in order, waiting `micros` between them
      --   (the rate comes from the art via the pure core, Phase H/H1)
    , feDiagnostics :: [String] -> IO ()
      -- ^ engine diagnostics channel (stderr today) — never game text
    }

-- | Haskeline/stdout implementation: today's terminal behaviour, byte for
--   byte. Completion is built from the pure 'completionFor'; history is kept
--   by Haskeline (@autoAddHistory@).
haskelineFrontend :: OutputFilter -> Frontend
haskelineFrontend f = Frontend
    { feEmitLine    = putStrLn . f
    , feEmitRaw     = putStr . f
    , feReadInput   = \st p -> runInputT (haskelineSettings st) (getInputLine p)
    , feReadPlain   = \p -> runInputT defaultSettings (getInputLine p)
    , feReadPause   = putStr (f "  [Press Enter to continue]") >> void getLine
    , fePlayFrames  = \micros -> mapM_ (\fr -> putStrLn (f fr) >> threadDelay micros)
    , feDiagnostics = mapM_ (hPutStrLn stderr)
    }

haskelineSettings :: GameState -> Settings IO
haskelineSettings state =
    (defaultSettings :: Settings IO)
        { autoAddHistory = True
        , complete = commandCompletion state
        }

-- | Haskeline completion function built on the pure 'completionFor'.
commandCompletion :: GameState -> CompletionFunc IO
commandCompletion state (left, _) = do
    let (currentWord, options) = completionFor state left
    pure (currentWord, map simpleCompletion options)
