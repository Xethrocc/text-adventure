-- | The brick-based terminal UI for the text adventure engine (Phase T).
--
--   Architecture (D20/D22): the engine loop runs in a worker thread and is
--   driven exclusively through the engine's 'Frontend' record (Phase V) — the
--   TUI is just another frontend, the loop logic stays untouched. The worker
--   appends output lines to a shared buffer and blocks on a signal MVar for
--   the next input line; the Brick event loop renders the shared buffer and
--   feeds submitted lines back. ANSI colour is stripped for now (the engine's
--   art arrives with SGR sequences; mapping them to vty attributes is later
--   work), and the art panel from D21 arrives with Phase H — until then art
--   travels inline with the room text, as in the plain CLI.
module TextAdventure.Tui
  ( runTui
  , TuiName (..)
  ) where

import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Edit (Editor, applyEdit, editorText, getEditContents,
                           handleEditorEvent, renderEditor)
import Brick.Widgets.Center (hCenter)
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import qualified Data.Text as T
import qualified Data.Text.Zipper as TZ
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, takeMVar,
                                tryPutMVar, withMVar)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (get, put)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (intercalate)

import Ansi (stripAnsi)
import Completion (completionFor)
import Frontend (Frontend (..))
import GameLoop (runGameWithFrontend)
import Types (GameState)

-- | Brick names used by this UI.
data TuiName = HistoryVp    -- ^ vertical viewport over the game history
             | CmdEdit      -- ^ the single-line command editor
             deriving (Eq, Ord, Show)

-- | Custom events from the worker thread to the UI.
data TuiEvent = EvLines      -- ^ the shared history buffer changed
              | EvEnded      -- ^ the game loop finished
              deriving (Eq, Show)

-- | State shared between the worker thread (game loop) and the UI thread.
data TuiShared = TuiShared
    { shLines   :: IORef [String]  -- ^ game text, oldest first
    , shLock    :: MVar ()         -- ^ guards 'shLines'
    , shSignal  :: MVar ()         -- ^ signalled when a line was submitted
    , shPending :: IORef [String]  -- ^ submitted-but-not-yet-consumed lines
    , shState   :: IORef GameState -- ^ the state the next command runs in
    , shHistory :: IORef [String]  -- ^ submitted commands, newest first
    , shHistIdx :: IORef Int       -- ^ how far back the editor currently is
    }

-- | Brick-side state.
data TuiState = TuiState
    { tsEditor  :: Editor T.Text TuiName
    , tsLines   :: [T.Text]        -- ^ snapshot of 'shLines' for drawing
    , tsSuggest :: [String]        -- ^ current completion suggestions
    , tsTitle   :: String
    , tsEnded   :: Bool
    }

-- | Help line shown under the command editor.
helpText :: String
helpText = "Tab: vervollständigen  ·  ↑/↓: Verlauf  ·  PgUp/PgDn: scrollen  ·  Ctrl-Q: beenden"

-- | Append one (possibly multi-line) chunk of game text to the shared buffer.
appendShared :: TuiShared -> String -> IO ()
appendShared shared s = do
    let ls = lines (stripAnsi s)
    withMVar (shLock shared) $ \_ ->
        modifyIORef' (shLines shared) (++ if null ls then [""] else ls)
    pure ()

-- | Pop the next submitted line, waiting on the signal MVar. Never returns
--   end-of-input: quitting is a game command in the TUI.
nextLine :: TuiShared -> IO String
nextLine shared = do
    _ <- takeMVar (shSignal shared)
    xs <- readIORef (shPending shared)
    case xs of
        []       -> nextLine shared
        (x:rest) -> writeIORef (shPending shared) rest >> pure x

-- | The engine 'Frontend' implemented on top of the shared state.
tuiFrontend :: TuiShared -> BChan TuiEvent -> Frontend
tuiFrontend shared chan = Frontend
    { feEmitLine    = \l -> appendShared shared l >> notify
    , feEmitRaw     = \s -> when (not (null s)) (appendShared shared s >> notify)
    , feReadInput   = \st _prompt -> writeIORef (shState shared) st >> Just <$> nextLine shared
    , feReadPlain   = \_prompt -> Just <$> nextLine shared
    , feReadPause   = void (nextLine shared)
    , fePlayFrames  = \frames -> mapM_ (\fr -> appendShared shared fr
                                                  >> threadDelay 350000) frames
    , feDiagnostics = \ms -> mapM_ (\m -> appendShared shared ("[Diagnose] " ++ m)) ms
    }
  where
    notify = writeBChan chan EvLines

-- | Common prefix of two strings ('commonPrefix' is associative here in the
--   way Tab-completion needs: fold over the option list).
commonPrefix :: String -> String -> String
commonPrefix a b = map fst (takeWhile (uncurry (==)) (zip a b))

dropLast :: Int -> String -> String
dropLast n s = take (max 0 (length s - n)) s

-- | Replace the whole editor content, cursor at end.
setEditorText :: TuiState -> String -> TuiState
setEditorText st new =
    st { tsEditor = applyEdit (const (TZ.gotoEOF (TZ.textZipper [T.pack new] (Just 1))))
                              (tsEditor st) }

editorContent :: TuiState -> String
editorContent st = T.unpack (T.concat (getEditContents (tsEditor st)))

-- | Submit the current editor content to the game loop.
handleSubmit :: TuiShared -> TuiState -> EventM TuiName TuiState ()
handleSubmit shared st = do
    let line = editorContent st
    liftIO $ do
        appendShared shared ("> " ++ line)
        when (not (all (`elem` " \t") line)) $ do
            hist <- readIORef (shHistory shared)
            writeIORef (shHistory shared) (line : hist)
            writeIORef (shHistIdx shared) 0
        modifyIORef' (shPending shared) (++ [line])
        void (tryPutMVar (shSignal shared) ())
    put st { tsEditor = applyEdit (const (TZ.textZipper [T.pack ""] (Just 1))) (tsEditor st)
           , tsSuggest = [] }

-- | Tab completion over the pure 'completionFor': one match replaces the word,
--   several matches complete to their common prefix and are listed.
handleTab :: TuiShared -> TuiState -> EventM TuiName TuiState ()
handleTab shared st = do
    gst <- liftIO (readIORef (shState shared))
    let cur = editorContent st
        (word, opts) = completionFor gst cur
    case opts of
        [] -> put st
        _  -> let lcp  = foldr1 commonPrefix opts
                  repl = if length opts == 1 then head opts else lcp
              in if repl == word || null repl
                 then put st { tsSuggest = opts }
                 else put (setEditorText st (dropLast (length word) cur ++ repl))

-- | Recall submitted commands with the arrow keys. 'shHistIdx' counts how far
--   back we are (0 = current line).
handleHistory :: TuiShared -> Int -> TuiState -> EventM TuiName TuiState ()
handleHistory shared delta st = do
    hist <- liftIO (readIORef (shHistory shared))
    idx  <- liftIO (readIORef (shHistIdx shared))
    let idx' = max 0 (min (length hist) (idx + delta))
        entry | idx' == 0 || null hist = ""
              | otherwise              = hist !! (idx' - 1)
    liftIO (writeIORef (shHistIdx shared) idx')
    put (setEditorText st entry)

-- | The Brick application: history viewport on top, command line below.
tuiApp :: TuiShared -> App TuiState TuiEvent TuiName
tuiApp shared = App
    { appDraw         = drawTui
    , appChooseCursor = \_ -> showCursorNamed CmdEdit
    , appHandleEvent  = handleEvent
    , appStartEvent   = pure ()
    , appAttrMap      = const (attrMap V.defAttr [])
    }
  where
    handleEvent ev = do
        st <- get
        case ev of
            AppEvent EvLines -> do
                ls <- liftIO (withMVar (shLock shared) (\_ -> readIORef (shLines shared)))
                vScrollToEnd (viewportScroll HistoryVp)
                put st { tsLines = map T.pack ls }
            AppEvent EvEnded ->
                put st { tsEnded = True }
            VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]) -> halt
            VtyEvent (V.EvKey V.KEsc [])               -> halt
            VtyEvent (V.EvKey V.KEnter [])             -> handleSubmit shared st
            VtyEvent (V.EvKey (V.KChar '\t') [])       -> handleTab shared st
            VtyEvent (V.EvKey V.KUp [])                -> handleHistory shared 1 st
            VtyEvent (V.EvKey V.KDown [])              -> handleHistory shared (-1) st
            VtyEvent (V.EvKey V.KPageUp []) ->
                vScrollPage (viewportScroll HistoryVp) Up
            VtyEvent (V.EvKey V.KPageDown []) ->
                vScrollPage (viewportScroll HistoryVp) Down
            -- Everything else goes to the editor, which mutates itself as the
            -- EventM state (nestEventM' embeds that into the app state).
            VtyEvent e@V.EvKey {} -> do
                ed <- nestEventM' (tsEditor st) (handleEditorEvent (VtyEvent e))
                put st { tsEditor = ed }
            _ -> put st

drawTui :: TuiState -> [Widget TuiName]
drawTui st =
    [ vBox
        [ withBorderStyle unicode $
          borderWithLabel (str (" " ++ tsTitle st ++ " ")) $
            viewport HistoryVp Vertical $
              vBox (map txt (if null (tsLines st) then [T.empty] else tsLines st))
        , padLeftRight 1 $ vBox
            [ suggestionLine
            , hCenter (hBox [ str "> "
                            , vLimit 1 $ viewport CmdEdit Horizontal $
                                renderEditor (txt . T.concat) True (tsEditor st) ])
            , hCenter (str helpLine)
            ]
        ]
    ]
  where
    suggestionLine
        | null (tsSuggest st) = str ""
        | otherwise           = str ("  " ++ intercalate "   " (tsSuggest st))
    helpLine | tsEnded st = "Spiel beendet — Ctrl-Q beendet das TUI."
             | otherwise  = "Tab: vervollständigen   ↑/↓: Verlauf   PgUp/PgDn: scrollen   Ctrl-Q: beenden"

-- | Run the TUI: fork the engine loop against a shared state, then hand the
--   terminal to Brick. Returns when the user leaves the UI (Ctrl-Q/Esc).
runTui :: [String] -> GameState -> IO ()
runTui initialLines st0 = do
    lock    <- newMVar ()
    linesR  <- newIORef initialLines
    signal  <- newEmptyMVar
    pending <- newIORef []
    stateR  <- newIORef st0
    histR   <- newIORef []
    idxR    <- newIORef 0
    let shared = TuiShared
            { shLines = linesR, shLock = lock, shSignal = signal
            , shPending = pending, shState = stateR
            , shHistory = histR, shHistIdx = idxR
            }
    chan <- newBChan 64
    void . forkIO $ do
        runGameWithFrontend (tuiFrontend shared chan) st0
        writeBChan chan EvEnded
    let buildVty = mkVty V.defaultConfig
    initialVty <- buildVty
    let st0' = TuiState { tsEditor = editorText CmdEdit (Just 1) T.empty
                        , tsLines = map T.pack initialLines
                        , tsSuggest = []
                        , tsTitle = "Text Adventure"
                        , tsEnded = False
                        }
    void (customMain initialVty buildVty (Just chan) (tuiApp shared) st0')
