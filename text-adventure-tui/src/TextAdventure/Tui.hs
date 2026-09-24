-- | The brick-based terminal UI for the text adventure engine (Phase T).
--
--   Architecture (D20/D22): the engine loop runs in a worker thread and is
--   driven exclusively through the engine's 'Frontend' record (Phase V) — the
--   TUI is just another frontend, the loop logic stays untouched. The worker
--   appends output lines to a shared buffer and blocks on a signal MVar for
--   the next input line; the Brick event loop renders the shared buffer and
--   feeds submitted lines back. ANSI colour is kept: SGR sequences in engine
--   output (hotspot highlights, half-block art) are parsed into attribute
--   segments and rendered via vty attributes — see TextAdventure.Tui.Color.
--
--   Art panel (Phase H/H4b, D21): animated art plays in its own panel —
--   cutscenes ('fePlayFrames') in-place, once, at the clip's own rate, then
--   transitioning into the current room's ambient loop; rooms with an
--   'ambient' block loop there while they are current. A panel exists only
--   when it has something to show — a game without art is indistinguishable
--   from a plain text game, and empty frames draw no box. The engine loop
--   only ever blocks inside 'fePlayFrames' (cutscenes are allowed to block,
--   D11); the ambient loop ticks entirely inside the UI.
module TextAdventure.Tui
  ( runTui
  , TuiName (..)
  , PanelState (..)
  , PanelStep (..)
  , roomAmbient
  , panelFrame
  , advancePanel
  , drawTui          -- ^ exported for offscreen render tests (Rogue Phase 5)
  , TuiState (..)    -- ^ exported so tests can build a state offscreen
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
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar,
                                tryPutMVar, tryTakeMVar, withMVar)
import Control.Monad (forever, guard, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isLower, isSpace)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (intercalate, nub)
import Data.Maybe (fromMaybe)
import qualified Brick.Types as BT

import Ansi (stripAnsi)
import TextAdventure.Tui.Color (attrOfSgr, colorAttrName, parseSgrLine)
import TextAdventure.Tui.Hud (HudView (..), MapCell (..), MapGrid (..), buildHud,
                              buildHudWithFloor, statsLines)
import Completion (completionFor)
import Frontend (Frontend (..))
import GameLoop (runGameWithFrontend)
import Game (lookupRoom)
import Types

-- | Brick names used by this UI.
data TuiName = HistoryVp    -- ^ vertical viewport over the game history
             | CmdEdit      -- ^ the single-line command editor
             deriving (Eq, Ord, Show)

-- | Custom events from the worker/ticker threads to the UI.
data TuiEvent = EvLines      -- ^ the shared history buffer changed
              | EvEnded      -- ^ the game loop finished
              | EvArt        -- ^ the art panel's content changed (redraw it)
              | EvTick       -- ^ advance the art panel by one frame
              | EvHud        -- ^ the game state moved — rebuild the HUD snapshot
              deriving (Eq, Show)

-- | What the art panel is currently doing (Phase H/H4b).
--
--   * 'PanelNone': no art to show — the panel is not drawn (D21).
--   * 'PanelCutscene': a clip/watch playback, once, blocking the loop until
--     the last frame was shown (then the UI signals 'panelDone' and the
--     frontend transitions).
--   * 'PanelAmbient': the current room's ambient loop, ticking forever while
--     the room is current. Persists across ordinary output (panel-isolated:
--     no scrollback cost, so D17's sticky/non-sticky distinction does not
--     apply the same way in the TUI).
data PanelState
    = PanelNone
    | PanelCutscene [String] Int Int (MVar ())       -- ^ frames, µs rate, index, done
    | PanelAmbient  [String] Int Int String          -- ^ frames, µs rate, index, label

-- | The pure outcome of one panel tick (the UI turns it into IO effects).
data PanelStep
    = PanelHold                          -- ^ nothing to advance
    | PanelAdvanced PanelState           -- ^ show this as the new panel state
    | PanelCutsceneDone (MVar ())        -- ^ last cutscene frame shown: signal the loop

-- | The frame a panel currently shows, or 'Nothing' when it must not be
--   drawn at all (D21: no panel, no box for blank art).
panelFrame :: PanelState -> Maybe String
panelFrame p = case p of
    PanelNone -> Nothing
    PanelCutscene frames _ idx _ -> keep (atIdx frames idx)
    PanelAmbient  frames _ idx _ -> keep (atIdx frames idx)
  where
    atIdx fs i | null fs   = ""
               | otherwise = fs !! min (length fs - 1) (max 0 i)
    keep f = if all isSpace (stripAnsi f) then Nothing else Just f

-- | One tick of the panel: advance a frame, or finish a cutscene. Pure; the
--   UI applies the effects (update shared panel, signal 'panelDone').
advancePanel :: PanelState -> PanelStep
advancePanel p = case p of
    PanelNone -> PanelHold
    PanelCutscene frames micros idx done
        | idx + 1 >= length frames -> PanelCutsceneDone done
        | otherwise                -> PanelAdvanced (PanelCutscene frames micros (idx + 1) done)
    PanelAmbient frames micros idx label
        | null frames              -> PanelHold
        | otherwise                -> PanelAdvanced (PanelAmbient frames micros
                                                    ((idx + 1) `mod` length frames) label)

-- | The current room's ambient loop: frames, µs rate, room name (for the
--   panel label). 'Nothing' when the room has no usable ambient.
roomAmbient :: GameState -> Maybe ([String], Int, String)
roomAmbient st = do
    r <- lookupRoom (currentRoom (save st)) st
    amb <- aaAmbient (roomAscii r)
    guard (not (null (ambFrames amb)) && ambFps amb > 0)
    pure (ambFrames amb, 1000000 `div` ambFps amb, roomName r)

-- | State shared between the worker thread (game loop) and the UI thread.
data TuiShared = TuiShared
    { shLines   :: IORef [String]  -- ^ game text, oldest first
    , shLock    :: MVar ()         -- ^ guards 'shLines'
    , shSignal  :: MVar ()         -- ^ signalled when a line was submitted
    , shPending :: IORef [String]  -- ^ submitted-but-not-yet-consumed lines
    , shState   :: IORef GameState -- ^ the state the next command runs in
    , shHistory :: IORef [String]  -- ^ submitted commands, newest first
    , shHistIdx :: IORef Int       -- ^ how far back the editor currently is
    , shArt     :: MVar PanelState -- ^ what the art panel shows
    }

-- | Brick-side state.
data TuiState = TuiState
    { tsEditor  :: Editor T.Text TuiName
    , tsLines   :: [T.Text]        -- ^ snapshot of 'shLines' for drawing
    , tsSuggest :: [String]        -- ^ current completion suggestions
    , tsTitle   :: String
    , tsEnded   :: Bool
    , tsPanel   :: PanelState      -- ^ snapshot of 'shArt' for drawing
    , tsHud     :: HudView         -- ^ snapshot of the HUD model (Rogue Phase 5)
    , tsFloor   :: Maybe Int       -- ^ currently viewed floor override (Nothing = follow player)
    }

-- | Append one (possibly multi-line) chunk of game text to the shared buffer.
appendShared :: TuiShared -> String -> IO ()
appendShared shared s = do
    let ls = lines s
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

-- | Art panel plumbing: read (non-destructively) and replace.
readArtPanel :: TuiShared -> IO PanelState
readArtPanel shared = withMVar (shArt shared) pure

setArtPanel :: TuiShared -> BChan TuiEvent -> PanelState -> IO ()
setArtPanel shared chan p = do
    void (tryTakeMVar (shArt shared))
    putMVar (shArt shared) p
    writeBChan chan EvArt

-- | The engine 'Frontend' implemented on top of the shared state.
tuiFrontend :: TuiShared -> BChan TuiEvent -> Frontend
tuiFrontend shared chan = Frontend
    { feEmitLine    = \l -> appendShared shared l >> notify
    , feEmitRaw     = \s -> when (not (null s)) (appendShared shared s >> notify)
    , feReadInput   = \st _prompt -> do
        writeIORef (shState shared) st
        writeBChan chan EvHud
        syncAmbientPanel st
        Just <$> nextLine shared
    , feReadPlain   = \st _prompt -> do
        writeIORef (shState shared) st
        writeBChan chan EvHud
        syncAmbientPanel st
        Just <$> nextLine shared
    , feReadPause   = void (nextLine shared)
    , fePlayFrames  = \micros frames -> do
        -- H4b: in-place playback. Set the panel, let the ticker advance it,
        -- and block until the UI signals the last frame (cutscenes may block,
        -- D11). Then transition into the current room's ambient loop — the
        -- Kameraschwenk that stops on the strand and keeps waving — or clear.
        done <- newEmptyMVar
        setArtPanel shared chan (PanelCutscene frames micros 0 done)
        takeMVar done
        st <- readIORef (shState shared)
        case roomAmbient st of
            Just (af, am, nm) -> setArtPanel shared chan (PanelAmbient af am 0 nm)
            Nothing           -> setArtPanel shared chan PanelNone
    , feDiagnostics = \ms -> mapM_ (\m -> appendShared shared ("[Diagnose] " ++ m)) ms
    }
  where
    notify = writeBChan chan EvLines
    -- Keep the panel in sync with the current room (ambient per room, none
    -- elsewhere) — but never disturb a running playback (cutscene/watch).
    syncAmbientPanel st = do
        now <- readArtPanel shared
        let keepNow = case now of
                PanelCutscene _ _ _ _ -> True
                PanelAmbient _ _ _ nm ->
                    fmap (\(_, _, nm') -> nm') (roomAmbient st) == Just nm
                PanelNone -> roomAmbient st == Nothing
        unless keepNow $
            setArtPanel shared chan $ case roomAmbient st of
                Just (af, am, nm) -> PanelAmbient af am 0 nm
                Nothing           -> PanelNone

-- | The ticker thread: wakes the UI at the panel's current rate.
panelTicker :: TuiShared -> BChan TuiEvent -> IO ()
panelTicker shared chan = forever $ do
    p <- readArtPanel shared
    case p of
        PanelCutscene _ micros _ _ -> threadDelay micros >> writeBChan chan EvTick
        PanelAmbient  _ micros _ _ -> threadDelay micros >> writeBChan chan EvTick
        PanelNone                  -> threadDelay 50000

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

-- | Heuristic for lines that must not rewrap on a narrow terminal. ASCII art
--   is structurally aligned; wrapping it would scramble it. Two cases:
--   block/box-drawing characters (img2ascii half-block mode, Unicode art),
--   and lines without a single lowercase letter that are long enough to
--   matter (the text2ascii banner fonts render in ASCII @#@, and the img2ascii
--   ramps use only @ .:-=+*#%@ — neither contains lowercase; normal prose
--   essentially never goes eight-plus characters without one). This is a
--   stopgap: with the D21 art panel (Phase H) art leaves the text stream
--   entirely, and the B5 reflow non-goal (art reflow at runtime) stays closed.
isArtLine :: String -> Bool
isArtLine s = any isArtChar s || (length s > 8 && not (any isLower s))
  where
    isArtChar c = (c >= '\x2500' && c <= '\x259F')   -- box drawing + block elements

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

-- | The colour attribute map (Phase T rest post, colour): built from the
--   SGR states that actually occur in the current lines and panel. Brick
--   resolves widget colours through 'AttrName's in this finite map; the
--   encoder in TextAdventure.Tui.Color is injective, so every state the
--   renderer can produce finds its entry. Rebuilt per render — the state is
--   small and parsing is cheap.
colorAttrMap :: TuiState -> AttrMap
colorAttrMap st = attrMap V.defAttr
    [ (colorAttrName sgr, attrOfSgr sgr) | sgr <- nub used ]
  where
    used = lineStates ++ panelStates
    lineStates = [ st' | l <- tsLines st
                       , (seg, st') <- parseSgrLine (T.unpack l)
                       , not (null seg) ]
    panelStates = case panelFrame (tsPanel st) of
        Nothing   -> []
        Just frame -> [ st' | l <- lines frame
                            , (seg, st') <- parseSgrLine l
                            , not (null seg) ]

-- | The Brick application: history viewport on top, command line below.
tuiApp :: TuiShared -> BChan TuiEvent -> App TuiState TuiEvent TuiName
tuiApp shared chan = App
    { appDraw         = drawTui
    , appChooseCursor = \_ -> showCursorNamed CmdEdit
    , appHandleEvent  = handleEvent
    , appStartEvent   = pure ()
    , appAttrMap      = colorAttrMap
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
            AppEvent EvArt -> do
                p <- liftIO (readArtPanel shared)
                put st { tsPanel = p }
            AppEvent EvHud -> do
                gst <- liftIO (readIORef (shState shared))
                put st { tsHud = buildHudWithFloor (tsFloor st) gst }
            AppEvent EvTick -> do
                p <- liftIO (readArtPanel shared)
                liftIO $ case advancePanel p of
                    PanelHold -> pure ()
                    PanelAdvanced p' -> setArtPanel shared chan p'
                    PanelCutsceneDone done -> void (tryPutMVar done ())
                p2 <- liftIO (readArtPanel shared)
                put st { tsPanel = p2 }
            VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]) -> halt
            VtyEvent (V.EvKey V.KEsc [])               -> halt
            VtyEvent (V.EvKey V.KEnter [])             -> do
                handleSubmit shared (st { tsFloor = Nothing })
            VtyEvent (V.EvKey (V.KChar '\t') [])       -> handleTab shared st
            VtyEvent (V.EvKey V.KUp [])                -> handleHistory shared 1 st
            VtyEvent (V.EvKey V.KDown [])              -> handleHistory shared (-1) st
            VtyEvent (V.EvKey V.KPageUp []) ->
                vScrollPage (viewportScroll HistoryVp) BT.Up
            VtyEvent (V.EvKey V.KPageDown []) ->
                vScrollPage (viewportScroll HistoryVp) BT.Down
            VtyEvent (V.EvKey (V.KFun 2) [])           -> handleCycleFloor shared st
            VtyEvent (V.EvKey (V.KChar 'f') [V.MCtrl]) -> handleCycleFloor shared st
            -- Everything else goes to the editor, which mutates itself as the
            -- EventM state (nestEventM' embeds that into the app state).
            VtyEvent e@V.EvKey {} -> do
                ed <- nestEventM' (tsEditor st) (handleEditorEvent (VtyEvent e))
                put st { tsEditor = ed }
            _ -> put st

-- | Cycle through explored floors on the minimap (F2 / Ctrl-F).
handleCycleFloor :: TuiShared -> TuiState -> EventM TuiName TuiState ()
handleCycleFloor shared st = do
    gst <- liftIO (readIORef (shState shared))
    let floors = hvFloors (tsHud st)
    case floors of
        [] -> put st
        [_] -> put st
        fs ->
            let currentFl = fromMaybe (head fs) (hvFloor (tsHud st))
                nextFl = case dropWhile (/= currentFl) fs of
                    (_:next:_) -> next
                    _          -> head fs
            in put st { tsFloor = Just nextFl
                      , tsHud = buildHudWithFloor (Just nextFl) gst }

-- | The multi-panel layout (Rogue Phase 5): narrative history plus command
--   line at the bottom (the classic view), HUD panels above. Side by side:
--   the map (left) and character/stats + conditional combat panel (right).
--   Panels vanish when empty (D21 principle): a game without combat never
--   shows a combat box, a screen without exploration skips the map.
drawTui :: TuiState -> [Widget TuiName]
drawTui st =
    [ vBox $
        [ hudWidgets ]
        ++ [ withBorderStyle unicode $
             borderWithLabel (str (" " ++ tsTitle st ++ " ")) $
               viewport HistoryVp Vertical $
                 vBox (map renderLine (if null (tsLines st) then [T.empty] else tsLines st))
           ]
        ++ panelWidgets
        ++ [ padLeftRight 1 $ vBox
               [ suggestionLine
               , hCenter (hBox [ str "> "
                               , renderEditor (txt . T.concat) True (tsEditor st) ])
               , hCenter (str helpLine)
               ]
           ]
    ]
  where
    -- ------------------------------------------------------------------
    -- HUD panels (Rogue Phase 5): map left, stats+combat right; both
    -- vanish when they have nothing to show (D21 principle).
    -- ------------------------------------------------------------------
    hudWidgets
        | null sidePanels = str ""                       -- no HUD, zero height
        | otherwise = padBottom (Pad 1) $ hBox sidePanels
    mapWidget = case hvMap (tsHud st) of
        Nothing -> []
        Just g  ->
            let isHere = any mcHere (mgCells g)
                label = case hvFloor (tsHud st) of
                    Just fl -> " [Karte: Ebene " ++ show fl ++ (if isHere then " (hier)" else "") ++ "] "
                    Nothing -> " [Karte] "
            in [ withBorderStyle unicode $
                 borderWithLabel (str label) $
                   vBox (map str (mgFog g)) ]
    statsWidget = [ withBorderStyle unicode $
                    borderWithLabel (str (" [Status] " ++ hudRoom ++ " ")) $
                      vBox (map str (statsLines (tsHud st))) ]
    combatWidget = case hvCombat (tsHud st) of
        []   -> []
        cls  -> [ withBorderStyle unicode $
                  borderWithLabel (str " [Kampf] ") $
                    vBox (map str cls) ]
    -- map left, stats + conditional combat box stacked right
    sidePanels = mapWidget ++ [ vBox (statsWidget ++ combatWidget) ]
    hudRoom = hvRoom (tsHud st)

    -- D21: the panel exists only when it has something to show; a blank
    -- frame draws no box. Art is rendered with `txt` — never rewrapped.
    panelWidgets = case panelFrame (tsPanel st) of
        Nothing -> []
        Just frame -> case tsPanel st of
            PanelAmbient _ _ _ label -> [artBox label frame]
            PanelCutscene _ _ _ _    -> [artBox "Szene" frame]
            PanelNone                -> []
    artBox label frame =
        withBorderStyle unicode $
        borderWithLabel (str (" " ++ label ++ " ")) $
            vBox (map frameLine (lines frame))
      where
        -- per frame line: attribute segments (colour art), blank rows as spaces
        frameLine l
            | all isSpace (stripAnsi l) = str " "
            | otherwise                 = styledLine l
    -- Prose reflows to the available width; art lines and empty lines keep
    -- their exact shape (an empty txt would collapse to zero height). Lines
    -- carrying SGR render as attribute segments instead: hotspot highlights
    -- and coloured art depend on them, and rewrapping or stripping would
    -- damage layout and information alike. SGR occupies no columns, so the
    -- segment text is what is measured — the alignment survives.
    renderLine t
        | T.null strippedT           = str " "
        | isArtLine strippedS        = styledLine rawText
        | '\ESC' `elem` rawText     = styledLine rawText
        | otherwise                  = txtWrap t
      where
        rawText = T.unpack t
        strippedS = stripAnsi rawText
        strippedT = T.pack strippedS
    styledLine l = hBox
        [ withAttr (colorAttrName st') (txt (T.pack seg))
        | (seg, st') <- parseSgrLine l
        , not (null seg) ]
    suggestionLine
        | null (tsSuggest st) = str ""
        | otherwise           = str ("  " ++ intercalate "   " (tsSuggest st))
    helpLine | tsEnded st = "Spiel beendet — Ctrl-Q beendet das TUI."
             | length (hvFloors (tsHud st)) > 1
                          = "Tab: vervollständigen   ↑/↓: Verlauf   F2: Ebene   Ctrl-Q: beenden"
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
    artR    <- newMVar PanelNone
    let shared = TuiShared
            { shLines = linesR, shLock = lock, shSignal = signal
            , shPending = pending, shState = stateR
            , shHistory = histR, shHistIdx = idxR, shArt = artR
            }
    chan <- newBChan 64
    void . forkIO $ panelTicker shared chan
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
                        , tsPanel = PanelNone
                        , tsHud = buildHud st0
                        , tsFloor = Nothing
                        }
    void (customMain initialVty buildVty (Just chan) (tuiApp shared chan) st0')
