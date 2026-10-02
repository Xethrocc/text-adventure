{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Structured output events (Phase 1.2).
--
--   The engine's pure core produces an event stream instead of a flat text:
--   catalog messages keep their key and arguments ('EvMessage'), prose is
--   ANSI-free and carries optional style spans ('EvText'/'StyledText'), art
--   blocks travel as structured payloads with hotspot anchors ('EvArt'), and
--   audio/animation/state-change side information travels as its own events.
--   CLI/TUI render events back to today's text byte for byte
--   ('renderEvents'); the WebUI (Phase 3) and the protocol (Phase 1.4)
--   consume the structure directly.
--
--   Styling decision (Phase 1.2, documented in docs/output-events.md):
--   * prose is ANSI-free; colour arrives as spans, the CLI renders them to
--     ANSI at the boundary (replacing string-embedded codes long-term),
--   * art blocks are raw monospace strings — authored art legitimately
--     contains ANSI (img2ascii output, hotspot markers); it is quarantined
--     in 'EvArt' and accompanied by structured hotspot data so consumers do
--     not have to parse it.
--
--   Leaf module on purpose: Types.Core imports it ('CommandResultEv'), so it
--   must not import anything from Types.
module Types.Output
    ( -- * Styling model
      Color (..)
    , Style (..)
    , plainStyle
    , Span (..)
    , StyledText (..)
    , styledText
    , styleToAnsi
    , renderStyled
      -- * Payloads
    , MsgPayload (..)
    , ArtHotspot (..)
    , ArtPayload (..)
      -- * Events
    , OutputEvent (..)
      -- * Fragment algebra (byte-identical text assembly)
    , evTextOf
    , renderEvents
    , evText
    , evRaw
    , nl
    , nl2
    , joinEv
    , joinAllEv
    , evIntercalate
    , unlinesEv
    ) where

import Control.Applicative ((<|>))
import qualified Data.Foldable as F
import Data.List (intersperse, sortOn)
import qualified Data.Text as T
import Data.Aeson
    ( ToJSON (..)
    , FromJSON (..)
    , Value (..)
    , object
    , (.=)
    , (.:)
    , (.:?)
    , (.!=)
    , withObject
    , withText
    )
import GHC.Generics (Generic)

-- ---------------------------------------------------------------------------
-- Styling model
-- ---------------------------------------------------------------------------

-- | Named colours of the span model. 'CDefault' renders no colour.
data Color
    = CDefault | CBlack | CRed | CGreen | CYellow
    | CBlue | CMagenta | CCyan | CWhite
    deriving (Show, Eq, Generic)

instance ToJSON Color where
    toJSON c = case c of
        CDefault -> "default"
        CBlack   -> "black"
        CRed     -> "red"
        CGreen   -> "green"
        CYellow  -> "yellow"
        CBlue    -> "blue"
        CMagenta -> "magenta"
        CCyan    -> "cyan"
        CWhite   -> "white"

instance FromJSON Color where
    parseJSON = withText "Color" $ \t -> case T.toLower t of
        "default" -> pure CDefault
        "black"   -> pure CBlack
        "red"     -> pure CRed
        "green"   -> pure CGreen
        "yellow"  -> pure CYellow
        "blue"    -> pure CBlue
        "magenta" -> pure CMagenta
        "cyan"    -> pure CCyan
        "white"   -> pure CWhite
        other     -> fail ("Unknown color: " ++ T.unpack other)

-- | Character style of a span. Absent fields mean "unchanged".
data Style = Style
    { stColor     :: Maybe Color
    , stBold      :: Bool
    , stDim       :: Bool
    , stItalic    :: Bool
    , stUnderline :: Bool
    } deriving (Show, Eq, Generic)

instance ToJSON Style where
    toJSON st = object
        [ "color"     .= stColor st
        , "bold"      .= stBold st
        , "dim"       .= stDim st
        , "italic"    .= stItalic st
        , "underline" .= stUnderline st
        ]

instance FromJSON Style where
    parseJSON = withObject "Style" $ \o -> Style
        <$> o .:? "color"
        <*> o .:? "bold" .!= False
        <*> o .:? "dim" .!= False
        <*> o .:? "italic" .!= False
        <*> o .:? "underline" .!= False

-- | The neutral style: no colour, no attributes.
plainStyle :: Style
plainStyle = Style Nothing False False False False

-- | A styled region of a 'StyledText': start index and length in @Char@s of
--   the plain text, plus the style to apply.
data Span = Span
    { spStart  :: Int
    , spLength :: Int
    , spStyle  :: Style
    } deriving (Show, Eq, Generic)

instance ToJSON Span where
    toJSON sp = object
        [ "start"  .= spStart sp
        , "length" .= spLength sp
        , "style"  .= spStyle sp
        ]

instance FromJSON Span where
    parseJSON = withObject "Span" $ \o -> Span
        <$> o .: "start"
        <*> o .: "length"
        <*> o .: "style"

-- | Prose text with optional style spans. Invariant: spans are
--   non-overlapping and refer to indices within 'stText'.
data StyledText = StyledText
    { stText  :: String
    , stSpans :: [Span]
    } deriving (Show, Eq, Generic)

instance ToJSON StyledText where
    toJSON st = object
        [ "text"  .= stText st
        , "spans" .= stSpans st
        ]

instance FromJSON StyledText where
    parseJSON = withObject "StyledText" $ \o -> StyledText
        <$> o .: "text"
        <*> o .:? "spans" .!= []

-- | Plain text without styling.
styledText :: String -> StyledText
styledText s = StyledText s []

-- | SGR foreground code of a colour (3x base band).
colorToAnsi :: Color -> String
colorToAnsi c = case c of
    CDefault -> "39"
    CBlack   -> "30"
    CRed     -> "31"
    CGreen   -> "32"
    CYellow  -> "33"
    CBlue    -> "34"
    CMagenta -> "35"
    CCyan    -> "36"
    CWhite   -> "37"

-- | Render a style as an SGR sequence; 'plainStyle' renders as @""@ so
--   unstyled text stays byte-identical to the unstyled string.
styleToAnsi :: Style -> String
styleToAnsi st
    | st == plainStyle = ""
    | otherwise =
        let attrs = [ colorToAnsi (maybe CDefault id (stColor st)) ]
                     ++ ["1" | stBold st] ++ ["2" | stDim st]
                     ++ ["3" | stItalic st] ++ ["4" | stUnderline st]
        in "\ESC[" ++ concat (intersperse ";" attrs) ++ "m"

-- | Render styled text: spans become @SGR-on … SGR-off@ segments. Text with
--   no spans renders as the plain string (byte-identical).
renderStyled :: StyledText -> String
renderStyled st = case stSpans st of
    []   -> stText st
    spans -> go 0 (sortOn spStart spans) (stText st)
  where
    go _ [] rest = rest
    go pos (Span start len style : rest) s =
        let (before, at) = splitAt (start - pos) s
            (inside, after) = splitAt len at
        in before ++ styleToAnsi style ++ inside ++ "\ESC[0m"
           ++ go (start + len) rest after

-- ---------------------------------------------------------------------------
-- Payloads
-- ---------------------------------------------------------------------------

-- | A catalog message: stable key, the arguments it was rendered with, and
--   the rendered text (byte-identical to the former hardcoded string).
--   @mpKey@ is 'Nothing' for non-catalog prose.
data MsgPayload = MsgPayload
    { mpKey  :: Maybe String
    , mpArgs :: [(String, String)]
    , mpText :: String
    } deriving (Show, Eq, Generic)

instance ToJSON MsgPayload where
    toJSON mp = object
        [ "key"  .= mpKey mp
        , "args" .= [ object ["key" .= k, "val" .= v] | (k, v) <- mpArgs mp ]
        , "text" .= mpText mp
        ]

instance FromJSON MsgPayload where
    parseJSON = withObject "MsgPayload" $ \o -> do
        k <- o .:? "key"
        mArgsVal <- o .:? "args"
        args <- case mArgsVal of
            Just (Array arr) -> mapM parsePair (F.toList arr)
            _                -> pure []
        t <- o .: "text"
        pure (MsgPayload k args t)
      where
        parsePair (Object obj) = (,) <$> obj .: "key" <*> (obj .: "val" <|> obj .: "value")
        parsePair (Array arr)  | [String k, String v] <- F.toList arr = pure (T.unpack k, T.unpack v)
        parsePair _ = fail "Expected key-value object or pair for argument"

-- | One clickable/hoverable anchor inside an art block.
data ArtHotspot = ArtHotspot
    { ahIndex  :: Int     -- ^ 1-based number shown in the legend
    , ahGlyph  :: Char    -- ^ the glyph in the art that is the anchor
    , ahTarget :: String  -- ^ item/NPC id the hotspot points at
    } deriving (Show, Eq, Generic)

instance ToJSON ArtHotspot where
    toJSON ah = object
        [ "index"  .= ahIndex ah
        , "glyph"  .= [ahGlyph ah]
        , "target" .= ahTarget ah
        ]

instance FromJSON ArtHotspot where
    parseJSON = withObject "ArtHotspot" $ \o -> do
        idx <- o .: "index"
        glyphStr <- o .: "glyph"
        let g = case (glyphStr :: String) of
                (c:_) -> c
                []    -> ' '
        tgt <- o .: "target"
        pure (ArtHotspot idx g tgt)

-- | An art block: the raw (monospace, possibly ANSI) rendering as the CLI
--   prints it today, plus structured hotspot anchors for graphical
--   frontends. Raw + structured on purpose: authored art legitimately
--   contains escape codes; consumers that cannot render them use the
--   hotspot list instead of parsing.
data ArtPayload = ArtPayload
    { apRaw      :: String
    , apHotspots :: [ArtHotspot]
    } deriving (Show, Eq, Generic)

instance ToJSON ArtPayload where
    toJSON ap = object
        [ "raw"      .= apRaw ap
        , "hotspots" .= apHotspots ap
        ]

instance FromJSON ArtPayload where
    parseJSON = withObject "ArtPayload" $ \o -> ArtPayload
        <$> o .: "raw"
        <*> o .:? "hotspots" .!= []

-- ---------------------------------------------------------------------------
-- Events
-- ---------------------------------------------------------------------------

-- | One item of engine output, in emission order.
data OutputEvent
    = EvMessage MsgPayload            -- ^ catalog message (key + args + text)
    | EvText StyledText               -- ^ prose (ANSI-free; spans carry style)
    | EvArt ArtPayload                -- ^ art block (raw + structured hotspots)
    | EvAnim Int [String]             -- ^ animation: frame delay in µs + frames
    | EvSfx FilePath                  -- ^ play one sound effect
    | EvMusicStart FilePath           -- ^ start/switch looping music
    | EvMusicStop                     -- ^ stop the current music
    | EvRoomChanged String            -- ^ currentRoom changed (new room id)
    | EvQuestUpdate                   -- ^ active/completed quests changed
    | EvDialogue                      -- ^ a dialogue became active (snapshot carries choices)
    | EvCombat Bool                   -- ^ combat engaged flag changed (new value)
    | EvGameOver                      -- ^ the game ended this command (reason in snapshot)
    | EvDisambiguate [String]         -- ^ target was ambiguous: candidate entity ids in prompt order (Phase 2.3)
    deriving (Show, Eq, Generic)

instance ToJSON OutputEvent where
    toJSON ev = case ev of
        EvMessage p       -> object [ "type" .= ("message" :: String), "payload" .= p ]
        EvText t          -> object [ "type" .= ("text" :: String), "styled" .= t ]
        EvArt a           -> object [ "type" .= ("art" :: String), "payload" .= a ]
        EvAnim d frames   -> object [ "type" .= ("anim" :: String), "delay_micros" .= d, "frames" .= frames ]
        EvSfx path        -> object [ "type" .= ("sfx" :: String), "path" .= path ]
        EvMusicStart path -> object [ "type" .= ("music_start" :: String), "path" .= path ]
        EvMusicStop       -> object [ "type" .= ("music_stop" :: String) ]
        EvRoomChanged rId -> object [ "type" .= ("room_changed" :: String), "room_id" .= rId ]
        EvQuestUpdate     -> object [ "type" .= ("quest_update" :: String) ]
        EvDialogue        -> object [ "type" .= ("dialogue" :: String) ]
        EvCombat eng      -> object [ "type" .= ("combat" :: String), "engaged" .= eng ]
        EvGameOver        -> object [ "type" .= ("game_over" :: String) ]
        EvDisambiguate ids -> object [ "type" .= ("disambiguate" :: String), "candidates" .= ids ]

instance FromJSON OutputEvent where
    parseJSON = withObject "OutputEvent" $ \o -> do
        t <- o .: "type" <|> o .: "tag"
        case (t :: String) of
            "message"      -> EvMessage <$> o .: "payload"
            "text"         -> EvText <$> (o .: "styled" <|> (styledText <$> o .: "text"))
            "art"          -> EvArt <$> o .: "payload"
            "anim"         -> EvAnim <$> (o .: "delay_micros" <|> o .: "delayMicros") <*> o .: "frames"
            "sfx"          -> EvSfx <$> o .: "path"
            "music_start"  -> EvMusicStart <$> o .: "path"
            "music_stop"   -> pure EvMusicStop
            "room_changed" -> EvRoomChanged <$> (o .: "room_id" <|> o .: "roomId")
            "quest_update" -> pure EvQuestUpdate
            "dialogue"     -> pure EvDialogue
            "combat"       -> EvCombat <$> o .: "engaged"
            "game_over"    -> pure EvGameOver
            "disambiguate" -> EvDisambiguate <$> o .: "candidates"
            other          -> fail ("Unknown OutputEvent type: " ++ other)

-- ---------------------------------------------------------------------------
-- Fragment algebra
--
--   The old engine assembled its message string with a handful of join
--   idioms (joinMessages with "\\n" between non-empty parts, direct `++`,
--   unlines with trailing newline, intercalate including empty parts). The
--   fragment algebra below replicates each idiom exactly on event lists, so
--   'renderEvents' reproduces the old string byte for byte.
--
--   NON-EMPTY CONTRACT (Phase 4.3): the drop-empty decisions below run on the
--   *default-rendered* message text, before any localization pass. Catalog
--   templates and `messages:` overrides must therefore never render empty --
--   the worldbuilder rejects empty override values ('EmptyMessageOverride')
--   and the language-pack generator rejects empty templates.
-- ---------------------------------------------------------------------------

-- | The text contribution of one event; non-text events contribute @""@ and
--   vanish from the CLI rendering (audio/state info is additive).
evTextOf :: OutputEvent -> String
evTextOf ev = case ev of
    EvMessage p       -> mpText p
    EvText t          -> stText t
    EvArt a           -> apRaw a
    EvAnim _ _        -> ""
    EvSfx _           -> ""
    EvMusicStart _    -> ""
    EvMusicStop       -> ""
    EvRoomChanged _   -> ""
    EvQuestUpdate     -> ""
    EvDialogue        -> ""
    EvCombat _        -> ""
    EvGameOver        -> ""
    EvDisambiguate _  -> ""

-- | Render an event stream to the CLI's text: concatenation of all text
--   contributions. Byte-identical to the pre-1.2 string pipeline by
--   construction (the fragment algebra below preserves every join idiom).
renderEvents :: [OutputEvent] -> String
renderEvents = concatMap evTextOf

-- | One text fragment as an event list.
evText :: String -> [OutputEvent]
evText s = [EvText (styledText s)]

-- | Alias emphasising "raw authored text" (no catalog key, no spans).
evRaw :: String -> [OutputEvent]
evRaw = evText

-- | A @\\n@ separator fragment (joinMessages idiom).
nl :: [OutputEvent]
nl = evText "\n"

-- | A @\\n\\n@ separator fragment (block idiom, e.g. dialogue choices).
nl2 :: [OutputEvent]
nl2 = evText "\n\n"

-- | @joinMessages@ on fragments: drop empty right side, no separator on
--   empty left side, otherwise join with one @\\n@.
joinEv :: [OutputEvent] -> [OutputEvent] -> [OutputEvent]
joinEv acc m
    | null (renderEvents m)    = acc
    | null (renderEvents acc)  = m
    | otherwise                = acc ++ nl ++ m

-- | Left-fold of 'joinEv' over a list of fragments.
joinAllEv :: [[OutputEvent]] -> [OutputEvent]
joinAllEv = foldl joinEv []

-- | @intercalate "\\n"@ on fragments, *including* empty pieces (the unfiltered
--   intercalate idiom, e.g. `take all`).
evIntercalate :: [[OutputEvent]] -> [OutputEvent]
evIntercalate frags = concat (intersperse nl frags)

-- | @unlines@ on fragments: every piece gets its trailing newline — including
--   empty pieces (exactly what @unlines@ does to strings).
unlinesEv :: [[OutputEvent]] -> [OutputEvent]
unlinesEv = concatMap (\f -> f ++ nl)