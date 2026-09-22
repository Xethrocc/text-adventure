-- | SGR-to-vty colour mapping for the TUI (the colour rest post of Phase T/H).
--
--   The engine and the art converters emit ANSI SGR sequences in their output
--   (hotspot highlights use bold yellow, the img2ascii half-block mode uses
--   24-bit foreground/background). The TUI was monochrome so far: it stripped
--   the sequences on the way in. This module instead *understands* them: it
--   parses a line into (text, state) segments and maps the SGR state onto vty
--   attributes.
--
--   Two design points:
--
--   * **Attributes are named, not anonymous.** Brick colours widgets through
--     'AttrName' lookups in a finite attribute map, so there is no per-state
--     Attr constructor. The name encodes the SGR state deterministically
--     ('colorAttrName'); the app builds its map from the states that actually
--     occur in its current lines and panel, using 'attrOfSgr' to convert.
--     Everything stays pure and testable.
--   * **Truecolor quantizes to the xterm 256-colour palette** (vty 6.6 offers
--     'V.ISOColor' and 'V.Color240' but no 24-bit colour type): the standard
--     6x6x6 cube for colour, the grayscale band for @r == g == b@.
module TextAdventure.Tui.Color
  ( SgrColor (..)
  , SgrState (..)
  , emptySgr
  , parseSgrLine
  , applySgrSeq
  , colorAttrName
  , attrOfSgr
  , rgbToColor240
  ) where

import Data.Char (isDigit)
import Text.Read (readMaybe)

import Brick.AttrMap (AttrName, attrName)
import qualified Graphics.Vty as V
-- | A resolved SGR colour: the 16 terminal colours ('Iso') or one of the
--   256-colour palette entries ('C256': the 6x6x6 cube at 16..231, the
--   grayscale band at 232..255).
data SgrColor = Iso Int    -- ^ @30..37@ / @90..97@ style terminal colours
              | C256 Int   -- ^ @38;5;n@ / quantized truecolor
    deriving (Eq, Show)

-- | The SGR state a position in a line carries.
data SgrState = SgrState
    { sfFg   :: Maybe SgrColor
    , sfBg   :: Maybe SgrColor
    , sfBold :: Bool
    } deriving (Eq, Show)

emptySgr :: SgrState
emptySgr = SgrState Nothing Nothing False

-- | Apply one SGR code list. The list form exists because @38@ and @48@
--   consume their followers (@38;5;n@, @38;2;r;g;b@); unknown codes are
--   ignored, @0@ resets.
applySgrSeq :: [Int] -> SgrState -> SgrState
applySgrSeq [] st = st
applySgrSeq (code : rest) st = case code of
    0                            -> applySgrSeq rest emptySgr
    1                            -> applySgrSeq rest st { sfBold = True }
    21                           -> applySgrSeq rest st { sfBold = False }
    22                           -> applySgrSeq rest st { sfBold = False }
    39                           -> applySgrSeq rest st { sfFg = Nothing }
    49                           -> applySgrSeq rest st { sfBg = Nothing }
    _ | inRange 30 37            -> fg (code - 30)
      | inRange 90 97            -> fg (code - 90 + 8)
      | inRange 40 47            -> bg (code - 40)
      | inRange 100 107          -> bg (code - 100 + 8)
      | code == 38 || code == 48 -> case extended rest of
            Just (color, more) -> applySgrSeq more (set color)
            Nothing            -> applySgrSeq rest st
      | otherwise                -> applySgrSeq rest st
  where
    inRange lo hi = code >= lo && code <= hi
    fg c = st { sfFg = Just (Iso c) }
    bg c = st { sfBg = Just (Iso c) }
    set color = if code == 38 then st { sfFg = Just color }
                              else st { sfBg = Just color }
    -- the mode code (5 = palette index, 2 = RGB triple) plus its parameters
    extended (5 : idx : more)
        | idx >= 0 && idx <= 255 = Just (C256 idx, more)
    extended (2 : r : g : b : more)
        | all (\c -> c >= 0 && c <= 255) [r, g, b] =
            Just (C256 (rgbToColor240 r g b), more)
    extended _ = Nothing

-- | Quantize a 24-bit RGB triple to an xterm 256-colour index. Equal channels
--   use the grayscale band (232..255, 24 steps of 10), everything else the
--   6x6x6 cube (16..231). Deterministic: the same art renders the same way
--   on every terminal.
rgbToColor240 :: Int -> Int -> Int -> Int
rgbToColor240 r g b
    | r == g && g == b, r >= 8, r <= 238 = 232 + min 23 ((r - 8) `div` 10)
    | otherwise = 16 + 36 * cube r + 6 * cube g + cube b
  where
    -- nearest of the cube levels 0, 95, 135, 175, 215, 255
    cube c | c < 48    = 0
           | c < 115   = 1
           | c < 155   = 2
           | c < 195   = 3
           | c < 235   = 4
           | otherwise = 5

-- | Parse one line into (text, 'SgrState') segments: every chunk carries the
--   state active at its position, escape sequences are consumed. Chunks are
--   maximal between escapes. A non-SGR CSI sequence or an unterminated escape
--   is dropped without changing the state (the engine only ever emits CSI).
parseSgrLine :: String -> [(String, SgrState)]
parseSgrLine input = go emptySgr input
  where
    go _ [] = []
    go st s =
        let (plain, rest) = break (== '\ESC') s
            emit k = if null plain then k else (plain, st) : k
        in case rest of
            [] -> emit []
            (_ : afterEsc) -> case csi afterEsc of
                -- unterminated/unknown escape: drop the marker, keep parsing
                Nothing -> emit (go st afterEsc)
                Just (Nothing, more) -> emit (go st more)
                Just (Just codes, more) ->
                    emit (go (applySgrSeq codes st) more)
    -- Parse the CSI body (after ESC): digits and semicolons up to a final
    -- byte. Only final byte @m@ carries SGR codes; other CSI finals (cursor
    -- movement and friends) are skipped without changing the state.
    csi ('[' : more) =
        let (params, rest') = span (\c -> isDigit c || c == ';') more
        in case rest' of
            ('m' : tail') -> Just (Just (sgrCodes params), tail')
            (fin : tail')
                | fin >= '@' && fin <= '~' -> Just (Nothing, tail')
                | otherwise -> Nothing
            [] -> Nothing
      where
        sgrCodes ps = case ps of
            "" -> [0]   -- bare "\ESC[m" means reset
            _  -> map codeOf (splitSemi ps)
        codeOf x = case readMaybe x of
            Just n -> n :: Int
            Nothing -> 0
        splitSemi p = case break (== ';') p of
            (a, [])     -> [a]
            (a, _ : b)  -> a : splitSemi b
    csi _ = Nothing

-- | Compact, deterministic attribute-name encoding of an 'SgrState', e.g.
--   @sgr:i1/c-196/b@ (bold, ISO foreground 1, quantized background 196).
--   Brick resolves widget colours through these names in the app's attribute
--   map; the map is built from the states that actually occur, so the
--   encoding only has to be injective — and it is, which keeps the whole
--   path pure and testable.
colorAttrName :: SgrState -> AttrName
colorAttrName st = attrName
    ( "sgr:" ++ enc (sfFg st) ++ "/" ++ enc (sfBg st) ++ "/"
             ++ (if sfBold st then "b" else "-") )
  where
    enc Nothing        = "-"
    enc (Just (Iso n)) = 'i' : show n
    enc (Just (C256 n)) = 'c' : show n

-- | The vty attribute for an SGR state.
attrOfSgr :: SgrState -> V.Attr
attrOfSgr st = V.defAttr
    { V.attrForeColor = maybe V.KeepCurrent (V.SetTo . vtyColor) (sfFg st)
    , V.attrBackColor = maybe V.KeepCurrent (V.SetTo . vtyColor) (sfBg st)
    , V.attrStyle = if sfBold st then V.SetTo V.bold else V.KeepCurrent
    }
  where
    vtyColor (Iso n)  = V.ISOColor (fromIntegral n)
    vtyColor (C256 n) = V.Color240 (fromIntegral n)