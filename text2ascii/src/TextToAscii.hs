-- | Text to ASCII banner art.
--
--   Three built-in fonts (decision D7) need no data files and carry no licence
--   question: a hand-drawn 5-row @Block@ bitmap, plus @Slant@ (a lean) and
--   @Outline@ (only the border cells), both derived from the same bitmap.
--
--   Colour is opt-in (Phase C path): @--color@ wraps each non-empty line in a
--   vertical 24-bit gradient and resets at the line end, so no colour leaks into
--   the following line. @--no-color@ (or the engine, when stdout is not a
--   terminal) yields plain text.
module TextToAscii
  ( Font (..)
  , TextConfig (..)
  , defaultTextConfig
  , fontNames
  , parseFont
  , renderText
  , renderLine
  , glyphHeight
  ) where

import Data.Char (toLower, toUpper)
import Data.List (intercalate)

-- | The built-in fonts.
data Font = Block | Slant | Outline
  deriving (Show, Eq, Enum, Bounded)

-- | Rendering configuration.
data TextConfig = TextConfig
  { tcFont  :: !Font   -- ^ Built-in font
  , tcColor :: !Bool   -- ^ Emit a 24-bit ANSI gradient
  , tcGap   :: !Int    -- ^ Blank columns between glyphs (>= 0)
  } deriving (Show, Eq)

defaultTextConfig :: TextConfig
defaultTextConfig = TextConfig
  { tcFont  = Block
  , tcColor = False
  , tcGap   = 1
  }

-- | Every built-in font name, as accepted by `parseFont`.
fontNames :: [String]
fontNames = map (map toLower . show) ([minBound .. maxBound] :: [Font])

-- | Parse a font name (case-insensitive).
parseFont :: String -> Maybe Font
parseFont s = case map lower s of
    "block"   -> Just Block
    "slant"   -> Just Slant
    "outline" -> Just Outline
    _         -> Nothing
  where lower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

-- | Every glyph is exactly this many rows tall.
glyphHeight :: Int
glyphHeight = 5

-- ---------------------------------------------------------------------------
-- The base bitmap
-- ---------------------------------------------------------------------------

-- | Uppercase letters, digits, space and a small punctuation set. '#' marks a
--   filled cell. Lowercase input is upper-cased before lookup.
blockGlyphs :: [(Char, [String])]
blockGlyphs =
  [ (' ', ["   ", "   ", "   ", "   ", "   "])
  , ('A', [" ### ", "#   #", "#####", "#   #", "#   #"])
  , ('B', ["#### ", "#   #", "#### ", "#   #", "#### "])
  , ('C', [" ####", "#    ", "#    ", "#    ", " ####"])
  , ('D', ["#### ", "#   #", "#   #", "#   #", "#### "])
  , ('E', ["#####", "#    ", "#### ", "#    ", "#####"])
  , ('F', ["#####", "#    ", "#### ", "#    ", "#    "])
  , ('G', [" ####", "#    ", "#  ##", "#   #", " ####"])
  , ('H', ["#   #", "#   #", "#####", "#   #", "#   #"])
  , ('I', ["###", " # ", " # ", " # ", "###"])
  , ('J', ["    #", "    #", "    #", "#   #", " ### "])
  , ('K', ["#   #", "#  # ", "###  ", "#  # ", "#   #"])
  , ('L', ["#    ", "#    ", "#    ", "#    ", "#####"])
  , ('M', ["#   #", "## ##", "# # #", "#   #", "#   #"])
  , ('N', ["#   #", "##  #", "# # #", "#  ##", "#   #"])
  , ('O', [" ### ", "#   #", "#   #", "#   #", " ### "])
  , ('P', ["#### ", "#   #", "#### ", "#    ", "#    "])
  , ('Q', [" ### ", "#   #", "# # #", "#  # ", " ## #"])
  , ('R', ["#### ", "#   #", "#### ", "#  # ", "#   #"])
  , ('S', [" ####", "#    ", " ### ", "    #", "#### "])
  , ('T', ["#####", "  #  ", "  #  ", "  #  ", "  #  "])
  , ('U', ["#   #", "#   #", "#   #", "#   #", " ### "])
  , ('V', ["#   #", "#   #", "#   #", " # # ", "  #  "])
  , ('W', ["#   #", "#   #", "# # #", "## ##", "#   #"])
  , ('X', ["#   #", " # # ", "  #  ", " # # ", "#   #"])
  , ('Y', ["#   #", " # # ", "  #  ", "  #  ", "  #  "])
  , ('Z', ["#####", "   # ", "  #  ", " #   ", "#####"])
  , ('0', [" ### ", "#  ##", "# # #", "##  #", " ### "])
  , ('1', [" # ", "## ", " # ", " # ", "###"])
  , ('2', [" ### ", "#   #", "   # ", "  #  ", "#####"])
  , ('3', ["#### ", "    #", " ### ", "    #", "#### "])
  , ('4', ["#  # ", "#  # ", "#####", "   # ", "   # "])
  , ('5', ["#####", "#    ", "#### ", "    #", "#### "])
  , ('6', [" ### ", "#    ", "#### ", "#   #", " ### "])
  , ('7', ["#####", "   # ", "  #  ", " #   ", "#    "])
  , ('8', [" ### ", "#   #", " ### ", "#   #", " ### "])
  , ('9', [" ### ", "#   #", " ####", "    #", " ### "])
  , ('.', ["  ", "  ", "  ", "  ", "# "])
  , (',', ["  ", "  ", "  ", "# ", " #"])
  , ('!', ["#", "#", "#", " ", "#"])
  , ('?', [" ### ", "#   #", "   # ", "     ", "  #  "])
  , ('-', ["   ", "   ", "###", "   ", "   "])
  , (':', [" ", "#", " ", "#", " "])
  , (';', [" ", "#", " ", "#", " #"])
  , ('\'', ["#", "#", " ", " ", " "])
  , ('(', ["  #", " # ", "#  ", " # ", "  #"])
  , (')', ["#  ", " # ", "  #", " # ", "#  "])
  , ('/', ["  #", "  #", " # ", "#  ", "#  "])
  , ('+', ["   ", " # ", "###", " # ", "   "])
  , ('=', ["   ", "###", "   ", "###", "   "])
  , ('*', ["   ", "# #", " # ", "# #", "   "])
  , ('#', [" # # ", "#####", " # # ", "#####", " # # "])
  ]

-- | Glyph for a character; unknown characters fall back to @?@.
glyph :: Char -> [String]
glyph c = case lookup (toUpper c) blockGlyphs of
    Just g  -> g
    Nothing -> case lookup '?' blockGlyphs of
        Just g  -> g
        Nothing -> replicate glyphHeight ""

-- ---------------------------------------------------------------------------
-- Derived fonts
-- ---------------------------------------------------------------------------

-- | Keep only cells with at least one empty 4-neighbour (the border), after a
--   one-cell dilation so thin strokes gain a hollow interior instead of
--   collapsing onto the solid Block shape.
outlineGlyph :: [String] -> [String]
outlineGlyph = borderGlyph . dilateGlyph
  where
    dilateGlyph rows =
        [ [ if anyFilled x y then '#' else ' ' | x <- [0 .. w - 1] ] | y <- [0 .. h - 1] ]
      where
        h = length rows
        w = if null rows then 0 else maximum (map length rows)
        at x y
          | y < 0 || y >= h = ' '
          | x < 0 || x >= length (rows !! y) = ' '
          | otherwise = rows !! y !! x
        anyFilled x y = any (== '#') [at x y, at (x - 1) y, at (x + 1) y, at x (y - 1), at x (y + 1)]

    borderGlyph rows = [ [ cell x y | x <- [0 .. w - 1] ] | y <- [0 .. h - 1] ]
      where
        h = length rows
        w = if null rows then 0 else maximum (map length rows)
        at x y
          | y < 0 || y >= h = ' '
          | x < 0 || x >= length (rows !! y) = ' '
          | otherwise = rows !! y !! x
        edge x y = at (x - 1) y == ' ' || at (x + 1) y == ' '
                || at x (y - 1) == ' ' || at x (y + 1) == ' '
        cell x y
          | at x y == '#' && edge x y = '#'
          | otherwise = ' '

-- | Lean the glyph rightwards: the top row shifts most.
slantGlyph :: [String] -> [String]
slantGlyph rows = zipWith shift [0 ..] rows
  where
    h = length rows
    shift i row = replicate (h - 1 - i) ' ' ++ row

-- | Apply a font to one glyph bitmap.
transformGlyph :: Font -> [String] -> [String]
transformGlyph Block   = id
transformGlyph Slant   = slantGlyph
transformGlyph Outline = outlineGlyph

-- ---------------------------------------------------------------------------
-- Rendering
-- ---------------------------------------------------------------------------

-- | One rendered glyph in the given font.
renderGlyph :: Font -> Char -> [String]
renderGlyph font = transformGlyph font . glyph

-- | Combine glyph bitmaps row by row, separated by `gap` blank columns.
combineGlyphs :: Int -> [[String]] -> [String]
combineGlyphs gap gs =
    [ intercalate (replicate gap ' ') [rows !! i | rows <- gs] | i <- [0 .. glyphHeight - 1] ]

-- | Render a single text line (no newlines) to a block of rows.
renderLine :: TextConfig -> String -> [String]
renderLine cfg = colorizeIf (tcColor cfg) . combineGlyphs (max 0 (tcGap cfg))
                     . map (renderGlyph (tcFont cfg))

-- | Render text (may contain @\\n@) to banner rows. Lines are separated by one
--   blank row.
renderText :: TextConfig -> String -> String
renderText cfg input = case lines input of
    [] -> ""
    ls -> unlines (intercalate [""] (map (renderLine cfg) ls))

-- ---------------------------------------------------------------------------
-- Colour
-- ---------------------------------------------------------------------------

sgrReset :: String
sgrReset = "\ESC[0m"

-- | Vertical gradient, warm at the top to cool at the bottom.
gradient :: Int -> Int -> (Int, Int, Int)
gradient i n =
    let t = if n <= 1 then 0 else i * 255 `div` (n - 1)
    in (255 - t, 110, t)

-- | Wrap every non-empty row in an SGR colour and a reset at the end.
colorizeIf :: Bool -> [String] -> [String]
colorizeIf False rows = rows
colorizeIf True  rows = zipWith colorRow [0 ..] rows
  where
    n = length rows
    colorRow i row
      | '#' `elem` row =
          let (r, g, b) = gradient i n
          in "\ESC[38;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m"
             ++ row ++ sgrReset
      | otherwise = row