{-# LANGUAGE BangPatterns #-}

-- | Image to ASCII art.
--
--   Two things matter for a usable result:
--
--   1. **Geometry.** A terminal character cell is roughly twice as tall as it is
--      wide. An image of @srcW x srcH@ pixels therefore needs @srcW -> width@
--      characters and about @width * srcH / (2 * srcW)@ rows to keep its
--      proportions — see `rowsForWidth` / `widthForRows`.
--   2. **Downscaling.** Sampling a single pixel per cell aliases badly (a fine
--      checkerboard turns into noise). `scaleToGrid` averages the source box
--      that each cell covers instead.
--
--   Two rendering modes are offered (`RenderMode`): a monochrome brightness ramp
--   that survives any terminal, and the half-block mode that packs two pixels
--   into one cell (double vertical resolution) at the price of needing colour.
module ImgToAscii
  ( AsciiConfig (..)
  , RenderMode (..)
  , CharSet (..)
  , defaultConfig
  , rowsForWidth
  , widthForRows
  , scaleToGrid
  , imageToAscii
  , imageToAsciiFrom
  ) where

import Codec.Picture
  ( DynamicImage (..)
  , Image
  , PixelRGB8 (..)
  , convertRGB8
  , generateImage
  , imageHeight
  , imageWidth
  , pixelAt
  )
import Data.List (foldl')
import Data.Word (Word8)
import qualified Data.Vector as V

-- | Predefined character sets for different levels of detail. The ramp runs
--   from dark (first character) to bright (last character).
data CharSet = Block     -- ^ ░▒▓█ (4 levels, high contrast)
             | Line      -- ^ reverse of Block
             | Fine      -- ^ ░▒▓█▄▀■ (7 levels)
             | Standard  -- ^ \" .:-=+*#%@\" (10 levels, balanced)
             | Simple    -- ^ \" #.\" (3 levels, minimal)
             deriving (Show, Eq, Enum, Bounded)

-- | Brightness-to-character lookup.
charsForSet :: CharSet -> V.Vector Char
charsForSet s = case s of
  Block    -> V.fromList " ░▒▓█"
  Line     -> V.fromList " █▓▒░"
  Fine     -> V.fromList " ░▒▓█▄▀■"
  Standard -> V.fromList " .:-=+*#%@"
  Simple   -> V.fromList " #."

-- | How one character cell is filled.
data RenderMode
  = CharRamp   -- ^ One brightness character per cell. Works in any terminal,
               --   including plain text output with all colour stripped.
  | HalfBlock  -- ^ @▀@ with the upper pixel as foreground and the lower one as
               --   background colour: two pixels per cell, twice the vertical
               --   resolution. **Needs a colour terminal** — without colour the
               --   blocks carry no information.
  deriving (Show, Eq, Enum, Bounded)

-- | Configuration for ASCII rendering.
data AsciiConfig = AsciiConfig
  { asciiWidth  :: !Int          -- ^ Target width in characters
  , asciiSet    :: !CharSet      -- ^ Character set (only used by `CharRamp`)
  , asciiInvert :: !Bool         -- ^ Invert brightness
  , asciiMode   :: !RenderMode   -- ^ How each cell is filled
  , asciiColor  :: !(Maybe Bool) -- ^ Emit 24-bit ANSI colour. @Nothing@ = auto
                                 --   (colour only for `HalfBlock`, where it is
                                 --   structural); @Just True@ forces colour for
                                 --   the character ramp too.
  } deriving (Show, Eq)

defaultConfig :: AsciiConfig
defaultConfig = AsciiConfig
  { asciiWidth  = 80
  , asciiSet    = Standard
  , asciiInvert = False
  , asciiMode   = CharRamp
  , asciiColor  = Nothing
  }

-- | Output rows for a character width, given the source pixel size. The factor
--   2 is the character cell's height/width ratio. Rounded to the nearest row;
--   the result is at least 1 for sane inputs and 0 for degenerate ones.
rowsForWidth :: Int -> Int -> Int -> Int
rowsForWidth width srcW srcH
  | width <= 0 || srcW <= 0 || srcH <= 0 = 0
  | otherwise = max 1 ((width * srcH + srcW) `div` (2 * srcW))

-- | Character width for a target row count — the inverse of `rowsForWidth`,
--   so `--height` can be resolved without duplicating the ratio.
widthForRows :: Int -> Int -> Int -> Int
widthForRows rows srcW srcH
  | rows <= 0 || srcW <= 0 || srcH <= 0 = 0
  | otherwise = max 1 ((2 * rows * srcW + srcH `div` 2) `div` srcH)

-- | Downscale to exactly @outW x outH@ pixels by averaging the source box each
--   target pixel covers (box filter — no aliasing, unlike point sampling).
scaleToGrid :: Int -> Int -> DynamicImage -> Either String (Image PixelRGB8)
scaleToGrid outW outH dyn
  | outW <= 0 || outH <= 0 = Left "target dimensions must be positive"
  | srcW <= 0 || srcH <= 0 = Left "source image must have positive dimensions"
  | otherwise = Right $ generateImage sample outW outH
  where
    rgb8 = convertRGB8 dyn
    srcW = imageWidth rgb8
    srcH = imageHeight rgb8

    -- Source range [lo, hi) covered by target index i along an axis.
    spanOf outN srcN i =
      let lo = (i * srcN) `div` outN
          hi = max (lo + 1) (((i + 1) * srcN) `div` outN)
      in (lo, min srcN hi)

    sample x y = PixelRGB8 (avg rSum) (avg gSum) (avg bSum)
      where
        (x0, x1) = spanOf outW srcW x
        (y0, y1) = spanOf outH srcH y
        (rSum, gSum, bSum, cnt) = foldl' rowAcc (0, 0, 0, 0 :: Int) [y0 .. y1 - 1]
        rowAcc (!rAcc, !gAcc, !bAcc, !cAcc) py =
          let (r', g', b', c') = foldl' (colAcc py) (0, 0, 0, 0 :: Int) [x0 .. x1 - 1]
          in (rAcc + r', gAcc + g', bAcc + b', cAcc + c')
        colAcc py (!rAcc, !gAcc, !bAcc, !cAcc) px =
          let PixelRGB8 pr pg pb = pixelAt rgb8 px py
          in (rAcc + fromIntegral pr, gAcc + fromIntegral pg, bAcc + fromIntegral pb, cAcc + 1)
        avg s = fromIntegral (s `div` max 1 cnt) :: Word8

-- | Convert an already scaled image to ASCII. In `HalfBlock` mode the image is
--   read in pairs of rows (top/bottom); an odd last row is dropped.
imageToAscii :: AsciiConfig -> Image PixelRGB8 -> String
imageToAscii cfg img = case asciiMode cfg of
  CharRamp
    | colorOn   -> unlines [ coloredRampRow y | y <- [0 .. h - 1] ]
    | otherwise -> unlines [ [ rampChar (pixelAt img x y) | x <- [0 .. w - 1] ] | y <- [0 .. h - 1] ]
  HalfBlock
    | colorOn   -> unlines [ halfBlockRow y | y <- [0, 2 .. h - 2] ]
    | otherwise -> unlines [ replicate w '\x2580' | _ <- [0, 2 .. h - 2] ]
  where
    w = imageWidth img
    h = imageHeight img
    rampChars = charsForSet (asciiSet cfg)
    n = V.length rampChars - 1
    invert = asciiInvert cfg

    -- Auto: colour is structural for half blocks and opt-in for the ramp.
    colorOn = case asciiColor cfg of
      Just b  -> b
      Nothing -> asciiMode cfg == HalfBlock

    -- BT.601 luma, normalised to the ramp index.
    rampChar p =
      let lvl = round (luma p / 255 * fromIntegral n) :: Int
          idx = if invert then n - lvl else lvl
          clamped = max 0 (min n idx)
      in V.unsafeIndex rampChars clamped

    sgrFg (PixelRGB8 r g b) = "\ESC[38;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m"
    sgrBg (PixelRGB8 r g b) = "\ESC[48;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m"

    -- One ramp row with the pixel colour as foreground. Like the half-block
    -- row, colour is emitted only when it changes and the row ends with a
    -- reset so no colour leaks into the following line.
    coloredRampRow y = concat (reverse (ansiReset : go 0 Nothing []))
      where
        go x !lastFg acc
          | x >= w = acc
          | otherwise =
              let p = pixelAt img x y
                  fgCode = if lastFg == Just p then "" else sgrFg p
              in go (x + 1) (Just p) ((fgCode ++ [rampChar p]) : acc)

    -- One row of half blocks. Colour codes are emitted only when they change,
    -- which keeps flat areas compact; the row ends with a reset so no colour
    -- bleeds into the following line.
    halfBlockRow y = concat (reverse (ansiReset : go 0 Nothing Nothing []))
      where
        go x !lastFg !lastBg acc
          | x >= w = acc
          | otherwise =
              let top = pixelAt img x y
                  bot = pixelAt img x (y + 1)
                  fgCode = if lastFg == Just top then "" else sgrFg top
                  bgCode = if lastBg == Just bot then "" else sgrBg bot
              in go (x + 1) (Just top) (Just bot)
                    ((fgCode ++ bgCode ++ "\x2580") : acc)

-- | BT.601 luma of a pixel (0..255).
luma :: PixelRGB8 -> Double
luma (PixelRGB8 r g b) =
  fromIntegral r * 0.299 + fromIntegral g * 0.587 + fromIntegral b * 0.114

-- | Scale and render in one step, using the character-cell geometry. This is
--   the entry point callers want: it derives the row count from the source
--   aspect so the result is not stretched.
imageToAsciiFrom :: AsciiConfig -> DynamicImage -> Either String String
imageToAsciiFrom cfg dyn
  | asciiWidth cfg <= 0 = Left "target width must be positive"
  | srcW <= 0 || srcH <= 0 = Left "source image must have positive dimensions"
  | otherwise = do
      grid <- scaleToGrid (asciiWidth cfg) gridH dyn
      pure (imageToAscii cfg grid)
  where
    rgb8 = convertRGB8 dyn
    srcW = imageWidth rgb8
    srcH = imageHeight rgb8
    rows = rowsForWidth (asciiWidth cfg) srcW srcH
    -- Half blocks read two source rows per output row.
    gridH = case asciiMode cfg of
      CharRamp  -> rows
      HalfBlock -> rows * 2

ansiReset :: String
ansiReset = "\ESC[0m"
