module ImgToAscii
  ( AsciiConfig (..)
  , defaultConfig
  , CharSet (..)
  , imageToAscii
  , scaleImage
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
import qualified Data.Vector as V

-- | Predefined character sets for different levels of detail.
data CharSet = Block     -- ^ █▓▒░  (4 chars, high contrast)
             | Line      -- ^ ░▒▓█  (reverse Block)
             | Fine      -- ^ ░▒▓█▄▀■ (detailed)
             | Standard  -- ^ "@%#*+=-:. " (10 levels, balanced)
             | Simple    -- ^ "#. " (minimal, 3 levels)
             deriving (Show, Eq, Enum, Bounded)

-- | Brightness-to-character lookup.
charsForSet :: CharSet -> V.Vector Char
charsForSet s = case s of
  Block    -> V.fromList " ░▒▓█"
  Line     -> V.fromList " █▓▒░"
  Fine     -> V.fromList " ░▒▓█▄▀■"
  Standard -> V.fromList " .:-=+*#%@"
  Simple   -> V.fromList " #."

-- | Configuration for ASCII rendering.
data AsciiConfig = AsciiConfig
  { asciiWidth    :: !Int          -- ^ Target width in characters
  , asciiSet      :: !CharSet      -- ^ Character set to use
  , asciiInvert   :: !Bool         -- ^ Invert brightness
  , asciiColor    :: !Bool         -- ^ Use ANSI color (future)
  } deriving (Show, Eq)

defaultConfig :: AsciiConfig
defaultConfig = AsciiConfig
  { asciiWidth  = 80
  , asciiSet    = Standard
  , asciiInvert = False
  , asciiColor  = False
  }

-- | Read a DynamicImage and scale down to a fixed pixel width,
--   preserving aspect ratio.
scaleImage :: Int -> DynamicImage -> Either String (Image PixelRGB8)
scaleImage targetW dyn
  | targetW <= 0 = Left "target width must be positive"
  | srcW <= 0 || srcH <= 0 = Left "source image must have positive dimensions"
  | otherwise = Right $ generateImage sample outW outH
  where
    rgb8 = convertRGB8 dyn
    srcW = imageWidth rgb8
    srcH = imageHeight rgb8
    scaledH = max 1 $ (toInteger srcH * toInteger targetW) `div` toInteger srcW
    outW = min srcW targetW
    outH = min srcH (fromInteger scaledH)
    scaleX = fromIntegral srcW / fromIntegral outW :: Double
    scaleY = fromIntegral srcH / fromIntegral outH :: Double
    sample x y =
      let sx = min (srcW - 1) $ floor (fromIntegral x * scaleX)
          sy = min (srcH - 1) $ floor (fromIntegral y * scaleY)
      in pixelAt rgb8 sx sy

-- | Convert a scaled RGB8 image to an ASCII string.
imageToAscii :: AsciiConfig -> Image PixelRGB8 -> String
imageToAscii cfg img =
  let w     = imageWidth img
      h     = imageHeight img
      chars = charsForSet (asciiSet cfg)
      n     = V.length chars - 1  -- highest index
      invert = asciiInvert cfg

      -- Luminosity weights (ITU-R BT.601)
      gray :: PixelRGB8 -> Double
      gray (PixelRGB8 r g b) =
        fromIntegral r * 0.299 +
        fromIntegral g * 0.587 +
        fromIntegral b * 0.114

      -- Normalise 0..n
      toChar p =
        let lvl = if invert
                  then n - round (gray p / 255 * fromIntegral n)
                  else round (gray p / 255 * fromIntegral n)
            clamped = max 0 (min n lvl)
        in V.unsafeIndex chars clamped

  in unlines [ [ toChar (pixelAt img x y) | x <- [0 .. w - 1] ]
             | y <- [0 .. h - 1]
             ]
