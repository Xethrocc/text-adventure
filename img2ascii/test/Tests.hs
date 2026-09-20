module Main where

-- | Tests for the image -> ASCII converter.
--
--   They pin the two things that were measurably wrong before (plan
--   `plan-ascii-kunst.md`, findings B1/B2): the output geometry (a terminal cell
--   is about twice as tall as it is wide) and the downscaling (box averaging
--   instead of point sampling).
--
--   All images are generated in memory, so the suite needs no fixture files.

import Codec.Picture
  ( DynamicImage (..)
  , Image
  , PixelRGB8 (..)
  , generateImage
  , pixelAt
  )
import Control.Monad (when)
import Data.Either (isLeft)
import Data.List (isInfixOf, isPrefixOf, nub)
import Data.Word (Word8)
import ImgToAscii
  ( AsciiConfig (..)
  , RenderMode (..)
  , defaultConfig
  , imageToAscii
  , imageToAsciiFrom
  , rowsForWidth
  , scaleToGrid
  , widthForRows
  )
import System.Exit (exitFailure)
import System.IO (hSetEncoding, stdout, utf8)

-- ----------------------------------------------------------------- harness

expectTrue :: String -> Bool -> IO Bool
expectTrue name ok = do
  putStrLn ((if ok then "[PASS] " else "[FAIL] ") ++ name)
  pure ok

expectEqual :: (Eq a, Show a) => String -> a -> a -> IO Bool
expectEqual name expected actual =
  expectTrue (name ++ " (expected " ++ show expected ++ ", got " ++ show actual ++ ")")
             (expected == actual)

countOccurrences :: String -> String -> Int
countOccurrences needle hay = length (filter (needle `isPrefixOf`) (suffixes hay))
  where
    suffixes [] = []
    suffixes s  = s : suffixes (drop 1 s)

-- | Remove ANSI SGR sequences (used to measure the visible width).
stripAnsi :: String -> String
stripAnsi [] = []
stripAnsi ('\ESC':'[':rest) = stripAnsi (drop 1 (dropWhile (/= 'm') rest))
stripAnsi (c:rest) = c : stripAnsi rest

-- -------------------------------------------------------- synthetic images

grey :: Int -> PixelRGB8
grey v = let w = fromIntegral (max 0 (min 255 v)) :: Word8 in PixelRGB8 w w w

solid :: Int -> Int -> PixelRGB8 -> DynamicImage
solid w h p = ImageRGB8 (generateImage (\_ _ -> p) w h)

-- | One-pixel checkerboard: averaged it is uniform mid grey, point sampled it
--   keeps only the extremes.
checker :: Int -> Int -> DynamicImage
checker w h = ImageRGB8 (generateImage (\x y -> if even (x + y) then grey 0 else grey 255) w h)

-- | Two rows: black on top, white below.
twoBands :: Int -> Image PixelRGB8
twoBands w = generateImage (\_ y -> if y == 0 then grey 0 else grey 255) w 2

-- ----------------------------------------------------------------- the tests

-- B1: the character cell is about twice as tall as it is wide, so a square
-- source must produce roughly half as many rows as it has columns.
testGeometry :: IO Bool
testGeometry = do
  r1 <- expectEqual "square 200x200 at width 60" 30 (rowsForWidth 60 200 200)
  r2 <- expectEqual "wide 200x100 at width 40" 10 (rowsForWidth 40 200 100)
  r3 <- expectEqual "tall 100x200 at width 40" 40 (rowsForWidth 40 100 200)
  r4 <- expectEqual "never fewer than one row" 1 (rowsForWidth 10 1000 10)
  r5 <- expectEqual "degenerate width" 0 (rowsForWidth 0 200 200)
  r6 <- expectEqual "degenerate source" 0 (rowsForWidth 40 0 200)
  r7 <- expectEqual "widthForRows inverts rowsForWidth" 60 (widthForRows 30 200 200)
  r8 <- expectEqual "widthForRows guard" 0 (widthForRows 0 200 200)
  pure (and [r1, r2, r3, r4, r5, r6, r7, r8])

-- B1, end to end: the art of a square image is a square block of characters.
-- Before the fix it was twice as tall as it was wide.
testEndToEndGeometry :: IO Bool
testEndToEndGeometry =
  case imageToAsciiFrom defaultConfig { asciiWidth = 60 } (checker 200 200) of
    Left err -> expectTrue ("imageToAsciiFrom failed: " ++ err) False
    Right art -> do
      let ls = lines art
      r1 <- expectEqual "square source -> 30 rows" 30 (length ls)
      r2 <- expectEqual "every row is 60 characters wide" [60] (nub (map length ls))
      r3 <- expectTrue "char ramp mode emits no escape codes" (not ("\ESC" `isInfixOf` art))
      pure (r1 && r2 && r3)

-- B2: downscaling averages the covered source box.
testAveraging :: IO Bool
testAveraging =
  case scaleToGrid 40 20 (checker 200 200) of
    Left err -> expectTrue ("scaleToGrid failed: " ++ err) False
    Right grid -> do
      let px@(PixelRGB8 r g b) = pixelAt grid 17 7
      r1 <- expectTrue ("checkerboard averages to mid grey, got " ++ show px)
                (abs (fromIntegral r - 128 :: Int) <= 2 && r == g && g == b)
      let cells = nub [ pixelAt grid x y | x <- [0 .. 39], y <- [0 .. 19] ]
      r2 <- expectEqual "the whole grid is uniform" 1 (length cells)
      let art = imageToAscii defaultConfig { asciiWidth = 40 } grid
      r3 <- expectEqual "uniform grey renders as a single character"
                1 (length (nub (concat (lines art))))
      pure (r1 && r2 && r3)

-- The ramp itself: black is the first character, white the last one.
testRamp :: IO Bool
testRamp =
  case (scaleToGrid 4 2 (solid 8 8 (grey 0)), scaleToGrid 4 2 (solid 8 8 (grey 255))) of
    (Right black, Right white) -> do
      let cfg = defaultConfig { asciiWidth = 4 }
          inv = defaultConfig { asciiWidth = 4, asciiInvert = True }
          render c img = concat (lines (imageToAscii c img))
      r1 <- expectEqual "black -> first ramp character (space)" (replicate 8 ' ') (render cfg black)
      r2 <- expectEqual "white -> last ramp character (@)" (replicate 8 '@') (render cfg white)
      r3 <- expectEqual "inverted black -> last character" (replicate 8 '@') (render inv black)
      r4 <- expectEqual "inverted white -> first character" (replicate 8 ' ') (render inv white)
      pure (r1 && r2 && r3 && r4)
    _ -> expectTrue "scaleToGrid failed for the ramp test" False

-- Every character set is a usable ramp: dark maps to its first character and
-- bright to a different one.
testCharSets :: IO Bool
testCharSets = do
  results <- mapM check [minBound .. maxBound]
  pure (and results)
  where
    check cs = case (scaleToGrid 2 1 (solid 4 2 (grey 0)), scaleToGrid 2 1 (solid 4 2 (grey 255))) of
      (Right black, Right white) -> do
        let cfg = defaultConfig { asciiWidth = 2, asciiSet = cs }
            darkChars = concat (lines (imageToAscii cfg black))
            brightChars = concat (lines (imageToAscii cfg white))
        r1 <- expectTrue (show cs ++ ": the ramp starts with a space")
                  (darkChars == replicate (length darkChars) ' ')
        r2 <- expectTrue (show cs ++ ": bright differs from dark") (brightChars /= darkChars)
        pure (r1 && r2)
      _ -> expectTrue (show cs ++ ": scaleToGrid failed") False

-- Half blocks: two source rows in one cell, colour carries both pixels.
testHalfBlock :: IO Bool
testHalfBlock = do
  let cfg = defaultConfig { asciiWidth = 4, asciiMode = HalfBlock }
      art = imageToAscii cfg (twoBands 4)
      l = case lines art of
            (x:_) -> x
            []    -> ""
  r1 <- expectEqual "two source rows collapse into one output row" 1 (length (lines art))
  r2 <- expectTrue "uses the upper half block" ("\x2580" `isInfixOf` l)
  r3 <- expectTrue "top pixel becomes the foreground (black)" ("38;2;0;0;0" `isInfixOf` l)
  r4 <- expectTrue "bottom pixel becomes the background (white)" ("48;2;255;255;255" `isInfixOf` l)
  r5 <- expectTrue "the row ends with a reset" ("\ESC[0m" `isInfixOf` l)
  r6 <- expectEqual "colour is emitted once per run" 1 (countOccurrences "38;2;" l)
  r7 <- expectEqual "one cell per column" 4 (countOccurrences "\x2580" l)
  pure (and [r1, r2, r3, r4, r5, r6, r7])

-- Half blocks follow the same geometry rule as the character ramp.
testHalfBlockGeometry :: IO Bool
testHalfBlockGeometry =
  case imageToAsciiFrom defaultConfig { asciiWidth = 60, asciiMode = HalfBlock } (checker 200 200) of
    Left err -> expectTrue ("imageToAsciiFrom failed: " ++ err) False
    Right art -> do
      let ls = lines art
      r1 <- expectEqual "square source -> 30 half-block rows" 30 (length ls)
      r2 <- expectEqual "every row is 60 visible characters wide" [60] (nub (map (length . stripAnsi) ls))
      pure (r1 && r2)

-- Degenerate inputs report cleanly instead of crashing.
testEdgeCases :: IO Bool
testEdgeCases = do
  r1 <- expectTrue "zero width is rejected" (isLeft (scaleToGrid 0 10 (checker 8 8)))
  r2 <- expectTrue "zero height is rejected" (isLeft (scaleToGrid 10 0 (checker 8 8)))
  r3 <- expectTrue "config width 0 is rejected"
            (isLeft (imageToAsciiFrom defaultConfig { asciiWidth = 0 } (checker 8 8)))
  -- a 1x1 image is the smallest legal input
  (r4, r5) <- case imageToAsciiFrom defaultConfig { asciiWidth = 1 } (solid 1 1 (grey 200)) of
    Left err -> do
      _ <- expectTrue ("1x1 image failed: " ++ err) False
      pure (False, False)
    Right art -> do
      a <- expectEqual "1x1 image -> one row" 1 (length (lines art))
      b <- expectEqual "1x1 image -> one character" 1 (length (concat (lines art)))
      pure (a, b)
  -- an odd row count in half-block mode drops the last row instead of crashing
  let oddArt = imageToAscii defaultConfig { asciiWidth = 3, asciiMode = HalfBlock }
                        (generateImage (\_ _ -> grey 100) 3 3 :: Image PixelRGB8)
  r6 <- expectEqual "odd source height: floor(3/2) rows" 1 (length (lines oddArt))
  pure (and [r1, r2, r3, r4, r5, r6])

-- Opt-in colour for the character ramp: escape codes are added, the visible
-- text is unchanged, and the run ends with a reset (no leak into the next line).
testColorRamp :: IO Bool
testColorRamp =
  case scaleToGrid 4 1 (ImageRGB8 (generateImage (\x _ -> if x < 2 then grey 0 else grey 255) 4 1)) of
    Left err -> expectTrue ("scaleToGrid failed: " ++ err) False
    Right grid -> do
      let plainCfg = defaultConfig { asciiWidth = 4 }
          colorCfg = plainCfg { asciiColor = Just True }
          plain = imageToAscii plainCfg grid
          colored = imageToAscii colorCfg grid
      r1 <- expectTrue "coloured ramp emits escapes" ("\ESC" `isInfixOf` colored)
      r2 <- expectEqual "stripped colour equals plain text" plain (stripAnsi colored)
      r3 <- expectTrue "the coloured line ends with a reset"
                ("\ESC[0m" `isInfixOf` colored)
      pure (r1 && r2 && r3)

-- `--no-color` strips half-block colour: plain output is just the block
-- character, with no escape sequences and no trailing reset.
testNoColorHalfBlock :: IO Bool
testNoColorHalfBlock = do
  let cfg = defaultConfig { asciiWidth = 4, asciiMode = HalfBlock, asciiColor = Just False }
      art = imageToAscii cfg (twoBands 4)
      l = case lines art of
            (x:_) -> x
            []    -> ""
  r1 <- expectTrue "plain half-block has no escapes" (not ("\ESC" `isInfixOf` l))
  r2 <- expectEqual "plain half-block is just one cell per column" 4 (countOccurrences "\x2580" l)
  r3 <- expectEqual "plain half-block row width" 4 (length l)
  pure (r1 && r2 && r3)

tests :: [(String, IO Bool)]
tests =
  [ ("cell geometry (B1)", testGeometry)
  , ("square source renders square (B1)", testEndToEndGeometry)
  , ("box averaging on downscale (B2)", testAveraging)
  , ("brightness ramp and inversion", testRamp)
  , ("every character set is a usable ramp", testCharSets)
  , ("half-block cells carry two pixels", testHalfBlock)
  , ("half-block geometry follows the same rule", testHalfBlockGeometry)
  , ("opt-in colour for the character ramp (C)", testColorRamp)
  , ("--no-color strips half-block colour (C)", testNoColorHalfBlock)
  , ("degenerate inputs report instead of crashing", testEdgeCases)
  ]

main :: IO ()
main = do
  hSetEncoding stdout utf8
  results <- mapM run tests
  let failed = length (filter not results)
  putStrLn ""
  putStrLn $ show (length results - failed) ++ " passed, " ++ show failed ++ " failed"
  when (failed > 0) exitFailure
  where
    run (name, t) = do
      putStrLn ""
      putStrLn ("-- " ++ name)
      t