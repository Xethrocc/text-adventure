module Main where

-- | Tests for the text -> banner converter.
--
--   They pin the contract that matters for the engine: every built-in font
--   renders the supported characters on a stable grid, unknown characters have
--   a defined fallback, and the colour output is exactly the plain output with
--   SGR sequences removed (so the engine can strip it).

import Control.Monad (when)
import Data.List (isInfixOf, nub)
import System.Exit (exitFailure)
import System.IO (hSetEncoding, stdout, utf8)
import TextToAscii

-- ----------------------------------------------------------------- harness

expectTrue :: String -> Bool -> IO Bool
expectTrue name ok = do
  putStrLn ((if ok then "[PASS] " else "[FAIL] ") ++ name)
  pure ok

expectEqual :: (Eq a, Show a) => String -> a -> a -> IO Bool
expectEqual name expected actual =
  expectTrue (name ++ " (expected " ++ show expected ++ ", got " ++ show actual ++ ")")
             (expected == actual)

-- | Remove ANSI CSI sequences (used to compare colour output with plain).
stripAnsi :: String -> String
stripAnsi [] = []
stripAnsi ('\ESC':'[':rest) = stripAnsi (drop 1 (dropWhile (/= 'm') rest))
stripAnsi (c:rest) = c : stripAnsi rest

-- ----------------------------------------------------------------- the tests

-- | Every supported glyph is `glyphHeight` rows tall and lower-case maps to
--   upper-case.
testGlyphCoverage :: IO Bool
testGlyphCoverage = do
  let chars = ['A' .. 'Z'] ++ ['0' .. '9'] ++ " .,!?-:;'()/+ =*#"
  r1 <- expectTrue "all supported glyphs have a positive width"
            (all (\c -> not (null (renderLine defaultTextConfig [c]))
                        && length (renderLine defaultTextConfig [c]) == glyphHeight) chars)
  r2 <- expectEqual "lower-case renders like upper-case"
            (renderText defaultTextConfig "abc")
            (renderText defaultTextConfig "ABC")
  pure (r1 && r2)

-- | Unknown characters fall back to the `?` glyph instead of crashing.
testUnknownFallback :: IO Bool
testUnknownFallback = do
  r1 <- expectEqual "unknown char uses the ? fallback"
            (renderLine defaultTextConfig "?")
            (renderLine defaultTextConfig "~")
  r2 <- expectTrue "unknown char still renders rows"
            (length (renderLine defaultTextConfig "~") == glyphHeight)
  pure (r1 && r2)

-- | Glyph widths compose with the gap.
testWidth :: IO Bool
testWidth = do
  let rows = renderLine defaultTextConfig "AB"
      widths = nub (map length rows)
  r1 <- expectEqual "AB is one row width (5 + 1 + 5)" [11] widths
  r2 <- expectTrue "gap 0 is honoured"
            (all (== 10) (map length (renderLine defaultTextConfig { tcGap = 0 } "AB")))
  pure (r1 && r2)

-- | The three fonts are actually different, and outline keeps only borders.
testFonts :: IO Bool
testFonts = do
  let block = renderText defaultTextConfig "A"
      slant = renderText defaultTextConfig { tcFont = Slant } "A"
      outline = renderText defaultTextConfig { tcFont = Outline } "A"
  r1 <- expectTrue "slant differs from block" (slant /= block)
  r2 <- expectTrue "outline differs from block" (outline /= block)
  r3 <- expectTrue "outline uses only # and space"
            (all (`elem` "# \n") outline)
  r4 <- expectTrue "outline is non-empty" ('#' `elem` outline)
  r5 <- expectEqual "font names are exposed" ["block", "slant", "outline"] fontNames
  pure (r1 && r2 && r3 && r4 && r5)

-- | Colour output is plain output plus SGR sequences, reset per row.
testColor :: IO Bool
testColor = do
  let plain = renderText defaultTextConfig "HI"
      colored = renderText defaultTextConfig { tcColor = True } "HI"
      coloredLines = lines colored
  r1 <- expectTrue "coloured banner emits escapes" ("\ESC" `isInfixOf` colored)
  r2 <- expectEqual "stripped colour equals plain text" plain (stripAnsi colored)
  r3 <- expectTrue "every coloured row ends with a reset"
            (all (\l -> not ('#' `elem` l) || "\ESC[0m" `isInfixOf` l) coloredLines)
  r4 <- expectTrue "no escape survives stripping" (not ('\ESC' `elem` stripAnsi colored))
  pure (r1 && r2 && r3 && r4)

-- | Empty and multi-line input behave predictably.
testShape :: IO Bool
testShape = do
  r1 <- expectEqual "empty input renders empty" "" (renderText defaultTextConfig "")
  let multi = renderText defaultTextConfig "A\nB"
  r2 <- expectTrue "multi-line input separates lines with a blank row"
            ("" `elem` lines multi)
  r3 <- expectEqual "parseFont is case-insensitive" (Just Slant) (parseFont "SlAnT")
  r4 <- expectEqual "parseFont rejects unknown names" Nothing (parseFont "nope")
  pure (r1 && r2 && r3 && r4)

tests :: [(String, IO Bool)]
tests =
  [ ("glyph coverage and case folding", testGlyphCoverage)
  , ("unknown characters fall back", testUnknownFallback)
  , ("layout width follows glyph + gap", testWidth)
  , ("built-in fonts differ (block/slant/outline)", testFonts)
  , ("colour is opt-in and resettable", testColor)
  , ("empty / multi-line shape", testShape)
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