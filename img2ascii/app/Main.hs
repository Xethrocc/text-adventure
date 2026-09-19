module Main where

import Codec.Picture (convertRGB8, imageHeight, imageWidth, readImage)
import Control.Monad (when)
import Data.Char (toLower)
import Data.List (isPrefixOf)
import ImgToAscii
  ( AsciiConfig (..)
  , CharSet (..)
  , RenderMode (..)
  , defaultConfig
  , imageToAsciiFrom
  , widthForRows
  )
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hIsTerminalDevice, hPutStrLn, stderr, stdout)

-- | Everything the command line can say.
data Options = Options
  { optConfig :: AsciiConfig
  , optHeight :: Maybe Int        -- ^ `--height`: rows; the width is derived
  , optFile   :: FilePath
  , optColor  :: Maybe Bool       -- ^ `--color` / `--no-color`; Nothing = auto
  }

-- | Parser state: the config plus what was said on the command line. Tracking
--   width/height explicitly is what makes "use either, not both" exact.
data PState = PState
  { psConfig :: AsciiConfig
  , psWidth  :: Maybe Int
  , psHeight :: Maybe Int
  , psFile   :: Maybe FilePath
  , psColor  :: Maybe Bool
  }

parseCharSet :: String -> Maybe CharSet
parseCharSet s = case map toLower s of
  "block"    -> Just Block
  "line"     -> Just Line
  "fine"     -> Just Fine
  "standard" -> Just Standard
  "simple"   -> Just Simple
  _          -> Nothing

parseMode :: String -> Maybe RenderMode
parseMode s = case map toLower s of
  "char" -> Just CharRamp
  "ramp" -> Just CharRamp
  "half" -> Just HalfBlock
  _      -> Nothing

printUsage :: IO ()
printUsage = do
  pn <- getProgName
  putStrLn $ "Usage: " ++ pn ++ " [options] <image-file>"
  putStrLn ""
  putStrLn "Options:"
  putStrLn "  -w, --width N     Output width in characters (default: 80)"
  putStrLn "  -H, --height N    Output rows instead; the width is derived from the"
  putStrLn "                    image so the result keeps its proportions"
  putStrLn "  -c, --charset S   Character set: block, line, fine, standard, simple"
  putStrLn "  -m, --mode M      char (default, works without colour) or half"
  putStrLn "                    (two pixels per cell, needs a colour terminal)"
  putStrLn "      --color       Colour the character ramp with 24-bit ANSI (opt-in)"
  putStrLn "      --no-color    Never emit ANSI; half-block output loses its shading"
  putStrLn "  -i, --invert      Invert brightness"
  putStrLn "  -h, --help        Show this help"
  putStrLn ""
  putStrLn "The character cell is about twice as tall as it is wide, so the row"
  putStrLn "count is derived from the image aspect: a square image comes out as a"
  putStrLn "roughly square block of characters instead of a stretched one."

parsePos :: String -> Either String Int
parsePos value = case reads value of
  [(n, "")] | n > 0 -> Right n
  _ -> Left $ "invalid number '" ++ value ++ "' (expected a positive integer)"

parseArgs :: [String] -> Either String Options
parseArgs argv = finish =<< go (PState defaultConfig Nothing Nothing Nothing Nothing) argv
  where
    finish (PState cfg w h (Just path) color)
      | Just _ <- w, Just _ <- h = Left "use either --width or --height, not both"
      | otherwise = Right (Options cfg h path color)
    finish (PState _ _ _ Nothing _) = Left "no input file given"

    -- options that need a value
    withValue flag rest k = case rest of
      (value:more) -> k value more
      []           -> Left $ "option " ++ flag ++ " requires a value"

    go st [] = pure st
    go st (arg:rest) = case arg of
      "-h"        -> go st rest          -- handled before parsing
      "--help"    -> go st rest
      "-i"        -> go st { psConfig = (psConfig st) { asciiInvert = True } } rest
      "--invert"  -> go st { psConfig = (psConfig st) { asciiInvert = True } } rest
      "--color"   -> go st { psColor = Just True } rest
      "--no-color" -> go st { psColor = Just False } rest
      "-w"        -> withValue arg rest (setWidth st)
      "--width"   -> withValue arg rest (setWidth st)
      "-H"        -> withValue arg rest (setHeight st)
      "--height"  -> withValue arg rest (setHeight st)
      "-c"        -> withValue arg rest (setCharset st)
      "--charset" -> withValue arg rest (setCharset st)
      "-m"        -> withValue arg rest (setMode st)
      "--mode"    -> withValue arg rest (setMode st)
      "--"        -> go st { psFile = Just (unwords rest) } []
      _ | "-" `isPrefixOf` arg -> Left $ "unknown option '" ++ arg ++ "'"
        | otherwise -> case psFile st of
            Nothing -> go st { psFile = Just arg } rest
            Just _  -> Left $ "unexpected extra input file '" ++ arg ++ "'"

    setWidth st value rest = do
      n <- parsePos value
      if psWidth st == Nothing && psHeight st /= Nothing
        then Left "use either --width or --height, not both"
        else go st { psConfig = (psConfig st) { asciiWidth = n }, psWidth = Just n } rest

    setHeight st value rest = do
      n <- parsePos value
      if psHeight st == Nothing && psWidth st /= Nothing
        then Left "use either --width or --height, not both"
        else go st { psHeight = Just n } rest

    setCharset st value rest = case parseCharSet value of
      Just cs -> go st { psConfig = (psConfig st) { asciiSet = cs } } rest
      Nothing -> Left $ "invalid character set '" ++ value ++ "'"

    setMode st value rest = case parseMode value of
      Just m  -> go st { psConfig = (psConfig st) { asciiMode = m } } rest
      Nothing -> Left $ "invalid mode '" ++ value ++ "'"

helpRequested :: [String] -> Bool
helpRequested [] = False
helpRequested ("--":_) = False
helpRequested (arg:rest) = arg == "-h" || arg == "--help" || helpRequested rest

main :: IO ()
main = do
  args <- getArgs
  when (helpRequested args) $ printUsage >> exitSuccess
  case parseArgs args of
    Left err -> do
      hPutStrLn stderr $ "Error: " ++ err
      hPutStrLn stderr "Use --help for usage information."
      exitFailure
    Right opts -> do
      result <- readImage (optFile opts)
      case result of
        Left err -> hPutStrLn stderr ("Error reading image: " ++ err) >> exitFailure
        Right dyn -> do
          -- Resolve --height into a width using the image's own aspect, and
          -- apply the colour choice (Nothing keeps the auto default).
          let rgb8 = convertRGB8 dyn
              cfg0 = (optConfig opts) { asciiColor = optColor opts }
              cfg = case optHeight opts of
                Nothing -> cfg0
                Just rows -> cfg0
                  { asciiWidth = widthForRows rows (imageWidth rgb8) (imageHeight rgb8) }
              colorOn = case asciiColor cfg of
                Just b  -> b
                Nothing -> asciiMode cfg == HalfBlock
          when (asciiMode cfg == HalfBlock) $ do
            tty <- hIsTerminalDevice stdout
            when (not colorOn || not tty) $
              hPutStrLn stderr "Warning: --mode half needs a colour terminal; without 24-bit colour the blocks carry no information."
          case imageToAsciiFrom cfg dyn of
            Left err -> hPutStrLn stderr ("Error scaling image: " ++ err) >> exitFailure
            Right art -> putStr art