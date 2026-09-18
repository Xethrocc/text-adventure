module Main where

import Codec.Picture (readImage)
import Control.Monad (when)
import Data.Char (toLower)
import ImgToAscii
  ( AsciiConfig (..)
  , CharSet (..)
  , defaultConfig
  , imageToAscii
  , scaleImage
  )
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

parseCharSet :: String -> Maybe CharSet
parseCharSet s = case map toLower s of
  "block"    -> Just Block
  "line"     -> Just Line
  "fine"     -> Just Fine
  "standard" -> Just Standard
  "simple"   -> Just Simple
  _          -> Nothing

printUsage :: IO ()
printUsage = do
  pn <- getProgName
  putStrLn $ "Usage: " ++ pn ++ " [options] <image-file>"
  putStrLn ""
  putStrLn "Options:"
  putStrLn "  -w, --width N     Output width in characters (default: 80)"
  putStrLn "  -c, --charset S   Character set: block, line, fine, standard, simple"
  putStrLn "  -i, --invert      Invert brightness"
  putStrLn "  -h, --help        Show this help"

parseWidth :: String -> Either String Int
parseWidth value = case reads value of
  [(width, "")] | width > 0 -> Right width
  _ -> Left $ "invalid width '" ++ value ++ "' (expected a positive integer)"

parseArgs :: [String] -> Either String (AsciiConfig, FilePath)
parseArgs = go defaultConfig Nothing
  where
    go _ Nothing [] = Left "no input file given"
    go cfg (Just path) [] = Right (cfg, path)
    go _ _ ("-w":[]) = Left "option -w requires a value"
    go _ _ ("--width":[]) = Left "option --width requires a value"
    go cfg path ("-w":value:rest) = setWidth cfg path value rest
    go cfg path ("--width":value:rest) = setWidth cfg path value rest
    go _ _ ("-c":[]) = Left "option -c requires a value"
    go _ _ ("--charset":[]) = Left "option --charset requires a value"
    go cfg path ("-c":value:rest) = setCharset cfg path value rest
    go cfg path ("--charset":value:rest) = setCharset cfg path value rest
    go cfg path ("-i":rest) = go cfg { asciiInvert = True } path rest
    go cfg path ("--invert":rest) = go cfg { asciiInvert = True } path rest
    go cfg Nothing ("--":[file]) = Right (cfg, file)
    go _ Nothing ("--":[]) = Left "no input file given after --"
    go _ Nothing ("--":_:extra:_) = Left $ "unexpected extra input file '" ++ extra ++ "'"
    go cfg path ("--":[]) = go cfg path []
    go _ (Just _) ("--":file:_) = Left $ "unexpected extra input file '" ++ file ++ "'"
    go _ _ (arg:_)
      | "-" `isPrefixOf` arg = Left $ "unknown option '" ++ arg ++ "'"
    go cfg Nothing (file:rest) = go cfg (Just file) rest
    go _ (Just _) (file:_) = Left $ "unexpected extra input file '" ++ file ++ "'"

    setWidth cfg path value rest = do
      width <- parseWidth value
      go cfg { asciiWidth = width } path rest

    setCharset cfg path value rest = case parseCharSet value of
      Just charset -> go cfg { asciiSet = charset } path rest
      Nothing -> Left $ "invalid character set '" ++ value ++ "'"

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (p:ps) (x:xs) = p == x && isPrefixOf ps xs

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
    Right (cfg, imgFile) -> do
      result <- readImage imgFile
      case result of
        Left err -> hPutStrLn stderr ("Error reading image: " ++ err) >> exitFailure
        Right dyn -> case scaleImage (asciiWidth cfg) dyn of
          Left err -> hPutStrLn stderr ("Error scaling image: " ++ err) >> exitFailure
          Right scaled -> putStr $ imageToAscii cfg scaled
