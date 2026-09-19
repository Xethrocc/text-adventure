module Main where

import Data.List (isPrefixOf)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import TextToAscii
  ( TextConfig (..)
  , defaultTextConfig
  , fontNames
  , parseFont
  , renderText
  )

-- | Parsed command line.
data Options = Options
  { optConfig :: TextConfig
  , optText   :: Maybe String   -- ^ Nothing = read stdin
  }

parsePos :: String -> Either String Int
parsePos value = case reads value of
  [(n, "")] | n >= 0 -> Right n
  _ -> Left $ "invalid number '" ++ value ++ "' (expected a non-negative integer)"

parseArgs :: [String] -> Either String (Maybe Options)
parseArgs = go (Options defaultTextConfig Nothing)
  where
    go opts [] = pure (Just opts)
    go _ ("-h" : _) = pure Nothing
    go _ ("--help" : _) = pure Nothing
    go opts ("-f" : v : rest) = setFont opts v rest
    go opts ("--font" : v : rest) = setFont opts v rest
    go opts ("--color" : rest) = go opts { optConfig = (optConfig opts) { tcColor = True } } rest
    go opts ("--no-color" : rest) = go opts { optConfig = (optConfig opts) { tcColor = False } } rest
    go opts ("-g" : v : rest) = setGap opts v rest
    go opts ("--gap" : v : rest) = setGap opts v rest
    go _ (arg : _) | "-" `isPrefixOf` arg = Left $ "unknown option '" ++ arg ++ "'"
    go opts args = pure (Just opts { optText = Just (unwords args) })

    setFont opts v rest = case parseFont v of
      Just f  -> go opts { optConfig = (optConfig opts) { tcFont = f } } rest
      Nothing -> Left $ "invalid font '" ++ v ++ "' (expected one of: " ++ unwords fontNames ++ ")"

    setGap opts v rest = do
      n <- parsePos v
      go opts { optConfig = (optConfig opts) { tcGap = n } } rest

printUsage :: IO ()
printUsage = do
  pn <- getProgName
  putStrLn $ "Usage: " ++ pn ++ " [options] <text...>"
  putStrLn ""
  putStrLn "Options:"
  putStrLn "  -f, --font F   Built-in font: block (default), slant, outline"
  putStrLn "      --color    Colour the banner with a vertical 24-bit gradient"
  putStrLn "      --no-color Plain text (default; the engine also strips ANSI off a"
  putStrLn "                 terminal)"
  putStrLn "  -g, --gap N    Blank columns between glyphs (default 1)"
  putStrLn "  -h, --help     Show this help"
  putStrLn ""
  putStrLn "With no text argument the banner text is read from stdin."

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> do
      hPutStrLn stderr $ "Error: " ++ err
      hPutStrLn stderr "Use --help for usage information."
      exitFailure
    Right Nothing -> printUsage >> exitSuccess
    Right (Just opts) -> do
      text <- case optText opts of
        Just t  -> pure t
        Nothing -> getContents
      putStr (renderText (optConfig opts) text)