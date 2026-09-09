-- | Placeholder entry point for the worldbuilder CLI (see Worldbuilder.Placeholder)
module Main where

import Worldbuilder.Placeholder (placeholderNote)
import System.Environment (getArgs)

main :: IO ()
main = do
    _ <- getArgs
    putStr placeholderNote
