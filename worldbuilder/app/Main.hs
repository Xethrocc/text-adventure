-- | Entry point for the worldbuilder CLI tool
module Main where

import Worldbuilder.CLI (runCLI)

main :: IO ()
main = runCLI