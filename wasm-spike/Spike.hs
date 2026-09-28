-- | WASM spike main (Plan 1.0). NOT production code.
--
--   Proves that the pure engine core (Types, Game, Parser, GameLoop,
--   Effects, Cards, Combat, Quests, Vehicles, …) builds with the GHC WASM
--   backend and that 'applyLoopCommand' runs inside a WASI runtime:
--
--   1. drives the pure in-code sample game through several commands,
--   2. loads a real compiled adventure (worldbuilder world.json + save.json)
--      via aeson and drives it through take/room-change/inventory commands,
--   3. prints a structured summary so the runtime can be verified
--      byte-comparably against a native build of the same program.
module Main (main) where

import qualified Data.Set as Set

import Types
import GameLoop (LoopState (..), applyLoopCommand, initLoopState)
import Parser (parseCommandWith)
import Sample (initSampleGame)
import World (loadGame)
import System.Exit (exitSuccess)
import System.IO (BufferMode (LineBuffering), hSetBuffering, hSetEncoding, stdout, stderr, utf8)

-- | Drive one command through the pure core and print the engine's answer.
runCmd :: GameState -> String -> IO GameState
runCmd st input = do
    let cmd = parseCommandWith (verbDefs (world st)) input
        (ls', out) = applyLoopCommand cmd (initLoopState st)
        st' = lsCurrent ls'
    putStrLn ("    > " ++ input)
    mapM_ (putStrLn . ("      " ++)) (lines out)
    pure st'

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    hSetBuffering stdout LineBuffering
    putStrLn "=== WASM-SPIKE: applyLoopCommand unter wasm32-wasi ==="

    -- Part 1: pure in-code sample game.
    putStrLn "[1] Sample-Spiel (initSampleGame, rein):"
    s1 <- runCmd initSampleGame "look"
    s2 <- runCmd s1 "take torch"
    s3 <- runCmd s2 "inventory"
    s4 <- runCmd s3 "go north"
    _ <- runCmd s4 "help"
    putStrLn "[1] OK"

    -- Part 2: a real compiled adventure from the worldbuilder, if present.
    putStrLn "[2] Kompiliertes Adventure (data/world.json):"
    result <- loadGame "data/world.json" (Just "data/save.json")
    case result of
        Left err -> putStrLn ("    UEBERSPRUNGEN: " ++ err)
        Right st0 -> do
            st1 <- runCmd st0 "look"        -- dark: blocked
            st2 <- runCmd st1 "take relic"  -- dark: blocked (not feelable)
            st3 <- runCmd st2 "take torch"  -- feelable: works
            st4 <- runCmd st3 "inventory"
            st5 <- runCmd st4 "use lever"
            st6 <- runCmd st5 "look"        -- cellar lit now
            putStrLn ("    turns after the run: " ++ show (turnCount (save st6)))
            putStrLn ("    visited rooms: " ++ show (Set.size (visitedRooms (save st6))))
            putStrLn "[2] OK"

    putStrLn "=== SPIKE LAEUFT: applyLoopCommand unter wasm32-wasi ausgefuehrt ==="
    exitSuccess