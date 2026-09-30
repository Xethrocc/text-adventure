-- | Content tests as data (B1): `tests:` sections authored inside the
--   adventure YAML, executed by `worldbuilder test` against the compiled
--   world. Marker semantics: **ordered** — every marker must appear in the
--   rendered output, in the declared order (subsequence). The runner is
--   pure (no IO per command, no save files): `save`/`load` produce their
--   messages but touch no disk.
module Worldbuilder.Test
    ( checkMarkers
    , executeContentTest
    , runContentTests
    ) where

import Data.List (isInfixOf, isPrefixOf)

import Types as E
import GameLoop (LoopState (..), initLoopState, applyLoopCommandEv)
import Parser (Command (..), parseCommandWith)
import Worldbuilder.Types (AContentTest (..), Adventure (..))
import Worldbuilder.Compile (CompileResult (..), CompileIssue (..), compileAdventure)
import Worldbuilder.ParseFile (parseAdventureFile)

-- | Ordered marker check (subsequence semantics): returns the first marker
--   that was not reached after its predecessors, or 'Nothing' when all
--   markers were reached. Empty markers are skipped.
checkMarkers :: String -> [String] -> Maybe String
checkMarkers out = go out
  where
    go _ [] = Nothing
    go s (m:rest)
        | null m    = go s rest
        | otherwise = case findAfter m s of
            Nothing -> Just m
            Just s' -> go s' rest

-- | Remainder of the haystack after the first occurrence of the needle.
findAfter :: String -> String -> Maybe String
findAfter needle = go
  where
    go [] = Nothing
    go s@(_:xs)
        | needle `isPrefixOf` s = Just (drop (length needle) s)
        | otherwise             = go xs

-- | Execute one authored test against a compiled world/save pair. Returns
--   the first marker that was not reached (in order), or 'Nothing' on
--   success. Mirrors the live loop: the initial `look` opens the session and
--   commands are parsed with the adventure's own verbs (`parseCommandWith`).
--   Commands after game over are not fed (their messages would differ from a
--   live session; markers must be reached before the end).
executeContentTest :: AContentTest -> E.GameWorld -> E.SaveState -> Maybe String
executeContentTest ct gw sv =
    checkMarkers (concat (go (initLoopState st0) commands)) (actExpect ct)
  where
    st0 = E.GameState gw sv Nothing Nothing Nothing [] Nothing [] Nothing Nothing []
    commands = Look : map (parseCommandWith (E.verbDefs gw)) (actInput ct)
    go _ [] = []
    go ls (cmd:rest)
        | E.gameOver (E.save (lsCurrent ls)) = []
        | otherwise =
            let (ls', evs) = applyLoopCommandEv cmd ls
            in renderEvents evs : go ls' rest

-- | Run the content tests of one adventure file. Prints a CI-style report
--   (one line per test) and returns the number of failures (0 = success).
--   With a name filter, only tests whose name contains the filter run.
runContentTests :: FilePath -> Maybe String -> IO Int
runContentTests path mFilter = do
    parsed <- parseAdventureFile path
    case parsed of
        Left err -> do
            putStrLn ("FAIL " ++ path ++ "  (parse error: " ++ show err ++ ")")
            pure 1
        Right adv -> case compileAdventure adv of
            Left errs -> do
                putStrLn ("FAIL " ++ path ++ "  (compile errors: "
                          ++ unwords [ciCode e | e <- errs] ++ ")")
                pure 1
            Right cr -> do
                let tests = [ ct | ct <- advTests adv
                            , maybe True (`isInfixOf` actName ct) mFilter ]
                if null tests
                    then do
                        putStrLn ("OK   " ++ path ++ "  (no content tests)")
                        pure 0
                    else sum <$> mapM (runOne (crWorld cr) (crSave cr)) tests
  where
    runOne gw sv ct = case executeContentTest ct gw sv of
        Nothing -> do
            putStrLn ("OK   " ++ actName ct
                      ++ "  (" ++ show (length (actExpect ct)) ++ " markers)")
            pure 0
        Just missing -> do
            putStrLn ("FAIL " ++ actName ct
                      ++ "  (marker not reached: " ++ show missing ++ ")")
            pure 1
