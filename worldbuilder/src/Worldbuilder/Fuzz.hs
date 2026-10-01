-- | Content fuzzer (B5, Tuers III — Werkzeuge & Nachweise): seeded random and
--   heuristic command streams against a compiled adventure, hunting for
--   crashes, non-terminating steps and frozen loops ("Endlosschleifen").
--
--   Design invariants:
--
--   * Pure and deterministic: the same seed yields the same runs and the same
--     findings. Every finding reports its exact input sequence so it can be
--     replayed verbatim (`worldbuilder fuzz ... --replay <file>`).
--   * No engine changes: the fuzzer drives the same pure stepping core as the
--     live loop ('applyLoopCommandEv'), mirroring 'Worldbuilder.Test'. The
--     initial `look` opens every run exactly like a live session.
--   * Per-step wall-clock timeout: a step that never returns is a finding
--     ("Haenger"). Sync exceptions become crash findings; the async timeout
--     exception is deliberately not swallowed as a crash.
--   * Frozen-loop finding: `window` consecutive steps leave the state (turn
--     counter included) byte-identical although at least one command was
--     turn-shaped ('consumesTurnIn') — the engine was asked to advance the
--     game and a veto (or refusal) blocked every action. That is the
--     soft-lock signature; ordinary no-op input (look/help/unknown) can never
--     form a window because it never claims to advance the clock.
--   * Runs continue past game over (the pure core accepts commands there and
--     it is crash surface), but those steps never feed the loop detector.
module Worldbuilder.Fuzz
    ( FuzzConfig (..)
    , defaultFuzzConfig
    , FindingKind (..)
    , FuzzFinding (..)
    , FuzzVocab (..)
    , fuzzVocab
    , runSeedFor
    , genInputs
    , frozenWindow
    , fuzzRun
    , runFuzzer
    ) where

import Control.Exception (SomeAsyncException (..), SomeException, displayException,
                          evaluate, fromException, tryJust)
import Data.Char (toLower)
import Data.List (intercalate, isPrefixOf, nub)
import Data.Word (Word64)
import qualified Data.Map.Strict as Map
import System.Timeout (timeout)

import qualified Types as E
import GameLoop (LoopState (..), applyLoopCommandEv, consumesTurnIn, initLoopState)
import Parser (parseCommandWith)
import Worldbuilder.Compile (CompileResult (..), CompileIssue (..), compileAdventure)
import Worldbuilder.ParseFile (parseAdventureFile)
import Worldbuilder.Rng (Rng, newRng, rngGolden, stepRng)

-- ---------------------------------------------------------------------------
-- Configuration and findings
-- ---------------------------------------------------------------------------

-- | Knobs of one fuzzing session.
data FuzzConfig = FuzzConfig
    { fcPath      :: FilePath        -- ^ adventure file (yaml/json)
    , fcSeed      :: Word64          -- ^ base seed; run i derives its own seed
    , fcRuns      :: Int             -- ^ number of independent runs
    , fcSteps     :: Int             -- ^ command budget per run
    , fcTimeoutMs :: Int             -- ^ wall clock budget per single step
    , fcWindow    :: Int             -- ^ frozen-loop window length (steps)
    , fcReplay    :: Maybe FilePath  -- ^ replay file (one command per line) instead of generated runs
    } deriving (Show, Eq)

-- | Defaults: seed 42 (repo convention), 10 runs of 120 steps, 2s per step,
--   frozen window of 25 steps.
defaultFuzzConfig :: FilePath -> FuzzConfig
defaultFuzzConfig path = FuzzConfig
    { fcPath      = path
    , fcSeed      = 42
    , fcRuns      = 10
    , fcSteps     = 120
    , fcTimeoutMs = 2000
    , fcWindow    = 25
    , fcReplay    = Nothing
    }

-- | What went wrong in a run.
data FindingKind
    = FCrash   -- ^ the step raised a (sync) exception
    | FHang    -- ^ the step never returned within the timeout
    | FLoop    -- ^ frozen window: no state/turn progress over `fcWindow` steps
    deriving (Show, Eq)

-- | One reproducible finding. 'ffInputs' is the full command sequence of the
--   run up to and including 'ffStep' — feed it via `--replay` to reproduce.
data FuzzFinding = FuzzFinding
    { ffKind   :: FindingKind
    , ffRun    :: Int        -- ^ 1-based run index (0 = replay)
    , ffStep   :: Int        -- ^ 1-based step of the run (0 = initial look)
    , ffSeed   :: Word64     -- ^ seed of this run
    , ffInput  :: String     -- ^ the command that triggered the finding
    , ffDetail :: String     -- ^ human readable detail
    , ffInputs :: [String]   -- ^ replay sequence (initial look excluded)
    } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Vocabulary extraction
-- ---------------------------------------------------------------------------

-- | Input words the generator draws from: verbs, target words and directions.
data FuzzVocab = FuzzVocab
    { fvActions :: [String]
    , fvNouns   :: [String]
    , fvDirs    :: [String]
    } deriving (Show, Eq)

-- | Extract the generator vocabulary from a compiled world: action words
--   (core verbs + adventure verbs/aliases), target words (items, NPCs,
--   containers, vehicles, dialogue topics — names and keywords) and the
--   standard directions. All lower-cased, duplicates removed, order stable
--   ('Map.elems' is sorted — declaration order is not a contract here).
fuzzVocab :: E.GameWorld -> FuzzVocab
fuzzVocab gw = FuzzVocab
    { fvActions = uniqueWords (coreActionWords ++ adventureVerbWords)
    , fvNouns   = uniqueWords (itemWords ++ npcWords ++ topicWords
                               ++ containerWords ++ vehicleWords)
    , fvDirs    = uniqueWords (map lower coreDirections)
    }
  where
    adventureVerbWords = concat [ map lower (E.vdName v : E.vdAliases v)
                                | v <- Map.elems (E.verbDefs gw) ]
    itemWords = concat [ map lower (E.itemName d : E.itemKeywords d)
                       | d <- Map.elems (E.itemDefs gw) ]
    npcWords  = concat [ map lower (E.npcName d : E.npcKeywords d)
                       | d <- Map.elems (E.npcDefs gw) ]
    topicWords = uniqueWords (concat [ Map.keys (E.npcTopics d)
                                     | d <- Map.elems (E.npcDefs gw) ])
    containerWords = [ lower (E.conName c) | c <- Map.elems (E.containerDefs gw) ]
    vehicleWords   = [ lower (E.vehicleName v) | v <- Map.elems (E.vehicleDefs gw) ]

-- | Core action words the parser understands (English + German aliases).
coreActionWords :: [String]
coreActionWords =
    [ "take", "nimm", "drop", "lege", "use", "benutze"
    , "open", "oeffne", "close", "schliesse", "lock", "unlock"
    , "look", "schaue", "inventory", "inventar", "search", "untersuche"
    , "attack", "attackiere", "talk", "sprich", "ask", "frage"
    , "tell", "erzaehle", "give", "gib", "equip", "unequip"
    , "status", "bilanz", "finanzen", "wait", "warte"
    ]

-- | The eight cardinal/relative directions the engine routes on.
coreDirections :: [String]
coreDirections = ["north", "south", "east", "west", "up", "down", "in", "out"]

lower :: String -> String
lower = map toLower

uniqueWords :: [String] -> [String]
uniqueWords = nub . filter (not . null)

-- ---------------------------------------------------------------------------
-- Deterministic input generation
-- ---------------------------------------------------------------------------

-- | Infinite, purely generated input stream for one run.
genInputs :: Word64 -> FuzzVocab -> [String]
genInputs seed vocab = go (newRng seed)
  where
    go rng = let (cmd, rng') = genInput vocab rng in cmd : go rng'

-- | One generated input line. Bucket layout (deterministic, seed-driven):
--   15% garbage (parser fuzz), 10% movement, 15% bare action, 25% action +
--   target, 15% two-target forms, 8% ask/tell, 6% system commands, 6% vehicle
--   and compound forms.
genInput :: FuzzVocab -> Rng -> (String, Rng)
genInput vocab rng0
    | bucket < 15 = garbageInput rng1
    | bucket < 25 = pickOne (map (\d -> "go " ++ d) dirs ++ dirs) rng1
    | bucket < 40 = pickOne actions rng1
    | bucket < 65 = let (v, rngA)  = drawFrom actions rng1
                        (n, rngB)  = drawFrom nouns rngA
                    in (v ++ " " ++ n, rngB)
    | bucket < 80 = let (tpl, rngA) = drawFrom twoTargetTemplates rng1
                        (n1, rngB)  = drawFrom nouns rngA
                        (n2, rngC)  = drawFrom nouns rngB
                    in (tpl n1 n2, rngC)
    | bucket < 88 = let (q, rngA)  = drawFrom ["ask", "tell", "frage", "erzaehle"] rng1
                        (n, rngB)  = drawFrom nouns rngA
                        (t, rngC)  = drawFrom nouns rngB
                    in (q ++ " " ++ n ++ " about " ++ t, rngC)
    | bucket < 94 = pickOne systemCommands rng1
    | otherwise   = let (tpl, rngA) = drawFrom vehicleTemplates rng1
                        (n1, rngB)  = drawFrom nouns rngA
                        (n2, rngC)  = drawFrom nouns rngB
                        (v2, rngD)  = drawFrom actions rngC
                    in (tpl n1 n2 v2, rngD)
  where
    (bucket, rng1) = drawInt 100 rng0
    actions = nonEmptyList "look" (fvActions vocab)
    nouns   = nonEmptyList "ding" (fvNouns vocab)
    dirs    = nonEmptyList "north" (fvDirs vocab)

-- | Two-target verb forms: use X on Y, take X from Y, put X in Y, give X to Y.
twoTargetTemplates :: [String -> String -> String]
twoTargetTemplates =
    [ \a b -> "use " ++ a ++ " on " ++ b
    , \a b -> "take " ++ a ++ " from " ++ b
    , \a b -> "put " ++ a ++ " in " ++ b
    , \a b -> "give " ++ a ++ " to " ++ b
    ]

-- | Vehicle forms plus one compound ("X and Y") shape. The third drawn word
--   only matters for the compound template.
vehicleTemplates :: [String -> String -> String -> String]
vehicleTemplates =
    [ \a _ _   -> "enter " ++ a
    , \_ _ _   -> "exit"
    , \a _ _   -> "drive to " ++ a
    , \a _ _   -> "refuel " ++ a
    , \a _ _   -> "repair " ++ a
    , \a b v   -> v ++ " " ++ a ++ " and " ++ v ++ " " ++ b
    ]

-- | Non-target commands (system, cards, bulk, meta).
systemCommands :: [String]
systemCommands =
    [ "1", "2", "3", "4", "5"
    , "play 1", "play 2", "play 3"
    , "hand", "deck", "discard"
    , "wait", "undo", "map", "journal", "stats", "help"
    , "look", "inventory", "search"
    , "take all", "drop all", "unequip all"
    , "save fuzz", "load fuzz", "restart", "quit"
    ]

-- | Fixed nonsense words for parser fuzzing (kept closed and deterministic so
--   findings stay reproducible).
garbageTokens :: [String]
garbageTokens =
    [ "xyzzy", "plugh", "foo", "bar", "baz", "asdf", "qwe"
    , "", "take take take", "ask about about", "the the the"
    , "x", "!!!", "go go", "use use", "a b c d e", "nimm nimm", "look look look"
    ]

garbageInput :: Rng -> (String, Rng)
garbageInput rng0 =
    let (count, rng1) = drawInt 3 rng0
        (parts, rng2) = drawMany (count + 1) garbageTokens rng1
    in (unwords parts, rng2)

-- | Draw @n@ elements (with repetition) from a non-empty list.
drawMany :: Int -> [a] -> Rng -> ([a], Rng)
drawMany n xs rng
    | n <= 0    = ([], rng)
    | otherwise = let (x, rngA)  = drawFrom xs rng
                      (rest, rngB) = drawMany (n - 1) xs rngA
                  in (x : rest, rngB)

pickOne :: [String] -> Rng -> (String, Rng)
pickOne = drawFrom

drawFrom :: [a] -> Rng -> (a, Rng)
drawFrom xs rng = let (i, rng') = drawInt (length xs) rng in (xs !! i, rng')

nonEmptyList :: a -> [a] -> [a]
nonEmptyList fallback xs = if null xs then [fallback] else xs

-- | Uniform value in @[0, n)@, threaded SplitMix64 state.
drawInt :: Int -> Rng -> (Int, Rng)
drawInt n rng =
    let (w, rng') = stepRng rng
        modulus = fromIntegral (max 1 n) :: Word64
    in (fromIntegral (w `mod` modulus), rng')

-- | Per-run seed derivation: base seed plus golden-ratio stride, so run i is
--   independent of how many runs executed before it.
runSeedFor :: Word64 -> Int -> Word64
runSeedFor base i = base + fromIntegral i * rngGolden

-- ---------------------------------------------------------------------------
-- Frozen-window detector
-- ---------------------------------------------------------------------------

-- | State key for the frozen-window detector: the serialized save state with
--   the volatile per-command echo variables (@cmd.*@) stripped. The engine
--   rewrites them for every command (they mirror the input), so keeping them
--   would mask a completely frozen game state as "progress".
stateKey :: E.SaveState -> String
stateKey sv =
    show (sv { E.variables = Map.filterWithKey (\k _ -> not ("cmd." `isPrefixOf` k))
                                      (E.variables sv) })

-- | Does the newest-first trace (input, state key, turn-shaped) end in a
--   frozen window of @k@ steps? True when the last @k@ steps share one state
--   key, at least one of them was turn-shaped (the engine was asked to
--   advance) and at least three distinct inputs appear (one spammed command
--   is not a loop).
frozenWindow :: Int -> [(String, String, Bool)] -> Bool
frozenWindow k traceRev
    | k < 2              = False
    | length win < k     = False
    | otherwise          = sameKey && any turnShaped win && length (nub [ i | (i, _, _) <- win ]) >= 3
  where
    win = take k traceRev
    sameKey = case win of
        []               -> False
        ((_, key0, _) : _) -> all (\(_, keyI, _) -> keyI == key0) win
    turnShaped (_, _, t) = t

-- ---------------------------------------------------------------------------
-- The stepping core
-- ---------------------------------------------------------------------------

-- | Why one step failed.
data StepFailure
    = StepHang
    | StepCrash String
    deriving (Show, Eq)

-- | One successful step: the new loop state, its rendered text and the
--   turn-shaped judgment for the frozen-window detector.
data StepResult = StepResult
    { srLoop       :: LoopState
    , srText       :: String
    , srTurnShaped :: Bool
    } deriving (Show, Eq)

-- | Force one step to normal form on the fields a finding depends on, so
--   both crashes inside the parse/apply/render path and hangs anywhere in
--   that path are attributed to exactly this input.
forceStep :: LoopState -> String -> StepResult
forceStep ls inp =
    let cmd = parseCommandWith (E.verbDefs (E.world (lsCurrent ls))) inp
        (ls', evs) = applyLoopCommandEv cmd ls
        turnShaped = consumesTurnIn (lsCurrent ls) cmd
        result = StepResult ls' (E.renderEvents evs) turnShaped
        forced = length (srText result)
               + length (show (E.save (lsCurrent ls')))
               + (if E.world (lsCurrent ls') == E.world (lsCurrent ls)
                    then 0
                    else length (show (E.world (lsCurrent ls'))))
               + (if turnShaped then 1 else 0)
    in forced `seq` result

-- | Only sync exceptions are crashes; the async timeout exception must fly
--   out of 'tryJust' so 'timeout' can turn it into 'StepHang'.
syncException :: SomeException -> Maybe SomeException
syncException exn = case fromException exn :: Maybe SomeAsyncException of
    Just _  -> Nothing
    Nothing -> Just exn

-- | Run one step under the wall-clock budget.
stepIO :: Int -> LoopState -> String -> IO (Either StepFailure StepResult)
stepIO micros ls inp = do
    outcome <- timeout micros (tryJust syncException (evaluate (forceStep ls inp)))
    pure $ case outcome of
        Nothing             -> Left StepHang
        Just (Left exn)     -> Left (StepCrash (displayException exn))
        Just (Right result) -> Right result

-- ---------------------------------------------------------------------------
-- Run driver
-- ---------------------------------------------------------------------------

-- | Execute one fuzz run: the initial `look`, then the given commands. Stops
--   at the first finding (its input prefix is the reproduction sequence) or
--   when the step budget / input list is exhausted.
fuzzRun :: Int         -- ^ per-step timeout in microseconds
        -> Int         -- ^ step budget
        -> Int         -- ^ frozen-window length
        -> Int         -- ^ run index (reported)
        -> Word64      -- ^ run seed (reported)
        -> E.GameWorld
        -> E.SaveState
        -> [String]    -- ^ input lines
        -> IO (Maybe FuzzFinding)
fuzzRun micros maxSteps windowK runIdx runSeed gw sv inputs = do
    opened <- stepIO micros (initLoopState st0) "look"
    case opened of
        Left failed -> pure (Just (mkFinding failed 0 "look" []))
        Right sr0   -> go 1 (srLoop sr0) [] [] (take maxSteps inputs)
  where
    st0 = E.GameState gw sv Nothing Nothing Nothing [] Nothing [] Nothing Nothing []

    go _ _ _ _ [] = pure Nothing
    go n ls accRev traceRev (inp : restInputs) = do
        stepped <- stepIO micros ls inp
        case stepped of
            Left failed -> pure (Just (mkFinding failed n inp accRev))
            Right sr -> do
                let ls' = srLoop sr
                    finished = E.gameOver (E.save (lsCurrent ls'))
                    entry = (inp, stateKey (E.save (lsCurrent ls')), srTurnShaped sr)
                    trace' = if finished then [] else entry : traceRev
                if frozenWindow windowK trace'
                    then pure (Just (loopFinding n inp accRev))
                    else go (n + 1) ls' (inp : accRev) trace' restInputs

    mkFinding failed n inp accRev = FuzzFinding
        { ffKind   = kindOf failed
        , ffRun    = runIdx
        , ffStep   = n
        , ffSeed   = runSeed
        , ffInput  = inp
        , ffDetail = detailOf failed
        , ffInputs = if n == 0 then [] else reverse (inp : accRev)
        }

    loopFinding n inp accRev = FuzzFinding
        { ffKind   = FLoop
        , ffRun    = runIdx
        , ffStep   = n
        , ffSeed   = runSeed
        , ffInput  = inp
        , ffDetail = "kein Fortschritt: Zustand und Zugzahl ueber " ++ show windowK
                     ++ " Schritte unveraendert (Endlosschleife/soft lock)"
        , ffInputs = reverse (inp : accRev)
        }

    kindOf StepHang        = FHang
    kindOf (StepCrash _)   = FCrash

    detailOf StepHang = "Schritt kehrt nicht zurueck (Timeout nach " ++ show micros ++ "us)"
    detailOf (StepCrash msg) = msg

-- ---------------------------------------------------------------------------
-- Session driver
-- ---------------------------------------------------------------------------

-- | Run one fuzzing session (or a replay) against an adventure file. Returns
--   the number of findings (0 = clean); prints a CI-style report.
runFuzzer :: FuzzConfig -> IO Int
runFuzzer cfg = do
    parsed <- parseAdventureFile (fcPath cfg)
    case parsed of
        Left err -> do
            putStrLn ("FAIL " ++ fcPath cfg ++ "  (parse error: " ++ show err ++ ")")
            pure 1
        Right adv -> case compileAdventure adv of
            Left errs -> do
                putStrLn ("FAIL " ++ fcPath cfg ++ "  (compile errors: "
                          ++ unwords [ciCode e | e <- errs] ++ ")")
                pure 1
            Right cr -> runAgainst cfg (crWorld cr) (crSave cr)

runAgainst :: FuzzConfig -> E.GameWorld -> E.SaveState -> IO Int
runAgainst cfg gw sv = case fcReplay cfg of
    Just file -> do
        cmds <- lines <$> readFile file
        found <- fuzzRun micros (fcSteps cfg) (fcWindow cfg) 0 (fcSeed cfg) gw sv cmds
        mapM_ (reportFinding (fcPath cfg) (fcSeed cfg)) found
        case found of
            Nothing -> do
                putStrLn ("OK   " ++ fcPath cfg ++ "  (replay: "
                          ++ show (length cmds) ++ " commands, 0 findings)")
                pure 0
            Just _  -> do
                putStrLn ("FUND " ++ fcPath cfg ++ "  (replay: "
                          ++ show (length cmds) ++ " commands, 1 finding)")
                pure 1
    Nothing -> do
        found <- mapM runOne [1 .. max 0 (fcRuns cfg)]
        let findings = [ f | Just f <- found ]
        mapM_ (reportFinding (fcPath cfg) (fcSeed cfg)) findings
        let summary = show (fcRuns cfg) ++ " runs x " ++ show (fcSteps cfg)
                      ++ " steps, seed " ++ show (fcSeed cfg)
        if null findings
            then do
                putStrLn ("OK   " ++ fcPath cfg ++ "  (" ++ summary ++ ", 0 findings)")
                pure 0
            else do
                putStrLn ("FUND " ++ fcPath cfg ++ "  (" ++ summary ++ ", "
                          ++ show (length findings) ++ " findings)")
                pure (length findings)
  where
    micros = fcTimeoutMs cfg * 1000
    runOne i = fuzzRun micros (fcSteps cfg) (fcWindow cfg) i (runSeedFor (fcSeed cfg) i)
                       gw sv (genInputs (runSeedFor (fcSeed cfg) i) (fuzzVocab gw))

-- | CI-style finding report with the reproduction recipe (base seed + run
--   index + step count re-derive the exact run).
reportFinding :: FilePath -> Word64 -> FuzzFinding -> IO ()
reportFinding src baseSeed f = do
    putStrLn ("FUND " ++ src ++ "  " ++ kindName (ffKind f)
              ++ " run=" ++ show (ffRun f) ++ " step=" ++ show (ffStep f)
              ++ " seed=" ++ show (ffSeed f))
    putStrLn ("     " ++ ffDetail f)
    putStrLn ("     Eingaben (" ++ show (length (ffInputs f)) ++ "): " ++ inputsLine (ffInputs f))
    if ffRun f > 0
        then putStrLn ("     Nachstellen: worldbuilder fuzz " ++ src ++ " --seed " ++ show baseSeed
                       ++ " --runs " ++ show (ffRun f) ++ " --steps " ++ show (ffStep f))
        else pure ()
    putStrLn "                  oder --replay <datei mit genau diesen Eingaben, eine pro Zeile>"
  where
    kindName FCrash = "Absturz"
    kindName FHang  = "Haenger"
    kindName FLoop  = "Endlosschleife"
    inputsLine cmds
        | length flat <= 300 = flat
        | otherwise = "\n" ++ intercalate "\n" [ "       > " ++ c | c <- cmds ]
      where
        flat = intercalate " | " cmds
