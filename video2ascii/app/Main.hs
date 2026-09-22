-- | CLI for the video-to-ASCII converter (Phase F).
--
--   Two modes, matching the two animation kinds the engine plays (D11):
--
--   * @--ambient@: 4-30 frames sampled evenly over a time window, played at
--     the window's own rate so the loop is period-exact; the loop seam is
--     checked and reported (D15). Output is a paste-ready inline YAML
--     snippet, or a frames-only JSON file with @--out@.
--   * @--cutscene@: the whole span at a chosen fps, written as a clip file
--     (D14: JSON array of multi-line strings) that the YAML references via
--     @file:@ and the worldbuilder embeds at compile time.
--
--   ffmpeg/ffprobe are external processes only (D6); nothing runs at engine
--   runtime.
module Main where

import VideoToAscii
    ( CharSet (..), VConfig (..), VideoInfo (..), defaultConfig
    , ambientYaml, clipYaml, extractFrames, grayToAscii, loopTimes
    , probeVideo, sceneTimes, seamStats, toJsonStringArray
    )

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Control.Monad (when)
import Data.Char (toLower)
import Data.List (isPrefixOf)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeBaseName)
import System.IO (hPutStrLn, stderr)

data Mode = Ambient | Cutscene deriving (Show, Eq)

data Options = Options
    { oMode    :: Mode
    , oVideo   :: FilePath
    , oFrames  :: Maybe Int    -- ^ --frames (ambient): loop length
    , oFrom    :: Maybe Double -- ^ --from: window start, seconds
    , oTo      :: Maybe Double -- ^ --to: window end, seconds
    , oFps     :: Maybe Int    -- ^ --fps: playback rate
    , oConfig  :: VConfig
    , oOut     :: Maybe FilePath
    , oClipId  :: Maybe String
    }

data PState = PState
    { psMode   :: Maybe Mode
    , psVideo  :: Maybe FilePath
    , psFrames :: Maybe Int
    , psFrom   :: Maybe Double
    , psTo     :: Maybe Double
    , psFps    :: Maybe Int
    , psCfg    :: VConfig
    , psOut    :: Maybe FilePath
    , psId     :: Maybe String
    }

printUsage :: IO ()
printUsage = do
    pn <- getProgName
    putStrLn $ "Usage: " ++ pn ++ " [options] <video-file>"
    putStrLn ""
    putStrLn "Modes (exactly one):"
    putStrLn "      --ambient     An ambient loop: 4-30 frames sampled evenly"
    putStrLn "                    over the window, played at the window's own"
    putStrLn "                    rate; the loop seam is checked and reported."
    putStrLn "      --cutscene    A cutscene clip: the whole span at --fps,"
    putStrLn "                    written as a clip file (JSON array of frames)."
    putStrLn ""
    putStrLn "Options:"
    putStrLn "      --frames N    Ambient: loop length in frames (4-30, default 16)"
    putStrLn "      --from S      Start of the sampled window in seconds"
    putStrLn "      --to S        End of the sampled window in seconds"
    putStrLn "      --fps N       Playback fps (ambient default: the window's own"
    putStrLn "                    rate; cutscene default: 12)"
    putStrLn "  -w, --width N     ASCII width in characters (default: 48)"
    putStrLn "  -c, --charset S   block, fine, standard, simple (default: standard)"
    putStrLn "  -i, --invert      Invert brightness (for dark-on-light themes)"
    putStrLn "      --out FILE    Write the frames as a clip JSON file and print"
    putStrLn "                    the YAML that references it (cutscene requires"
    putStrLn "                    this; ambient default is an inline snippet)"
    putStrLn "      --id NAME     Clip id in the printed YAML (default: video name)"
    putStrLn "  -h, --help        Show this help"
    putStrLn ""
    putStrLn "ffmpeg/ffprobe are used as external tools; the output is plain"
    putStrLn "frames the engine already plays (ambient loop or clip)."

parseCharSet :: String -> Maybe CharSet
parseCharSet s = case map toLower s of
    "block"    -> Just Block
    "fine"     -> Just Fine
    "standard" -> Just Standard
    "simple"   -> Just Simple
    _          -> Nothing

parsePos :: String -> Either String Int
parsePos value = case reads value of
    [(n, "")] | n > 0 -> Right n
    _ -> Left $ "invalid number '" ++ value ++ "' (expected a positive integer)"

parseSec :: String -> Either String Double
parseSec value = case reads value of
    [(n, "")] | n >= 0 -> Right n
    _ -> Left $ "invalid seconds '" ++ value ++ "' (expected a non-negative number)"

parseArgs :: [String] -> Either String Options
parseArgs argv = finish =<< go emptyState argv
  where
    emptyState = PState Nothing Nothing Nothing Nothing Nothing Nothing
                        defaultConfig Nothing Nothing

    finish (PState (Just m) (Just v) fr fm tw fp cfg out cid) =
        if m == Cutscene && out == Nothing
            then Left "cutscene output belongs in a clip file; add --out FILE"
            else Right (Options m v fr fm tw fp cfg out cid)
    finish (PState Nothing _ _ _ _ _ _ _ _) =
        Left "choose a mode: --ambient or --cutscene"
    finish (PState _ Nothing _ _ _ _ _ _ _) = Left "no input video given"

    withValue flag rest k = case rest of
        (value:more) -> k value more
        []           -> Left $ "option " ++ flag ++ " requires a value"

    go st [] = pure st
    go st (arg:rest) = case arg of
        "-h"         -> go st rest
        "--help"     -> go st rest
        "--ambient"  -> setMode st Ambient rest
        "--cutscene" -> setMode st Cutscene rest
        "--frames"   -> withValue arg rest (setInt (\v s -> s { psFrames = Just v }) st)
        "--from"     -> withValue arg rest (setSec (\v s -> s { psFrom = Just v }) st)
        "--to"       -> withValue arg rest (setSec (\v s -> s { psTo = Just v }) st)
        "--fps"      -> withValue arg rest (setInt (\v s -> s { psFps = Just v }) st)
        "--out"      -> withValue arg rest (\v r -> go st { psOut = Just v } r)
        "--id"       -> withValue arg rest (\v r -> go st { psId = Just v } r)
        "-w"         -> withValue arg rest (setInt setWidth st)
        "--width"    -> withValue arg rest (setInt setWidth st)
        "-c"         -> withValue arg rest (setCharset st)
        "--charset"  -> withValue arg rest (setCharset st)
        "-i"         -> go st { psCfg = (psCfg st) { vcInvert = True } } rest
        "--invert"   -> go st { psCfg = (psCfg st) { vcInvert = True } } rest
        "--"         -> go st { psVideo = Just (unwords rest) } []
        _ | "-" `isPrefixOf` arg -> Left $ "unknown option '" ++ arg ++ "'"
          | otherwise -> case psVideo st of
              Nothing -> go st { psVideo = Just arg } rest
              Just _  -> Left $ "unexpected extra input file '" ++ arg ++ "'"

    setMode st m rest = case psMode st of
        Just _  -> Left "use either --ambient or --cutscene, not both"
        Nothing -> go st { psMode = Just m } rest

    setInt k st value rest = do
        n <- parsePos value
        go (k n st) rest
    setSec k st value rest = do
        n <- parseSec value
        go (k n st) rest
    setWidth n st = st { psCfg = (psCfg st) { vcWidth = n } }
    setCharset st value rest = case parseCharSet value of
        Just cs -> go st { psCfg = (psCfg st) { vcSet = cs } } rest
        Nothing -> Left $ "invalid character set '" ++ value ++ "'"

helpRequested :: [String] -> Bool
helpRequested [] = False
helpRequested ("--":_) = False
helpRequested (arg:rest) = arg == "-h" || arg == "--help" || helpRequested rest

-- | Ambient loop bounds (D11): 4-30 frames.
ambientFramesBound :: (Int, Int)
ambientFramesBound = (4, 30)

clamp :: Int -> Int -> Int -> Int
clamp lo hi x = max lo (min hi x)

warn :: String -> IO ()
warn = hPutStrLn stderr . ("Warning: " ++)

main :: IO ()
main = do
    args <- getArgs
    when (helpRequested args) $ printUsage >> exitSuccess
    case parseArgs args of
        Left err -> do
            hPutStrLn stderr $ "Error: " ++ err
            hPutStrLn stderr "Use --help for usage information."
            exitFailure
        Right opts -> run opts

run :: Options -> IO ()
run opts = do
    probed <- probeVideo (oVideo opts)
    case probed of
        Left err -> hPutStrLn stderr ("Error: " ++ err) >> exitFailure
        Right vi -> case windowSpan opts vi of
            Left err -> hPutStrLn stderr ("Error: " ++ err) >> exitFailure
            Right (from, to) -> case oMode opts of
                Ambient  -> runAmbient opts vi from to
                Cutscene -> runCutscene opts vi from to

-- | The sampled window: --from/--to default to the whole video; --to beyond
--   the video's duration is an error (the loop would stall on missing frames).
windowSpan :: Options -> VideoInfo -> Either String (Double, Double)
windowSpan opts vi
    | to <= from = Left $ "empty window: --to " ++ show to
                        ++ " must be after --from " ++ show from
    | viDuration vi > 0 && to > viDuration vi + 0.05 =
        Left $ "window end " ++ show to ++ " is beyond the video duration "
             ++ show (viDuration vi)
    | otherwise = Right (from, to)
  where
    from = maybe 0 id (oFrom opts)
    to = maybe (viDuration vi) id (oTo opts)

-- | ffprobe + ffmpeg per frame: the raw frames plus their ASCII rendering.
convert :: Options -> VideoInfo -> [Double]
         -> IO (Either String ([ByteString], [[String]]))
convert opts vi times = do
    rawE <- extractFrames (oVideo opts) vi times
    pure $ case rawE of
        Left err -> Left err
        Right raw
            | any BS.null raw -> Left "a frame came back empty"
            | otherwise -> Right
                ( raw
                , map (grayToAscii (oConfig opts) (viWidth vi) (viHeight vi)) raw )

videoId :: Options -> String
videoId opts = maybe (takeBaseName (oVideo opts)) id (oClipId opts)

-- | Ambient loop: even sampling, the window's own rate, seam report (D15).
runAmbient :: Options -> VideoInfo -> Double -> Double -> IO ()
runAmbient opts vi from to = do
    n <- case oFrames opts of
        Just k | k < lo || k > hi -> do
            warn $ "--frames " ++ show k ++ " is outside the ambient range "
                 ++ show lo ++ "-" ++ show hi ++ "; using "
                 ++ show (clamp lo hi k) ++ " (D11: ambient loops are 4-30 frames)"
            pure (clamp lo hi k)
        Just k -> pure k
        Nothing -> pure 16
    let dt = (to - from) / fromIntegral n :: Double
        ownFps = max 1 (round (1 / dt) :: Int)
        fps = maybe (clamp 1 30 ownFps) id (oFps opts)
    when (oFps opts == Nothing && ownFps > 30) $
        warn $ "the window's own rate would be " ++ show ownFps
             ++ " fps; clamped to 30 (the loop plays slower than the source)"
    converted <- convert opts vi (loopTimes from to n)
    case converted of
        Left err -> hPutStrLn stderr ("Error: " ++ err) >> exitFailure
        Right (raw, arts) -> do
            emitOutput opts arts fps
            reportSeam raw
  where (lo, hi) = ambientFramesBound

-- | Cutscene clip: whole span at --fps, clip file output (D14).
runCutscene :: Options -> VideoInfo -> Double -> Double -> IO ()
runCutscene opts vi from to = do
    let fps = maybe 12 id (oFps opts)
        times = sceneTimes from to fps
    when (length times > 240) $
        warn $ "the clip has " ++ show (length times)
             ++ " frames; consider a shorter span or lower --fps for a snappy cutscene"
    converted <- convert opts vi times
    case converted of
        Left err -> hPutStrLn stderr ("Error: " ++ err) >> exitFailure
        Right (_, arts) -> emitOutput opts arts fps

-- | Write/print the material: --out writes the clip JSON file (D14) and
--   prints the YAML that references it; otherwise print the inline YAML.
emitOutput :: Options -> [[String]] -> Int -> IO ()
emitOutput opts arts fps = case oOut opts of
    Just file -> do
        -- Frame strings carry the trailing newline the YAML block-scalar
        -- route produces, so both material routes compile identically.
        let frameStrings = map unlines arts
        writeFile file (toJsonStringArray frameStrings ++ "\n")
        putStr (clipYaml 0 (videoId opts) file fps)
        hPutStrLn stderr ("Wrote " ++ show (length frameStrings) ++ " frames to "
                          ++ file ++ " (playback: " ++ show fps ++ " fps)")
    Nothing -> putStr (ambientYaml 0 arts fps)

-- | The D15 loop-seam report, on the already extracted raw frames.
reportSeam :: [ByteString] -> IO ()
reportSeam raw = do
    let (avg, seam, ratio) = seamStats raw
    hPutStrLn stderr $ "Loop seam: last/first frame distance "
        ++ showF seam ++ " vs average consecutive " ++ showF avg
        ++ " (ratio " ++ showF ratio ++ ")"
    when (ratio > 2) $ warn
        "the loop seam is more than twice as visible as a normal frame step;\
        \ cut the window harder or pick a section that starts where it ends"

showF :: Double -> String
showF x = show (fromIntegral (round (x * 1000) :: Int) / 1000 :: Double)
