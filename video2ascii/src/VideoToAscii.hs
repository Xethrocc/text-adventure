{-# LANGUAGE BangPatterns #-}

-- | Video to ASCII frames for the text-adventure engine (Phase F).
--
--   The pipeline (D6): ffprobe reads geometry and timing, ffmpeg extracts
--   selected frames as raw grayscale, this module turns them into ASCII lines.
--   ffmpeg/ffprobe are external processes and are used only by this tool —
--   nothing at engine runtime. The result is plain frames in the exact shape
--   Phase H already plays: an inline @ambient@ loop (D11: 4–30 frames at the
--   ambient rate) or a clip file (D14: a JSON array of multi-line strings the
--   worldbuilder embeds at compile time).
--
--   Two matters carried over from img2ascii (D9: deliberate copy, no engine
--   or img2ascii dependency):
--
--   1. **Geometry.** A terminal cell is about twice as tall as it is wide, so
--      @rowsForWidth@ halves the aspect (see img2ascii's identical helper).
--   2. **Downscaling.** Each target cell averages the source box it covers
--      instead of sampling a single pixel — video noise would flicker frame
--      to frame otherwise.
--
--   Loop seam (D15): an ambient loop is sampled evenly over @[from, to)@, so
--   the period is exactly @to - from@ and the first frame follows the last
--   naturally. How well that actually reads is reported by 'seamStats': the
--   last frame's distance to the first, relative to the average distance of
--   consecutive frames. A ratio near 1 means the seam looks like any other
--   frame step; well above 2 means the cut is visible (warn, and the author
--   cuts the material differently — the plan's escape hatch).
module VideoToAscii
  ( -- * Rendering configuration
    VConfig (..)
  , CharSet (..)
  , defaultConfig
  , charsForSet
    -- * Pure frame conversion
  , rowsForWidth
  , grayToAscii
    -- * Pure loop-seam analysis
  , frameDistance
  , seamStats
    -- * Pure time selection
  , loopTimes
  , sceneTimes
    -- * Output formats (pure)
  , toJsonStringArray
  , ambientYaml
  , clipYaml
    -- * External-process pipeline (ffmpeg/ffprobe)
  , VideoInfo (..)
  , probeVideo
  , extractFrame
  , extractFrames
  , ffmpegAvailable
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (chr, isDigit, ord)
import Data.List (intercalate)
import Data.Word (Word8)
import Numeric (showHex)
import System.Exit (ExitCode (..))
import System.Process (createProcess, proc, readProcessWithExitCode,
                       waitForProcess, CreateProcess (..), StdStream (..))

-- | Predefined character sets (copied from img2ascii, D9). The ramp runs
--   from dark (first character) to bright (last character).
data CharSet = Block     -- ^ ░▒▓█ (4 levels, high contrast)
             | Fine      -- ^ ░▒▓█▄▀■ (7 levels)
             | Standard  -- ^ \" .:-=+*#%@\" (10 levels, balanced)
             | Simple    -- ^ \" #.\" (3 levels, minimal)
             deriving (Show, Eq, Enum, Bounded)

charsForSet :: CharSet -> String
charsForSet s = case s of
    Block    -> " ░▒▓█"
    Fine     -> " ░▒▓█▄▀■"
    Standard -> " .:-=+*#%@"
    Simple   -> " #."

-- | Everything the ASCII renderer needs.
data VConfig = VConfig
    { vcWidth  :: Int      -- ^ output width in characters
    , vcSet    :: CharSet
    , vcInvert :: Bool     -- ^ invert brightness
    } deriving (Show, Eq)

defaultConfig :: VConfig
defaultConfig = VConfig 48 Standard False

-- | Rows needed to keep proportions: a terminal cell is about twice as tall
--   as it is wide, so the aspect is halved.
rowsForWidth :: Int -> Int -> Int -> Int
rowsForWidth w srcW srcH
    | srcW <= 0 || srcH <= 0 || w <= 0 = 1
    | otherwise = max 1 (round (fromIntegral srcH * fromIntegral w
                                / (2 * fromIntegral srcW) :: Double))

-- | Raw grayscale frame (@srcW * srcH@ bytes, row-major) to ASCII lines.
--   Each target cell averages the source box it covers; boxes never sample
--   fewer than one source pixel, so upscaling degenerates gracefully.
grayToAscii :: VConfig -> Int -> Int -> ByteString -> [String]
grayToAscii cfg srcW srcH bytes =
    [ [ cell i j | j <- [0 .. w - 1] ] | i <- [0 .. h - 1] ]
  where
    w = vcWidth cfg
    h = rowsForWidth w srcW srcH
    ramp = charsForSet (vcSet cfg)
    rampLen = length ramp
    sample i = fromIntegral (BS.index bytes i) / 255 :: Double
    cell i j =
        let x0 = (j * srcW) `div` w
            x1 = max (x0 + 1) (((j + 1) * srcW) `div` w)
            y0 = (i * srcH) `div` h
            y1 = max (y0 + 1) (((i + 1) * srcH) `div` h)
            n = fromIntegral ((x1 - x0) * (y1 - y0)) :: Double
            avg = sum [ sample (y * srcW + x)
                      | y <- [y0 .. y1 - 1], x <- [x0 .. x1 - 1] ] / n
            level = if vcInvert cfg then 1 - avg else avg
            k = min (rampLen - 1) (floor (level * fromIntegral rampLen))
        in ramp !! k

-- | Mean absolute pixel difference of two equally sized grayscale frames,
--   normalized to [0, 1]. Empty or mismatched input counts as 0.
frameDistance :: ByteString -> ByteString -> Double
frameDistance a b
    | BS.null a || BS.null b = 0
    | otherwise = diffSum / fromIntegral n
  where
    n = min (BS.length a) (BS.length b)
    -- sum in Int: a Word8 accumulator would overflow (16 x 255 > 255)
    diffSum = fromIntegral (sum (map fromIntegral (BS.zipWith dist a b)) :: Int)
              / 255 :: Double
    dist x y = if x >= y then x - y else y - x :: Word8

-- | Loop-seam analysis (D15) over the sampled frames: average distance of
--   consecutive frames, distance of last to first, and their ratio. A ratio
--   near 1 means the seam reads like any other step; @999@ marks "no motion
--   to compare against but the seam differs" (degenerate: identical frames
--   with a differing seam — practically unreachable).
seamStats :: [ByteString] -> (Double, Double, Double)
seamStats frames = case frames of
    (_:_:_) -> (avg, seam, ratio)
    _       -> (0, 0, 1)
  where
    ds = [ frameDistance f g | (f, g) <- zip frames (drop 1 frames) ]
    avg = sum ds / fromIntegral (length ds) :: Double
    seam = frameDistance (last frames) (head frames)
    ratio | avg <= 1e-9 = if seam <= 1e-9 then 1.0 else 999.0
          | otherwise = seam / avg

-- | Sample times of an ambient loop: @n@ frames evenly over @[from, to)@, so
--   playing @n@ frames at @1 \/ dt@ fps loops the period exactly (D15).
loopTimes :: Double -> Double -> Int -> [Double]
loopTimes from to n
    | n <= 0 || to <= from = []
    | otherwise = [ from + dt * fromIntegral i | i <- [0 .. n - 1] ]
  where dt = (to - from) / fromIntegral n

-- | Sample times of a cutscene: the whole @[from, to]@ span at @fps@ frames
--   per second (D14: frame count is free here).
sceneTimes :: Double -> Double -> Int -> [Double]
sceneTimes from to fps
    | fps <= 0 || to <= from = []
    | otherwise = [ from + fromIntegral i / fromIntegral fps
                  | i <- [0 .. count - 1] ]
  where count = max 1 (floor ((to - from) * fromIntegral fps) :: Int)

-- | Escape a string as a JSON string literal (control chars as \\u00XX).
jsonStr :: String -> String
jsonStr s = "\"" ++ concatMap esc s ++ "\""
  where
    esc c = case c of
        '"'  -> "\\\""
        '\\' -> "\\\\"
        '\n' -> "\\n"
        '\r' -> "\\r"
        '\t' -> "\\t"
        _ | ord c < 0x20 -> "\\u" ++ pad4 (showHex (ord c) "")
          | otherwise -> [c]
    pad4 h = replicate (4 - length h) '0' ++ h

-- | A clip file (D14): the JSON array of multi-line frame strings the
--   worldbuilder's @file:@ declaration reads and embeds at compile time.
--   Frames carry the trailing newline the YAML block-scalar form produces,
--   so both routes compile identically.
toJsonStringArray :: [String] -> String
toJsonStringArray frames = "[" ++ intercalate "," (map jsonStr frames) ++ "]"

-- | Paste-ready inline @ambient:@ snippet (D11): @- |@ block scalars, art
--   indented two past the dash (YAML needs content indented deeper than the
--   block-scalar marker's line). @ind@ is the indentation of the @ambient:@
--   line itself.
ambientYaml :: Int -> [[String]] -> Int -> String
ambientYaml ind frames fps = unlines $
    [ pad ind "ambient:"
    , pad (ind + 2) "frames:" ]
    ++ concat
        [ [ pad (ind + 4) "- |" ]
          ++ map (pad (ind + 6)) f
        | f <- frames ]
    ++ [ pad (ind + 2) ("fps: " ++ show fps) ]
  where
    pad k s = replicate k ' ' ++ s

-- | Paste-ready clip declaration (D14): the @clips:@ segment referencing a
--   companion clip file. @ind@ is the indentation of the @clips:@ line.
clipYaml :: Int -> String -> String -> Int -> String
clipYaml ind cid file fps = unlines
    [ pad ind "clips:"
    , pad (ind + 2) "- id: " ++ cid
    , pad (ind + 4) "file: " ++ file
    , pad (ind + 4) ("fps: " ++ show fps) ]
  where
    pad k s = replicate k ' ' ++ s

-- | What ffprobe reports about the video.
data VideoInfo = VideoInfo
    { viWidth    :: Int
    , viHeight   :: Int
    , viFps      :: Double
    , viDuration :: Double   -- ^ seconds (0 when unknown)
    } deriving (Show, Eq)

-- | True when both external tools are on the PATH (test gating).
ffmpegAvailable :: IO Bool
ffmpegAvailable = do
    okFfmpeg <- probe "ffmpeg" ["-version"]
    okProbe <- probe "ffprobe" ["-version"]
    pure (okFfmpeg && okProbe)
  where
    probe cmd args = do
        (code, _, _) <- readProcessWithExitCode cmd args ""
        pure (code == ExitSuccess)

-- | Probe geometry, frame rate and duration via ffprobe.
probeVideo :: FilePath -> IO (Either String VideoInfo)
probeVideo path = do
    stream <- runFF ["-v", "error", "-select_streams", "v:0"
                    , "-show_entries", "stream=width,height,r_frame_rate"
                    , "-of", "csv=p=0", path]
    fmt <- runFF ["-v", "error", "-show_entries", "format=duration"
                 , "-of", "csv=p=0", path]
    pure $ do
        streamLines <- stream
        fmtLines <- fmt
        (w, h, rate) <- parseStream streamLines
        dur <- parseDouble (concat fmtLines)
        pure VideoInfo { viWidth = w, viHeight = h, viFps = rate, viDuration = dur }
  where
    runFF args = do
        (_, Just hout, _, ph) <- createProcess
            (proc "ffprobe" args) { std_out = CreatePipe, std_err = Inherit }
        out <- BS.hGetContents hout
        ec <- waitForProcess ph
        pure $ case ec of
            ExitSuccess -> Right (lines (map (chr . fromIntegral) (BS.unpack out)))
            _ -> Left ("ffprobe failed on '" ++ path ++ "'")
    parseStream ls = case ls of
        (row:_) -> case splitCommas row of
            [wStr, hStr, rateStr] -> do
                w <- parseInt wStr
                h <- parseInt hStr
                rate <- parseRate rateStr
                pure (w, h, rate)
            _ -> Left ("ffprobe: unexpected stream line: " ++ row)
        _ -> Left ("ffprobe: no video stream in '" ++ path ++ "'")
    parseInt s = case reads s of
        [(n, "")] -> Right (n :: Int)
        _ -> Left ("ffprobe: not an integer: " ++ s)
    parseDouble s
        | not (null s) && all isDigitNum s = Right (read s)
        | otherwise = Left ("ffprobe: not a duration: " ++ s)
      where isDigitNum c = isDigit c || c == '.'
    parseRate s = case break (== '/') s of
        (num, '/' : den) -> do
            n <- parseInt num
            d <- parseInt den
            if d <= 0 then Left ("ffprobe: bad rate: " ++ s)
                      else Right (fromIntegral n / fromIntegral d :: Double)
        _ -> case reads s of
            [(r, "")] -> Right (r :: Double)
            _ -> Left ("ffprobe: bad frame rate: " ++ s)
    splitCommas = go ""
      where
        go cur (',' : rest) = cur : go "" rest
        go cur (c : rest) = go (cur ++ [c]) rest
        go cur "" = [cur | not (null cur)]

-- | Extract one frame at time @t@ (seconds) as raw grayscale.
extractFrame :: FilePath -> VideoInfo -> Double -> IO (Either String ByteString)
extractFrame path vi t = do
    (_, Just hout, _, ph) <- createProcess (proc "ffmpeg" args)
        { std_out = CreatePipe, std_err = Inherit }
    bytes <- BS.hGetContents hout
    ec <- waitForProcess ph
    let want = viWidth vi * viHeight vi
        got = BS.length bytes
    pure $ case ec of
        ExitSuccess
            | got == want -> Right bytes
            | otherwise -> Left ("ffmpeg: frame at " ++ show t ++ "s: expected "
                                 ++ show want ++ " gray bytes, got " ++ show got)
        _ -> Left ("ffmpeg failed extracting the frame at " ++ show t ++ "s")
  where
    args = [ "-v", "error", "-ss", show t, "-i", path
           , "-frames:v", "1", "-f", "rawvideo", "-pix_fmt", "gray", "-" ]

-- | Extract frames at several times, in order.
extractFrames :: FilePath -> VideoInfo -> [Double]
              -> IO (Either String [ByteString])
extractFrames path vi times = do
    results <- mapM (extractFrame path vi) times
    pure (sequence results)
