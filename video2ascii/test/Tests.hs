-- | Tests for the video-to-ASCII converter (Phase F): the pure helpers
--   (frame conversion, seam analysis, time selection, output formats) plus an
--   end-to-end pass over a tiny self-generated video. The integration part is
--   skipped when ffmpeg/ffprobe are not on the PATH (CI containers without
--   them stay green).
module Main (main) where

import VideoToAscii
    ( CharSet (..), VConfig (..), VideoInfo (..), defaultConfig
    , ambientYaml, clipYaml, extractFrames, ffmpegAvailable, frameDistance
    , grayToAscii, loopTimes, probeVideo, rowsForWidth, sceneTimes
    , seamStats, toJsonStringArray
    )

import qualified Data.ByteString as BS
import Data.List (isInfixOf, isPrefixOf)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory,
                         removeFile)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.Process (callCommand)

runTest :: String -> IO Bool -> IO Bool
runTest name action = do
    passed <- action
    putStrLn ((if passed then "[PASS] " else "[FAIL] ") ++ name)
    pure passed

expectEqual :: (Eq a, Show a) => String -> a -> a -> IO Bool
expectEqual what expected actual
    | expected == actual = pure True
    | otherwise = do
        putStrLn $ "  " ++ what ++ ": expected " ++ show expected
        putStrLn $ "  actual:   " ++ show actual
        pure False

expectTrue :: String -> Bool -> IO Bool
expectTrue _ True = pure True
expectTrue msg False = do
    putStrLn $ "  expected True: " ++ msg
    pure False

-- | One gray frame: width x height bytes.
mkFrame :: Int -> Int -> (Int -> Int -> Int) -> BS.ByteString
mkFrame w h f = BS.pack [ fromIntegral (f x y) | y <- [0 .. h - 1], x <- [0 .. w - 1] ]

main :: IO ()
main = do
    ffmpeg <- ffmpegAvailable
    let wantIntegration = ffmpeg
    pureResults <- sequence
        [ runTest "rowsForWidth halves the aspect" testRows
        , runTest "grayToAscii: dimensions and ramp direction" testGray
        , runTest "grayToAscii: invert swaps the ramp" testInvert
        , runTest "frameDistance: zero, symmetric, bounded" testDistance
        , runTest "seamStats: perfect loop reads like any step" testSeam
        , runTest "loopTimes: count and exact period" testLoopTimes
        , runTest "sceneTimes: count is span * fps" testSceneTimes
        , runTest "toJsonStringArray escapes JSON specials" testJson
        , runTest "ambientYaml: block scalars and fps" testAmbientYaml
        , runTest "clipYaml: clip declaration shape" testClipYaml
        ]
    ffmpegResults <- if not wantIntegration
        then do
            putStrLn "[SKIP] ffmpeg end-to-end (ffmpeg/ffprobe not available)"
            pure []
        else sequence
            [ runTest "ffmpeg: probe reports geometry and duration" testProbe
            , runTest "ffmpeg: extract produces the right frame count" testExtract
            , runTest "ffmpeg: black loop is seamless (D15)" testSeamlessLoop
            , runTest "ffmpeg: moving video yields changing frames" testMoving
            ]
    if and (pureResults ++ ffmpegResults) then pure () else exitFailure

-- ---------------------------------------------------------------- pure tests

cfg :: Int -> VConfig
cfg w = defaultConfig { vcWidth = w }

testRows :: IO Bool
testRows = do
    r1 <- expectEqual "4:3 source at width 40 -> 15 rows" 15 (rowsForWidth 40 64 48)
    r2 <- expectEqual "square source at width 40 -> 20 rows" 20 (rowsForWidth 40 64 64)
    r3 <- expectEqual "degenerate source -> 1 row" 1 (rowsForWidth 40 0 48)
    pure (r1 && r2 && r3)

testGray :: IO Bool
testGray = do
    -- 8x4 source, width 8 -> 2 rows (aspect halved); top black, bottom white.
    let frame = mkFrame 8 4 (\_ y -> if y < 2 then 0 else 255)
        lines' = grayToAscii (cfg 8) 8 4 frame
    r1 <- expectEqual "two rows" 2 (length lines')
    r2 <- expectTrue "top row is all dark" (all (\c -> c `elem` " .:") (head lines'))
    r3 <- expectTrue "bottom row is all bright (@)"
                     (all (== '@') (concat (drop 1 lines')))
    pure (r1 && r2 && r3)

testInvert :: IO Bool
testInvert = do
    let frame = mkFrame 4 1 (\x _ -> if x < 2 then 0 else 255)
        normal = grayToAscii (cfg 4) 4 1 frame
        inv = grayToAscii ((cfg 4) { vcInvert = True }) 4 1 frame
    r1 <- expectTrue "normal: dark left, bright right"
                     (head normal !! 0 == ' ' && head normal !! 3 == '@')
    r2 <- expectTrue "invert: bright left, dark right"
                     (head inv !! 0 == '@' && head inv !! 3 == ' ')
    pure (r1 && r2)

testDistance :: IO Bool
testDistance = do
    let a = mkFrame 4 4 (const (const 0))
        b = mkFrame 4 4 (const (const 255))
    r1 <- expectEqual "identical frames -> 0" 0.0 (frameDistance a a)
    r2 <- expectEqual "full step -> 1" 1.0 (frameDistance a b)
    r3 <- expectTrue "symmetric" (frameDistance a b == frameDistance b a)
    r4 <- expectTrue "empty -> 0" (frameDistance a BS.empty == 0.0)
    pure (r1 && r2 && r3 && r4)

testSeam :: IO Bool
testSeam = do
    let black = mkFrame 4 4 (const (const 0))
        white = mkFrame 4 4 (const (const 255))
    -- last == first: the seam vanishes entirely (ratio 0, better than a
    -- normal step; only ratio > 2 warns).
    r1 <- expectEqual "last == first: seamless (ratio 0)"
                      (1.0, 0.0, 0.0) (seamStats [black, white, black])
    -- a hard jump: consecutive 0.5 on average, seam 1 -> ratio 2 (warn edge).
    let (avg, seam, ratio) = seamStats [black, white, white]
    r2 <- expectTrue "consecutive avg is 0.5" (avg == 0.5)
    r3 <- expectTrue "seam is 1" (seam == 1.0)
    r4 <- expectTrue "ratio 2 (warn edge)" (ratio == 2.0)
    pure (r1 && r2 && r3 && r4)

testLoopTimes :: IO Bool
testLoopTimes = do
    let ts = loopTimes 3.0 5.0 8 :: [Double]
    r1 <- expectEqual "8 samples" 8 (length ts)
    r2 <- expectEqual "first at from" 3.0 (head ts)
    r3 <- expectTrue "last strictly before to (period exact)"
                     (last ts < 5.0 && 5.0 - last ts < 0.3)
    r4 <- expectEqual "empty for n <= 0" [] (loopTimes 0 5 0)
    pure (r1 && r2 && r3 && r4)

testSceneTimes :: IO Bool
testSceneTimes = do
    let ts = sceneTimes 0 2 12 :: [Double]
    r1 <- expectEqual "24 samples for 2s at 12 fps" 24 (length ts)
    r2 <- expectTrue "evenly at 1/fps" (abs (ts !! 5 - 5 / 12) < 1e-9)
    r3 <- expectEqual "empty for fps 0" [] (sceneTimes 0 2 0)
    pure (r1 && r2 && r3)

testJson :: IO Bool
testJson = do
    r1 <- expectEqual "plain frames" "[\"a\",\"b\"]" (toJsonStringArray ["a", "b"])
    r2 <- expectEqual "escapes quote/backslash/newline"
                      "[\"a\\\"b\\\\c\\nd\"]"
                      (toJsonStringArray ["a\"b\\c\nd"])
    r3 <- expectEqual "escapes control chars"
                      "[\"x\\u0001y\"]" (toJsonStringArray ["x\x01y"])
    pure (r1 && r2 && r3)

testAmbientYaml :: IO Bool
testAmbientYaml = do
    let y = ambientYaml 0 [["~  ~", "~~~~"], [" ~ ~", "~~~~~"]] 8
    r1 <- expectTrue "starts with ambient:" ("ambient:" `isPrefixOf` y)
    r2 <- expectTrue "frames: at indent 2" ("  frames:" `isInfixOf` y)
    r3 <- expectTrue "two block scalars at indent 4" ("    - |" `isInfixOf` y)
    r4 <- expectTrue "art indented 6 (deeper than the marker)"
                     ("\n      ~  ~\n" `isInfixOf` y)
    r5 <- expectTrue "fps last" ("  fps: 8" `isInfixOf` y)
    pure (r1 && r2 && r3 && r4 && r5)

testClipYaml :: IO Bool
testClipYaml = do
    let y = clipYaml 0 "hall-pan" "hall-pan.json" 12
    r1 <- expectTrue "clips: first" ("clips:" `isPrefixOf` y)
    r2 <- expectTrue "id at indent 2" ("  - id: hall-pan" `isInfixOf` y)
    r3 <- expectTrue "file at indent 4" ("    file: hall-pan.json" `isInfixOf` y)
    r4 <- expectTrue "fps at indent 4" ("    fps: 12" `isInfixOf` y)
    pure (r1 && r2 && r3 && r4)

-- ------------------------------------------------------- integration (ffmpeg)

-- | Generate a tiny video with ffmpeg's lavfi source, then run the pipeline.
genVideo :: FilePath -> String -> IO ()
genVideo path src =
    callCommand $ "ffmpeg -v error -y -f lavfi -i " ++ src
                ++ " -pix_fmt yuv420p " ++ path

testProbe :: IO Bool
testProbe = do
    tmp <- getTemporaryDirectory
    let dir = tmp </> "video2ascii-test"
        vid = dir </> "probe.mp4"
    createDirectoryIfMissing True dir
    genVideo vid "color=c=black:size=64x48:rate=10:duration=2"
    probed <- probeVideo vid
    case probed of
        Left err -> expectTrue ("probe failed: " ++ err) False
        Right vi -> do
            r1 <- expectEqual "width 64" 64 (viWidth vi)
            r2 <- expectEqual "height 48" 48 (viHeight vi)
            r3 <- expectTrue "duration ~2s" (abs (viDuration vi - 2) < 0.5)
            r4 <- expectTrue "fps ~10" (abs (viFps vi - 10) < 1.0)
            _ <- removeFile vid
            pure (r1 && r2 && r3 && r4)

testExtract :: IO Bool
testExtract = do
    tmp <- getTemporaryDirectory
    let dir = tmp </> "video2ascii-test"
        vid = dir </> "extract.mp4"
    createDirectoryIfMissing True dir
    genVideo vid "color=c=black:size=64x48:rate=10:duration=2"
    probed <- probeVideo vid
    rawE <- case probed of
        Left _ -> pure (Left "no probe")
        Right vi -> extractFrames vid vi (loopTimes 0 1 5)
    let frames = case rawE of { Right fs -> fs; Left _ -> [] }
    r1 <- expectTrue "5 frames extracted" (length frames == 5)
    r2 <- expectTrue "frame size 64*48" (all (\f -> BS.length f == 64 * 48) frames)
    _ <- removeFile vid
    pure (r1 && r2)

testSeamlessLoop :: IO Bool
testSeamlessLoop = do
    tmp <- getTemporaryDirectory
    let dir = tmp </> "video2ascii-test"
        vid = dir </> "loop.mp4"
    createDirectoryIfMissing True dir
    genVideo vid "color=c=black:size=32x24:rate=10:duration=2"
    probed <- probeVideo vid
    result <- case probed of
        Left _ -> pure False
        Right vi -> do
            rawE <- extractFrames vid vi (loopTimes 0 1 6)
            let raw = case rawE of { Right fs -> fs; Left _ -> [] }
                (_, seam, ratio) = seamStats raw
            r1 <- expectTrue "black loop: zero seam" (seam == 0.0)
            r2 <- expectTrue "ratio 1" (ratio == 1.0)
            pure (r1 && r2)
    _ <- removeFile vid
    pure result

testMoving :: IO Bool
testMoving = do
    tmp <- getTemporaryDirectory
    let dir = tmp </> "video2ascii-test"
        vid = dir </> "moving.mp4"
    createDirectoryIfMissing True dir
    genVideo vid "testsrc=size=64x48:rate=10:duration=2"
    probed <- probeVideo vid
    result <- case probed of
        Left _ -> pure False
        Right vi -> do
            rawE <- extractFrames vid vi (loopTimes 0 1 4)
            let raw = case rawE of { Right fs -> fs; Left _ -> [] }
                arts = map (grayToAscii (cfg 32) (viWidth vi) (viHeight vi)) raw
            r1 <- expectTrue "4 frames" (length arts == 4)
            r2 <- expectTrue "frames differ" (not (allEqual arts))
            pure (r1 && r2)
    _ <- removeFile vid
    pure result
  where
    allEqual (a : b : rest) = a == b && allEqual (b : rest)
    allEqual _ = True
