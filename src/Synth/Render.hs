-- | Pure synth renderer (plan-synth.md §3.4/§4, stage 5.2b).
--
--   22050 Hz, mono, 16 bit. Deterministic: no IO, no global RNG. The noise
--   generator is a fixed xorshift32 seeded from the sound id (never from the
--   game's @rngState@), so a sound is reproducible per id and playing it
--   cannot shift the game's random streams.
--
--   Float determinism: the sine is a polynomial (only @+@, @*@, @floor@), not
--   libm's @sin@, so the samples do not depend on the platform's math library
--   (native vs. WASM).
module Synth.Render
  ( renderSound
  , renderTrack
  , renderVoice
  , soundIdSeed
  , rendererVersion
  ) where

import Data.Bits (shiftL, shiftR, xor)
import Data.Char (ord)
import Data.Int (Int16)
import Data.List (foldl', sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Word (Word32)

import Synth.Patch

-- | Renderer version: part of the Synth.Cache key (O1) — bump it whenever the
--   rendered samples change, so stale cache files are never played again.
rendererVersion :: Int
rendererVersion = 1

-- | Render a sound to PCM samples. The sound id seeds the noise generator.
--   Layers (at most 'maxLayers') are mixed; the result is as long as the
--   longest layer and its peak is limited to 'peakCeiling' (O4, −3 dBFS) by
--   scaling the whole buffer down — quieter sounds keep their level.
renderSound :: String -> SynthSound -> [Int16]
renderSound sid (SynthSound vs) = limitAndQuantise mixed0
  where
    seed0 = soundIdSeed sid
    layers = zipWith (\i v -> renderVoice (seed0 + fromIntegral (i :: Int) * 0x9E3779B1) v)
                     [0 ..] (take maxLayers vs)
    mixed0 = foldr mixLong [] layers

-- | Render a music track to PCM samples (plan-synth.md §3.2/§5.2d).
--   22050 Hz, mono, 16-bit, peak-limited to 'peakCeiling' (O4, −3 dBFS).
--   Total length comes from @bpm@, @steps@ (sixteenth notes) and @order@
--   (clamped to 'maxTrackSeconds' = 120 s, S7).
--   Loop seam (5.2d): when 'trLoop' is 'True', any release tail extending
--   beyond the end of the track is wrapped around into the start of the
--   buffer, keeping oscillator phase and ADSR envelope continuous across
--   the loop boundary.
renderTrack :: String -> Track -> [Int16]
renderTrack tid tr
  | null (trChannels tr) || trBpm tr <= 0 || trSteps tr <= 0 || totalSamples <= 0 = []
  | otherwise = limitAndQuantise mixed0
  where
    sr = fromIntegral sampleRate :: Double
    bpm = max 1.0 (fromIntegral (trBpm tr) :: Double)
    stepSec = 15.0 / bpm
    stepsPerPat = max 1 (trSteps tr)
    ordr = effectiveOrder tr
    totalSteps = stepsPerPat * length ordr
    rawSec = fromIntegral totalSteps * stepSec
    trackSec = clampD 0 maxTrackSeconds rawSec
    totalSamples = round (trackSec * sr) :: Int
    seed0 = soundIdSeed tid
    chStreams =
      zipWith (renderChannel totalSamples stepSec stepsPerPat ordr (trLoop tr) (trInstruments tr) seed0)
              [0 :: Int ..]
              (take maxChannels (trChannels tr))
    mixed0 = take totalSamples (foldr mixLong (replicate totalSamples 0) chStreams)

limitAndQuantise :: [Double] -> [Int16]
limitAndQuantise mixed0 = map quantise mixed
  where
    peak = foldl' (\m x -> max (abs x) m) 0 mixed0
    scale = if peak > peakCeiling then peakCeiling / peak else 1
    mixed = if scale < 1 then map (* scale) mixed0 else mixed0
    quantise x = fromIntegral (max (-32768) (min 32767 (round x :: Int))) :: Int16

renderChannel
  :: Int
  -> Double
  -> Int
  -> [Int]
  -> Bool
  -> Map.Map String Voice
  -> Word32
  -> Int
  -> Channel
  -> [Double]
renderChannel totalSamples stepSec stepsPerPat ordr isLoop insts seed0 chIdx ch =
    mixSegments totalSamples (sortOn fst segments)
  where
    sr = fromIntegral sampleRate :: Double
    inst = fromMaybe defaultVoice (Map.lookup (chInstrument ch) insts)
    pats = chPatterns ch
    patAt idx
      | null pats = ""
      | otherwise = pats !! (max 0 idx `mod` length pats)
    parsePatSteps p =
      let toks = [ fromMaybe StepHold (parseStepToken tok) | (_, tok) <- tokenizePattern p ]
      in take stepsPerPat (toks ++ repeat StepHold)
    allSteps = concatMap (parsePatSteps . patAt) ordr
    notes = zip [0 :: Int ..] (extractNotes allSteps)
    segments = concatMap toSegs notes
    toSegs (nIdx, (sStart, sEnd, freq)) =
      let iStart   = round (fromIntegral sStart * stepSec * sr) :: Int
          iGateEnd = min totalSamples (round (fromIntegral sEnd * stepSec * sr) :: Int)
          gateLen  = iGateEnd - iStart
          maxRel   = if isLoop then totalSamples else max 1 (totalSamples - iGateEnd)
          rawSeed  = seed0 + fromIntegral chIdx * 0x9E3779B1 + fromIntegral nIdx * 0x85EBCA6B
          nSeed    = if rawSeed == 0 then 0x6D2B79F5 else rawSeed
          samples  = renderTrackNote nSeed inst freq gateLen maxRel
          roomLeft = totalSamples - iStart
          (pre, post) = splitAt roomLeft samples
          wrap = take totalSamples post
      in if iStart >= totalSamples || gateLen <= 0
           then []
           else (iStart, pre) : [ (0, wrap) | isLoop && not (null wrap) ]

-- | Turn a step token list into @(startStep, endStep, freqHz)@ gate intervals.
extractNotes :: [StepToken] -> [(Int, Int, Double)]
extractNotes = go 0 Nothing
  where
    go _ Nothing [] = []
    go s (Just (s0, f)) [] = [(s0, s, f)]
    go s active (tok : rest) = case tok of
      StepNote f ->
        let prev = maybe [] (\(s0, f0) -> [(s0, s, f0)]) active
        in prev ++ go (s + 1) (Just (s, f)) rest
      StepHold ->
        go (s + 1) active rest
      StepOff ->
        let prev = maybe [] (\(s0, f0) -> [(s0, s, f0)]) active
        in prev ++ go (s + 1) Nothing rest

-- | Render a single note of a track channel: @gateSamples@ of attack/decay/sustain
--   followed by @relSamples@ of linear release down to 0.
renderTrackNote :: Word32 -> Voice -> Double -> Int -> Int -> [Double]
renderTrackNote seed inst freq gateSamples maxRelSamples
  | gateSamples <= 0 = []
  | otherwise =
      zipWith (\k o -> o * vol * envAt k) [0 .. totalLen - 1] (take totalLen osc)
  where
    sr = fromIntegral sampleRate :: Double
    vol = 32767 * clampD 0 1 (vVolume inst)
    env = vEnv inst
    atk = max (1 / sr) (adAttack env)
    dcy = max 0 (adDecay env)
    sus = clampD 0 1 (adSustain env)
    minRelSamples = min 32 (max 1 maxRelSamples)
    rawRelSamples = round (max 0 (adRelease env) * sr) :: Int
    relSamples = max minRelSamples (min maxRelSamples rawRelSamples)
    totalLen = gateSamples + relSamples
    level t
      | t < atk = t / atk
      | dcy > 0 && t < atk + dcy = 1 - (1 - sus) * (t - atk) / dcy
      | otherwise = sus
    gateLevel = level (fromIntegral gateSamples / sr)
    envAt k
      | k < gateSamples = level (fromIntegral k / sr)
      | relSamples <= 1 = 0
      | otherwise =
          let rIdx = k - gateSamples
          in gateLevel * fromIntegral (relSamples - 1 - rIdx) / fromIntegral relSamples
    f0 = freq
    sw = vSweep inst
    tStop = if sw < 0 && f0 > 0 then f0 / negate sw else 1 / 0
    phaseAt t = let t' = min t tStop in f0 * t' + 0.5 * sw * t' * t'
    duty = clampD 0.05 0.95 (vDuty inst)
    osc = case vWave inst of
      Noise    -> noiseStream seed
      Sine     -> [sineCyc (phaseAt (fromIntegral i / sr)) | i <- [0 :: Int ..]]
      Square   -> [if frac (phaseAt (fromIntegral i / sr)) < duty then 1 else -1 | i <- [0 :: Int ..]]
      Saw      -> [2 * frac (phaseAt (fromIntegral i / sr)) - 1 | i <- [0 :: Int ..]]
      Triangle -> [triCyc (phaseAt (fromIntegral i / sr)) | i <- [0 :: Int ..]]

-- | Mix a start-sorted list of @(startSample, samples)@ segments into a stream
--   of @totalSamples@ samples in O(totalSamples) time.
mixSegments :: Int -> [(Int, [Double])] -> [Double]
mixSegments totalSamples segs0 = go 0 [] (filter (not . null . snd) segs0)
  where
    go pos active upcoming
      | pos >= totalSamples = []
      | otherwise =
          let (nowSegs, laterSegs) = span (\(s, _) -> s <= pos) upcoming
              active' = map snd nowSegs ++ active
          in case active' of
               [] -> case laterSegs of
                 [] -> replicate (totalSamples - pos) 0
                 ((nextPos, _) : _) ->
                   let gap = min totalSamples nextPos - pos
                   in replicate gap 0 ++ go (pos + gap) [] laterSegs
               _ ->
                 let sampleSum = foldl' (\acc xs -> acc + head xs) 0 active'
                     activeNext = [ rest | (_ : rest) <- active', not (null rest) ]
                 in sampleSum `seq` (sampleSum : go (pos + 1) activeNext laterSegs)

-- | Sum of two sample streams, zero-padding the shorter one.
mixLong :: [Double] -> [Double] -> [Double]
mixLong (x : xs) (y : ys) = let z = x + y in z `seq` (z : mixLong xs ys)
mixLong xs [] = xs
mixLong [] ys = ys

-- | FNV-1a (32 bit) over the sound id; never zero (xorshift needs that).
soundIdSeed :: String -> Word32
soundIdSeed s = if h == 0 then 0x6D2B79F5 else h
  where h = foldl (\a c -> (a `xor` fromIntegral (ord c)) * 16777619) 2166136261 s

-- | One layer in sample units (±32767 · volume · envelope). Length is
--   @round (dur * 22050)@ with @dur@ clamped to 0–'maxSfxSeconds' (S7).
renderVoice :: Word32 -> Voice -> [Double]
renderVoice seed v = case n of
    0 -> []
    1 -> [0]
    _ -> zipWith (\i o -> o * vol * envAt (fromIntegral i / sr)) [0 :: Int ..] (take n osc)
  where
    sr = fromIntegral sampleRate :: Double
    dur = clampD 0 maxSfxSeconds (vDur v)
    n = round (dur * sr) :: Int
    -- the envelope ends exactly at the last sample (value 0, no click)
    dEnd = fromIntegral (n - 1) / sr
    vol = 32767 * clampD 0 1 (vVolume v)
    env = vEnv v
    atk = clampD (1 / sr) (max (1 / sr) (dEnd / 2)) (adAttack env)
    dcy = max 0 (adDecay env)
    sus = clampD 0 1 (adSustain env)
    -- at least one sample of release, so even release 0 ends at exactly 0
    relMax = max 0 (dEnd - atk)
    rel = clampD (min (1 / sr) relMax) relMax (adRelease env)
    tRel = dEnd - rel
    level t
      | t < atk = t / atk
      | dcy > 0 && t < atk + dcy = 1 - (1 - sus) * (t - atk) / dcy
      | otherwise = sus
    envAt t
      | t >= tRel && rel > 0 = level tRel * (dEnd - t) / rel
      | otherwise = level t
    f0 = vFreq v
    sw = vSweep v
    -- a falling sweep stops at 0 Hz instead of turning around
    tStop = if sw < 0 && f0 > 0 then f0 / negate sw else 1 / 0
    phaseAt t = let t' = min t tStop in f0 * t' + 0.5 * sw * t' * t'
    duty = clampD 0.05 0.95 (vDuty v)
    osc = case vWave v of
        Noise    -> noiseStream seed
        Sine     -> [sineCyc (phaseAt (fromIntegral i / sr)) | i <- [0 :: Int ..]]
        Square   -> [if frac (phaseAt (fromIntegral i / sr)) < duty then 1 else -1 | i <- [0 :: Int ..]]
        Saw      -> [2 * frac (phaseAt (fromIntegral i / sr)) - 1 | i <- [0 :: Int ..]]
        Triangle -> [triCyc (phaseAt (fromIntegral i / sr)) | i <- [0 :: Int ..]]

clampD :: Double -> Double -> Double -> Double
clampD lo hi x = max lo (min hi x)

frac :: Double -> Double
frac p = p - fromIntegral (floor p :: Int)

-- | Triangle over one cycle, starting at 0 (0 → 1 → 0 → −1).
triCyc :: Double -> Double
triCyc p
  | x < 0.25 = 4 * x
  | x < 0.75 = 2 - 4 * x
  | otherwise = 4 * x - 4
  where x = frac p

-- | @sin (2π · p)@ via Taylor polynomial on the folded quarter wave.
sineCyc :: Double -> Double
sineCyc p = th * (1 + t2 * (c3 + t2 * (c5 + t2 * (c7 + t2 * (c9 + t2 * (c11 + t2 * c13))))))
  where
    x = frac p
    y | x < 0.25 = x
      | x < 0.75 = 0.5 - x
      | otherwise = x - 1
    th = 2 * 3.141592653589793 * y
    t2 = th * th
    c3 = -1 / 6
    c5 = 1 / 120
    c7 = -1 / 5040
    c9 = 1 / 362880
    c11 = -1 / 39916800
    c13 = 1 / 6227020800

-- | White noise in [−1, 1) from xorshift32.
noiseStream :: Word32 -> [Double]
noiseStream = map toD . tail . iterate step
  where
    step a0 = let a1 = a0 `xor` (a0 `shiftL` 13)
                  a2 = a1 `xor` (a1 `shiftR` 17)
              in a2 `xor` (a2 `shiftL` 5)
    toD w = fromIntegral w / 2147483648 - 1
