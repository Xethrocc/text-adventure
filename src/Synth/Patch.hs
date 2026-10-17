{-# LANGUAGE OverloadedStrings #-}

-- | Synth core types (plan-synth.md §3.2/§3.4, stages 5.2b–5.2d).
--
--   Pure data only: no IO, no @process@/@directory@ (WASM rule A1). The
--   renderer lives in "Synth.Render", the WAV encoder in "Synth.Wav".
--
--   5.2c: JSON instances. A 'SynthSound' is written canonically as
--   @{"layers": [voice, ...]}@ with every voice field explicit (stable for
--   world.json); it is read leniently — either that form or a single voice
--   object, missing fields taking the plan's defaults (§3.1).
--
--   5.2d: 'Track', 'Channel', 'StepToken' and O3 pattern/note parsing.
module Synth.Patch
  ( Wave (..)
  , Adsr (..)
  , Voice (..)
  , SynthSound (..)
  , Channel (..)
  , chPattern
  , mkChannel
  , Track (..)
  , StepToken (..)
  , defaultAdsr
  , defaultVoice
  , defaultTrack
  , waveName
  , waveFromName
  , parseNote
  , parseStepToken
  , tokenizePattern
  , parsePattern
  , effectiveOrder
  , trackTotalSteps
  , trackDurationSeconds
  , instrumentToJSON
    -- * Limits and format constants
  , sampleRate
  , maxSfxSeconds
  , maxTrackSeconds
  , maxLayers
  , maxChannels
  , minBpm
  , maxBpm
  , maxStepsPerPattern
  , peakCeiling
  ) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser)
import Data.Char (isSpace, ord)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

-- | Oscillator shape. 'Noise' ignores frequency, sweep and duty.
data Wave = Sine | Square | Saw | Triangle | Noise
  deriving (Eq, Show, Enum, Bounded)

-- | Envelope in seconds (attack, decay, release) and level 0–1 (sustain).
data Adsr = Adsr
  { adAttack  :: Double
  , adDecay   :: Double
  , adSustain :: Double
  , adRelease :: Double
  } deriving (Eq, Show)

-- | One oscillator layer.
--   'vFreq' in Hz, 'vSweep' in Hz/s (linear), 'vDuty' pulse width of
--   'Square' (0.05–0.95), 'vDur' in seconds, 'vVolume' 0–1.
data Voice = Voice
  { vWave   :: Wave
  , vFreq   :: Double
  , vSweep  :: Double
  , vDuty   :: Double
  , vDur    :: Double
  , vEnv    :: Adsr
  , vVolume :: Double
  } deriving (Eq, Show)

-- | A sound effect: 1–4 layers, mixed.
newtype SynthSound = SynthSound [Voice]
  deriving (Eq, Show)

-- | One sequencer channel of a 'Track' (at most 'maxChannels' per track, §3.2).
--   Single-pattern channels carry @[pat]@ in 'chPatterns'; multi-pattern
--   channels carry @[pat0, pat1, ...]@ indexed by 'trOrder'.
data Channel = Channel
  { chInstrument :: String
  , chPatterns   :: [String]
  } deriving (Eq, Show)

-- | First (or only) pattern string of a channel.
chPattern :: Channel -> String
chPattern ch = case chPatterns ch of
  (p : _) -> p
  []      -> ""

-- | Construct a single-pattern channel.
mkChannel :: String -> String -> Channel
mkChannel inst pat = Channel { chInstrument = inst, chPatterns = [pat] }

-- | A short synthesised music track (plan-synth.md §3.2/§3.4, stage 5.2d).
--   'trInstruments' holds patches without @freq@/@dur@ (stored as 'Voice' with
--   default 'vFreq'/'vDur', overridden per note during rendering). 'trOrder'
--   is the optional 0-based pattern sequence; when empty (@[]@), patterns play
--   in declaration order @[0 .. maxPatterns - 1]@.
data Track = Track
  { trBpm         :: Int
  , trSteps       :: Int
  , trLoop        :: Bool
  , trInstruments :: Map.Map String Voice
  , trChannels    :: [Channel]
  , trOrder       :: [Int]
  } deriving (Eq, Show)

-- | One step (sixteenth note) in a track pattern (O3).
data StepToken
  = StepNote Double  -- ^ Trigger note at frequency in Hz
  | StepHold         -- ^ @.@: hold active note or rest
  | StepOff          -- ^ @-@: note-off (begin release phase)
  deriving (Eq, Show)

-- | ADSR defaults of the plan: 0.005 / 0 / 1.0 / 0.05.
defaultAdsr :: Adsr
defaultAdsr = Adsr { adAttack = 0.005, adDecay = 0, adSustain = 1.0, adRelease = 0.05 }

-- | Voice with the plan's defaults (sweep 0, duty 0.5, volume 0.8).
defaultVoice :: Voice
defaultVoice = Voice
  { vWave = Sine, vFreq = 440, vSweep = 0, vDuty = 0.5
  , vDur = 0.25, vEnv = defaultAdsr, vVolume = 0.8 }

-- | Track defaults (120 BPM, 16 steps per pattern, looping).
defaultTrack :: Track
defaultTrack = Track
  { trBpm         = 120
  , trSteps       = 16
  , trLoop        = True
  , trInstruments = Map.empty
  , trChannels    = []
  , trOrder       = []
  }

-- | Output sample rate in Hz (mono, 16 bit).
sampleRate :: Int
sampleRate = 22050

-- | S7: an SFX is at most this long; the renderer clamps defensively.
maxSfxSeconds :: Double
maxSfxSeconds = 10

-- | S7: a music track is at most this long (120 s).
maxTrackSeconds :: Double
maxTrackSeconds = 120

-- | At most this many layers are rendered; the rest is ignored.
maxLayers :: Int
maxLayers = 4

-- | At most this many channels are rendered in a 'Track'.
maxChannels :: Int
maxChannels = 4

-- | Minimum and maximum allowed BPM in 'Track' validation.
minBpm, maxBpm :: Int
minBpm = 20
maxBpm = 300

-- | Maximum steps per pattern in 'Track' validation.
maxStepsPerPattern :: Int
maxStepsPerPattern = 256

-- | O4: peak limit of the synth output, −3 dBFS (10^(−3/20) · 32767).
peakCeiling :: Double
peakCeiling = 0.7079457843841379 * 32767

-- | Author-facing wave name (@sine@, @square@, @saw@, @triangle@, @noise@).
waveName :: Wave -> String
waveName w = case w of
  Sine -> "sine"
  Square -> "square"
  Saw -> "saw"
  Triangle -> "triangle"
  Noise -> "noise"

-- | Inverse of 'waveName'.
waveFromName :: String -> Maybe Wave
waveFromName n = lookup n [ (waveName w, w) | w <- [minBound .. maxBound] ]

-- | Parse a note token (@C4@, @F#3@, @Bb2@) to its equal-temperament frequency
--   in Hz (A4 = 440 Hz). Uses a fixed 12-semitone ratio table and exact binary
--   octave scaling so the result does not depend on @libm@ (WASM parity).
parseNote :: String -> Maybe Double
parseNote s = do
  (baseSemi, rest1) <- case s of
    ('C' : r) -> Just (0, r)
    ('D' : r) -> Just (2, r)
    ('E' : r) -> Just (4, r)
    ('F' : r) -> Just (5, r)
    ('G' : r) -> Just (7, r)
    ('A' : r) -> Just (9, r)
    ('B' : r) -> Just (11, r)
    _         -> Nothing
  (acc, rest2) <- case rest1 of
    ('#' : r) | baseSemi `elem` [0, 2, 5, 7, 9]  -> Just (1, r)
    ('b' : r) | baseSemi `elem` [2, 4, 7, 9, 11] -> Just (-1, r)
    ('#' : _) -> Nothing
    ('b' : _) -> Nothing
    r         -> Just (0, r)
  oct <- case rest2 of
    [d] | d >= '0' && d <= '8' -> Just (ord d - ord '0')
    _                          -> Nothing
  let midi = (oct + 1) * 12 + baseSemi + acc
      (octDiff, semiIdx) = (midi - 69) `divMod` 12
      ratio = semiRatios !! semiIdx
      octScale
        | octDiff >= 0 = fromIntegral ((1 :: Int) * (2 ^ octDiff))
        | otherwise    = 1.0 / fromIntegral ((1 :: Int) * (2 ^ negate octDiff))
  pure (440.0 * ratio * octScale)
  where
    -- 2^(k/12) for k = 0..11 relative to A
    semiRatios :: [Double]
    semiRatios =
      [ 1.0
      , 1.0594630943592953
      , 1.122462048309373
      , 1.189207115002721
      , 1.2599210498948732
      , 1.3348398541700344
      , 1.4142135623730951
      , 1.4983070768766815
      , 1.5874010519681994
      , 1.681792830507429
      , 1.7817974362806785
      , 1.8877486253633868
      ]

-- | Parse one step token (O3): @.@ = hold/rest, @-@ = note-off, or a note name.
parseStepToken :: String -> Maybe StepToken
parseStepToken "." = Just StepHold
parseStepToken "-" = Just StepOff
parseStepToken tok = StepNote <$> parseNote tok

-- | Tokenize a whitespace-separated pattern string with 1-based column numbers.
tokenizePattern :: String -> [(Int, String)]
tokenizePattern = go 1
  where
    go _ [] = []
    go col (c : cs)
      | isSpace c = go (col + 1) cs
      | otherwise =
          let (tok, rest) = break isSpace (c : cs)
          in (col, tok) : go (col + length tok) rest

-- | Parse a full pattern string into step tokens, or return @Left (col, token)@
--   on the first unreadable token.
parsePattern :: String -> Either (Int, String) [StepToken]
parsePattern pat = mapM step (tokenizePattern pat)
  where
    step (col, tok) = case parseStepToken tok of
      Just st -> Right st
      Nothing -> Left (col, tok)

-- | Effective 0-based pattern order of a track: 'trOrder' when non-empty,
--   otherwise @[0 .. maxPatterns - 1]@.
effectiveOrder :: Track -> [Int]
effectiveOrder tr
  | not (null (trOrder tr)) = trOrder tr
  | otherwise =
      let maxPats = maximum (1 : map (length . chPatterns) (trChannels tr))
      in [0 .. maxPats - 1]

-- | Total number of sixteenth-note steps across the track's effective order.
trackTotalSteps :: Track -> Int
trackTotalSteps tr = max 0 (trSteps tr) * length (effectiveOrder tr)

-- | Unclamped duration of a track in seconds (@totalSteps * 15 / bpm@).
trackDurationSeconds :: Track -> Double
trackDurationSeconds tr
  | trBpm tr <= 0 || trSteps tr <= 0 = 0
  | otherwise = fromIntegral (trackTotalSteps tr) * (15.0 / fromIntegral (trBpm tr))

-- | Encode an instrument 'Voice' without redundant @freq@/@dur@ fields when
--   they carry the default placeholder values (§3.2: instruments are patches
--   without @freq@/@dur@).
instrumentToJSON :: Voice -> Value
instrumentToJSON v = object $
  [ "wave"    .= vWave v
  , "sweep"   .= vSweep v
  , "duty"    .= vDuty v
  , "attack"  .= adAttack e
  , "decay"   .= adDecay e
  , "sustain" .= adSustain e
  , "release" .= adRelease e
  , "volume"  .= vVolume v
  ]
  ++ [ "freq" .= vFreq v | vFreq v /= vFreq defaultVoice ]
  ++ [ "dur"  .= vDur v  | vDur v  /= vDur defaultVoice  ]
  where e = vEnv v

instance ToJSON Wave where
  toJSON = toJSON . waveName

instance FromJSON Wave where
  parseJSON = withText "Wave" $ \t ->
    maybe (fail ("unknown wave: " ++ T.unpack t)) pure (waveFromName (T.unpack t))

instance ToJSON Voice where
  toJSON v = object
    [ "wave"    .= vWave v
    , "freq"    .= vFreq v
    , "sweep"   .= vSweep v
    , "duty"    .= vDuty v
    , "dur"     .= vDur v
    , "attack"  .= adAttack e
    , "decay"   .= adDecay e
    , "sustain" .= adSustain e
    , "release" .= adRelease e
    , "volume"  .= vVolume v
    ]
    where e = vEnv v

instance FromJSON Voice where
  parseJSON = withObject "Voice" $ \o -> do
    let d = defaultVoice
        e = defaultAdsr
        opt k def = maybe def id <$> (o .:? k :: Parser (Maybe Double))
    w  <- o .: "wave"
    f  <- opt "freq" (vFreq d)
    sw <- opt "sweep" (vSweep d)
    du <- opt "duty" (vDuty d)
    dr <- opt "dur" (vDur d)
    at <- opt "attack" (adAttack e)
    dc <- opt "decay" (adDecay e)
    su <- opt "sustain" (adSustain e)
    re <- opt "release" (adRelease e)
    vo <- opt "volume" (vVolume d)
    pure Voice { vWave = w, vFreq = f, vSweep = sw, vDuty = du, vDur = dr
               , vEnv = Adsr at dc su re, vVolume = vo }

instance ToJSON SynthSound where
  toJSON (SynthSound vs) = object [ "layers" .= vs ]

instance FromJSON SynthSound where
  parseJSON val = withObject "SynthSound" go val
    where
      go o = case KM.lookup "layers" o of
        Just ls -> SynthSound <$> parseJSON ls
        Nothing -> (\x -> SynthSound [x]) <$> parseJSON val

instance ToJSON Channel where
  toJSON ch = case chPatterns ch of
    [p] -> object [ "instrument" .= chInstrument ch, "pattern" .= p ]
    ps  -> object [ "instrument" .= chInstrument ch, "patterns" .= ps ]

instance FromJSON Channel where
  parseJSON = withObject "Channel" $ \o -> do
    inst <- o .: "instrument"
    mPats <- o .:? "patterns"
    case mPats of
      Just ps -> pure Channel { chInstrument = inst, chPatterns = ps }
      Nothing -> do
        pv <- o .: "pattern"
        ps <- case pv of
          String s -> pure [T.unpack s]
          Array _  -> parseJSON pv
          _        -> fail "pattern must be a string or list of strings"
        pure Channel { chInstrument = inst, chPatterns = ps }

instance ToJSON Track where
  toJSON tr = object $
    [ "bpm"         .= trBpm tr
    , "steps"       .= trSteps tr
    , "loop"        .= trLoop tr
    , "instruments" .= Map.map instrumentToJSON (trInstruments tr)
    , "channels"    .= trChannels tr
    ] ++ [ "order" .= trOrder tr | not (null (trOrder tr)) ]

instance FromJSON Track where
  parseJSON = withObject "Track" $ \o -> do
    bpm   <- o .:? "bpm"         .!= trBpm defaultTrack
    steps <- o .:? "steps"       .!= trSteps defaultTrack
    loop  <- o .:? "loop"        .!= trLoop defaultTrack
    insts <- o .:? "instruments" .!= Map.empty
    chans <- o .:? "channels"    .!= []
    ordr  <- o .:? "order"       .!= []
    pure Track
      { trBpm         = bpm
      , trSteps       = steps
      , trLoop        = loop
      , trInstruments = insts
      , trChannels    = chans
      , trOrder       = ordr
      }

