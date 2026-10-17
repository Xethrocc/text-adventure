{-# LANGUAGE OverloadedStrings #-}

-- | Synth core types (plan-synth.md §3.4, stage 5.2b).
--
--   Pure data only: no IO, no @process@/@directory@ (WASM rule A1). The
--   renderer lives in "Synth.Render", the WAV encoder in "Synth.Wav".
--
--   5.2c: JSON instances. A 'SynthSound' is written canonically as
--   @{"layers": [voice, ...]}@ with every voice field explicit (stable for
--   world.json); it is read leniently — either that form or a single voice
--   object, missing fields taking the plan's defaults (§3.1).
module Synth.Patch
  ( Wave (..)
  , Adsr (..)
  , Voice (..)
  , SynthSound (..)
  , defaultAdsr
  , defaultVoice
  , waveName
  , waveFromName
    -- * Limits and format constants
  , sampleRate
  , maxSfxSeconds
  , maxLayers
  , peakCeiling
  ) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser)
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

-- | ADSR defaults of the plan: 0.005 / 0 / 1.0 / 0.05.
defaultAdsr :: Adsr
defaultAdsr = Adsr { adAttack = 0.005, adDecay = 0, adSustain = 1.0, adRelease = 0.05 }

-- | Voice with the plan's defaults (sweep 0, duty 0.5, volume 0.8).
defaultVoice :: Voice
defaultVoice = Voice
  { vWave = Sine, vFreq = 440, vSweep = 0, vDuty = 0.5
  , vDur = 0.25, vEnv = defaultAdsr, vVolume = 0.8 }

-- | Output sample rate in Hz (mono, 16 bit).
sampleRate :: Int
sampleRate = 22050

-- | S7: an SFX is at most this long; the renderer clamps defensively.
maxSfxSeconds :: Double
maxSfxSeconds = 10

-- | At most this many layers are rendered; the rest is ignored.
maxLayers :: Int
maxLayers = 4

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
