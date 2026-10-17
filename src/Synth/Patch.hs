-- | Synth core types (plan-synth.md §3.4, stage 5.2b).
--
--   Pure data only: no IO, no @process@/@directory@ (WASM rule A1). The
--   renderer lives in "Synth.Render", the WAV encoder in "Synth.Wav".
module Synth.Patch
  ( Wave (..)
  , Adsr (..)
  , Voice (..)
  , SynthSound (..)
  , defaultAdsr
  , defaultVoice
    -- * Limits and format constants
  , sampleRate
  , maxSfxSeconds
  , maxLayers
  , peakCeiling
  ) where

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
