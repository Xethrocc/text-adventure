-- | WAV encoder (RIFF/PCM, 44-byte header). Pure.
module Synth.Wav (encodeWav) where

import Data.ByteString.Builder (Builder, byteString, int16LE, toLazyByteString, word16LE, word32LE)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int16)

-- | @encodeWav rate samples@: mono, 16 bit PCM, little endian.
encodeWav :: Int -> [Int16] -> BL.ByteString
encodeWav rate samples = toLazyByteString (header <> foldMap int16LE samples)
  where
    dataBytes = fromIntegral (2 * length samples) :: Integer
    r = fromIntegral rate :: Integer
    w32 :: Integer -> Builder
    w32 = word32LE . fromIntegral
    header =
         tag "RIFF" <> w32 (36 + dataBytes) <> tag "WAVE"
      <> tag "fmt " <> w32 16
      <> word16LE 1            -- PCM
      <> word16LE 1            -- mono
      <> w32 r
      <> w32 (r * 2)           -- byte rate
      <> word16LE 2            -- block align
      <> word16LE 16           -- bits per sample
      <> tag "data" <> w32 dataBytes
    tag = byteString . BC.pack
