-- | Pure SplitMix64 RNG for the Phase 4 pre-run world generator
--   (Rogue Phase 4, detail plan section 2).
--
--   Design invariants from the detail plan:
--
--   * No dependencies beyond @base@/@containers@ — no @random@ library.
--   * Fully pure: an 'Rng' is threaded explicitly through every call, no
--     global state, no 'unsafePerformIO'. Same seed + same template must
--     yield a bit-identical world.
--   * The golden-gamma constant matches the engine's @initialRngState@
--     (@src/Types.hs@) so both halves of a generated run share one seed
--     vocabulary; the runtime state of the emitted @save.json@ is derived
--     via 'deriveRuntimeSeed'.
--
--   Note on the engine's runtime RNG: @nextRng@ in @src/Game.hs@ is a plain
--   LCG that draws from the high bits. The generator deliberately uses full
--   SplitMix64 instead (stronger mixing, no low-bit degeneracy) — the two
--   never mix streams: generation-time randomness never reaches the engine's
--   @rngState@ except through the derived initial seed.
module Worldbuilder.Rng
    ( Rng (..)
    , rngGolden
    , newRng
    , stepRng
    , randInt
    , pickWeighted
    , shuffle
    , deriveRuntimeSeed
    ) where

import Data.Bits (shiftR, xor)
import Data.Word (Word64)
import qualified Data.Foldable as Foldable
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- | The golden-ratio gamma constant — same value as the engine's
--   @initialRngState@.
rngGolden :: Word64
rngGolden = 0x9E3779B97F4A7C15

-- | Threaded generator state. Purely a wrapped 'Word64'.
newtype Rng = Rng Word64
    deriving (Show, Eq)

-- | Seed the generator from a @--seed@ / template seed. Zero is a legal
--   seed (SplitMix64 handles it fine); the detail plan makes the seed a
--   required input, so there is no hidden default here.
newRng :: Word64 -> Rng
newRng = Rng

-- | One SplitMix64 step: @state += GOLDEN@, then the standard output mix.
--   Returns the mixed word and the advanced state.
stepRng :: Rng -> (Word64, Rng)
stepRng (Rng s) =
    let s'  = s + rngGolden
        z1  = (s' `xor` (s' `shiftR` 30)) * 0xBF58476D1CE4E5B9
        z2  = (z1 `xor` (z1 `shiftR` 27)) * 0x94D049BB133111EB
        z3  = z2 `xor` (z2 `shiftR` 31)
    in (z3, Rng s')

-- | Draw an 'Int' in the inclusive range @[lo, hi]@. With @hi <= lo@ the
--   range is a single value and the state is returned unchanged (a draw
--   without alternatives must not perturb the stream).
--
--   The draw uses the high bits (@shiftR 33@) of the mixed word, mirroring
--   the engine's own convention, so results stay stable if this module is
--   ever aligned with the runtime LCG.
randInt :: Int -> Int -> Rng -> (Int, Rng)
randInt lo hi r0
    | hi <= lo  = (lo, r0)
    | otherwise =
        let (w, r1) = stepRng r0
            range   = fromIntegral (hi - lo + 1) :: Word64
            draw    = fromIntegral ((w `shiftR` 33) `mod` range) :: Int
        in (lo + draw, r1)

-- | Pick an element from a weighted pool. Weights are positive 'Int's; the
--   pool must be non-empty (the generator validates @weight >= 1@ at the
--   template level, so this is a precondition, not a runtime branch).
--   Degenerate pools (total weight @<= 0@) deterministically yield the first
--   element without advancing the state.
pickWeighted :: [(Int, a)] -> Rng -> (a, Rng)
pickWeighted [] _ = error "Worldbuilder.Rng.pickWeighted: empty pool"
pickWeighted xs r0
    | total <= 0 = (snd (head xs), r0)
    | otherwise  =
        let (pick, r1) = randInt 1 total r0
            go _  []             = (snd (last xs), r1)  -- cannot happen: pick <= total
            go acc ((w, v) : rest)
                | pick <= acc + w = (v, r1)
                | otherwise       = go (acc + w) rest
        in go 0 xs
  where
    total = sum (map fst xs)

-- | Fisher-Yates shuffle (from the back), built on 'randInt'. Returns a
--   permutation of the input — same elements, different (seeded) order.
shuffle :: [a] -> Rng -> ([a], Rng)
shuffle [] r  = ([], r)
shuffle xs r0 = go (Seq.fromList xs) (length xs) r0
  where
    go :: Seq a -> Int -> Rng -> ([a], Rng)
    go s n r
        | n <= 1    = (Foldable.toList s, r)
        | otherwise =
            let (i, r') = randInt 0 (n - 1) r
                xi      = Seq.index s i
                xj      = Seq.index s (n - 1)
                s'      = Seq.update (n - 1) xi (Seq.update i xj s)
            in go s' (n - 1) r'

-- | Derive the runtime @rngState@ for the generated @save.json@ from the
--   generation seed (detail plan section 2: @seed * GOLDEN@). This makes
--   runtime randomness (@RandomChoice@, encounters) inside a generated
--   dungeon deterministic per seed as well, while staying decorrelated from
--   the generation stream (different constant usage: plain multiplication,
--   no shared stepping sequence).
deriveRuntimeSeed :: Word64 -> Word64
deriveRuntimeSeed seed = seed * rngGolden
