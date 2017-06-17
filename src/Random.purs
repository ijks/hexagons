module Random where

import Prelude

import Data.Int (pow)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))

-- | This class identifies datatypes that can be used as a pseudo-random number
-- | generator.
-- |
-- | In addition to the generator yielding sufficiently random numbers, the
-- | following laws should hold:
-- |
-- | - `fst $ next g` is always between `bottom` and `top` (inclusive).
-- |   (`top` and `bottom` are from the class `Bounded`.)
class RandomGen g where
  -- | Advance the generator, yielding a number and the new generator state.
  next :: g -> Tuple Int g

  -- I'm leaving out 'split' here, because that's probably tricky to get right.

-- | A PRNG based on a Linear Congruential Generator.
-- |
-- | See: https://en.wikipedia.org/wiki/Linear_congruential_generator
newtype Lcg = Lcg { state :: Int }

derive instance newtypeLcg :: Newtype Lcg _

instance randomGenLCG :: RandomGen Lcg where
  next (Lcg { state }) =
    let
      multiplier = 1664525
      increment = 1013904223
      modulus = bottom - top
      state' = (multiplier * state + increment) `mod` modulus
    in
      Tuple state' (Lcg { state: state' })
