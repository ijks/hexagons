module Random where

import Prelude

import Control.Monad.State (StateT, runStateT)
import Control.Monad.Trans.Class (class MonadTrans)
import Data.Generic (class Generic, gShow)
import Data.Newtype (class Newtype, unwrap, wrap)
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
derive instance genericLcg :: Generic Lcg

instance showLcg :: Show Lcg where
  show = gShow

instance randomGenLcg :: RandomGen Lcg where
  next (Lcg { state }) =
    let
      multiplier = 1664525
      increment = 1013904223
      -- An LCG usually also has a modulus parameter, but that's implicit here
      -- due to overflow.
      state' = (multiplier * state + increment)
    in
      Tuple state' (Lcg { state: state' })

newtype RandomT g m a = RandomT (StateT g m a)

derive instance newtypeRandomT :: Newtype (RandomT g m a) _
derive newtype instance functorRandomT :: Functor m => Functor (RandomT g m)
derive newtype instance applyRandomT :: Monad m => Apply (RandomT g m)
derive newtype instance applicativeRandomT :: Monad m => Applicative (RandomT g m)
derive newtype instance bindRandomT :: Monad m => Bind (RandomT g m)
derive newtype instance monadRandomT :: Monad m => Monad (RandomT g m)
derive newtype instance mondadTransRandomT :: MonadTrans (RandomT g)
-- There are a lot of applicable newtype instances here, but CBA to write them right now.
-- But it should *not* include MonadState, because then we can't prevent the generator
-- from being modified directly.

liftRandomT :: forall g m a. (g -> m (Tuple a g)) -> RandomT g m a
liftRandomT = wrap <<< wrap

runRandomT :: forall g m a. RandomT g m a -> g -> m (Tuple a g)
runRandomT = runStateT <<< unwrap
