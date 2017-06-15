module Generate where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Random (random, randomBool, randomInt, randomRange) as R
import Control.Monad.Free (Free, foldFree, liftF)
import Control.Monad.Gen (class MonadGen, chooseBool)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Traversable (class Traversable, traverse)

import Grid

-- Exploiting free monads to encode what random values we want before getting them.
-- I feel like a genius for coming up with this, but it's probably not that special.

data RandomF n
  = Random (Number -> n)
  | RandomBool (Boolean -> n)
  | RandomInt Int Int (Int -> n)
  | RandomRange Number Number (Number -> n)

derive instance functorRandomF :: Functor RandomF

newtype Random a = MkRandom (Free RandomF a)

derive instance newtypeRandom :: Newtype (Random a) _
derive newtype instance functorRandom :: Functor Random
derive newtype instance applyRandom :: Apply Random
derive newtype instance applicativeRandom :: Applicative Random
derive newtype instance monadRandom :: Monad Random

random :: Random Number
random = wrap $ liftF $ Random id

randomBool :: Random Boolean
randomBool = wrap $ liftF $ RandomBool id

randomInt :: Int -> Int -> Random Int
randomInt min max = wrap $ liftF $ RandomInt min max id

randomRange :: Number -> Number -> Random Number
randomRange min max = wrap $ liftF $ RandomRange min max id

runRandom :: forall e. Random ~> Eff (random :: RANDOM | e)
runRandom = foldFree interpret <<< unwrap
  where
  interpret :: forall e. RandomF ~> Eff (random :: RANDOM | e)
  interpret (Random f) = f <$> R.random
  interpret (RandomBool f) = f <$> R.randomBool
  interpret (RandomInt min max f) = f <$> R.randomInt min max
  interpret (RandomRange min max f) = f <$> R.randomRange min max

instance monadGenRandom :: MonadGen Random where
  chooseBool = randomBool
  chooseFloat = randomRange
  chooseInt = randomInt

  -- I'm not writing for actual QuickCheck-esque testing, so ¯\_(ツ)_/¯ on these.
  sized f = f 1
  resize f = id

genGrid :: forall m a b. MonadGen m => m a -> Grid b -> m (Grid a)
genGrid content shape = traverse (const content) shape

noise :: forall m a. MonadGen m => Grid a -> m (Grid Boolean)
noise = genGrid chooseBool
