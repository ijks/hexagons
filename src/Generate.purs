module Generate where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Random (random, randomBool, randomInt, randomRange) as R
import Control.Monad.Free (Free, foldFree, liftF)
import Control.Monad.Gen (class MonadGen, chooseBool, elements)
import Control.Monad.State (class MonadState, StateT, execStateT, get, gets, modify)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Foldable (class Foldable, for_)
import Data.List (List(Nil), (:))
import Data.List as List
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Tuple (Tuple(..), snd)

-- REMOVE ME --
import Debug.Trace as T

import Grid
import Hexagon

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

type Walls = Set Direction

type MazeGen =
  { grid :: Grid Walls
  , position :: Hex Int
  , unvisited :: Set (Hex Int)
  , stack :: List (Hex Int)
  }

type Generate m a = StateT MazeGen m a

maze :: forall m a. MonadGen m => Grid a -> Hex Int -> m (Grid Walls)
maze shape start =
  _.grid <$>
    execStateT
      go
      { grid: map (const $ Set.fromFoldable allDirections) shape
      , position: start
      , unvisited: Set.fromFoldable $ coordinates shape
      , stack: Nil
      }
  where
    go :: MonadGen m => Generate m Unit
    go =
      do
        neighbors >>= NonEmptyList.fromList >>> case _ of
          -- We can move to one or more cells.
          Just ns -> do
            -- First, we store our current position on the stack.
            pos <- gets _.position
            push pos

            -- We pick a random direction to move in, and its resulting position.
            { dir, pos' } <- lift $ elements $ unwrap ns

            -- We remove the appropriate walls.
            removeWall pos dir
            removeWall pos' (opposite dir)

            -- Finally, we move to the new position, and mark it as visited.
            moveTo pos'
            visit pos'

          -- There are no more cells to move to from here.
          Nothing -> do
            -- So, we go back to the last cell we visited, by looking at the stack.
            next <- pop
            for_ next moveTo

        whenM (not <$> Set.isEmpty <$> gets _.unvisited) go

    neighbors =
      do
        gen <- get
        pure $ List.mapMaybe (check gen) allDirections

    check gen dir =
      let
        pos' = gen.position + direction dir
      in
        if pos' `member` gen.grid && pos' `Set.member` gen.unvisited
        then Just { dir, pos' }
        else Nothing

    removeWall :: Hex Int -> Direction -> Generate m Unit
    removeWall pos dir =
      do
        grid <- gets (_.grid)
        modify (_ { grid = grid # update (Set.delete dir) pos})

    moveTo :: Monad m => Hex Int -> Generate m Unit
    moveTo pos = modify (_ { position = pos })

    visit :: Monad m => Hex Int -> Generate m Unit
    visit pos = modify (\gen -> gen { unvisited = Set.delete pos gen.unvisited })

    push :: Monad m => Hex Int -> Generate m Unit
    push h = modify (\gen -> gen { stack = h : gen.stack })

    pop :: Monad m => Generate m (Maybe (Hex Int))
    pop =
      do
        stack <- gets _.stack
        case stack of
          (top : rest) -> do
            modify (_ { stack = rest })
            pure $ Just top
          _ ->
            pure $ Nothing
