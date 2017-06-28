module Grid where

import Prelude

import Data.Foldable (class Foldable)
import Data.List as List
import Data.List (List(..), mapMaybe, (:), (..))
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..), fst)
import Data.Unfoldable (class Unfoldable)

import Hexagon

newtype Grid a = Grid (Map (Hex Int) a)

derive instance newtypeGrid :: Newtype (Grid a) _
derive newtype instance foldableGrid :: Foldable Grid
derive newtype instance traversableGrid :: Traversable Grid

instance showGrid :: Show a => Show (Grid a) where
  show (Grid map) = "Grid " <> (show (Map.toUnfoldable map :: Array _))

fromFoldable :: forall f a. Foldable f => f (Tuple (Hex Int) a) -> Grid a
fromFoldable = Map.fromFoldable >>> wrap

toUnfoldable :: forall f a. Unfoldable f => Grid a -> f (Tuple (Hex Int) a)
toUnfoldable = unwrap >>> Map.toUnfoldable

fromCoordinates :: forall f a. Functor f => Foldable f => (Hex Int -> a) -> f (Hex Int) -> Grid a
fromCoordinates fun = map (\c -> Tuple c (fun c)) >>> fromFoldable

coordinates :: forall a. Grid a -> List (Hex Int)
coordinates = unwrap >>> Map.keys

values :: forall a. Grid a -> List a
values = unwrap >>> Map.values

index :: forall a. Grid a -> Hex Int -> Maybe a
index (Grid grid) hex = Map.lookup hex grid

infixl 8 index as !!

update :: forall a. (a -> a) -> Hex Int -> Grid a -> Grid a
update f coord = wrap <<< Map.update (Just <<< f) coord <<< unwrap

alter :: forall a. (Maybe a -> Maybe a) -> Hex Int -> Grid a -> Grid a
alter f coord = wrap <<< Map.alter f coord <<< unwrap

alterMaybe :: forall a. (a -> a) -> Hex Int -> Grid a -> Maybe (Grid a)
alterMaybe f coord (Grid grid) =
  do
    x <- Map.lookup coord grid
    pure $ wrap $ Map.insert coord (f x) grid

neighborhood :: forall a. Hex Int -> Grid a -> List a
neighborhood hex grid = mapMaybe (grid !! _) (List.fromFoldable $ neighbors hex)

mask :: forall a b. Grid a -> Grid b -> Grid b
mask m = toUnfoldable >>> List.filter (fst >>> (m !! _) >>> isJust) >>> fromFoldable

square :: Parity -> { width :: Int, height :: Int } -> List (Hex Int)
square par { width, height } =
  do
    column <- 1 .. width
    row <- 1 .. height
    pure $ fromOffset par { column, row }

line :: Direction -> Int -> Hex Int -> List (Hex Int)
line _ 0 _ = Nil
line dir n start = start : line dir (n - 1) (neighbor dir start)

ring :: Hex Int -> Int -> List (Hex Int)
ring center radius =
  do
    dir <- List.fromFoldable allDirections
    let corner = center + scale radius (direction dir)
    let dir' = turnLeft $ turnLeft dir
    line dir' radius corner

hexagon :: Hex Int -> Int -> List (Hex Int)
hexagon center radius = center : (1 .. radius >>= ring center)
