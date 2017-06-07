module Grid where

import Prelude

import Data.List (List(..), fromFoldable, mapMaybe, (:), (..))
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)

import Hexagon

newtype Grid a = Grid (Map (Hex Int) a)

derive instance newtypeGrid :: Newtype (Grid a) _

index :: forall a. Grid a -> Hex Int -> Maybe a
index (Grid grid) hex = Map.lookup hex grid

infixl 8 index as !!

neighborhood :: forall a. Hex Int -> Grid a -> List a
neighborhood hex grid = mapMaybe (grid !! _) (fromFoldable $ neighbors hex)

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
    dir <- fromFoldable allDirections
    let corner = center + scale radius (direction dir)
    let dir' = turnLeft $ turnLeft dir
    line dir' radius corner

hexagon :: Hex Int -> Int -> List (Hex Int)
hexagon center radius = center : (1 .. radius >>= ring center)
