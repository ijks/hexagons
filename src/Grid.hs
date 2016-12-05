module Grid where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

import Hexagon

newtype Grid a = Grid { unGrid :: Map (Hex Int) a }

elemAt :: Hex Int -> Grid a -> Maybe a
elemAt h (Grid m) = Map.lookup h m

(!?) :: Grid a -> Hex Int -> Maybe a
(!?) = flip elemAt

neighborhood :: Hex Int -> Grid a -> [a]
neighborhood h g = mapMaybe (g !?) (neighbors h)

square :: Integral a => Parity -> (a, a) -> [Hex a]
square par (w, h) =
    [ fromOffset par (x, y)
    | x <- [1 .. w]
    , y <- [1 .. h]
    ]

straightLine :: Integral a => Direction -> a -> Hex a -> [Hex a]
straightLine _ 0 s = []
straightLine d n s = s : straightLine d (n - 1) (neighbor d s)

ring :: Integral a => Hex a -> a -> [Hex a]
ring c r = do
    dir <- allDirections
    let corner = scale dir r c
    let dir' = turnLeft (turnLeft dir)
    straightLine dir' r corner

hexagon :: Integral a => Hex a -> a -> [Hex a]
hexagon c r = c : ([1 .. r] >>= ring c)
