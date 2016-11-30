module Grid where

import Hexagon

squareGrid :: Integral a => Parity -> (a, a) -> [Hex a]
squareGrid par (w, h) =
    [ fromOffset par (x, y)
    | x <- [1 .. w]
    , y <- [1 .. h]
    ]

straightLine :: Integral a => Direction -> a -> Hex a -> [Hex a]
straightLine _ 0 s = []
straightLine d n s = s : straightLine d (n - 1) (neighbor d s)
