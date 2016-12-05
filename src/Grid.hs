module Grid where

import Hexagon

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
