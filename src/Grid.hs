module Grid where

import Hexagon

squareGrid :: Integral a => Parity -> (a, a) -> [Hex a]
squareGrid par (w, h) =
    [ fromOffset par (x, y)
    | x <- [1 .. w]
    , y <- [1 .. h]
    ]
