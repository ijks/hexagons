module Gloss where

import Graphics.Gloss

import Hexagon

grid :: Parity -> (Int, Int) -> Float -> Picture
grid par (cols, rows) s =
    pictures
        [ drawHex s $ fromOffset par (x, y)
        | x <- [0 .. cols - 1]
        , y <- [0 .. cols - 1]
        ]

drawHex :: Integral a => Float -> Hex a -> Picture
drawHex s h =
      scale s s
    $ uncurry translate (pixelCoords (fromIntegral <$> h))
    $ lineLoop
        [ (cos angle, sin angle)
        | i <- [0 .. 5]
        , let angle = pi / 3 * i
        ]
