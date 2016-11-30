module Main where

import Graphics.Gloss

import Hexagon hiding (scale)
import Grid

main :: IO ()
main = display
    (InWindow "hexagons" (512, 512) (50, 50))
    black
    (color white $ drawGrid Odd (10, 10) 20)

drawGrid :: Parity -> (Int, Int) -> Float -> Picture
drawGrid par size s = pictures $ drawHex s <$> squareGrid par size

drawHex :: Integral a => Float -> Hex a -> Picture
drawHex s h =
      scale s s
    $ uncurry translate (pixelCoords (fromIntegral <$> h))
    $ lineLoop
        [ (cos angle, sin angle)
        | i <- [0 .. 5]
        , let angle = pi / 3 * i
        ]
