module Main where

import Graphics.Gloss

import Hexagon
import Gloss

main :: IO ()
main = display
    (InWindow "grid" (512, 512) (50, 50))
    black
    (color white $ grid Odd (10, 10) 20)
