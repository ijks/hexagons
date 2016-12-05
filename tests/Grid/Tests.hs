{-# OPTIONS_GHC -fno-warn-orphans #-}

module Grid.Tests
    ( tests
    ) where

import Test.Tasty
import Test.Tasty.QuickCheck

import Grid
import Hexagon (Direction, Hex, Parity)

import Instances

tests :: TestTree
tests = testGroup "Grid"
    [ testProperty "a square grid of size (w, h) has w * h tiles"
        (squareSize :: Parity -> (NonNegative Int, NonNegative Int) -> Bool)
    , testProperty "a line of length l has l tiles"
        (lineLength :: Direction -> NonNegative Int -> Hex Int -> Bool)
    , testProperty "a ring of size r has r * 6 tiles"
        (ringSize :: Hex Int -> NonNegative Int -> Bool)
    , testProperty "a hexagonal grid has the right amount of tiles"
        (hexagonSize :: Hex Int -> NonNegative Int -> Bool)
    , testProperty "a hexagonal grid always contains the center tile"
        (hexagonCenter :: Hex Int -> NonNegative Int -> Bool)
    ]

squareSize par (NonNegative w, NonNegative h) =
    length (square par (w, h)) == w * h

lineLength dir (NonNegative l) h =
    length (straightLine dir l h) == l

ringSize h (NonNegative r) =
    length (ring h r) == r * 6

hexagonSize h (NonNegative r) =
    length (hexagon h r) == 1 + 6 * sum [1 .. r]

hexagonCenter h (NonNegative r) =
    h `elem` hexagon h r
