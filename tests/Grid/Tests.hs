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
    [ testProperty "a grid of size (w, h) has w * h tiles"
        (gridSize :: Parity -> (NonNegative Int, NonNegative Int) -> Bool)
    , testProperty "a line of length l has l tiles"
        (lineLength :: Direction -> NonNegative Int -> Hex Int -> Bool)
    , testProperty "a ring of size r has r * 6 tiles"
        (ringSize :: Hex Int -> NonNegative Int -> Bool)
    ]

gridSize par (NonNegative w, NonNegative h) =
    length (squareGrid par (w, h)) == w * h

lineLength dir (NonNegative l) h =
    length (straightLine dir l h) == l

ringSize h (NonNegative r) =
    length (ring h r) == r * 6
