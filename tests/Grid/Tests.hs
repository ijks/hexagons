{-# OPTIONS_GHC -fno-warn-orphans #-}

module Grid.Tests
    ( tests
    ) where

import Test.Tasty
import Test.Tasty.QuickCheck

import Grid
import Hexagon (Parity)

import Instances

tests :: TestTree
tests = testGroup "Grid"
    [ testProperty "a grid of size (w, h) has w * h tiles"
        (gridSize :: Parity -> (NonNegative Int, NonNegative Int) -> Bool)
    ]

gridSize par (NonNegative w, NonNegative h) =
    length (squareGrid par (w, h)) == w * h
