{-# LANGUAGE ScopedTypeVariables #-}

module Hexagon.Tests
    ( tests
    ) where

import Test.Tasty
import Test.Tasty.QuickCheck

import Hexagon

import Instances
import Util

tests :: TestTree
tests = testGroup "Hexagon"
    [ testProperty "conversion to and then from cube is idempotent"
        (cubeIdentity :: Hex Int -> Bool)
    , testProperty "conversion to and then from axial is idempotent"
        (axialIdentity :: Hex Int -> Bool)
    , testProperty "conversion to and then from offset is idempotent"
        (offsetIdentity :: Parity -> Hex Int -> Bool)
    , testProperty "conversion to and then from pixel is idempotent"
        (pixelIdentity :: Hex Float -> Bool)
    , testProperty "cube coords are always in the same plane"
        (cubePlanar :: Hex Int -> Bool)
    , testProperty "a hexagon has exactly 6 neighbors"
        (neighborsCount :: Hex Int -> Bool)
    ]

cubeIdentity = (fromCube . cubeCoords) `preserves` id

axialIdentity = (fromAxial . axialCoords) `preserves` id

offsetIdentity par = (fromOffset par . offsetCoords par) `preserves` id

pixelIdentity h = approxEq 10e-4 h (fromPixel (pixelCoords h))

cubePlanar = planar . cubeCoords
    where planar (x, y, z) = x + y + z == 0

neighborsCount h = length (neighbors h) == 6
