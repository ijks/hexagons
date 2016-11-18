{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE ScopedTypeVariables #-}

module Hexagon.Tests
    ( tests
    ) where

import Test.Tasty
import Test.Tasty.QuickCheck

import Hexagon

import Util

tests :: TestTree
tests = testGroup "Hexagon"
    [ testProperty "conversion to and then from cube is idempotent"
        (cubeIdentity :: Hex Int -> Bool)
    , testProperty "conversion to and then from axial is idempotent"
        (axialIdentity :: Hex Int -> Bool)
    , testProperty "conversion to and then from offset is idempotent"
        (offsetIdentity :: Parity -> Hex Int -> Bool)
    , testProperty "cube coords are always in the same plane"
        (cubePlanar :: Hex Int -> Bool)
    , testProperty "a hexagon has exactly 6 neighbors"
        (neighborsCount :: Hex Int -> Bool)
    ]

instance (Arbitrary a, Num a) => Arbitrary (Hex a) where
    arbitrary = fromAxial <$> arbitrary

cubeIdentity :: (Eq a, Num a, Show a) => Hex a -> Bool
cubeIdentity = checkIdentity cubeCoords fromCube

axialIdentity :: (Eq a, Num a, Show a) => Hex a -> Bool
axialIdentity = checkIdentity axialCoords fromAxial

instance Arbitrary Parity where
    arbitrary = elements [Odd, Even]

offsetIdentity :: (Eq a, Integral a, Show a) => Parity -> Hex a -> Bool
offsetIdentity par = checkIdentity (offsetCoords par) (fromOffset par)

cubePlanar :: (Eq a, Num a, Show a) => Hex a -> Bool
cubePlanar = planar . cubeCoords
    where planar (x, y, z) = x + y + z == 0

neighborsCount :: Num a => Hex a -> Bool
neighborsCount h = length (neighbors h) == 6
