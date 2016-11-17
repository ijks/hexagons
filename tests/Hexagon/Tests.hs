{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE ScopedTypeVariables #-}

module Hexagon.Tests
    ( tests
    ) where

import Test.Tasty
import Test.Tasty.QuickCheck

import Hexagon

tests :: TestTree
tests = testGroup "Hexagon"
    [ testProperty "axial coords ensure planar hexes"
        (axialPlanar :: (Int, Int) -> Bool)
    , testProperty "neighbors of a planar hex are planar"
        (neighborsPlanar :: Hex Int -> Bool)
    ]

instance (Arbitrary a, Num a) => Arbitrary (Hex a) where
    arbitrary = fromAxial <$> arbitrary

planar :: (Eq a, Num a) => Hex a -> Bool
planar (Hex x y z) = x + y + z == 0

axialPlanar :: (Eq a, Num a) => (a, a) -> Bool
axialPlanar c = planar $ fromAxial c

neighborsPlanar :: (Eq a, Num a) => Hex a -> Bool
neighborsPlanar h = all planar (neighbors h)
