{-# OPTIONS_GHC -fno-warn-orphans #-}

module Instances where

import Test.Tasty.QuickCheck

import Grid
import Hexagon

instance (Arbitrary a, Num a) => Arbitrary (Hex a) where
    arbitrary = fromAxial <$> arbitrary

instance Arbitrary Parity where
    arbitrary = elements [Odd, Even]
