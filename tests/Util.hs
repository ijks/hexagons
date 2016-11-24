module Util where

preserves :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
preserves f g x = f x == g x

approxEq :: (Ord a, Num a) => a -> a -> a -> Bool
approxEq epsilon a b = abs (a - b) < epsilon
