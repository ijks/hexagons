module Util where

checkIdentity :: (Eq a, Show a) => (a -> b) -> (b -> a) -> a -> Bool
checkIdentity to from x = from (to x) == x
