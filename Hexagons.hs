module Hexagon where

-- | Based on http://www.redblobgames.com/grids/hexagons/

import Data.Monoid ((<>))

data Hex a = Hex
    { cubeX :: a
    , cubeY :: a
    , cubeZ :: a
    } deriving (Show, Eq, Ord)

instance Functor Hex where
    fmap f (Hex x y z) = Hex (f x) (f y) (f z)

instance Applicative Hex where
    pure v = Hex v v v
    (Hex f g h) <*> (Hex x y z) = Hex (f x) (g y) (h z)

instance Monoid a => Monoid (Hex a) where
    mempty = pure mempty
    mappend a b = mappend <$> a <*> b

instance Foldable Hex where
    foldMap f (Hex x y z) = (f x) <> (f y) <> (f z)

instance Num a => Num (Hex a) where
    a + b = (+) <$> a <*> b
    a * b = (*) <$> a <*> b
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = fmap fromInteger . pure

instance Fractional a => Fractional (Hex a) where
    recip = fmap recip
    fromRational = fmap fromRational . pure

cubeCoords :: Hex a -> (a, a, a)
cubeCoords (Hex x y z) = (x, y, z)

fromCube :: (a, a, a) -> Hex a
fromCube (x, y, z) = Hex x y z

axialCoords :: Hex a -> (a, a)
axialCoords (Hex x _ z) = (x, z)

fromAxial :: Num a => (a, a) -> Hex a
fromAxial (col, row) = Hex col (-col - row) row

axialColumn :: Hex a -> a
axialColumn = fst . axialCoords

axialRow :: Hex a -> a
axialRow = snd . axialCoords

top :: Num a => Hex a -> Hex a
top = (+) $ Hex 0 1 (-1)

topLeft :: Num a => Hex a -> Hex a
topLeft = (+) $ Hex (-1) 1 0

topRight :: Num a => Hex a -> Hex a
topRight = (+) $ Hex 1 0 (-1)

bottom :: Num a => Hex a -> Hex a
bottom = (+) $ Hex 0 (-1) 1

bottomLeft :: Num a => Hex a -> Hex a
bottomLeft = (+) $ Hex (-1) 0 1

bottomRight :: Num a => Hex a -> Hex a
bottomRight = (+) $ Hex 1 (-1) 0

neighbors :: Num a => Hex a -> [Hex a]
neighbors h = map ($ h)
    [ top
    , topLeft
    , topRight
    , bottom
    , bottomLeft
    , bottomRight
    ]

distance :: Num a => Ord a => Hex a -> Hex a -> a
distance a b = foldr max 0 $ a - b
