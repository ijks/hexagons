module Hexagon where

-- Based on http://www.redblobgames.com/grids/hexagons/

import Data.Monoid ((<>))

-- | The coordinates of a hexagon in a grid.
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

-- * Coordinate Systems

-- | Get the cube coordinates of a hex.
cubeCoords :: Hex a -> (a, a, a)
cubeCoords (Hex x y z) = (x, y, z)

-- | Create a hex from a tuple of cube coordinates.
fromCube :: (a, a, a) -> Hex a
fromCube (x, y, z) = Hex x y z

-- | Get the axial coordinates of a hex.
axialCoords :: Hex a -> (a, a)
axialCoords (Hex x _ z) = (x, z)

-- | Create a hex from a tuple of axial coordinates.
fromAxial :: Num a => (a, a) -> Hex a
fromAxial (col, row) = Hex col (-col - row) row

-- | Get the axial columnf of a hex.
axialColumn :: Hex a -> a
axialColumn = fst . axialCoords

-- | Get the axial row of a hex.
axialRow :: Hex a -> a
axialRow = snd . axialCoords

-- * Neighbors

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

-- | All the neighbors of a hex, excluding the hex itself.
neighbors :: Num a => Hex a -> [Hex a]
neighbors h = map ($ h)
    [ top
    , topLeft
    , topRight
    , bottom
    , bottomLeft
    , bottomRight
    ]

-- * Calculations with hexes

-- | Calculate the distance between two hexes
distance :: Num a => Ord a => Hex a -> Hex a -> a
distance a b = foldr max 0 $ a - b
