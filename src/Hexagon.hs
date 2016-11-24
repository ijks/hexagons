module Hexagon where

-- Based on http://www.redblobgames.com/grids/hexagons/

import Data.Monoid ((<>))

-- | The coordinates of a hexagon in a grid.
data Hex a = Hex
    { column :: a
    , row :: a
    } deriving (Show, Eq, Ord)

instance Functor Hex where
    fmap f (Hex col row) = Hex (f col) (f row)

instance Applicative Hex where
    pure v = Hex v v
    (Hex f g) <*> (Hex col row) = Hex (f col) (g row)

instance Monoid a => Monoid (Hex a) where
    mempty = pure mempty
    mappend a b = mappend <$> a <*> b

instance Foldable Hex where
    foldMap f (Hex col row) = (f col) <> (f row)

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

-- * Axial Coordinates

-- | Get the axial coordinates of a hex.
axialCoords :: Hex a -> (a, a)
axialCoords (Hex col row) = (col, row)

-- | Create a hex from a tuple of axial coordinates.
fromAxial :: (a, a) -> Hex a
fromAxial (col, row) = Hex col row

-- * Cube Coordinates

-- | Get the cube coordinates of a hex.
cubeCoords :: Num a => Hex a -> (a, a, a)
cubeCoords (Hex col row) = (col, -col - row, row)

-- | Create a hex from a tuple of cube coordinates.
fromCube :: (a, a, a) -> Hex a
fromCube (x, y, z) = Hex x z

cubeX :: Num a => Hex a -> a
cubeX h = case cubeCoords h of (x, _, _) -> x

cubeY :: Num a => Hex a -> a
cubeY h = case cubeCoords h of (_, y, _) -> y

cubeZ :: Num a => Hex a -> a
cubeZ h = case cubeCoords h of (_, _, z) -> z

-- ** Offset Coordinates

data Parity = Odd | Even
    deriving (Eq, Show)

parity :: Integral a => a -> a
parity n
    | odd n = 1
    | otherwise = 0

offsetCoords :: Integral a => Parity -> Hex a -> (a, a)
offsetCoords par (Hex col row) =
    case par of
        Even -> (col, row + (col + parity col) `div` 2)
        Odd -> (col, row + (col - parity col) `div` 2)

fromOffset :: Integral a => Parity -> (a, a) -> Hex a
fromOffset par (col, row) =
    case par of
        Even -> fromAxial (col, row - (col + parity col) `div` 2)
        Odd -> fromAxial (col, row - (col - parity col) `div` 2)

-- * Neighbors

top :: Num a => Hex a -> Hex a
top = (+) $ Hex 0 (-1)

topLeft :: Num a => Hex a -> Hex a
topLeft = (+) $ Hex (-1) 0

topRight :: Num a => Hex a -> Hex a
topRight = (+) $ Hex 1 (-1)

bottom :: Num a => Hex a -> Hex a
bottom = (+) $ Hex 0 1

bottomLeft :: Num a => Hex a -> Hex a
bottomLeft = (+) $ Hex (-1) 1

bottomRight :: Num a => Hex a -> Hex a
bottomRight = (+) $ Hex 1 0

-- | All the neighbors of a hex, excluding the hex itself.
neighbors :: Integral a => Hex a -> [Hex a]
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
