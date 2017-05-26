module Hexagon where

-- Based on http://www.redblobgames.com/grids/hexagons/

import Prelude

import Data.Int (odd)
import Math (sqrt)
import Data.Monoid (class Monoid, mempty)

-- | The coordinates of a hexagon in a grid.
data Hex a = Hex a a

derive instance eqHex :: (Eq a) => Eq (Hex a)
derive instance functorHex :: Functor Hex
derive instance ordHex :: (Ord a) => Ord (Hex a)

instance applyHex :: Apply Hex where
    apply (Hex f g) (Hex row col) =
        Hex (f row) (g col)

instance applicativeHex :: Applicative Hex where
    pure x = Hex x x

instance semigroupHex :: (Semigroup a) => Semigroup (Hex a) where
    append a b = append <$> a <*> b

instance monoidHex :: (Monoid a) => Monoid (Hex a) where
    mempty = pure mempty

instance semiringHex :: (Semiring a) => Semiring (Hex a) where
    one = pure one
    mul a b = mul <$> a <*> b
    zero = pure zero
    add a b = add <$> a <*> b

instance ringHex :: (Ring a) => Ring (Hex a) where
    sub a b = sub <$> a <*> b

instance commutativeRingHex :: (CommutativeRing a) => CommutativeRing (Hex a)

-- * Coordinate Systems

-- ** Axial Coordinates

column :: forall a. Hex a -> a
column = axialCoords >>> _.column

row :: forall a. Hex a -> a
row = axialCoords >>> _.row

-- | Get the axial coordinates of a hexagon.
axialCoords :: forall a. Hex a -> { column :: a, row :: a }
axialCoords (Hex column row) = { column, row }

-- | Create a hexagon from axial coordinates.
fromAxial :: forall a. { column :: a, row :: a } -> Hex a
fromAxial { column, row } = Hex column row

-- ** Cube Coordinates

-- | Get the cube coordinates of a hex.
cubeCoords :: forall a. Ring a => Hex a -> { x :: a, y :: a, z :: a }
cubeCoords (Hex col row) = { x: col, y: -col - row, z: row}

-- | Create a hex from cube coordinates.
fromCube :: forall a. { x :: a, y :: a, z :: a } -> Hex a
fromCube { x, y, z } = Hex x z

cubeX :: forall a. Ring a => Hex a -> a
cubeX = cubeCoords >>> _.x

cubeY :: forall a. Ring a => Hex a -> a
cubeY = cubeCoords >>> _.y

cubeZ :: forall a. Ring a => Hex a -> a
cubeZ = cubeCoords >>> _.z

-- ** Offset Coordinates
data Parity = Odd | Even

derive instance eqParity :: Eq Parity

parity :: Int -> Int
parity n
    | odd n = 1
    | otherwise = 0

offsetCoords :: Parity -> Hex Int -> { column :: Int, row :: Int }
offsetCoords par (Hex col row) =
    case par of
        Even -> { column: col, row: row + (col + parity col) / 2 }
        Odd -> { column: col, row: row + (col - parity col) / 2 }

fromOffset :: Parity -> { column :: Int, row :: Int } -> Hex Int
fromOffset par { row, column } =
    case par of
        Even -> Hex column (row - (column + parity column) / 2)
        Odd -> Hex column (row - (column - parity column) / 2)

-- ** Pixel Coordinates

pixelCoords :: Hex Number -> { x :: Number, y :: Number }
pixelCoords (Hex col row) =
    -- This is better expressed as a matrix multiplication, but this is the
    -- only case I've needed it so far, so it's just inlined.
    { x: 3.0 / 2.0 * col, y: sqrt 3.0 * (row + col / 2.0) }

fromPixel :: { x :: Number, y :: Number } -> Hex Number
fromPixel { x, y } =
    let
        col = x * 2.0 / 3.0
        row = -x / 3.0 + sqrt 3.0 / 3.0 * y
    in
        Hex col row
