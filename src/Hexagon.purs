module Hexagon where

-- Based on http://www.redblobgames.com/grids/hexagons/

import Prelude

import Math (pi, sqrt)
import Data.Generic (class Generic, gShow)
import Data.Int (odd, toNumber)
import Data.List (List)
import Data.List as List
import Data.Monoid (class Monoid, mempty)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Ord (abs)
import Data.Pair (Pair(..))

-- | The coordinates of a hexagon in a grid.
data Hex a = Hex a a

derive instance eqHex :: Eq a => Eq (Hex a)
derive instance functorHex :: Functor Hex
derive instance genericHex :: Generic a => Generic (Hex a)
derive instance ordHex :: Ord a => Ord (Hex a)

instance showHex :: Generic a => Show (Hex a) where
  show = gShow

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

-- * Neighbors

-- FIXME: Put `Direction` in its own module
data Direction
  = Top
  | TopLeft
  | TopRight
  | Bottom
  | BottomLeft
  | BottomRight

derive instance eqDirection :: Eq Direction
derive instance ordDirection :: Ord Direction

allDirections :: List Direction
allDirections =
  List.fromFoldable
    [ Top
    , TopLeft
    , TopRight
    , Bottom
    , BottomLeft
    , BottomRight
    ]

turnLeft :: Direction -> Direction
turnLeft Top = TopLeft
turnLeft TopLeft = BottomLeft
turnLeft BottomLeft = Bottom
turnLeft Bottom = BottomRight
turnLeft BottomRight = TopRight
turnLeft TopRight = Top

turnRight :: Direction -> Direction
turnRight Top = TopRight
turnRight TopRight = BottomRight
turnRight BottomRight = Bottom
turnRight Bottom = BottomLeft
turnRight BottomLeft = TopLeft
turnRight TopLeft = Top

opposite :: Direction -> Direction
opposite Top = Bottom
opposite Bottom = Top
opposite TopRight = BottomLeft
opposite BottomLeft = TopRight
opposite TopLeft = BottomRight
opposite BottomRight = TopLeft

angles :: Direction -> Pair Number
angles = map (\i -> pi / 3.0 * toNumber i) <<< corners
  where
  corners TopLeft = Pair 0 1
  corners BottomLeft = Pair 1 2
  corners Bottom = Pair 2 3
  corners BottomRight = Pair 3 4
  corners TopRight = Pair 4 5
  corners Top = Pair 5 0

-- | Get a hex that describes an offset in a direction
direction :: forall a. Ring a => Direction -> Hex a
direction Top = Hex zero (negate one)
direction TopLeft = Hex (negate one) zero
direction TopRight = Hex one (negate one)
direction Bottom = Hex zero one
direction BottomLeft = Hex (negate one) one
direction BottomRight = Hex one zero

-- | A neighbor of a hex in a specific direction.
neighbor :: forall a. Ring a => Direction -> Hex a -> Hex a
neighbor = add <<< direction

-- | All the neighbors of a hex, excluding the hex itself.
neighbors :: forall a. Ring a => Hex a -> List (Hex a)
neighbors h = map (_ `neighbor` h) allDirections

-- * Calculations with Hexes

-- | Calculate the Manhattan distance between two hexes
distance :: forall a. Ord a => Ring a => Hex a -> Hex a -> a
distance (Hex c1 r1) (Hex c2 r2) =
  abs (c1 - c2) + abs (r1 - r2)

-- | Scale a hex by a scalar value
scale :: forall a. Ring a => a -> Hex a -> Hex a
scale s = map (_ * s)
