module Main where

import Prelude

import Color (black)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.Foldable (foldMap)
import Data.Int (toNumber)
import Data.List (List)
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..))
import Graphics.Drawing (Drawing)
import Graphics.Drawing (closed, path) as Shape
import Graphics.Drawing (scale, translate) as Drawing
import Graphics.Canvas (CANVAS)
import Math (cos, sin)
import Test.QuickCheck.Gen (Gen, evalGen)
import Test.QuickCheck.LCG (Seed, mkSeed)

import DOM (DOM)
import Flare (UI, int, intSlider, numberSlider)
import Flare.Drawing (runFlareDrawing)
import Signal.Channel (CHANNEL)

import Drawing
import Generate
import Grid
import Hexagon

import Data.Set as Set

main :: Eff (console :: CONSOLE, dom :: DOM, channel :: CHANNEL, canvas :: CANVAS, random :: RANDOM) Unit
main = do
  initialSeed <- randomInt bottom top
  runFlareDrawing "controls" "drawing" $ ui initialSeed

ui :: forall e. Int -> UI e Drawing
ui initialSeed = drawing
  <$> int "Seed" initialSeed
  <*> intSlider "Width" 0 20 10
  <*> intSlider "Height" 0 20 10
  <*> numberSlider "X Offset" (-10.0) 10.0 0.1 0.0
  <*> numberSlider "Y Offset" (-10.0) 10.0 0.1 0.0
  <*> numberSlider "Scale" 1.0 80.0 0.1 20.0

drawing :: Int -> Int -> Int -> Number -> Number -> Number -> Drawing
drawing seed w h x y s = Drawing.scale s s
  $ Drawing.translate x y
  $ drawGrid (const mempty) drawWalls
  $ evalGen (maze shape (Hex 0 0)) genState
  where
    drawHex = const $ draw (lineColor black <> lineWidth 0.1) $ hex 0.0 0.0 1.0
    drawBool = if _ then draw (fillColor black) $ hex 0.0 0.0 0.5 else mempty
    genState = { newSeed: mkSeed seed, size: 10 }
    shape = fromCoordinates (const unit) $ square Even { width: w, height: h }
    drawWalls =
      foldMap
        ( draw (lineColor black <> lineWidth 0.1)
        <<< Shape.path
        <<< map (\a -> { x: cos a, y: sin a })
        <<< angles
        )

drawGrid :: forall a. (Hex Int -> Drawing) -> (a -> Drawing) -> Grid a -> Drawing
drawGrid drawHex drawContents grid =
  foldMap drawItem $ (toUnfoldable grid :: List _)
  where
    drawItem (Tuple h x) =
      let coords = pixelCoords $ map toNumber h
      in Drawing.translate coords.x coords.y
        $ drawHex h <> drawContents x
