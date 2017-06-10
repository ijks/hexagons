module Main where

import Prelude

import Color (black)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foldable (class Foldable, foldMap)
import Data.Int (toNumber)
import Data.List ((..))
import Data.Maybe (isJust)
import Graphics.Drawing (Drawing, Shape)
import Graphics.Drawing (closed) as Shape
import Graphics.Drawing (scale) as Drawing
import Graphics.Canvas (CANVAS)
import Math (cos, pi, sin)

import DOM (DOM)
import Flare (UI)
import Flare.Drawing (runFlareDrawing)
import Signal.Channel (CHANNEL)

import Drawing
import Hexagon
import Grid

main :: Eff (console :: CONSOLE, dom :: DOM, channel :: CHANNEL, canvas :: CANVAS) Unit
main = runFlareDrawing "controls" "drawing" $ pure drawing

drawing :: Drawing
drawing = Drawing.scale 10.0 10.0
  $ drawGrid
  $ fromCoordinates (const unit)
  $ square Even { width: 10, height: 10 }

drawGrid :: forall a. Grid a -> Drawing
drawGrid grid = foldMap drawHex $ coordinates grid
  where
    drawHex h =
      let coords = pixelCoords $ map toNumber h
      in draw style $ hex coords.x coords.y 1.0
    style = lineColor black <> lineWidth 0.1
