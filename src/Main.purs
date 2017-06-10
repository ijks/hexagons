module Main where

import Prelude

import Color (black)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Int (toNumber)
import Data.List ((..))
import Data.Maybe (isJust)
import Graphics.Drawing (closed) as Shape
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
drawing = do
  Drawing.scale 10.0 10.0 $ draw (fillColor black) hexShape

-- drawGrid :: forall a. Grid a -> Drawing
-- drawGrid =

hexShape :: Shape
hexShape = Shape.closed $ do
  corner <- 0 .. 5
  let angle = pi / 3.0 * toNumber corner
  pure { x: cos angle, y: sin angle }
