module Drawing where

-- TODO: Re-export most of "Graphics.Drawing" here.

import Prelude

import Color (Color)
import Data.Int (toNumber)
import Data.List ((..))
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Tuple (Tuple(..))
import Math (cos, pi, sin)

import Graphics.Drawing as GD
import Graphics.Drawing (Drawing, Shape, FillStyle, OutlineStyle, Shadow)

data DrawingMode
  = Filled FillStyle
  | Outlined OutlineStyle

newtype Style = Style
  { fill :: Maybe FillStyle
  , outline :: Maybe OutlineStyle
  , shadow :: Maybe Shadow
  }

derive instance eqStyle :: Eq Style

instance semigroupStyle :: Semigroup Style where
  append (Style a) (Style b)=
    Style
      { fill: a.fill <> b.fill
      , outline: a.outline <> b.outline
      , shadow: a.shadow <> b.shadow
      }

instance monoidStyle :: Monoid Style where
  mempty =
    Style
      { fill: mempty
      , outline: mempty
      , shadow: mempty
      }

withFill :: FillStyle -> Style
withFill fill =
  Style { fill: Just fill, outline: mempty, shadow: mempty }

fillColor :: Color -> Style
fillColor = withFill <<< GD.fillColor

withOutline :: OutlineStyle -> Style
withOutline outline =
  Style { outline: Just outline, fill: mempty, shadow: mempty }

lineColor :: Color -> Style
lineColor = withOutline <<< GD.outlineColor

lineWidth :: Number -> Style
lineWidth = withOutline <<< GD.lineWidth

withShadow :: Shadow -> Style
withShadow shadow =
  Style { shadow: Just shadow, fill: mempty, outline: mempty }

shadowColor :: Color -> Style
shadowColor = withShadow <<< GD.shadowColor

shadowBlur :: Number -> Style
shadowBlur = withShadow <<< GD.shadowBlur

shadowOffset :: Number -> Number -> Style
shadowOffset x y = withShadow $ GD.shadowOffset x y

draw :: Style -> Shape -> Drawing
draw (Style style) shape =
  let
    drawShape =
      -- FIXME: this logic could probably be expressed more nicely.
      case Tuple style.fill style.outline of
        Tuple (Just f) (Just o) ->
          GD.filled f <> GD.outlined o
        Tuple (Just f) Nothing ->
          GD.filled f
        Tuple Nothing (Just o) ->
          GD.outlined o
        Tuple Nothing Nothing ->
          -- We don't want to use the 'mempty' for 'Drawing' here, because that
          -- might not preserve shapes that aren't filled or outlined in any way,
          -- but do have a shadow.
          GD.filled mempty
  in
    case style.shadow of
      Just shadow -> GD.shadow shadow $ drawShape shape
      Nothing -> drawShape shape

hex :: Number -> Number -> Number -> Shape
hex left top radius =
  GD.closed $ do
    corner <- 0 .. 5
    let angle = pi / 3.0 * toNumber corner
    pure
      { x: radius * cos angle + left
      , y: radius * sin angle + top
      }
