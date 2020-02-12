module Fungine.Component.Layout.AspectRatio where

import Fungine.Prelude
import Fungine.Component
import Fungine.Graphics.Geometry

-- | scales the child to fit the closest dimension to the bounding box keeping a specified ratio
aspectRatio :: (Float, Float) -> UIComponent e -> UIComponent e
aspectRatio (wr, hr) c = c
  { uiRectangle = \info ->
    let
      r        = uiBounds info
      (bw, bh) = rectangleDim r
      cr       = uiRectangle c info
      (cw, ch) = rectangleDim cr
      -- fit in box
      -- assume width is box width
      ch'      = bw / wr * hr
      -- assume height is box height
      cw'      = bh / hr * wr
    in if ch' < bh
      then mkRectangle (rectangleTopLeft r) bw ch'
      else mkRectangle (rectangleTopLeft r) cw' bh
  }

{-
bw=200
bh=20
cw=41,20
200/3 = 70
20/4 = 5
mkRectangle 200 (200 / 3 * 4 )
-}
