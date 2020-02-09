module Fungine.Component.Layout.AspectRatio where

import Fungine.Prelude
import Fungine.Component
import Fungine.Graphics.Geometry

-- | scales the child to fit the closest dimension to the bounding box keeping a specified ratio
aspectRatio :: (Float, Float) -> UIComponent e -> UIComponent e
aspectRatio (wr, hr) c = c{uiRectangle = \info ->
  let r = uiBounds info
      (bw,bh) = rectangleDim r
      cr = uiRectangle c info
      (cw,ch) = rectangleDim cr 
      wRatio = bw / cw
      hRatio = bh / ch
      in
        if wRatio > hRatio
          then mkRectangle (rectangleTopLeft r) bw (bw / wr * hr) 
          else mkRectangle (rectangleTopLeft r) (bh / hr * wr) bh
}
 