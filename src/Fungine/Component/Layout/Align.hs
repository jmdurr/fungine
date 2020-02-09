module Fungine.Component.Layout.Align where

import Fungine.Component
import Fungine.Prelude
import Fungine.Graphics.Geometry


centerHAlign :: UIComponent e -> UIComponent e
centerHAlign c = c{uiRectangle= \info ->
  let r = uiBounds info
      (bw,_) = rectangleDim r
      cr = uiRectangle c info
      (cw,_) = rectangleDim cr in
        if cw > bw 
          then rectangleOffset cr (negate (cw - bw / 2), 0)
          else rectangleOffset cr ((bw - cw ) / 2, 0)
}


centerVAlign :: UIComponent e -> UIComponent e 
centerVAlign c = c{uiRectangle= \info ->
  let r = uiBounds info
      (_,bh) = rectangleDim r
      cr = uiRectangle c info
      (_,ch) = rectangleDim cr in
        if ch > bh 
          then rectangleOffset cr (0, negate (ch - bh / 2))
          else rectangleOffset cr (0, (bh - ch ) / 2)
  }