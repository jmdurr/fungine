module Fungine.Component.Image where

import Fungine.Prelude
import Fungine.Component
import Fungine.Graphics.Geometry
import qualified Data.Map.Strict as M

image :: FilePath -> UIComponent e
image fp = emptyUIComponent { uiRenderable = UIImage fp
                            , uiRectangle = \info ->
                              case M.lookup (UIImage fp) (uiLoads info) of
                                Just (UILoadedImage ((iw,ih),_)) ->
                                  let (bw,bh) = rectangleDim (uiBounds info)
                                      w = if fromIntegral iw > bw
                                            then bw
                                            else fromIntegral iw
                                      h = if fromIntegral ih > bh
                                            then bh
                                            else fromIntegral ih
                                  in
                                  mkRectangle (rectangleTopLeft (uiBounds info)) w h
                                _ -> uiBounds info
}
