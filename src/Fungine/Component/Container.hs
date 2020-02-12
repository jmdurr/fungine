module Fungine.Component.Container where

import Fungine.Prelude
import Fungine.Component
import Fungine.Graphics.Measure
import Fungine.Graphics.Geometry

data Side = LeftSide | TopSide | BottomSide | RightSide | AllSides


data ContainerStyle = Width Measure -- ^breakpoint at which measure is active above, and width
                    | Height Measure
                    | Padding Side Measure
                    | Margin Side Measure

data BoxProps = BoxProps { left :: Float
                         , right ::  Float
                         , top :: Float
                         , bottom :: Float
                         }

data Container = Container { width :: Float
                           , height :: Float
                           , padding :: BoxProps
                           , margins :: BoxProps
                           }

emptyContainer :: (Float, Float) -> Container
emptyContainer (wb, hb) =
  Container { width = wb, height = hb, padding = BoxProps 0 0 0 0, margins = BoxProps 0 0 0 0 }

convertMeasure :: UIInfo -> Measure -> ((Float, Float) -> Float) -> Float
convertMeasure info m pos = measureToPx (uiEmSize info) m (pos $ rectangleDim (uiBounds info))

getMod :: ContainerStyle -> (UIInfo -> Container -> Container)
getMod (Width  m) = \info c -> c { width = convertMeasure info m fst }
getMod (Height m) = \info c -> c { height = convertMeasure info m snd }
getMod (Padding LeftSide m) =
  \info c -> c { padding = (padding c) { left = convertMeasure info m fst } }
getMod (Padding RightSide m) =
  \info c -> c { padding = (padding c) { right = convertMeasure info m fst } }
getMod (Padding TopSide m) =
  \info c -> c { padding = (padding c) { top = convertMeasure info m snd } }
getMod (Padding BottomSide m) =
  \info c -> c { padding = (padding c) { bottom = convertMeasure info m snd } }
getMod (Padding AllSides m) = \info c -> foldr
  (\cs c' -> getMod cs info c')
  c
  [Padding BottomSide m, Padding TopSide m, Padding LeftSide m, Padding RightSide m]

getMod (Margin LeftSide m) =
  \info c -> c { margins = (margins c) { left = convertMeasure info m fst } }

getMod (Margin RightSide m) =
  \info c -> c { margins = (margins c) { right = convertMeasure info m fst } }

getMod (Margin TopSide m) =
  \info c -> c { margins = (margins c) { top = convertMeasure info m snd } }

getMod (Margin BottomSide m) =
  \info c -> c { margins = (margins c) { bottom = convertMeasure info m snd } }
getMod (Margin AllSides m) = \info c -> foldr
  (\cs c' -> getMod cs info c')
  c
  [Margin BottomSide m, Margin TopSide m, Margin LeftSide m, Margin RightSide m]



container :: [ContainerStyle] -> UIComponent e -> UIComponent e
container css c = emptyUIComponent
  { uiRectangle = \info ->
    let
      dims     = rectangleDim (uiBounds info)
      cont     = foldr (\cs c' -> getMod cs info c') (emptyContainer dims) css
      (tx, ty) = rectangleTopLeft (uiBounds info)
    in mkRectangle
      (tx, ty)
      (width cont + left (margins cont) + right (margins cont))
      (height cont + top (margins cont) + bottom (margins cont))
  , uiChildren  =
    [ c
        { uiRectangle = \info ->
          let
            dims     = rectangleDim (uiBounds info)
            cont     = foldr (\cs c' -> getMod cs info c') (emptyContainer dims) css
            (tx, ty) = rectangleTopLeft (uiBounds info)
          in uiRectangle
            c
            info
              { uiBounds = mkRectangle
                ( tx + left (margins cont) + left (padding cont)
                , ty + top (margins cont) + top (padding cont)
                )
                ( width cont
                - left (margins cont)
                - right (margins cont)
                - left (padding cont)
                - right (padding cont)
                )
                ( height cont
                - top (margins cont)
                - bottom (margins cont)
                - top (padding cont)
                - bottom (padding cont)
                )
              }
        }
    ]
  }

{-
|contain bounds (child bounds)                          |

-}

{-

  data UIComponent = UIComponent { position :: (Int,Int)
                                 , dimension :: (Int,Int)
                                 , image :: Matrix RGBA
                                 , child :: Maybe (Component e)
                                 }

  }

  data Component e = UI (UIComponent e) 

  center
  aspectRatio :: SizedComponent c => (Int,Int) -> c -> Component e
  opacity :: Double -> Component e -> Component e
  container :: [ContainerStyle] -> Component e -> Component e
  image :: FilePath -> Component e
  valign :: VAlignment -> Component e -> Component e
  opengl :: Things -> Component e

  BoundingBox = BoundingBox Int Int Int Int

  SizedComponent e = getBoundingBox :: BoundingBox -> BoundingBox 

  renderState :: framebufferSize

  render :: boundingbox -> Component e -> IO [e]

  Component e = (GLView,[e -> Maybe e])
  
  center $ aspectRatio (1,1) $ opacity (animate mdl.time (fadeIn 0.5 >> pure 4 >> fadeOut 0.5))
    container [Width (Percent 80)] $
      image "./resources/logo"
    
    1024x768 - center 800x640
    414x896
    800x600
    1980x1024

  ]


-}
