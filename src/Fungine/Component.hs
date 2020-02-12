module Fungine.Component where
import Fungine.Prelude
import Fungine.Graphics.Geometry
import Fungine.Graphics.RGBA
import Graphics.Rendering.OpenGL.GL.Shaders
import Graphics.Rendering.OpenGL.GL.Texturing.Objects

type Font = Text
type FontSize = Float

data UIInfo = UIInfo { uiBounds :: Rectangle
                     , uiEmSize :: Float
                     , uiLoads :: Map UIRenderable UILoadedRenderable
                     , uiFreeBuffers :: [Word32]
                     , uiShader :: Program
                     }

data UIRenderable = UIImage FilePath
                  | UIText Text
                  | UINoRender
                  deriving (Eq, Ord)

data UILoadedRenderable = UILoadedImage ((Int,Int),TextureObject)
                        | UILoadedText Text

data ComponentEvent = ComponentClickEvent
                    | ComponentMouseDownEvent
                    | ComponentMouseUpEvent
                    | ComponentMouseDragEvent
                    | ComponentMouseOverEvent
                    | ComponentMouseOutEvent
                    | ComponentKeyEvent

data UIComponent e = UIComponent { uiOpacity :: Float
                             , uiColor :: Maybe RGBA
                             , uiRenderable :: UIRenderable
                             , uiRotation :: Float
                             , uiHandler :: [ComponentEvent -> Maybe e]
                             , uiRectangle :: UIInfo -> Rectangle
                             , uiChildren :: [UIComponent e]
}

emptyUIComponent :: UIComponent e
emptyUIComponent = UIComponent
  { uiOpacity    = 1
  , uiColor      = Nothing
  , uiRenderable = UINoRender
  , uiRotation   = 0
  , uiHandler    = []
  , uiRectangle  = uiBounds
  , uiChildren   = []
  }

{-
onMouseOver :: InputEvent -> Rectangle -> e -> Maybe e
onMouseOver (MouseMoveEvent x y) r ev =
  let
    (tx, ty) = rectangleTopLeft r
    (w , h ) = rectangleDim r
  in if x > tx && x < tx + w && y > ty && y < ty + h then Just ev else Nothing
onMouseOver _ _ _ = Nothing
-}
