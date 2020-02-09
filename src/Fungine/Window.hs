module Fungine.Window where

import Protolude
import Fungine.Component

{- This is a user defined window, it depends purely on what the
   application wants it to do. It will however react to system 
   events via the engine
-}

data VideoMode = BestVideoMode
               | VideoMode Int Int Int -- ^width, height, bits per pixel
               deriving (Eq,Show)

data WindowSize = Fullscreen VideoMode
                | SizedWindow Int Int Int Int -- ^x y w h
                deriving (Eq,Show)

type WindowTitle = Text
type WindowId = Text

data Window e = Window { wSize :: WindowSize
                       , wId :: WindowId
                       , wTitle :: WindowTitle
                       , wHandlers :: [WindowEventHandler e]
                       , wUI :: UIComponent e
                       }

data WindowEvent = WindowResizeEvent (Int,Int)
                 | WindowFramebufferResizeEvent (Int,Int)
                 | WindowCloseEvent
                 | WindowCreateEvent
                 | WindowIconifyEvent
                 | WindowRestoreEvent
                 | WindowFocusEvent
                 | WindowBlurEvent
                 | WindowRefreshEvent
                 | WindowPositionEvent (Int,Int)
                 deriving (Show)

type WindowEventHandler e = WindowEvent -> Maybe e

window :: WindowId -> WindowSize -> Text -> [WindowEventHandler e] -> UIComponent e -> Window e
window wid ws = Window ws wid

onResize :: ((Int, Int) -> e) -> WindowEventHandler e
onResize rf (WindowResizeEvent sz) = Just $ rf sz
onResize _  _                      = Nothing

onClose :: e -> WindowEventHandler e
onClose e WindowCloseEvent = Just e
onClose _ _                = Nothing

onCreate :: e -> WindowEventHandler e
onCreate e WindowCreateEvent = Just e
onCreate _ _                 = Nothing

onIconify :: e -> WindowEventHandler e
onIconify e WindowIconifyEvent = Just e
onIconify _ _                  = Nothing

onRestore :: e -> WindowEventHandler e
onRestore e WindowRestoreEvent = Just e
onRestore _ _                  = Nothing

onFocus :: e -> WindowEventHandler e
onFocus e WindowFocusEvent = Just e
onFocus _ _                = Nothing

onBlur :: e -> WindowEventHandler e
onBlur e WindowBlurEvent = Just e
onBlur _ _               = Nothing

onFramebufferResize :: ((Int, Int) -> e) -> WindowEventHandler e
onFramebufferResize rf (WindowFramebufferResizeEvent sz) = Just $ rf sz
onFramebufferResize _  _ = Nothing
