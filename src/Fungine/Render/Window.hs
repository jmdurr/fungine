module Fungine.Render.Window where

import           Protolude
import           FRP.Behavior
import           Fungine.Window
import qualified Data.Map as M
import Fungine.Error

data WindowCallback mon win = FramebufferSizeCallback (win -> (Int,Int) -> IO ())
                            | IconifyCallback (win -> IO ())
                            | RestoreCallback (win -> IO ())
                            | FocusCallback (win -> IO ())
                            | BlurCallback (win -> IO ())
                            | RefreshCallback (win -> IO ())
                            | CloseCallback (win -> IO ())
                            | SizeCallback (win -> (Int,Int) -> IO ())
                            | PositionCallback (win -> (Int,Int) -> IO ())

class WindowSystem mon win a where
    initWindowSystem :: IO ()
    exitWindowSystem :: IO ()
    resizeWindow :: win -> (Int,Int) -> IO ()
    createWindow :: WindowSize -> WindowTitle -> IO (CanError (Window mon win))


data RenderWindowEvent mon win = CreateWindow (Window mon win)
                               | DestroyWindow (Window mon win)
                               | ResizeWindow (Window mon win) (Int,Int)
                               | InitRenderWindow
                               | ExitRenderWindow
                               | AddWindowCallbacks (Window mon win) [WindowCallback mon win]

type RenderWindowHandler mon win = RenderWindowEvent mon win -> IO ()

init :: Chan WindowEvent -> RenderWindowHandler mon win -> IO ()
init ch hndl = 
    hndl InitRenderWindow

render :: Window -> WindowState mon win ->  

