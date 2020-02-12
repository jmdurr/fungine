module Fungine.Render.Window where

import Protolude
import Fungine.Window
import qualified Data.Map.Strict as M
import Fungine.Error
import Control.Concurrent.STM.TChan
import Fungine.Render.Component as FC
import Fungine.Component
import Graphics.Rendering.OpenGL.GL.Shaders as S
import Fungine.Graphics.Geometry
import Fungine.Render.Shader
import qualified Graphics.Rendering.OpenGL.GL.Framebuffer as GF
import qualified Graphics.Rendering.OpenGL.GL.Polygons as GP
import Data.StateVar (($=))
import qualified Graphics.Rendering.OpenGL.GL.VertexSpec as GV
import Fungine.Input

data WindowCallback mon win = FramebufferSizeCallback (win -> (Int,Int) -> IO ())
                            | IconifyCallback (win -> IO ())
                            | RestoreCallback (win -> IO ())
                            | FocusCallback (win -> IO ())
                            | BlurCallback (win -> IO ())
                            | RefreshCallback (win -> IO ())
                            | CloseCallback (win -> IO ())
                            | SizeCallback (win -> (Int,Int) -> IO ())
                            | PositionCallback (win -> (Int,Int) -> IO ())

data WindowSystem ws mon win e = WindowSystem { resizeWindow :: win -> WindowSize -> StateT ws IO ()
                                          , createWindow :: Window e -> StateT ws IO (CanError win)
                                          , setWindowTitle :: win -> Text -> StateT ws IO ()
                                          , setCallbacks :: win -> [WindowCallback mon win] -> StateT ws IO ()
                                          , makeGLContextCurrent :: win -> IO ()
                                          , getFramebufferSize :: win -> IO (Int,Int)
                                          , swapBuffers :: win -> IO ()
                                          }


data WindowInternal e mon win = WindowInternal { iWindow :: Window e
                                               , iCurrWindow :: Window e
                                               , iEvents :: TChan WindowEvent
                                               , iSysWindow :: win
                                               , iUIInfo :: UIInfo
                                               , iShaders :: Map Text Program

                                               }

data WindowStateData ws e mon win = WindowStateData { windows :: Map WindowId (WindowInternal e mon win)
                                                 , windowSys :: WindowSystem ws mon win e
                                                 , inputSys :: InputSystem win
                                                 }

type WindowState e ws mon win a = StateT (WindowStateData ws e mon win) (StateT ws IO) a

installInputCallbacks :: TChan WindowEvent -> win -> WindowState e ws mon win ()
installInputCallbacks chan win = do
    ws <- gets inputSys
    liftIO
        (setInputCallback ws win $ InputKeyCallback $ \win' ke ->
            atomically $ writeTChan chan (WindowInputKeyEvent ke)
        )
    liftIO
        (setInputCallback ws win $ InputMouseCallback $ \win' me ->
            atomically $ writeTChan chan (WindowInputMouseEvent me)
        )

installCallbacks :: TChan WindowEvent -> win -> WindowState e ws mon win ()
installCallbacks chan win = do
    ws <- gets windowSys
    lift (setCallbacks ws win cbs)
    installInputCallbacks chan win
  where
    addEvent we = atomically $ writeTChan chan we
    cbs =
        [ FramebufferSizeCallback $ \_ sz -> addEvent $ WindowFramebufferResizeEvent sz
        , IconifyCallback $ \_ -> addEvent WindowIconifyEvent
        , RestoreCallback $ \_ -> addEvent WindowRestoreEvent
        , FocusCallback $ \_ -> addEvent WindowFocusEvent
        , BlurCallback $ \_ -> addEvent WindowBlurEvent
        , RefreshCallback $ \_ -> addEvent WindowRefreshEvent
        , CloseCallback $ \_ -> addEvent WindowCloseEvent
        , SizeCallback $ \_ sz -> addEvent $ WindowResizeEvent sz
        , PositionCallback $ \_ pos -> addEvent $ WindowPositionEvent pos
        ]

newWindow :: Window e -> WindowState e ws mon win (CanError (WindowInternal e mon win))
newWindow win = do
    ws <- gets windowSys
    w  <- lift (createWindow ws win)
    case w of
        Error   t  -> pure (Error t)
        Success w' -> do
            c <- liftIO newTChanIO
            installCallbacks c w'
            liftIO $ makeGLContextCurrent ws w'
            smp <- liftIO loadShaders
            let uis = M.lookup ("ui" :: Text) <$> smp
            case (smp, uis) of
                (Error t     , _                 ) -> pure (Error t)
                (_           , Error _           ) -> pure (Error "shader named 'ui' not found")
                (_           , Success Nothing   ) -> pure (Error "shader named 'ui' not found")
                (Success smp', Success (Just uip)) -> do
                    let
                        wi = WindowInternal
                            { iWindow     = win
                            , iCurrWindow = win
                            , iEvents     = c
                            , iSysWindow  = w'
                            , iShaders    = smp'
                            , iUIInfo     = UIInfo
                                { uiBounds      = mkRectangle (0, 0) 0 0
                                , uiEmSize      = 12
                                , uiLoads       = M.empty
                                , uiFreeBuffers = [1 .. 64000]
                                , uiShader      = uip
                                }
                            }
                    pure (Success wi)


updateWindow
    :: Window e
    -> WindowInternal e mon win
    -> WindowState e ws mon win (CanError (WindowInternal e mon win))
updateWindow win winI = do
    ws <- gets windowSys
    when (wSize win /= wSize (iWindow winI)) (lift $ resizeWindow ws (iSysWindow winI) (wSize win))
    when
        (wTitle win /= wTitle (iWindow winI))
        (lift $ setWindowTitle ws (iSysWindow winI) (wTitle win))
    installCallbacks (iEvents winI) (iSysWindow winI)
    pure (Success winI)

-- TODO collect component events
collectEvents :: Show e => WindowState e ws mon win [e]
collectEvents = do
    wins <- gets $ (M.toList . windows)
    concat
        <$> mapM
                (\(_, w) -> do
                    let ehs = wHandlers (iWindow w)
                    we <- liftIO $ atomically $ tryReadTChan (iEvents w) -- take up to x events
                    return $ mapMaybe (we >>=) ehs
                )
                wins

renderWindow :: Show e => WindowInternal e mon win -> WindowState e ws mon win (CanError [e])
renderWindow w = do
    s   <- gets windowSys
    evs <- collectEvents
    liftIO $ makeGLContextCurrent s (iSysWindow w)
    liftIO $ GF.clearColor $= GV.Color4 1 0 0.5 0.5
    liftIO $ GF.clear [GF.DepthBuffer, GF.ColorBuffer]
    liftIO $ GP.cullFace $= Nothing
    sc <- liftIO $ FC.render (iUIInfo w) (wUI (iWindow w))
    liftIO $ swapBuffers s (iSysWindow w)
    pure ((evs ++) . snd <$> sc)


render :: Show e => Window e -> WindowState e ws mon win (CanError [e])
render win = do
    wins <- gets windows
    mWin <- case M.lookup (wId win) wins of
        Nothing   -> newWindow win
        Just winI -> updateWindow win winI
    case mWin of
        Error   t     -> pure (Error t)
        Success mWin' -> do
            ws <- gets windowSys
            sz <- liftIO $ getFramebufferSize ws (iSysWindow mWin')
            modify
                (\s -> s
                    { windows = M.insert
                        (wId win)
                        mWin'
                            { iUIInfo = (iUIInfo mWin')
                                { uiBounds = mkRectangle
                                    (0, 0)
                                    (fromIntegral $ fst sz)
                                    (fromIntegral $ snd sz)
                                }
                            }
                        (windows s)
                    }
                )
            renderWindow mWin'


    -- set viewport (glViewport)
    -- draw component
    -- swap buffers

