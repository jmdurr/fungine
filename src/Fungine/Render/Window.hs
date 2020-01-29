module Fungine.Render.Window where

import Protolude
import Fungine.Window
import qualified Data.Map.Strict as M
import Fungine.Error
import Control.Concurrent.STM.TChan

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
                                          }


data WindowInternal e mon win = WindowInternal { iWindow :: Window e
                                               , iCurrWindow :: Window e
                                               , iEvents :: TChan WindowEvent
                                               , iSysWindow :: win
                                               }

data WindowStateData ws e mon win = WindowStateData { windows :: Map WindowId (WindowInternal e mon win)
                                                 , errorHandler :: Text -> IO ()
                                                 , windowSys :: WindowSystem ws mon win e
                                                 }

type WindowState e ws mon win a = StateT (WindowStateData ws e mon win) (StateT ws IO) a

init :: WindowSystem ws mon win e -> (Text -> IO ()) -> IO (WindowStateData ws e mon win)
init ws eh = return WindowStateData { windows = M.empty, errorHandler = eh, windowSys = ws }



installCallbacks :: TChan WindowEvent -> win -> WindowState e ws mon win ()
installCallbacks chan win = do
    ws <- gets windowSys
    lift (setCallbacks ws win cbs)
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

newWindow :: Window e -> WindowState e ws mon win ()
newWindow win = do
    eh <- gets errorHandler
    ws <- gets windowSys
    w  <- lift (createWindow ws win)
    case w of
        Error   t  -> liftIO $ eh t
        Success w' -> do
            c <- liftIO newTChanIO
            installCallbacks c w'
            modify
                (\s -> s
                    { windows = M.insert
                        (wId win)
                        (WindowInternal
                            { iWindow     = win
                            , iCurrWindow = win
                            , iEvents     = c
                            , iSysWindow  = w'
                            }
                        )
                        (windows s)
                    }
                )

updateWindow :: Window e -> WindowInternal e mon win -> WindowState e ws mon win ()
updateWindow win winI = do
    ws <- gets windowSys
    when (wSize win /= wSize (iWindow winI)) (lift $ resizeWindow ws (iSysWindow winI) (wSize win))
    when
        (wTitle win /= wTitle (iWindow winI))
        (lift $ setWindowTitle ws (iSysWindow winI) (wTitle win))
    installCallbacks (iEvents winI) (iSysWindow winI)


collectEvents :: Show e => WindowState e ws mon win [e]
collectEvents = do
    wins <- gets $ (M.toList . windows)
    concat
        <$> mapM
                (\(_, w) -> do
                    let ehs = wHandlers (iWindow w)
                    wes <-
                        traceShow (length ehs) $ replicateM 1 $ liftIO $ atomically $ tryReadTChan
                            (iEvents w) -- take up to x events
                    return $ catMaybes [ eh e | eh <- ehs, e <- catMaybes wes ]
                )
                wins

render :: Show e => Window e -> WindowState e ws mon win [e]
render win = do
    wins <- gets windows
    case M.lookup (wId win) wins of
        Nothing   -> newWindow win
        Just winI -> updateWindow win winI
    collectEvents

