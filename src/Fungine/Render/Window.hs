module Fungine.Render.Window where

import           Protolude
import           Fungine.Window
import qualified Data.Map.Strict               as M
import           Fungine.Error
import           Control.Concurrent.STM.TChan

data WindowCallback mon win = FramebufferSizeCallback (win -> (Int,Int) -> IO ())
                            | IconifyCallback (win -> IO ())
                            | RestoreCallback (win -> IO ())
                            | FocusCallback (win -> IO ())
                            | BlurCallback (win -> IO ())
                            | RefreshCallback (win -> IO ())
                            | CloseCallback (win -> IO ())
                            | SizeCallback (win -> (Int,Int) -> IO ())
                            | PositionCallback (win -> (Int,Int) -> IO ())

class WindowSystem a where
    initWindowSystem :: IO a
    exitWindowSystem :: a -> IO ()
    resizeWindow :: a -> win -> WindowSize -> IO ()
    createWindow :: a -> Window e -> IO (CanError win)
    setWindowTitle :: a -> win -> Text -> IO ()
    primaryMonitor :: a -> IO mon
    hasError :: a -> IO (Maybe Text)
    setCallbacks :: a -> win -> [WindowEventHandler e] -> IO a


data WindowInternal e mon win = WindowInternal { iWindow :: Window e
                                               , iSysWindow :: win
                                               , iCurrWindow :: Window e
                                               , iEvents :: TChan WindowEvent
                                               }

data WindowStateData e ws mon win = WindowStateData { windows :: Map WindowId (WindowInternal e mon win)
                                                    , windowSys :: ws
                                                    , errorHandler :: (Text -> IO ())
                                                    }

type WindowState e ws mon win a = StateT (WindowStateData e ws mon win) IO a

init :: WindowSystem ws => ws -> (Text -> IO ()) -> IO (WindowStateData e ws mon win)
init sys eh = return WindowStateData { windows = M.empty, windowSys = sys, errorHandler = eh }

newWindow :: WindowSystem ws => Window e -> WindowState e ws mon win ()
newWindow win = do
    ws <- gets windowSys
    eh <- gets errorHandler
    e  <- lift $ createWindow ws win
    case e of
        Error   t      -> lift $ eh t
        Success (w, _) -> do
            c   <- lift $ newTChanIO
            ws' <- lift $ setCallbacks ws w (wHandlers win)
            modify
                (\s' -> s'
                    { windows   = M.insert
                                      (wId win)
                                      (WindowInternal { iWindow = win, iSysWindow = w, iCurrWindow = win, iEvents = c })
                                      (windows s')
                    , windowSys = ws'
                    }
                )

updateWindow :: WindowSystem ws => Window e -> WindowInternal e mon win -> WindowState e ws mon win ()
updateWindow win winI = do
    ws <- gets windowSys
    when (wSize win /= wSize (iWindow winI))   (lift $ resizeWindow ws (iSysWindow winI) (wSize win))
    when (wTitle win /= wTitle (iWindow winI)) (lift $ setWindowTitle ws (iSysWindow winI) (wTitle win))
    ws' <- lift $ setCallbacks ws (iSysWindow winI) (wHandlers win)
    modify (\s -> s { windowSys = ws' })

collectEvents :: WindowSystem ws => WindowState e ws mon win [e]
collectEvents = do
    wins <- gets $ (M.toList . windows)
    concat
        <$> mapM
                (\(_, w) -> do
                    let ehs = wHandlers (iWindow w)
                    wes <- replicateM 10 $ lift $ atomically $ tryReadTChan (iEvents w) -- take up to x events
                    return $ catMaybes [ eh e | eh <- ehs, e <- catMaybes wes ]
                )
                wins

render :: WindowSystem ws => Window e -> WindowState e ws mon win [e]
render win = do
    wins <- gets windows
    case M.lookup (wId win) wins of
        Nothing   -> newWindow win
        Just winI -> updateWindow win winI
    collectEvents

