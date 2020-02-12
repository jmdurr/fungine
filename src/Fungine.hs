module Fungine where

import Protolude
import Fungine.Command
import Fungine.Window
import Fungine.Render.Window
import Control.Concurrent.STM.TChan
import Control.Concurrent as C
import Fungine.Error
import Fungine.Input
import qualified Data.Map.Strict as M

data FungineSystem ws mon win e = FungineSystem { fWindowSystem :: WindowSystem ws mon win e
                                                    , fInputSystem :: InputSystem win
                                                    , fPollEvents :: IO ()
                                                    }



getEvents :: TChan e -> STM [e]
getEvents chan = catMaybes <$> replicateM 10 (tryReadTChan chan)

-- TODO create a fungine state to hold all of these arguments
loop
    :: Show e
    => FungineSystem ws mon win e
    -> TChan e
    -> mdl
    -> ViewF e mdl
    -> UpdateF e mdl
    -> WindowState e ws mon win (CanError ())
loop fsys chan mdl view update = do
    es <- liftIO $ atomically $ getEvents chan
    let
        (mdl', cmds) =
            foldl (\(m, cs) e' -> let (m', c) = update e' m in (m', c : cs)) (mdl, []) es
    liftIO $ mapM_ (startCommand chan) cmds
    e' <- render (view mdl')
    case e' of
        Success evs -> do
            liftIO $ do
                fPollEvents fsys
                atomically $ mapM_ (writeTChan chan) evs
                C.yield
            loop fsys chan mdl' view update
        Error t -> pure $ Error t

-- TODO maybe move this out to another file
startCommand :: TChan e -> Command e -> IO ()
startCommand _ CommandNone                = pure ()
startCommand _ CommandExit                = liftIO exitSuccess
startCommand _ (CommandIO            io ) = liftIO io
startCommand c (CommandIOEvent ioe) = void $ liftIO $ forkIO $ ioe >>= (atomically . writeTChan c)
startCommand c (CommandIORepeatEvent f  ) = void $ liftIO $ forkIO $ f c

type UpdateF e mdl = e -> mdl -> (mdl, Command e)
type ViewF e mdl = mdl -> Window e
type InitF e mdl = (mdl, Command e)

-- TODO also clean up args to this function
runFungine
    :: Show e
    => FungineSystem ws mon win e
    -> ws
    -> InitF e mdl
    -> ViewF e mdl
    -> UpdateF e mdl
    -> IO (CanError ())
runFungine fsys iwstate (mdl, c) view update = do
    chan <- newTChanIO
    startCommand chan c
    evalStateT
        (evalStateT
            (do
                e <- render (view mdl)
                case e of
                    Error   t  -> pure (Error t)
                    Success e' -> do
                        liftIO $ atomically $ mapM_ (writeTChan chan) e'
                        loop fsys chan mdl view update
            )
            (WindowStateData M.empty (fWindowSystem fsys) (fInputSystem fsys))
        )
        iwstate
