module Fungine where

import Protolude
import Fungine.Command
import Fungine.Window
import Fungine.Render.Window
import Control.Concurrent.STM.TChan
import Control.Concurrent as C
import Data.Time.Clock (getCurrentTime, UTCTime(..), diffUTCTime)

getEvents :: TChan e -> STM [e]
getEvents chan = catMaybes <$> replicateM 10 (tryReadTChan chan)

-- TODO create a fungine state to hold all of these arguments
loop
    :: Show e
    => IO ()
    -> mdl
    -> UpdateF e mdl
    -> ViewF e mdl
    -> TChan e
    -> UTCTime
    -> Int
    -> WindowState e ws mon win ()
loop poller mdl update view chan time frame = do
    (frame', ntime) <- if frame == 1000
        then do
            tm <- liftIO $ getCurrentTime
            let ms = toRational 1000 * toRational (diffUTCTime tm time)
            putStrLn (show (floor (toRational frame / ms * toRational 1000)) :: Text)
            return (0, tm)
        else return (frame, time)
    es <- liftIO $ atomically $ getEvents chan -- this will never return...
    let
        (mdl', cmds) =
            foldl (\(m, cs) e' -> let (m', c) = update e' m in (m', c : cs)) (mdl, []) es
    mapM_ (startCommand chan) cmds
    liftIO poller
    let w = view mdl'
    e' <- render w
    liftIO $ do
        atomically $ mapM_ (writeTChan chan) e'
        C.yield
    when (not $ null e') (liftIO $ putStrLn (show e' :: Text))
    loop poller mdl' update view chan ntime (max 0 (frame' + 1))

errH :: Text -> IO ()
errH = putStrLn

startCommand :: TChan e -> Command e -> WindowState e ws mon win ()
startCommand _ CommandNone          = pure ()
startCommand _ CommandExit          = liftIO $ exitSuccess
startCommand _ (CommandIO      io ) = liftIO $ io
startCommand c (CommandIOEvent ioe) = liftIO $ do
    tid <- forkIO $ ioe >>= (\e -> atomically $ writeTChan c e)
    return ()

type UpdateF e mdl = e -> mdl -> (mdl, Command e)
type ViewF e mdl = mdl -> Window e
type InitF e mdl = (mdl, Command e)

runFungine
    :: Show e
    => IO ()
    -> ws
    -> WindowSystem ws mon win e
    -> InitF e mdl
    -> ViewF e mdl
    -> UpdateF e mdl
    -> IO ()
runFungine poller wstate winsys (mdl, c) view update = do
    s    <- init winsys errH -- WindowStateData ws e mon win
    chan <- liftIO newTChanIO
    evalStateT
        (evalStateT
            (do
                tm <- liftIO $ getCurrentTime
                startCommand chan c
                let w = view mdl
                e <- render w
                liftIO $ atomically $ mapM_ (writeTChan chan) e
                loop poller mdl update view chan tm 0
            )
            s
        )
        wstate
