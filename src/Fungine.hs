module Fungine where

import Protolude
import Fungine.Command
import Fungine.Window
import Fungine.Render.Window
import Control.Concurrent.STM.TChan
import Control.Concurrent as C
import Fungine.Render.Shader

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
    -> Map Text (Map ShaderType Shader)
    -> WindowState e ws mon win ()
loop poller mdl update view chan smp = do
    es <- liftIO $ atomically $ getEvents chan
    let
        (mdl', cmds) =
            foldl (\(m, cs) e' -> let (m', c) = update e' m in (m', c : cs)) (mdl, []) es
    mapM_ (startCommand chan) cmds
    liftIO poller
    let w = view mdl'
    e' <- render w smp
    liftIO $ do
        atomically $ mapM_ (writeTChan chan) e'
        C.yield

    when (not $ null e') (liftIO $ putStrLn (show e' :: Text))
    loop poller mdl' update view chan

-- TODO better way of error handling for top level errors
errH :: Text -> IO ()
errH = putStrLn

-- TODO maybe move this out to another file
startCommand :: TChan e -> Command e -> WindowState e ws mon win ()
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
    => IO ()
    -> ws
    -> WindowSystem ws mon win e
    -> InitF e mdl
    -> ViewF e mdl
    -> UpdateF e mdl
    -> IO ()
runFungine poller wstate winsys (mdl, c) view update = do
    smp <- loadShaders
    s    <- init winsys errH -- WindowStateData ws e mon win
    chan <- liftIO newTChanIO
    evalStateT
        (evalStateT
            (do
                startCommand chan c
                let w = view mdl
                e <- render w smp
                liftIO $ atomically $ mapM_ (writeTChan chan) e
                loop poller mdl update view chan smp
            )
            s
        )
        wstate
