module Fungine where

import Protolude
import Fungine.Command
import Fungine.Window
import Fungine.Render.Window
import Control.Concurrent.STM.TChan
import Control.Concurrent as C
import Fungine.Render.Shader
import Graphics.Rendering.OpenGL.GL.Shaders as S
import qualified Data.Map.Strict as M
import Fungine.Error

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
    -> WindowState e ws mon win (CanError ())
loop poller mdl update view chan = do
    es <- liftIO $ atomically $ getEvents chan
    let
        (mdl', cmds) =
            foldl (\(m, cs) e' -> let (m', c) = update e' m in (m', c : cs)) (mdl, []) es
    mapM_ (startCommand chan) cmds
    liftIO poller
    let w = view mdl'
    e' <- render w
    case e' of
        Success evs -> do
            liftIO $ do
                atomically $ mapM_ (writeTChan chan) evs
                C.yield
            loop poller mdl' update view chan
        Error t -> pure $ Error t

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

uiShader :: CanError (Map Text S.Program) -> CanError S.Program
uiShader (Error t) = Error t
uiShader (Success mp) = case M.lookup "ui" mp of
    Nothing -> Error "Must have a shader with filename ui_vertex.glsl and ui_fragment.glsl"
    Just uip -> Success uip

-- TODO also clean up args to this function
runFungine
    :: Show e
    => IO ()
    -> ws
    -> WindowSystem ws mon win e
    -> InitF e mdl
    -> ViewF e mdl
    -> UpdateF e mdl
    -> IO (CanError ())
runFungine poller wstate winsys (mdl, c) view update = do
    smp <- loadShaders
    case uiShader smp of
        Error t -> pure (Error t)
        Success prog -> do
            s    <- init winsys prog errH -- WindowStateData ws e mon win
            chan <- liftIO newTChanIO
            evalStateT
                (evalStateT
                    (do
                        startCommand chan c
                        let w = view mdl
                        e <- render w
                        case e of
                            Error t -> pure (Error t)
                            Success e' -> do
                                liftIO $ atomically $ mapM_ (writeTChan chan) e'
                                loop poller mdl update view chan
                    )
                    s
                )
                wstate
