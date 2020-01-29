module Fungine where

import Protolude
import Fungine.Command
import Fungine.Window
import Fungine.Render.Window

loop
    :: Show e
    => IO ()
    -> [e]
    -> mdl
    -> UpdateF e mdl cmd
    -> ViewF e mdl
    -> WindowState e ws mon win ()
loop poller e mdl update view = do
    let
        (mdl', cmds) =
            foldl (\(m, cs) e' -> let (m', c) = update e' m in (m', c : cs)) (mdl, []) e
    mapM_ startCommand cmds
    liftIO poller
    let w = view mdl'
    e' <- render w
    when (not $ null e') (liftIO $ putStrLn (show e' :: Text))
    loop poller e' mdl' update view

errH :: Text -> IO ()
errH = putStrLn

startCommand :: Command cmd -> WindowState e ws mon win ()
startCommand CommandNone = pure ()
startCommand CommandExit = liftIO $ exitSuccess

type UpdateF e mdl cmd = e -> mdl -> (mdl, Command cmd)
type ViewF e mdl = mdl -> Window e
type InitF mdl cmd = (mdl, Command cmd)

runFungine
    :: Show e
    => IO ()
    -> ws
    -> WindowSystem ws mon win e
    -> InitF mdl cmd
    -> ViewF e mdl
    -> UpdateF e mdl cmd
    -> IO ()
runFungine poller wstate winsys (mdl, c) view update = do
    s <- init winsys errH -- WindowStateData ws e mon win
    evalStateT
        (evalStateT
            (do
                startCommand c
                let w = view mdl
                e <- render w
                loop poller (traceShowId e) mdl update view
            )
            s
        )
        wstate
