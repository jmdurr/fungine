module Fungine where

import           Protolude
import           FRP.Behavior
import           Fungine.Window
import           Fungine.Render.Window

loop ::  [e] -> (Window e,Behavior e (Window e)) -> WindowState e ws mon win ()
loop es bw = do
    let (w'',bw') = foldl (\(_,b) e -> b `step` e) bw es 
    es' <- render w''
    loop es' (w'',bw')


errH :: Text -> IO ()
errH = putStrLn

runFungine :: ws -> WindowSystem ws mon win e -> e -> Behavior e (Window e) -> IO ()
runFungine wstate winsys startE win = do
    s <- init winsys errH
    let (w,win') = win `step` startE in
        evalStateT (evalStateT (do
            es <- render w
            loop es (w,win')
        ) s) wstate