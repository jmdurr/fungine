module Fungine where

import           Protolude
import           FRP.Behavior
import           Fungine.Window
import           Fungine.Render.Window

loop ::  WindowSystem ws => [e] -> (Window e,Behavior e (Window e)) -> WindowState e ws mon win ()
loop es bw = do
    let (w'',bw') = foldl (\(_,b) e -> b `step` e) bw es 
    es' <- render w''
    loop es' (w'',bw')


errH :: Text -> IO ()
errH t = putStrLn t

runFungine :: WindowSystem ws => ws -> e -> Behavior e (Window e) -> IO ()
runFungine ws startE win = do
    s <- init ws errH
    let (w,win') = win `step` startE in
        evalStateT (do
            es <- render w
            loop es (w,win')
        ) s