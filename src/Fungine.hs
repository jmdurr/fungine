module Fungine where

import           Protolude
import           FRP.Behavior
import           Fungine.Window
import           Fungine.Render.Window



data FungineState mon win = FungineState {

                          }

type Fungine e a = StateT (FungineState e) IO a

runFungine :: e -> Behavior e (Window e) -> IO ()
runFungine startE win = do
    let (w,win') = win `step` e in
        -- render w with startE, window system event handlers, etc
        -- 