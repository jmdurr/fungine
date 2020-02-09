module Fungine.Command where
import Protolude

import Control.Concurrent.STM.TChan

data Command e = CommandExit
                 | CommandNone
                 | CommandIO (IO ())
                 | CommandIOEvent (IO e)
                 | CommandIORepeatEvent (TChan e -> IO ())
