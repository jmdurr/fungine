module Fungine.Command where
import Protolude

data Command e = CommandExit
                 | CommandNone
                 | CommandIO (IO ())
                 | CommandIOEvent (IO e)
