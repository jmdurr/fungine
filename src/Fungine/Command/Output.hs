module Fungine.Command.Output where

import Protolude
import Fungine.Command

outputCmd :: Text -> Command e
outputCmd t = CommandIO (putStrLn t)
