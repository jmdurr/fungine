module Fungine.Command.Heartbeat where

import Fungine.Command
import Control.Concurrent
import Control.Concurrent.STM.TChan

import Protolude

-- | given a millisecond interval, produce an event near that interval with ms passed 
-- or does it simple beat forever
heartbeat :: Int -> e -> Command e
heartbeat delayMs te = CommandIORepeatEvent $ \tchan -> hbio tchan
 where
  hbio tchan = do
    threadDelay (delayMs * 1000)
    atomically $ writeTChan tchan te
    hbio tchan
