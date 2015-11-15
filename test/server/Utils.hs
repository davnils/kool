module Server.Utils where

import           Control.Concurrent.MVar                            (MVar, newEmptyMVar, putMVar, takeMVar)
import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import           Network.Transport.InMemory
import           Server

runProcessTest :: Process a -> IO a
runProcessTest proc = do
  node <- createTransport >>= flip newLocalNode rtable

  m <- newEmptyMVar
  runProcess node $ do
    res <- proc
    liftIO (putMVar m res)

  closeLocalNode node
  takeMVar m
