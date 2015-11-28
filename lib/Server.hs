{-# LANGUAGE BangPatterns, GADTs, OverloadedStrings, ScopedTypeVariables #-}

module Server where

import           Control.Distributed.Process
import           Control.Distributed.Process.Async
import           Control.Distributed.Process.Extras.Time
import           Control.Distributed.Process.ManagedProcess
import           Control.Distributed.Process.Node
import           Control.Distributed.Process.Serializable

import           Control.Monad.Trans                             (liftIO)
import           Network.Transport.TCP                           (createTransport, defaultTCPParameters)
import           Types

runBuildServer :: Process ()
runBuildServer = do
  pid <- getSelfPid
  register buildRegName pid
  liftIO (print "launching example server")
  serve () (statelessInit Infinity) def
  where
  def = defaultProcess {
          apiHandlers = [
                handleCall_ (\(r :: String) -> logit "got build req" >> return ("" :: String))
          ]
        }
  logit = liftIO . putStrLn

main :: IO ()
main = do
  Right transport <- createTransport "127.0.0.1" "9999" defaultTCPParameters
  node <- newLocalNode transport initRemoteTable
  Control.Distributed.Process.Node.runProcess node runBuildServer
