{-# LANGUAGE BangPatterns, OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}

module Client where

import           Control.Monad                                      (when, liftM2, void)
import           Control.Monad.Trans                                (lift, MonadIO(..))
import           Control.Distributed.Process
import           Control.Distributed.Process.Async
import           Control.Distributed.Process.Backend.SimpleLocalnet
import           Control.Distributed.Process.Closure
import           Control.Distributed.Process.Extras.Time
import           Control.Distributed.Process.ManagedProcess.Client
import           Control.Distributed.Process.Node
import           Control.Distributed.Process.Serializable
import           Data.Monoid                                        ((<>))
import           Network.Transport.TCP                              (createTransport, defaultTCPParameters)
import qualified System.IO.Strict                                   as Strict
import           Types

callWrapper :: (Serializable a, Serializable b) => a -> ProcessId -> Process (Maybe b)
callWrapper !payload !sid = do
  res <- safeCall sid payload
  case res of
    Right ret -> return (Just ret)
    Left err -> liftIO (print err) >> return Nothing

getBuildProcess :: NodeId -> Process (Maybe ProcessId)
getBuildProcess node = do
  whereisRemoteAsync node buildRegName
  reply <- expectTimeout 500 :: Process (Maybe WhereIsReply)
  case reply of
    Just (WhereIsReply _ (Just sid)) -> return (Just sid)
    _                                -> return Nothing

main :: IO ()
main = do
  preprocessed <- Strict.readFile "debugstuff"
  backend <- initializeBackend "127.0.0.1" "4321" initRemoteTable
  node <- Control.Distributed.Process.Backend.SimpleLocalnet.newLocalNode backend
  runProcess node (liftIO (findPeers backend 100000) >>= tryNode preprocessed)
  where
  tryNode :: String -> [NodeId] -> Process ()
  tryNode _ [] = liftIO (putStrLn "[-] Failed to find any suitable build node")
  tryNode preprocessed (node:others) = do
        proc <- getBuildProcess node
        case proc of
          Nothing -> tryNode preprocessed others
          Just proc' -> do
            liftIO (putStrLn $ "Found service on " <> show node <> ", calling")
            (_ :: Maybe String) <- callWrapper preprocessed proc'
            liftIO (putStrLn "Call terminated")
            return ()
