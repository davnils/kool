{-# LANGUAGE BangPatterns, OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}

module Client where

import           Control.Monad                                      (when, liftM2, void)
import           Control.Monad.Trans                                (lift, MonadIO(..))
import           Control.Distributed.Process
import           Control.Distributed.Process.Async
import           Control.Distributed.Process.Closure
import           Control.Distributed.Process.Extras.Time
import           Control.Distributed.Process.ManagedProcess.Client
import           Control.Distributed.Process.Node
import           Control.Distributed.Process.Serializable
import qualified Data.ByteString.Char8                              as B
import           Data.Monoid                                        ((<>))
import           Network.Transport                                  (EndPointAddress(..))
import           Network.Transport.TCP                              (createTransport, defaultTCPParameters)
import qualified System.IO.Strict                                   as Strict
import           Types

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
    _                                -> liftIO (putStrLn "No regged service found") >> return Nothing

main :: IO ()
main = do
  preprocessed <- Strict.readFile "debugstuff"

  Right transport <- createTransport "127.0.0.1" "8888" defaultTCPParameters
  node <- newLocalNode transport initRemoteTable

  let server = EndPointAddress (B.pack "127.0.0.1:9999:0")

  runProcess node (tryNode preprocessed server)
  where
  tryNode preprocessed node = do
        proc <- getBuildProcess (NodeId node)
        case proc of
          Nothing -> return ()
          Just proc' -> do
            liftIO (putStrLn $ "Found service on " <> show proc' <> ", calling")
            (_ :: Maybe String) <- callWrapper preprocessed proc'
            liftIO (putStrLn "Call terminated")
