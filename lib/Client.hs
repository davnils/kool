module Client where

import           Control.Distributed.Process
import           Control.Distributed.Process.Async
import           Control.Distributed.Process.Extras.Time
import           Control.Distributed.Process.ManagedProcess.Client
import           Control.Distributed.Process.Node
import           Network.Transport.TCP                              (createTransport, defaultTCPParameters)
import           Types

type Servers = [(String, String)]

reserveTimeout :: TimeInterval
reserveTimeout = seconds 2

buildTimeout :: TimeInterval
buildTimeout = seconds 30

safeCallTimeout :: Serializable a => a -> TimeInterval -> ProcessId -> Process (Maybe ReserveReply)
safeCallTimeout payload timeout sid = do
  item <- callAsync sid payload 
  peek <$> waitCancelTimeout timeout item
  where
  peek (AsyncDone result) = Just result
  peek _                  = Nothing

reserveBuild :: Hash -> ProcessId -> Process (Maybe ReserveReply)
reserveBuild hash = safeCallTimeout (ReserveRequest hash) reserveTimeout

requestBuild :: Hash -> CompilerVersion -> Flags -> SourceUnit -> ProcessId -> Process (Maybe BuildReply)
requestBuild hash compiler flags src = safeCallTimeout req buildTimeout
  where
  req = BuildRequest hash compiler flags src

buildFlow :: CompilerVersion -> Flags -> SourceUnit -> IO ()
buildFlow compiler flags source = do
  backend <- initializeBackend "127.0.0.1" 4321 initRemoteTable
  startMaster backend (build backend)
  where
  build _ [] = putStrLn "Failed to find any build node"
  build backend (node:_) = do
    let hash = undefined
    case reserveBuild hash asd of
    -- TODO: integrate hash, do call based on nodeid, implement server, do basic test


main :: IO ()
main = undefined
