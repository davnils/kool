{-# LANGUAGE OverloadedStrings #-}

module Server where

import           Control.Concurrent                              (threadDelay)

import           Control.Distributed.Process
import           Control.Distributed.Process.Async
import           Control.Distributed.Process.Backend.SimpleLocalnet
import           Control.Distributed.Process.Extras.Time
import           Control.Distributed.Process.ManagedProcess
import           Control.Distributed.Process.Node
import           Control.Distributed.Process.Serializable

import           Control.Monad.Trans                             (liftIO)
import           Data.AffineSpace                                ((.-^))
import qualified Data.ByteString.Char8                           as B
import qualified Data.Text                                       as T
import qualified Data.Map                                        as M
import           Data.Monoid                                     ((<>))
import qualified Data.PSQueue                                    as P
import           Data.Thyme.Clock                                (UTCTime, getCurrentTime, fromSeconds)
import           Network.Transport.TCP                           (createTransport, defaultTCPParameters)
import           System.Exit                                     (ExitCode(..))
import           System.Process                                  (readProcessWithExitCode)
import           Types

-- | Queue of compilation slots that been reserved with timestamps.
type ReservedQueue = P.PSQ Hash UTCTime
--
-- | Active compilations identified by a monitoring reference.
type ActiveMap     = M.Map MonitorRef (Async CompilationResult, CallRef BuildReply)

-- | Server state composed of queue with reserved slots, and map of running compilations.
data ServerState
  = ServerState ReservedQueue ActiveMap

-- | Flush expired elements from the queue, as defined by a timeout in seconds.
flushExpired :: Int -> ReservedQueue -> IO ReservedQueue
flushExpired timeout queue = do
  limit <- fmap (.-^ fromSeconds timeout) getCurrentTime
  go limit queue
  where
  go limit queue = case P.findMin queue of
    Nothing -> return queue
    Just binding  -> do
      if P.prio binding < limit
        then go limit (P.deleteMin queue)
        else return queue

-- | Check if there's still capacity and reserve a slot for the requested build.
reserveRequest :: Int -> ServerState -> ReserveRequest -> Process (ProcessReply ReserveReply ServerState)
reserveRequest limit (ServerState queue active) (ReserveRequest hash) = do
  -- TODO: handle overlapping queue items (fail req with fast client retry). allow double if already active?
  flushedQueue <- liftIO $ flushExpired 5 queue

  case (P.size flushedQueue + M.size active >= limit) of
    True  -> reply NoCapacity (ServerState flushedQueue active)
    False -> do
      now <- liftIO getCurrentTime
      let flushedQueue' = P.insert hash now flushedQueue
      reply SlotReserved (ServerState flushedQueue' active)

-- | Initiate a build of the provided source on the previously acquired slot.
--   The item is only considered expired if previously flushed. (TODO: correct logic??)
buildRequest :: Serializable b => ServerState -> CallRef BuildReply -> BuildRequest -> Process (ProcessReply b ServerState)
buildRequest state@(ServerState reserved active) caller req@(BuildRequest hash _ _ _) = do
  case P.lookup hash reserved of
    Nothing -> replyTo caller Expired >> noReply_ state
    Just _  -> do
      -- Remove from reserved queue, insert into active, and run compilation
      item <- asyncLinked (task . liftIO $ compile req)
      mon <- monitorAsync item
      let reserved' = P.delete hash reserved
      let active'   = M.insert mon (item, caller) active
      noReply_ (ServerState reserved' active')

-- | Compile the provided unit with the requested compiler and flags.
compile :: BuildRequest -> IO CompilationResult
compile (BuildRequest _ _ flags source) = do
  let flags' = (map T.unpack flags) <> ["-c", "-o /dev/stdout", "-"]
  (code, stdout, stderr) <- readProcessWithExitCode "/usr/bin/gcc" flags' (T.unpack source)
  case code of
    ExitSuccess -> return $ CompilationSuccess (B.pack stdout)
    _           -> return $ CompilationFailed  (T.pack stderr)
  -- TODO: add flags for deterministic builds

-- | Process a completed compilation and send result to client
compilationDone :: ServerState -> ProcessMonitorNotification -> Process (ProcessAction ServerState)
compilationDone state@(ServerState other active) (ProcessMonitorNotification ref _ _) = do
  case M.lookup ref active of
    Nothing -> continue state
    Just (item, caller) -> do
      let active' = M.delete ref active
      fmap consider (wait item) >>= replyTo caller
      continue (ServerState other active')
  where
  consider (AsyncDone result) = CompilerOutput result
  consider _                  = UnknownError

runBuildServer :: Process ()
runBuildServer = do
  pid <- getSelfPid
  register buildRegName pid
  serve () (const $ return $ InitOk (ServerState P.empty M.empty) Infinity) def
  where
  def = defaultProcess {
          apiHandlers = [
                  handleCall     (reserveRequest limit)
                , handleCallFrom (buildRequest)
          ]
        , infoHandlers = [
                  handleInfo compilationDone
          ]
        }

  -- TODO: make this configurable or load #cores
  limit = 4

main :: IO ()
main = do
  backend <- initializeBackend "127.0.0.1" "9999" rtable
  node <- Control.Distributed.Process.Backend.SimpleLocalnet.newLocalNode backend
  Control.Distributed.Process.Node.runProcess node runBuildServer
  where
  rtable = __remoteTable initRemoteTable
