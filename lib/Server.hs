{-# LANGUAGE BangPatterns, GADTs, OverloadedStrings, ScopedTypeVariables #-}

module Server where

import           BuildQueue
import           Control.Concurrent                              (threadDelay)
import qualified Control.Concurrent.Async                        as A
import           Control.Exception                               (catch, IOException)

import           Control.Distributed.Process
import           Control.Distributed.Process.Async
import           Control.Distributed.Process.Backend.SimpleLocalnet
import           Control.Distributed.Process.Extras.Time
import           Control.Distributed.Process.ManagedProcess
import           Control.Distributed.Process.Node
import           Control.Distributed.Process.Serializable

import           Control.Monad                                   (when)
import           Control.Monad.Trans                             (liftIO)
import qualified Data.ByteString.Char8                           as B
import           Data.Digest.Pure.SHA                            (showDigest)
import           Data.Maybe                                      (fromMaybe, isNothing)
import qualified Data.Text                                       as T
import qualified Data.Text.IO                                    as T
import qualified Data.Map                                        as M
import           Data.Monoid                                     ((<>))
import           Data.Thyme.Clock                                (UTCTime, getCurrentTime, fromSeconds)
import           GHC.Conc                                        (getNumProcessors, setNumCapabilities)
import           Network.Transport.TCP                           (createTransport, defaultTCPParameters)
import           System.Exit                                     (ExitCode(..))
import           System.IO                                       (hSetBinaryMode, hClose)
import qualified System.Process                                  as Proc
import           Types
import           Utils

-- | Number of seconds before an entry in the reserved queue is expired.
queueTimeout :: Int
queueTimeout = 5

-- | Active compilations identified by a monitoring reference.
type ActiveMap meta = M.Map MonitorRef (Async CompilationResult, meta)

-- | Server state composed of queue with reserved slots, and map of running compilations.
type ServerState   = (ReservedQueue, ActiveMap (CallRef BuildReply))

-- | Check if there's still capacity and reserve a slot for the requested build.
reserveRequest :: Int -> ServerState -> ReserveRequest -> Process (ReserveReply, ServerState)
reserveRequest limit (queue, active) (ReserveRequest hash) = do
  -- TODO: handle overlapping queue items (fail req with fast client retry). allow double if already active? duplicate outgoing notifications? (sounds good)
  now <- liftIO getCurrentTime 
  let flushedQueue = flushExpired queueTimeout now queue
  case (countItems flushedQueue + M.size active >= limit) of
    True  -> return (NoCapacity, (flushedQueue, active))
    False -> do
      let flushedQueue' = insertItem hash now flushedQueue
      return (SlotReserved, (flushedQueue', active))

-- | Initiate a build of the provided source on the previously acquired slot.
--   The item is only considered expired if previously flushed.
buildRequest :: Serializable b => ServerState -> CallRef BuildReply -> BuildRequest -> Process (ProcessReply b ServerState)
buildRequest state caller req@(BuildRequest hash _ _ _) = do
  asyncRes <- performAsync state caller req (compile req)
  when (isNothing asyncRes) (replyTo caller Expired)
  noReply_ (fromMaybe state asyncRes)

-- | Check if the supplied request exists in reserve queue, and invoke an async operation.
performAsync :: st ~ (ReservedQueue, ActiveMap meta) => st -> meta -> BuildRequest -> IO CompilationResult -> Process (Maybe st)
performAsync state@(reserved, active) meta req@(BuildRequest hash _ _ _) action = do
  case itemExists hash reserved of
    Nothing -> return Nothing
    Just _  -> do
      -- Remove from reserved queue, insert into active, and run async action
      item <- asyncLinked . task . liftIO $ action
      mon <- monitorAsync item
      let reserved' = deleteItem hash reserved
      let active'   = M.insert mon (item, meta) active
      return (Just (reserved', active'))

-- | Compile the provided unit with the requested compiler and flags.
compile :: BuildRequest -> IO CompilationResult
compile (BuildRequest hash _ flags source) = do
  putStrLn "[*] Invoking compilation"
  let fileName = "/tmp/" <> "kool-" <> showDigest hash
  -- TODO: add flags for deterministic builds
  let flags' = ["-c", "-xc++", "-o" <> fileName, "-"] <> map T.unpack flags

  (code, _, err) <- invokeLocalCompiler flags' source
  putStrLn ("[*] Compilation terminated with code=" <> show code)

  case code of
    ExitSuccess -> fmap CompilationSuccess (B.readFile fileName)
    _           -> return $ CompilationFailed (T.pack err)

-- | Process a completed compilation and send result to client
compilationDone :: ServerState -> ProcessMonitorNotification -> Process (ProcessAction ServerState)
compilationDone state@(other, active) notification = do
  res <- lookupRef state notification
  case res of
    Nothing -> continue state -- TODO: print an error here
    Just (caller, output, state') -> do
      let converted = fmap CompilerOutput output
      replyTo caller (fromMaybe UnknownError converted)
      continue state'

-- | Lookup the corresponding monitor ref within the active computations.
--   Will try to receive the corresponding async result.
lookupRef
  :: (st ~ (ReservedQueue, ActiveMap meta))
  => st -> ProcessMonitorNotification -> Process (Maybe (meta, Maybe CompilationResult, st))
lookupRef state@(other, active) (ProcessMonitorNotification ref _ _) = do
  case M.lookup ref active of
    Nothing -> return Nothing
    Just (item, tag) -> do
      let active' = M.delete ref active
      result <- consider <$> wait item
      return (Just (tag, result, (other, active')))
  where
  consider (AsyncDone !result) = Just result
  consider _                   = Nothing

defaultServerState = (emptyQueue, M.empty)

runBuildServer :: Int -> Process ()
runBuildServer limit = do
  pid <- getSelfPid
  register buildRegName pid
  serve () (const $ return $ InitOk defaultServerState (Delay $ seconds 1)) def
  where
  def = defaultProcess {
          apiHandlers = [
                  handleCall     (\s r -> reserveRequest limit s r >>= uncurry reply)
                , handleCallFrom (buildRequest)
          ]
        , infoHandlers = [
                  handleInfo compilationDone
          ]
        }

rtable = __remoteTable initRemoteTable

main :: IO ()
main = do
  procs <- getNumProcessors
  putStrLn ("[*] Reconfiguring server process to use #cores=" <> show procs)
  setNumCapabilities procs

  backend <- initializeBackend "127.0.0.1" "9999" rtable
  node <- Control.Distributed.Process.Backend.SimpleLocalnet.newLocalNode backend
  Control.Distributed.Process.Node.runProcess node (runBuildServer procs)
