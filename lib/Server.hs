{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Server where

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

import           Control.Monad.Trans                             (liftIO)
import           Data.AffineSpace                                ((.-^))
import qualified Data.ByteString.Char8                           as B
import qualified Data.ByteString.Lazy.Char8                      as BL
import           Data.Digest.Pure.SHA                            (bytestringDigest, showDigest)
import qualified Data.Text                                       as T
import qualified Data.Text.IO                                    as T
import qualified Data.Map                                        as M
import           Data.Monoid                                     ((<>))
import qualified Data.PSQueue                                    as P
import           Data.Thyme.Clock                                (UTCTime, getCurrentTime, fromSeconds)
import           GHC.Conc                                        (getNumProcessors, setNumCapabilities)
import           Network.Transport.TCP                           (createTransport, defaultTCPParameters)
import           System.Exit                                     (ExitCode(..))
import           System.IO                                       (hSetBinaryMode, hClose)
import qualified System.Process                                  as Proc
import           Types

-- | Number of seconds before an entry in the reserved queue is expired.
queueTimeout :: Int
queueTimeout = 5

-- | Internal serialized representation of the source file hash.
type SerializedHash = B.ByteString

-- | Queue of compilation slots that been reserved with timestamps.
type ReservedQueue = P.PSQ SerializedHash UTCTime
--
-- | Active compilations identified by a monitoring reference.
type ActiveMap     = M.Map MonitorRef (Async CompilationResult, CallRef BuildReply)

-- | Server state composed of queue with reserved slots, and map of running compilations.
data ServerState
  = ServerState ReservedQueue ActiveMap

strictBytestrDigest = BL.toStrict . bytestringDigest

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
  flushedQueue <- liftIO $ flushExpired queueTimeout queue

  case (P.size flushedQueue + M.size active >= limit) of
    True  -> reply NoCapacity (ServerState flushedQueue active)
    False -> do
      now <- liftIO getCurrentTime
      let flushedQueue' = P.insert (strictBytestrDigest hash) now flushedQueue
      reply SlotReserved (ServerState flushedQueue' active)

-- | Initiate a build of the provided source on the previously acquired slot.
--   The item is only considered expired if previously flushed.
buildRequest :: Serializable b => ServerState -> CallRef BuildReply -> BuildRequest -> Process (ProcessReply b ServerState)
buildRequest state@(ServerState reserved active) caller req@(BuildRequest hash _ _ _) = do
  let hash' = strictBytestrDigest hash
  case P.lookup hash' reserved of
    Nothing -> replyTo caller Expired >> noReply_ state
    Just _  -> do
      -- Remove from reserved queue, insert into active, and run compilation
      item <- asyncLinked (task . liftIO $ compile req)
      mon <- monitorAsync item
      let reserved' = P.delete hash' reserved
      let active'   = M.insert mon (item, caller) active
      noReply_ (ServerState reserved' active')

-- | Compile the provided unit with the requested compiler and flags.
compile :: BuildRequest -> IO CompilationResult
compile (BuildRequest hash _ flags source) = do
  putStrLn "[*] Invoking compilation"
  let fileName = "/tmp/" <> "kool-" <> showDigest hash
  -- TODO: add flags for deterministic builds
  let flags' = ["-c", "-xc++", "-o" <> fileName, "-"]

  flip Control.Exception.catch handler $ do
    (code, _, err) <- Proc.readProcessWithExitCode "/usr/bin/g++" flags' (T.unpack source)
    putStrLn ("[*] Compilation terminated with code=" <> show code)

    case code of
      ExitSuccess -> fmap CompilationSuccess (B.readFile fileName)
      _           -> return $ CompilationFailed (T.pack err)
  where
  handler (e :: IOException) = print e >> return (CompilationFailed (T.pack (show e)))

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

runBuildServer :: Int -> Process ()
runBuildServer limit = do
  pid <- getSelfPid
  register buildRegName pid
  serve () (const $ return $ InitOk (ServerState P.empty M.empty) (Delay $ seconds 1)) def
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

main :: IO ()
main = do
  backend <- initializeBackend "127.0.0.1" "9999" rtable
  node <- Control.Distributed.Process.Backend.SimpleLocalnet.newLocalNode backend

  procs <- getNumProcessors
  putStrLn ("[*] Reconfiguring server process to use #cores=" <> show procs)
  setNumCapabilities procs

  Control.Distributed.Process.Node.runProcess node (runBuildServer procs)
  where
  rtable = __remoteTable initRemoteTable
