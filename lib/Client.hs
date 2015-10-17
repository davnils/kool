{-# LANGUAGE BangPatterns, OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}

module Client where

import qualified Control.Concurrent.Async                        as A
import           Control.Exception                                  (Exception(..), catch, IOException, SomeException)
import           Control.Monad                                      (when, liftM2)
import           Control.Monad.Trans                                (lift, MonadIO(..))
import           Control.Monad.Trans.Maybe                          (MaybeT(..), runMaybeT)
import           Control.Distributed.Process
import           Control.Distributed.Process.Async
import           Control.Distributed.Process.Backend.SimpleLocalnet
import           Control.Distributed.Process.Closure
import           Control.Distributed.Process.Extras.Time
import           Control.Distributed.Process.ManagedProcess.Client
import           Control.Distributed.Process.Node
import           Control.Distributed.Process.Serializable
import qualified Data.ByteString.Char8                           as B
import qualified Data.ByteString.Lazy.Char8                      as BL
import           Data.Monoid                                        ((<>))
import           Data.Digest.Pure.SHA                               (sha256)
import qualified Data.Text                                       as T
import qualified Data.Text.IO                                    as T
import           Network.Transport.TCP                              (createTransport, defaultTCPParameters)
import           System.Exit                                        (ExitCode(..))
import           System.IO                                          (writeFile) -- TODO
import qualified System.Process                                  as Proc
import           Types

type Servers = [(String, String)]

reserveTimeout :: TimeInterval
reserveTimeout = seconds 2

buildTimeout :: TimeInterval
buildTimeout = seconds 5

safeCallTimeout :: (Serializable a, Serializable b, Show a) => a -> TimeInterval -> ProcessId -> Process (Maybe b)
safeCallTimeout !payload !timeout !sid = do
  item <- callAsync sid payload 
  fmap peek (waitTimeout timeout item)
  where
  peek (Just (AsyncDone result)) = Just result
  peek _                         = Nothing

-- | Reserve a build slot for the provided hash.
reserveBuild :: SHADigest -> ProcessId  -> Process (Maybe ReserveReply)
reserveBuild hash = safeCallTimeout (ReserveRequest hash) reserveTimeout

-- | Request a build of the provided source unit and build configuration.
requestBuild :: SHADigest -> CompilerVersion -> Flags -> SourceUnit -> ProcessId -> Process (Maybe BuildReply)
requestBuild hash compiler flags src = safeCallTimeout req buildTimeout
  where
  req = BuildRequest hash compiler flags src

----------------------------------------------------------------------------------------

getBuildProcess :: NodeId -> MaybeT Process ProcessId
getBuildProcess node = MaybeT $ Control.Distributed.Process.call dict node (closure buildRegName)
  where
  dict = $(functionTDict 'whereis)
  closure = $(mkClosure 'whereis)

----------------------------------------------------------------------------------------

preprocess :: CompilerVersion -> Flags -> SourceUnit -> IO (Maybe SourceUnit)
preprocess _ flags source =  do
  let flags' = (map T.unpack flags) <> ["-E", "-xc++", "-o/dev/stdout", "-"]

  (code, out, err) <- flip Control.Exception.catch handler $
    Proc.readProcessWithExitCode "/usr/bin/g++" flags' (T.unpack source)

  case code of
    ExitSuccess -> do
      putStrLn $ "[+] Preprocessing succeeded with bytes=" <> show (length out)
      return . Just . T.pack $ out

    _           -> do
      putStrLn "[-] Preprocessing failed, output follows"
      putStr err
      return   Nothing
  where
  handler (e :: IOException) = return (ExitFailure 1, "", show e)

buildFlow :: CompilerVersion -> Flags -> SourceUnit -> String -> IO ()
buildFlow compiler flags source outputFile = do
  backend <- initializeBackend "127.0.0.1" "4321" rtable
  node <- Control.Distributed.Process.Backend.SimpleLocalnet.newLocalNode backend

  -- preprocess local file
  preprocessed <- A.async (preprocess compiler flags source)

  -- initialize build node identification etc
  runProcess node (liftIO (findPeers backend 100000) >>= tryNode preprocessed)
  where
  rtable = __remoteTable initRemoteTable

  tryNode _ [] = liftIO (putStrLn "[-] Failed to find any suitable build node")
  tryNode preprocessed (node:rest) = do
    source' <- liftIO (A.wait preprocessed)
    case source' of
      Nothing -> return ()

      Just source'' -> do
        res <- runNodeFlow compiler flags source'' node
        case res of
          Just (Right out) -> liftIO $ do
            putStrLn ("[+] Build succeeded, writing output to " <> outputFile)
            B.writeFile outputFile out

          Just (Left err) -> liftIO $ do
            putStrLn ("[-] Compilation failed, output follows")
            T.putStr err

          Nothing  -> tryNode preprocessed rest

runNodeFlow :: CompilerVersion -> Flags -> SourceUnit -> NodeId -> Process (Maybe (Either T.Text ObjectFile))
runNodeFlow compiler flags source node = runMaybeT $ do
  proc <- getBuildProcess node
  liftIO . putStrLn $ "[*] Found build service on node " <> show node
  hash <- tryReserve proc source
  tryBuild compiler flags source hash proc

failWith :: (MonadIO m, Monad m) => String -> m (Maybe a)
failWith msg = liftIO (putStrLn msg) >> return Nothing

tryReserve proc source = MaybeT $ do
  let hash = sha256 . BL.pack . T.unpack $ source
  res <- reserveBuild hash proc
  case res of
    Nothing -> failWith ("[-] Failed to build on host " <> nodeStr)

    Just NoCapacity -> failWith ("[*] Host " <> nodeStr <> " out of capacity")

    Just SlotReserved -> do
      liftIO (putStrLn $ "[*] Reserved build slot on host " <> nodeStr)
      return (Just hash)
  where
  nodeStr = show (processNodeId proc)

tryBuild :: CompilerVersion -> Flags -> SourceUnit -> SHADigest -> ProcessId -> MaybeT Process (Either T.Text ObjectFile)
tryBuild compiler flags source hash proc = MaybeT $ do
  res <- requestBuild hash compiler flags source proc
  liftIO $ case res of
    Nothing -> failWith "[-] Build request failed due to unresponsive build node"

    Just UnknownError -> failWith "[-] Build request failed due to unknown problem on build node"

    Just Expired -> failWith "[-] Build request expired"

    Just (CompilerOutput (CompilationFailed desc)) -> return . Just . Left $ desc

    Just (CompilerOutput (CompilationSuccess output)) -> return . Just . Right $ output

main :: IO ()
main = buildFlow ("gcc", 4, 5) ["-O2"] "#include <iostream> int main(void){}" "output.o"
