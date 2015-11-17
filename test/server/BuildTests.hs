{-# LANGUAGE OverloadedStrings #-}

module Server.BuildTests where

import           BuildQueue
import           Control.Concurrent.MVar                            (MVar, newEmptyMVar, putMVar, takeMVar)
import           Control.Distributed.Process
import           Control.Exception                                  (NonTermination(..), throw)
import           Control.Monad                                      (foldM)
import qualified Data.ByteString.Lazy.Char8                      as BL
import           Data.Digest.Pure.SHA                               (sha256)
import           Data.List                                          (foldl')
import qualified Data.Map                                        as M
import           Data.Maybe                                         (isNothing)
import           Data.Thyme.Clock                                   (getCurrentTime)
import           Server
import           Types
import           Test.Tasty
import           Test.Tasty.HUnit
import           Server.Utils

buildTree = testGroup "Build request tests" [
                buildWithoutReservation,
                buildRunsHandler,
                buildHandlerThrows,
                buildHandlerFailure,
                buildHandlerConcurrency 
            ]

defTestState :: (ReservedQueue, ActiveMap String)
defTestState = (emptyQueue, M.empty)

-------------------------------------------------------------

defHash :: SHADigest
defHash = sha256 ""

defRequest :: BuildRequest
defRequest = BuildRequest defHash ("g++", 4, 6) [] ""

defHandler = return (CompilationSuccess "")

-------------------------------------------------------------

-- | Job that doesn't exist in reserve queue
buildWithoutReservation = testCase "buildWithoutReservation" $ do
  res <- runProcessTest (performAsync defTestState "" defRequest defHandler)

  assertBool "Build of previously not reserved hash should fail"
    (isNothing res)

-- | Verify that IO handler is properly executed
buildRunsHandler = testCase "buildRunsHandler" $ do
  m <- newEmptyMVar
  _ <- runProcessTest $ do
    now <- liftIO getCurrentTime
    let state = (insertItem defHash now emptyQueue, M.empty)
    Just state' <- performAsync state "something" defRequest $ do
      putMVar m 0
      defHandler

    Just _ <- expect >>= lookupRef state'
    return ()

  val <- takeMVar m

  assertEqual "Did not find the expected value written by handler"
    val 0

-- | Verify that a handler that throws is recongnised
buildHandlerThrows = testCase "buildHandlerThrows" $ do
  ret <- runProcessTest $ do
    now <- liftIO getCurrentTime
    let state = (insertItem defHash now emptyQueue, M.empty)
    Just state' <- performAsync state "something" defRequest $ do
      throw NonTermination

    expect >>= lookupRef state'

  case ret of
    Nothing             -> assertString "Misc failure happened"
    Just (_, Just _, _) -> assertString "Result should not be available on exceptions"
    _                   -> return ()

-- | Verify that CompilationFailure from Handler is correctly recognised
buildHandlerFailure = testCase "buildHandlerFailure" $ do
  let error = CompilationFailed "error!"
  ret <- runProcessTest $ do
    now <- liftIO getCurrentTime
    let state = (insertItem defHash now emptyQueue, M.empty)
    Just state' <- performAsync state "something" defRequest $ do
      return error

    expect >>= lookupRef state'

  case ret of
    Nothing                 -> assertString "Misc failure happened"
    Just (_, Nothing, _)    -> assertString "No result was returned"
    Just (_, Just error, _) -> return ()

-- | Verify that concurrent calls are handled correctly
buildHandlerConcurrency = testCase "buildHandlerConcurrency" $ do
  let numJobs = 100
  let hashes  = map (sha256 . BL.pack . show) [1..numJobs]
  let jobs    = map ((\h -> BuildRequest h ("g++", 4, 6) [] "")) hashes

  ret <- runProcessTest $ do
    now <- liftIO getCurrentTime
    let state = Just (foldl' (\q hash -> insertItem hash now q) emptyQueue hashes, M.empty)
    state' <- foldM (\(Just state') job -> performAsync state' "something" job defHandler) state jobs
    foldM (\(Just state'') _ -> expect >>= fmap (\(Just (_,_,st)) -> Just st) . lookupRef state'') state' jobs

  case ret of
    Nothing -> assertString "Failed to run concurrently"
    _       -> return ()
