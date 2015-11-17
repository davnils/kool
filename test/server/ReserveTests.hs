{-# LANGUAGE OverloadedStrings #-}

module Server.ReserveTests where

import           BuildQueue
import           Control.Distributed.Process
import           Control.Monad                                      (foldM)
import           Data.Digest.Pure.SHA                               (sha256)
import qualified Data.Map                                        as M
import           Server
import           Types
import           Test.Tasty
import           Test.Tasty.HUnit
import           Server.Utils

reserveTree = testGroup "Reserve tests" [
                reserveSaturatedDefaultstate,
                reserveReturnsSuccess,
                reserveReturnsFailure,
                reserveSeveralItems,
                reserveFullTransition
              ]

-------------------------------------------------------------

someHash :: SHADigest
someHash = sha256 ""

someReserveReq :: ReserveRequest
someReserveReq = ReserveRequest someHash

-------------------------------------------------------------

-- | Test for reserving with combined limit 0
reserveSaturatedDefaultstate = testCase "reserveSaturatedDefaultstate" $ do
  let (queue, _) = defaultServerState
  (_, (queue', _)) <- runProcessTest (reserveRequest 0 defaultServerState someReserveReq)
  assertEqual "Nothing should be reserved when limit is 0" (countItems queue) (countItems queue')

-- | Test that reserve returns success when queue has capacity
reserveReturnsSuccess = testCase "reserveReturnsSuccess" $ do
  (status, _) <- runProcessTest (reserveRequest 1 defaultServerState someReserveReq)

  assertEqual "Reserve should return success when there's capacity"
    status SlotReserved

-- | Test that reserve returns failure when queue is full
reserveReturnsFailure = testCase "reserveReturnsFailure" $ do
  (status, _) <- runProcessTest (reserveRequest 0 defaultServerState someReserveReq)

  assertEqual "Reserve should return failure when there's no capacity"
    status NoCapacity

-- | Test many reservations, size of reserved queue == insertions, active state remains same
reserveSeveralItems = testCase "reserveSeveralItems" $ do
  let (_, defaultActive) = defaultServerState
  (queue, active) <- runProcessTest
    (foldM (\s req -> fmap snd (reserveRequest highLimit s req)) defaultServerState reqs)

  assertEqual "Active jobs should be unaffected" (M.size active) (M.size defaultActive)
  assertEqual "All hashes should appear in reserve queue" (countItems queue) (length hashes)
  where
  reqs = map (ReserveRequest . sha256) hashes
  hashes = ["1", "2", "3", "4"]
  highLimit = length hashes * 10

-- | Test transition into full state
reserveFullTransition = testCase "reserveFullTransition" $ do
  (status1, state1) <- runProcessTest (reserveRequest 1 defaultServerState req1)
  (status2, state2) <- runProcessTest (reserveRequest 1 state1             req2)

  assertEqual "Reserve should return success when there's capacity"
    status1 SlotReserved

  assertEqual "Reserve should return failure when there's no capacity"
    status2 NoCapacity
  where
  [req1, req2] = map (ReserveRequest . sha256) ["1", "2"]
