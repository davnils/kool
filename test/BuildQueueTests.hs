{-# LANGUAGE OverloadedStrings #-}

module BuildQueueTests where

import           BuildQueue
import           Data.Digest.Pure.SHA                               (sha256)
import           Data.Thyme.Clock                                   (getCurrentTime)
import           Test.Tasty
import           Test.Tasty.HUnit

buildQueueTree = testGroup "BuildQueue tests" [
                   nonExpiredNotFlushed,
                   everythingIsFlushed 
                 ]

nonExpiredNotFlushed = testCase "nonExpiredNotFlushed" $ do
  before <- getCurrentTime
  let queue1 = insertItem (sha256 "input1") before emptyQueue
  let queue2 = flushExpired 1 before queue1
  assertEqual "Non-expired item shouldn't be flushed" (countItems queue2) 1

everythingIsFlushed = testCase "everythingIsFlushed" $ do
  before <- getCurrentTime
  let queue1 = insertItem (sha256 "input1") before emptyQueue
  let queue2 = insertItem (sha256 "input2") before queue1
  let queue3 = flushExpired 0 before queue2
  assertEqual "Expired items should be flushed" (countItems queue3) 0
