{-# LANGUAGE OverloadedStrings #-}

module BuildQueueTests where

import           BuildQueue
import           Data.AffineSpace                                   ((.+^))
import           Data.Digest.Pure.SHA                               (sha256)
import           Data.Thyme.Clock                                   (fromSeconds, getCurrentTime)
import           Test.Tasty
import           Test.Tasty.HUnit

buildQueueTree = testGroup "BuildQueue tests" [
                   nonExpiredNotFlushed,
                   everythingIsFlushed,
                   subsetExpired
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

subsetExpired = testCase "subsetExpired" $ do
  before <- getCurrentTime
  let queue1 = insertItem (sha256 "input1") before emptyQueue
  let queue2 = insertItem (sha256 "input2") (before .+^ fromSeconds 5) queue1
  let queue3 = flushExpired 5 (before .+^ fromSeconds 8) queue2
  assertEqual "Only one item should be expired" (countItems queue3) 1
