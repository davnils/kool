{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module BuildQueue (ReservedQueue, flushExpired, insertItem, deleteItem, itemExists, countItems, emptyQueue) where

import           Data.AffineSpace                                ((.-^))
import qualified Data.ByteString.Char8                           as B
import qualified Data.ByteString.Lazy.Char8                      as BL
import           Data.Digest.Pure.SHA                            (bytestringDigest)
import qualified Data.PSQueue                                    as P
import           Data.Thyme.Clock                                (UTCTime, getCurrentTime, fromSeconds)
import           Types

-- | Internal serialized representation of the source file hash.
type SerializedHash = B.ByteString

-- | Queue of compilation slots that been reserved with timestamps.
type ReservedQueue = P.PSQ SerializedHash UTCTime

-- | Flush expired elements from a queue, as defined by a timeout in seconds.
flushExpired :: Int -> UTCTime -> ReservedQueue -> ReservedQueue
flushExpired timeout now queue = go queue
  where
  limit = now .-^ fromSeconds timeout
  go queue = case P.findMin queue of
    Nothing -> queue
    Just binding -> do
      if P.prio binding <= limit
        then go (P.deleteMin queue)
        else queue

strictBytestrDigest :: SHADigest -> SerializedHash
strictBytestrDigest = BL.toStrict . bytestringDigest

insertItem :: SHADigest -> UTCTime -> ReservedQueue -> ReservedQueue 
insertItem = P.insert . strictBytestrDigest

deleteItem :: SHADigest -> ReservedQueue -> ReservedQueue 
deleteItem = P.delete . strictBytestrDigest

itemExists :: SHADigest -> ReservedQueue -> Maybe UTCTime
itemExists = P.lookup . strictBytestrDigest

countItems :: ReservedQueue -> Int
countItems = P.size

emptyQueue :: ReservedQueue
emptyQueue = P.empty
