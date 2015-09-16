{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Types where

import           Data.Binary                                     (Binary)
import qualified Data.ByteString.Char8                           as B
import qualified Data.Text                                       as T
import           Data.Typeable                                   (Typeable)
import           Data.Word                                       (Word8)
import           GHC.Generics                                    (Generic)

-- | Compiler name, major, and minor version.
--   Example: ("gcc", 4, 5)
type CompilerVersion = (T.Text, Int, Int)

type Flags           = [T.Text]
type SourceUnit      = T.Text
type Hash            = Integer
type ObjectFile      = B.ByteString

-- | Result of invoking a compiler.
data CompilationResult
  = CompilationFailed  T.Text
  | CompilationSuccess ObjectFile
  deriving (Generic, Typeable, Show)

instance Binary CompilationResult  where

-- | TODO: probably want to base hash on everything except version, include it here
data ReserveRequest
  = ReserveRequest Hash
  deriving (Generic, Typeable, Show)

instance Binary ReserveRequest where

data ReserveReply
  = SlotReserved
  | NoCapacity
  deriving (Generic, Typeable, Show)

instance Binary ReserveReply where

data BuildRequest
  = BuildRequest Hash CompilerVersion Flags SourceUnit
  deriving (Generic, Typeable, Show)

instance Binary BuildRequest where

data BuildReply
  = UnknownError
  | Expired
  | CompilerOutput CompilationResult
  deriving (Generic, Typeable, Show)

instance Binary BuildReply where

-- TODO: add client/server executables and shared lib
--       add basic client with option parsing (gcc invocation)
--       add managed server initialization
--       integrate configuration containing compiler paths
--       integrate configuration options for timeouts
--       add retry mechanisms in client
--       add randomized node selection in client
--       integrate proper build host discovery
--       what about hashing a compressed version? sorting the flags etc? uniqify the hash.
