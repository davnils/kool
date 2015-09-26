{-# LANGUAGE DeriveGeneric, OverloadedStrings, TemplateHaskell #-}

module Types where
import           Control.Distributed.Process
import           Control.Distributed.Process.Closure

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
type Hash            = B.ByteString
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
  -- TODO: add CacheHit ...
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

-- | Name used in registry lookup for server providing compilations
buildRegName :: String
buildRegName = "build_server"

remotable ['whereis]

-- TODO: add client/server executables and shared lib
--       add basic client with option parsing (gcc invocation)
--       add managed server initialization
--       integrate configuration containing compiler paths
--       integrate configuration options for timeouts
--       add retry mechanisms in client
--       add randomized node selection in client
--       integrate proper build host discovery
--       what about hashing a compressed version? sorting the flags etc? uniqify the hash.
