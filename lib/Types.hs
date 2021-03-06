{-# LANGUAGE BangPatterns, DeriveGeneric, OverloadedStrings, TemplateHaskell #-}

module Types where
import           Control.Distributed.Process
import           Control.Distributed.Process.Closure

import           Data.Binary                                     (Binary)
import qualified Data.ByteString.Char8                           as B
import           Data.Digest.Pure.SHA                            (Digest, SHA256State)
import qualified Data.Text                                       as T
import           Data.Typeable                                   (Typeable)
import           Data.Word                                       (Word8)
import           GHC.Generics                                    (Generic)

-- | Compiler name, major, and minor version.
--   Example: ("gcc", 4, 5)
type CompilerVersion = (T.Text, Int, Int)

type Flags           = [T.Text]
type SourceUnit      = T.Text
type SHADigest       = Digest SHA256State
type ObjectFile      = B.ByteString

-- | Result of invoking a compiler.
data CompilationResult
  = CompilationFailed  !T.Text
  | CompilationSuccess !ObjectFile
  deriving (Generic, Typeable, Show)

instance Binary CompilationResult  where

-- | TODO: probably want to base hash on everything except version, include it here
data ReserveRequest
  = ReserveRequest !SHADigest
  deriving (Generic, Typeable, Show)

instance Binary ReserveRequest where

data ReserveReply
  = SlotReserved
  | NoCapacity
  -- TODO: add CacheHit ...
  deriving (Eq, Generic, Typeable, Show)

instance Binary ReserveReply where

data BuildRequest
  = BuildRequest !SHADigest !CompilerVersion !Flags !SourceUnit
  deriving (Generic, Typeable, Show)

instance Binary BuildRequest where

data BuildReply
  = UnknownError
  | Expired
  | CompilerOutput !CompilationResult
  deriving (Generic, Typeable, Show)

instance Binary BuildReply where

-- | Name used in registry lookup for server providing compilations
buildRegName :: String
buildRegName = "build_server"

remotable ['whereis]
