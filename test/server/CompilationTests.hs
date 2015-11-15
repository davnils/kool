{-# LANGUAGE OverloadedStrings #-}

module Server.CompilationTests where

import           BuildQueue
import           Control.Distributed.Process
import           Data.Digest.Pure.SHA                               (sha256)
import qualified Data.Map                                        as M
import qualified Data.Text                                       as T
import           Server
import           Types
import           Test.Tasty
import           Test.Tasty.HUnit
import           Server.Utils

compilationTree = testGroup "Compilation tests" [
                invalidFile,
                unsupportedArgument
              ]

-------------------------------------------------------------

someHash :: SHADigest
someHash = sha256 ""

someVersion :: CompilerVersion
someVersion = ("g++", 4, 6)

someSource :: T.Text
someSource = "int main() { return 0; }"

isFailure :: CompilationResult -> Bool
isFailure (CompilationFailed _ ) = True
isFailure _                      = False

-------------------------------------------------------------

-- | Test that compiling some invalid source fails.
invalidFile = testCase "invalidFile" $ do
  ret <- compile (BuildRequest someHash someVersion [] "syntax error")
  assertBool "Invalid source should fail" (isFailure ret)

-- | Test that an illegal argument fails.
unsupportedArgument = testCase "unsupportedArgument" $ do
  putStrLn ""
  ret <- compile (BuildRequest someHash someVersion ["--nonononono"] someSource)
  assertBool "Illegal argument should fail" (isFailure ret)
  putStrLn ""

-- TODO: add more tests!!
