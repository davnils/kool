{-# LANGUAGE OverloadedStrings #-}

module UtilsTests where

import           System.Exit                                        (ExitCode(..))
import           Test.Tasty
import           Test.Tasty.HUnit
import           Utils

-- Assumes existance of /usr/bin/g++

utilsTree = testGroup "Utils tests" [
              successInvocation,
              failedInvocation
            ]

successInvocation = testCase "successInvocation" $ do
  (code, out, err) <- invokeLocalCompiler ["--version"] ""
  assertEqual "Valid invocation should be indicated" ExitSuccess code 

failedInvocation = testCase "failedInvocation" $ do
  (code, out, err) <- invokeLocalCompiler ["--blablabla"] ""
  assertBool "Invalid invocation should be caught" (code /= ExitSuccess)
