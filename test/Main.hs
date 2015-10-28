module Main where

import ArgsParserTests
import BuildQueueTests
import ClientTests
import ServerTests

import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "Kool tests" [
         argsParserTree,
         buildQueueTree,
         clientTree,
         serverTree
       ]
