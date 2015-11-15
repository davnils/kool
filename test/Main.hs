module Main where

import ArgsParserTests
import BuildQueueTests
import Client.Tests
import Server.Tests
import UtilsTests

import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "Kool tests" [
         argsParserTree,
         buildQueueTree,
         clientTree,
         serverTree,
         utilsTree
       ]
