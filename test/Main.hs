module Main where

import ArgsParserTests
import Client.Tests
import Server.Tests
import UtilsTests

import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "Kool tests" [
         argsParserTree,
         clientTree,
         serverTree,
         utilsTree
       ]
