module Main where

import ArgsParserTests
import ClientTests
import ServerTests

import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Kool tests" [
         argsParserTree,
         clientTree,
         serverTree
       ]
