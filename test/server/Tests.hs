module Server.Tests where

import           Server.CompilationTests
import           Server.ReserveTests
import           Test.Tasty

serverTree = testGroup "Server tests" [
                compilationTree,
                reserveTree 
              ]
