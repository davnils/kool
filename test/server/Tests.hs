module Server.Tests where

import           Server.BuildQueueTests
import           Server.CompilationTests
import           Server.ReserveTests
import           Test.Tasty

serverTree = testGroup "Server tests" [
                buildQueueTree,
                compilationTree,
                reserveTree 
              ]
