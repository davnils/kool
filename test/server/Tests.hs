module Server.Tests where

import           Server.BuildQueueTests
import           Server.BuildTests
import           Server.CompilationTests
import           Server.ReserveTests
import           Test.Tasty

serverTree = testGroup "Server tests" [
                buildQueueTree,
                buildTree,
                compilationTree,
                reserveTree 
              ]
