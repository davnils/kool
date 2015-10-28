module ArgsParserTests where

import ArgsParser
import Test.Tasty
import Test.Tasty.HUnit

argsParserTree = testGroup "ArgParser tests" [
                    emptyArgs,
                    versionFlagOnly,
                    mixedFlagTypes,
                    compMissing,
                    inputMissing,
                    outputMissing,
                    simpleDistributed1,
                    simpleDistributed2,
                    simpleDistributed3,
                    simpleDistributed4,
                    simpleDistributed5,
                    distrFlags1,
                    distrFlags2,
                    distrFlags3,
                    distrFlags4,
                    distrMixedFlags1,
                    distrMixedFlags2
                 ]

-----------------------------------------------------------------

isLocal (InvokeLocal _) = True
isLocal _               = False

-----------------------------------------------------------------

emptyArgs = testCase "emptyArgs" $ do
  assertEqual "empty argument list should equal local computation"
    (classify []) (InvokeLocal [])

versionFlagOnly = testCase "versionFlagOnly" $ do
  assertEqual "--version should yield local computation"
    (classify ["--version"]) (InvokeLocal ["--version"])

mixedFlagTypes = testCase "mixedFlagTypes" $ do
  assertEqual "-arg --arg2 mixed should be preserved"
    (classify ["-O2", "--version"]) (InvokeLocal ["-O2", "--version"])

-----------------------------------------------------------------

compMissing = testCase "compMissing" $ do
  assertBool "missing -c should fail"
    (isLocal (classify ["-o", "out", "input"]))

inputMissing = testCase "inputMissing" $ do
  assertBool "missing input file should fail"
    (isLocal (classify ["-o", "out", "-c"]))

outputMissing = testCase "outputMissing" $ do
  assertBool "missing output file should fail"
    (isLocal (classify ["source.cc", "-c"]))

-----------------------------------------------------------------

input = "source.cc"
output = "out.o"
refDistr = DistributedCompilation input output

simpleDistributed1 = testCase "simpleDistributed1" $ do
  assertEqual "valid distributed computation"
    (classify ["-c", "-o", output, input]) (refDistr [])

simpleDistributed2 = testCase "simpleDistributed2" $ do
  assertEqual "valid distributed computation"
    (classify ["-o", output, "-c", input]) (refDistr [])

simpleDistributed3 = testCase "simpleDistributed3" $ do
  assertEqual "valid distributed computation"
    (classify ["-o", output, input, "-c"]) (refDistr [])

simpleDistributed4 = testCase "simpleDistributed4" $ do
  assertEqual "valid distributed computation"
    (classify [input, "-c", "-o", output]) (refDistr [])

simpleDistributed5 = testCase "simpleDistributed5" $ do
  assertEqual "valid distributed computation"
    (classify [input, "-o", output, "-c"]) (refDistr [])

distrFlags1 = testCase "distrFlags1" $ do
  assertEqual "valid distributed computation with flags"
    (classify ["-O2", "-c", "-o", output, input]) (refDistr ["-O2"])

distrFlags2 = testCase "distrFlags2" $ do
  assertEqual "valid distributed computation with flags"
    (classify ["-O2", "-v", "-c", "-o", output, input]) (refDistr ["-O2", "-v"])

distrFlags3 = testCase "distrFlags3" $ do
  assertEqual "valid distributed computation with flags"
    (classify ["-O2", "-c", "-o", output, "-v", input]) (refDistr ["-O2", "-v"])

distrFlags4 = testCase "distrFlags4" $ do
  assertEqual "valid distributed computation with flags"
    (classify [input, "-O2", "-c", "-o", output, "-v"]) (refDistr ["-O2", "-v"])

distrMixedFlags1 = testCase "distrMixedFlags1" $ do
  assertEqual "valid distributed computation with flags"
    (classify [input, "-O2", "-c", "-o", output, "--fast"]) (refDistr ["-O2", "--fast"])

distrMixedFlags2 = testCase "distrMixedFlags2" $ do
  assertEqual "valid distributed computation with flags"
    (classify [input, "--fast", "-c", "-o", output, "-O2"]) (refDistr ["--fast", "-O2"])
