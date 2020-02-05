import Test.Tasty
import Test.Tasty.HUnit
import qualified CPU.Nestest.Spec       as Nestest
import qualified CPU.Instr_test_v5.Spec as InstrTest_v5 


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Pure-Nes Tests" $
  [
    testGroup "CPU" $
    [
      testGroup "Quick test" [Nestest.test],
      testGroup "Exhaustive tests" [InstrTest_v5.test]
    ]
  ]
