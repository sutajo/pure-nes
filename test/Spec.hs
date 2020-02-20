import Test.Tasty
import Test.Tasty.HUnit
import qualified CPU.Quick.Nestest.Spec as Nestest
import qualified CPU.Exhaustive.Spec    as CPU.Exhaustive
import qualified PPU.Exhaustive.Spec    as PPU.Exhaustive


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = 
  testGroup "Pure-Nes Tests" $
  [
    testGroup "CPU" $
    [
      testGroup "Quick tests"      [Nestest.test],
      testGroup "Exhaustive tests" CPU.Exhaustive.tests
    ],

    testGroup "PPU" PPU.Exhaustive.tests
  ]
