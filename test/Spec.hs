import Test.Tasty
import Test.Tasty.HUnit
import qualified CPU.Quick.Nestest.Spec as Nestest
import qualified CPU.Exhaustive.Spec    as CPU.Exhaustive
import qualified PPU.Spec    as PPU
import qualified APU.Spec    as APU


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = localOption (mkTimeout $ 2 * 10^7) $  -- no test can take longer than 20 seconds
  testGroup "Pure-Nes Tests" $
  [
    testGroup "CPU" $
    [
      testGroup "Quick tests"      [Nestest.test],
      testGroup "Exhaustive tests" CPU.Exhaustive.tests
    ],

    testGroup "PPU" PPU.tests

    --testGroup "APU" APU.tests
  ]
