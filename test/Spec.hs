import Test.Tasty
import Test.Tasty.HUnit
import qualified CPU.Nestest.Spec as Nestest


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Pure-Nes Tests" $
  [
      Nestest.test
  ]
