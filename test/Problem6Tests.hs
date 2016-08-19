module Problem6Tests
  ( problem6Tests
  ) where


import Test.HUnit

import Problem6


testSumSquareDifference :: Test
testSumSquareDifference = TestCase $ do
  assertEqual "(solutoin 1) difference for sums of sequences up to 10" 2640 (sumSquareDifference 10)
  assertEqual "(solutoin 2) difference for sums of sequences up to 10" 2640 (sumSquareDifference' 10)


problem6Tests = TestList [testSumSquareDifference]
