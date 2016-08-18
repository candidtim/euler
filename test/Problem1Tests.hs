module Problem1Tests
  ( problem1Tests
  ) where


import Test.HUnit

import Problem1


testSolution1 :: Test
testSolution1 = TestCase $ do
  assertEqual "(solution 1) sum of multiples of 3 and 5 below 10" 23 (solveProblem1 10)
  assertEqual "(solution 2) sum of multiples of 3 and 5 below 10" 23 (solveProblem1' 10)
  assertEqual "(solution 3) sum of multiples of 3 and 5 below 10" 23 (solveProblem1'' 10)


problem1Tests = TestList [testSolution1]
