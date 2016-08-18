module Problem5Tests
  ( problem5Tests
  ) where


import Test.HUnit

import Problem5


testMultipleOfAll :: Test
testMultipleOfAll = TestCase $ do
  assertBool "6 is multiple of all 1,2,3" (6 `multipleOfAll` [1,2,3])
  assertBool "9 is multiple of 3" (9 `multipleOfAll` [3])
  assertBool "9 is not multiple of both 2 and 3" (not (9 `multipleOfAll` [2,3]))


testSolution5 :: Test
testSolution5 = TestCase $ do
  assertEqual "smallest multiple of 1..10 is 2520" 2520 (solveProblem5 10)


problem5Tests = TestList [testMultipleOfAll, testSolution5]
