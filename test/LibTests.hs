module LibTests
  ( libTests
  ) where


import Test.HUnit

import Lib (factor, fibs)


testFactor :: Test
testFactor = TestCase $ do
  assertBool "5 is factor of 10" (5 `factor` 10)
  assertBool "3 is factor of 9" (3 `factor` 9)
  assertBool "9 is not factor of 3" (not (9 `factor` 3))
  assertBool "4 is not factor of 9" (not (4 `factor` 9))


testFibs :: Test
testFibs = TestCase $ do
  assertEqual "first 10 fibonaci nummbers" [0, 1, 1, 2, 3, 5, 8, 13, 21, 34] (take 10 fibs)


libTests = TestList [testFactor, testFibs]
