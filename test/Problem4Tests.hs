module Problem4Tests
  ( problem4Tests
  ) where


import Test.HUnit

import Problem4


testPalindrome :: Test
testPalindrome = TestCase $ do
  assertBool "123 is not a palindrome" (not . palindrome $ 123)
  assertBool "9009 is a palindrome" (palindrome $ 9009)

testSolution4 :: Test
testSolution4 = TestCase $ do
  assertEqual "for 2-digit numbers, solution is 9009" 9009 (solveProblem4 99)


problem4Tests = TestList [testPalindrome, testSolution4]
