module Problem4Tests
  ( problem4Tests
  ) where


import Test.HUnit

import Problem4


testDigits :: Test
testDigits = TestCase $ do
  assertEqual "digits of 0" [0] (digits 0)
  assertEqual "digits of 1" [1] (digits 1)
  assertEqual "digits of 248" [2,4,8] (digits 248)

testNumber :: Test
testNumber = TestCase $ do
  assertEqual "number of [0]" 0 (number [0])
  assertEqual "number of [1]" 1 (number [1])
  assertEqual "number of [3,5,7]" 357 (number [3,5,7])

testNumberFromDigits :: Test
testNumberFromDigits = TestCase $ do
  assertEqual "123 to digits and back" 123 (number.digits $ 123)

testPalindrome :: Test
testPalindrome = TestCase $ do
  assertBool "123 is not a palindrome" (not . palindrome $ 123)
  assertBool "9009 is a palindrome" (palindrome $ 9009)

testSolution4 :: Test
testSolution4 = TestCase $ do
  assertEqual "for 2-digit numbers, solution is 9009" 9009 (solveProblem4 99)


problem4Tests = TestList [testDigits, testNumber, testNumberFromDigits, testPalindrome, testSolution4]
