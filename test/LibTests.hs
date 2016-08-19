module LibTests
  ( libTests
  ) where


import Test.HUnit

import Lib (factor, fibs, primes, arithmeticProgressionSum, squareNumbersSum)


testFactor :: Test
testFactor = TestCase $ do
  assertBool "5 is factor of 10" (5 `factor` 10)
  assertBool "3 is factor of 9" (3 `factor` 9)
  assertBool "9 is not factor of 3" (not (9 `factor` 3))
  assertBool "4 is not factor of 9" (not (4 `factor` 9))

testFibs :: Test
testFibs = TestCase $ do
  assertEqual "first 10 fibonaci nummbers" [0, 1, 1, 2, 3, 5, 8, 13, 21, 34] (take 10 fibs)

testPrimes :: Test
testPrimes = TestCase $ do
  assertEqual "prime numbers less than 30" [2, 3, 5, 7, 11, 13, 17, 19, 23, 29] (primes 30)

testArithmeticProgressionSum :: Test
testArithmeticProgressionSum = TestCase $ do
  assertEqual "sum or AP 1..10 with step 1" 55 (arithmeticProgressionSum 1 10 10)
  assertEqual "sum or AP 2..14 with step 3" 40 (arithmeticProgressionSum 2 14 5)
  assertEqual "sum or AP 14..2 with step -3" 40 (arithmeticProgressionSum 14 2 5)
  assertEqual "sum or AP 5..5 with 3 members" 15 (arithmeticProgressionSum 5 5 3)

testSquareNumbersSequenceSum :: Test
testSquareNumbersSequenceSum = TestCase $ do
  assertEqual "sum of 1^2 2^2 3^2" 14 (squareNumbersSum 3)
  assertEqual "sum of squares up to 10" 385 (squareNumbersSum 10)


libTests = TestList [testFactor, testFibs, testPrimes, testArithmeticProgressionSum, testSquareNumbersSequenceSum]
