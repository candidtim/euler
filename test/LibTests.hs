module LibTests
  ( libTests
  ) where


import Test.HUnit
import Data.Char as Char

import Lib


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

testTriangulars :: Test
testTriangulars = TestCase $ do
  assertEqual "first 7 triangular numbers" [1, 3, 6, 10, 15, 21, 28] (take 7 triangulars)

testFactorials :: Test
testFactorials = TestCase $ do
  assertEqual "first 7 factorials" [1, 2, 6, 24, 120, 720, 5040] (take 7 factorials)

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

testSlices :: Test
testSlices = TestCase $ do
  assertEqual "slices of size 2 of list of 4" [ [1,2], [2,3], [3,4] ] (slices [1,2,3,4] 2)

testDivisors :: Test
testDivisors = TestCase $ do
  assertEqual "divisors of 28" [1, 2, 4, 7, 14, 28] (divisors 28)

testProperDivisors :: Test
testProperDivisors = TestCase $ do
  assertEqual "proper divisors of 28" [1, 2, 4, 7, 14] (properDivisors 28)

testPerfect :: Test
testPerfect = TestCase $ do
  assertBool "496 is a perfect number" (perfect 496)
  assertBool "123 is not a perfect number" (not.perfect $ 123)
  assertBool "12 is not a perfect number" (not.perfect $ 12)

testAbundant :: Test
testAbundant = TestCase $ do
  assertBool "12 is abundant" (abundant 12)
  assertBool "496 is not abundant" (not.abundant $ 496)
  assertBool "16 is not abundant" (not.abundant $ 16)

testDeficient :: Test
testDeficient = TestCase $ do
  assertBool "16 is deficient" (deficient 16)
  assertBool "496 is not deficient" (not.deficient $ 496)
  assertBool "12 is not deficient" (not.deficient $ 12)

testWords' :: Test
testWords' = TestCase $ do
  assertEqual "words' as words" ["these", "are", "words"] (words' Char.isSpace "these are\twords")
  assertEqual "words' by commas" ["some", "CSV", "header"] (words' (==',') "some,CSV,header")

libTests = TestList [ testFactor, testFibs, testPrimes, testArithmeticProgressionSum, testSquareNumbersSequenceSum,
                      testDigits, testNumber, testNumberFromDigits, testSlices, testTriangulars, testFactorials,
                      testDivisors , testProperDivisors, testPerfect, testAbundant, testDeficient, testWords' ]
