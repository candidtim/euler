module Problem12Tests
  ( problem12Tests
  ) where


import Test.HUnit

import Problem12


testHighlyDivisibleTriangularNumber :: Test
testHighlyDivisibleTriangularNumber = TestCase $ do
  assertEqual "triangular number divisble by 5+ divisors" 28 (divisibleTriangularNumber 5)


problem12Tests = TestList [testHighlyDivisibleTriangularNumber]
