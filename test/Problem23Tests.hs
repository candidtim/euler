module Problem23Tests
  ( problem23Tests
  ) where


import Test.HUnit

import Problem23


testAbundants :: Test
testAbundants = TestCase $ do
  assertEqual "first 10 abundant numbers" [12, 18, 20, 24, 30, 36, 40, 42, 48, 54] (take 10 abundants)

testHasTwoAbundantAddends :: Test
testHasTwoAbundantAddends = TestCase $ do
  assertBool "24 has 2 abundant addends" (hasTwoAbundantAddends 24)
  assertBool "25 doesn't have 2 abundant addends" (not.hasTwoAbundantAddends $ 25)


problem23Tests = TestList [ testAbundants, testHasTwoAbundantAddends ]
