module Problem17Tests
  ( problem17Tests
  ) where


import Test.HUnit

import Problem17


testSpellNumber :: Test
testSpellNumber = TestCase $ do
  assertEqual "spell 342" "three hundred and forty-two" (spellNumber 342)
  assertEqual "spell 115" "one hundred and fifteen" (spellNumber 115)
  assertEqual "spell 200" "two hundred" (spellNumber 200)
  assertEqual "spell 1000" "one thousand" (spellNumber 1000)

testLettersInNumber :: Test
testLettersInNumber = TestCase $ do
  assertEqual "letters in 342" 23 (lettersInNumber 342)
  assertEqual "letters in 115" 20 (lettersInNumber 115)


testLettersInNumbersUpTo :: Test
testLettersInNumbersUpTo = TestCase $ do
  assertEqual "letters in 1..5" 19 (lettersInNumbersUpTo 5)


problem17Tests = TestList [ testSpellNumber, testLettersInNumber, testLettersInNumbersUpTo ]
