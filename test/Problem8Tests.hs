module Problem8Tests
  ( problem8Tests
  ) where


import Test.HUnit

import Problem8


testLargestProduct :: Test
testLargestProduct = TestCase $ do
  assertEqual "largets product of 4 adjacent digits" 5832 (largestProduct 4)


problem8Tests = TestList [testLargestProduct]
