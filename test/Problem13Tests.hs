module Problem13Tests
  ( problem13Tests
  ) where


import Test.HUnit

import Problem13


testColumnarAddition :: Test
testColumnarAddition = TestCase $ do
  assertEqual "123 + 456" (123 + 456) (columnarAddition [123, 456])
  assertEqual "3455 + 3555" (3455 + 3555) (columnarAddition [3455, 3555])
  assertEqual "123 + 456 + 789" (123 + 456 + 789) (columnarAddition [123, 456, 789])


problem13Tests = TestList [testColumnarAddition]
