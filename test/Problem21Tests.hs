module Problem21Tests
  ( problem21Tests
  ) where


import Test.HUnit

import Problem21


testAmicable :: Test
testAmicable = TestCase $ do
  assertBool "220 is amicable" (amicable 220)
  assertBool "284 is amicable" (amicable 284)


problem21Tests = TestList [ testAmicable ]
