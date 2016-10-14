module Problem84Tests
  ( problem84Tests
  ) where


import Test.HUnit

import Problem84


testFindPosition :: Test
testFindPosition = TestCase $ do
  assertEqual "GO position" 0 (findPosition "GO")
  assertEqual "JAIL position" 10 (findPosition "JAIL")


testNextCellFromGroup :: Test
testNextCellFromGroup = TestCase $ do
  assertEqual "U1 from GO" (findPosition "U1") (nextCellFromGroup 'U' (findPosition "GO"))
  assertEqual "U2 from U1" (findPosition "U2") (nextCellFromGroup 'U' (findPosition "U1"))
  assertEqual "U1 from U2" (findPosition "U1") (nextCellFromGroup 'U' (findPosition "U2"))



problem84Tests = TestList [ testFindPosition, testNextCellFromGroup ]
