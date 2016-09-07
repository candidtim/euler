module Problem18Tests
  ( problem18Tests
  ) where


import Test.HUnit

import Problem18


sampleTriangle = [ [ 3 ]
                 , [ 7, 4 ]
                 , [ 2, 4, 6 ]
                 , [ 8, 5, 9, 3 ] ]

leftS = [ [ 7 ]
        , [ 2, 4 ]
        , [ 8, 5, 9 ] ]

rightS = [ [ 4 ]
         , [ 4, 6 ]
         , [ 5, 9, 3 ] ]


testLeftSubTriangle :: Test
testLeftSubTriangle = TestCase $ do
  assertEqual "left sub-triangle" leftS (leftSubTriangle sampleTriangle)

testRightSubTriangle :: Test
testRightSubTriangle = TestCase $ do
  assertEqual "right sub-triangle" rightS (rightSubTriangle sampleTriangle)

testWeight :: Test
testWeight = TestCase $ do
  assertEqual "triangle weight" 51 (weight sampleTriangle)

testMaxPathSum :: Test
testMaxPathSum = TestCase $ do
  assertEqual "max path sum" (23, [3, 7, 4, 9]) (maxTrianglePathSum sampleTriangle)


problem18Tests = TestList [ testLeftSubTriangle, testRightSubTriangle, testWeight, testMaxPathSum ]
