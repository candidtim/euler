module Problem15Tests
  ( problem15Tests
  ) where


import Test.HUnit

import Problem15


grid2 = [ [1, 0]
        , [2, 1] ]

grid3 = [ [1, 1, 0]
        , [3, 2, 1]
        , [6, 3, 1] ]


testLatticePaths :: Test
testLatticePaths = TestCase $ do
  assertEqual "lattice paths on 2x2 grid" 6 (latticePaths 2)
  assertEqual "lattice paths on 3x3 grid" 20 (latticePaths 3)


testNextGrid :: Test
testNextGrid = TestCase $ do
  assertEqual "next grid from 2x2 grid" grid3 (nextGrid grid2)


problem15Tests = TestList [ testLatticePaths, testNextGrid ]
