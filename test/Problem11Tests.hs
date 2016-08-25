module Problem11Tests
  ( problem11Tests
  ) where


import Test.HUnit

import Problem11


matrix = [
  [02, 22, 97, 38, 15],
  [49, 99, 40, 17, 81],
  [49, 31, 73, 55, 79],
  [70, 95, 23, 04, 60],
  [31, 16, 71, 51, 67]]

matrixFirstColumnOff = [
  [22, 97, 38, 15],
  [99, 40, 17, 81],
  [31, 73, 55, 79],
  [95, 23, 04, 60],
  [16, 71, 51, 67]]

matrixWithUpperHalf = [
  [02, 22, 97, 38, 15],
  [    99, 40, 17, 81],
  [        73, 55, 79],
  [            04, 60],
  [                67]]

columnsFromUpperHalf = [ [02, 99, 73, 04, 67], [22, 40, 55, 60], [97, 17, 79], [38, 81], [15] ]

expectedDiagonals = [ [02, 99, 73, 04, 67], [22, 40, 55, 60], [49, 31, 23, 51], [99, 73, 04, 67] ]


testMaxProductInRow :: Test
testMaxProductInRow = TestCase $ do
  assertEqual "max product in row or 5 elements" (1216380, [22, 97, 38, 15]) (maxProductInList [08, 02, 22, 97, 38, 15, 00])

testDiagonals :: Test
testDiagonals = TestCase $ do
  assertEqual "digonals from matrix" expectedDiagonals (diagonalsLR matrix)

testCutFirstColumn :: Test
testCutFirstColumn = TestCase $ do
  assertEqual "cut first column of matrix" matrixFirstColumnOff (cutFirstColumn matrix)

testUpperRightHalf :: Test
testUpperRightHalf = TestCase $ do
  assertEqual "cut to diagonals" matrixWithUpperHalf (upperRightHalf matrix)

problem11Tests = TestList [testMaxProductInRow, testDiagonals, testCutFirstColumn, testUpperRightHalf]
