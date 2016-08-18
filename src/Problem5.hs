module Problem5
  ( solveProblem5
  , multipleOfAll
  ) where

import Lib (factor)

solveProblem5 :: Int -> Int
solveProblem5 upTo = head $ filter (`multipleOfAll` [1..upTo]) [1..]

multipleOfAll :: Int -> [Int] -> Bool
n `multipleOfAll` xs = all (`factor` n) xs
