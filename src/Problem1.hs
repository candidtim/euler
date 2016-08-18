module Problem1
    ( solveProblem1
    , solveProblem1'
    , solveProblem1''
    ) where

import Data.Set as Set
import Data.List as List


solveProblem1 :: Int -> Int
solveProblem1 cap =
  let m3sum = sum [3,6..cap-1]
      m5sum = sum [5,10..cap-1]
      m15sum = sum [15,30..cap-1]
   in m3sum + m5sum - m15sum

solveProblem1' :: Int -> Int
solveProblem1' cap = multiplesSum 3 + multiplesSum 5 - multiplesSum 15
  where multiplesSum n = let p = (cap-1) `div` n
                          in (n * p * (p+1)) `div` 2

solveProblem1'' :: Int -> Int
solveProblem1'' cap =
  let m3 = multiples 3
      m5 = multiples 5
      ms = Set.elems $ Set.union (Set.fromList m3) (Set.fromList m5)
  in  sum ms
  where multiples x = List.filter ((== 0).(`mod` x)) [1..(cap-1)]
