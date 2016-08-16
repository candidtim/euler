module Problem1
    ( solveProblem1
    , multipliers
    ) where

import Data.Set as Set
import Data.List as List


solveProblem1 :: Int
solveProblem1 =
  let upperLimit = 1000
      msBelowLimit = (`multipliers` upperLimit)
      m3 = msBelowLimit 3
      m5 = msBelowLimit 5
      ms = Set.elems $ Set.union (Set.fromList m3) (Set.fromList m5)
  in  sum ms


multipliers :: Int -> Int -> [Int]
multipliers x z = List.filter ((== 0).(`mod` x)) [1..(z-1)]
