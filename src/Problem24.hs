module Problem24
  ( lexicographicPermutation
  ) where


import Data.List (sort, permutations)

import Lib (number)


lexicographicPermutation :: Int -> Int
lexicographicPermutation n = number $ (sort $ permutations [0,1,2,3,4,5,6,7,8,9]) !! (n-1)
