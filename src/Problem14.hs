module Problem14
  ( longestCollatz
  , collatz
  ) where


import Data.List as L


longestCollatz :: Int -> Int
longestCollatz cap =
  let seqs = map (\n -> (length.collatz $ n, n)) [1..cap]
      longest = L.maximum seqs
   in snd longest


collatz :: Int -> [Int]
collatz 1 = [1]
collatz n =
  let n' = nextCollatz n
   in n' : collatz n'
  where nextCollatz n
          | even n = n `div` 2
          | otherwise = 3*n + 1
