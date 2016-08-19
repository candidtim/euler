module Problem7
  ( nthPrime
  ) where

import Lib

nthPrime :: Int -> Int
nthPrime n = last $ take n (primes 120000)
-- 120000 is roughly estimated based on prime count function
-- p(n) ~= n / ln n ; p(120000) = 10260 > 10001
