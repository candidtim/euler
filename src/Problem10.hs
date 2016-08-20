module Problem10
  ( sumPrimes
  ) where


import Lib (primes)


sumPrimes :: Int -> Int
sumPrimes n = sum $ primes n
