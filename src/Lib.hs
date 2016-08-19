module Lib
    ( factor
    , fibs
    , primes
    ) where


-- |Predicate to verify if first argument is a factor of a second one
-- (that is, if second argument divides by first without remainder)
factor :: Int -> Int -> Bool
x `factor` n = n `mod` x == 0


-- |Fibonaci inifinite sequence
fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)


-- |Prime numbers list up to given max value
primes :: Int -> [Int]
primes cap = sieve [2..cap]
    where sieve (x:xs) = x : (sieve $ filter (not.(factor x)) xs)
          sieve [] = []
