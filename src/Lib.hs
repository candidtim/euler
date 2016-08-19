module Lib
    ( factor
    , fibs
    , primes
    , arithmeticProgressionSum
    , squareNumbersSum
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


-- |Sum of the arithmetic progression a1..an with n members
arithmeticProgressionSum :: Int -> Int -> Int -> Int
arithmeticProgressionSum a1 an n = (a1+an)*n `div` 2


-- |Sum of the sequnce of squares of natural numbers from 0 to n
-- 1^2 + 2^2 + ... + n^2
squareNumbersSum :: Int -> Int
squareNumbersSum n = n * (n+1) * (2*n+1) `div` 6
