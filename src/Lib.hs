module Lib
    ( factor
    , fibs
    , primes
    , arithmeticProgressionSum
    , squareNumbersSum
    , digits
    , number
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


-- |List of digits representing a given number (inverse of `number`)
-- digits 123 = [1, 2, 3]
digits :: Integral a => a -> [a]
digits 0 = [0]
digits n = digits' n []
  where digits' 0 xs = xs
        digits' n xs = digits' (n `div` 10) (n `mod` 10 : xs)


-- |A number represented by a given list of digits (inverse of `digits`)
-- number [1, 2, 3] = 123
number :: Integral a => [a] -> a
number xs = number' xs 0
  where number' [] n = n
        number' (x:xs) n = number' xs (n*10+x)
