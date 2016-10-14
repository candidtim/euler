module Lib
    ( factor
    , fibs
    , primes
    , triangulars
    , factorials
    , arithmeticProgressionSum
    , squareNumbersSum
    , digits
    , number
    , slices
    , divisors
    , properDivisors
    , perfect
    , abundant
    , deficient
    , words'
    , shuffle
    ) where


import Data.List as L
import System.Random as Random


-- |Predicate to verify if first argument is a factor of a second one
-- (that is, if second argument divides by first without remainder)
factor :: Int -> Int -> Bool
x `factor` n = n `mod` x == 0


-- |Fibonaci inifinite sequence
fibs :: Integral a => [a]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)


-- |Prime numbers list up to given max value
primes :: Int -> [Int]
primes cap = sieve [2..cap]
    where sieve (x:xs) = x : (sieve $ filter (not.(factor x)) xs)
          sieve [] = []


-- |Triangular numbers infinite sequence
triangulars :: [Int]
triangulars = [ sum [1..n] | n <- [1..] ]


-- |Factorials
factorials :: [Integer]
factorials = [ product [1..n] | n <- [1..] ]


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


-- |Create slices of given size out a list
-- slice [1,2,3,4] 2 == [ [1,2], [2,3], [3,4] ]
slices :: [a] -> Int -> [[a]]
slices xs size
  | length xs > size = (take size xs) : slices (drop 1 xs) size
  | otherwise = [xs]


-- |Divisors of a number
-- uses trial division
divisors :: Int -> [Int]
divisors n =
  let cap = ceiling.sqrt.fromIntegral $ n
      xs = filter (`factor` n) [1..cap]
   in L.sort $ L.nub $ map (n `div`) xs ++ xs


-- |Proper divisors of a number
-- (same as divisors but excludinng the number itself)
properDivisors :: Int -> [Int]
properDivisors = init' . divisors
  where init' [] = []
        init' xs = init xs


-- |Predicate to verify if given number is perfect
perfect :: Int -> Bool
perfect n = (sum . properDivisors $ n) == n


-- |Predicate to verify if given number is abundant
abundant :: Int -> Bool
abundant n = (sum . properDivisors $ n) > n


-- |Predicate to verify if given number is deficient
deficient :: Int -> Bool
deficient n = (sum . properDivisors $ n) < n


-- |Same as `words` but using custom predicate to determine start of new word
-- Derived from `Data.List.words` implementation
words' :: (Char -> Bool) -> String -> [String]
words' p s =  case dropWhile p s of
                   "" -> []
                   s' -> w : words' p s''
                         where (w, s'') = break p s'


-- |Shuffle randomly a list of items given number of times
shuffle :: Random.RandomGen g => g -> [a] -> Int -> ([a], g)
shuffle g xs n = (iterate splitOnce (xs, g)) !! n
  where splitOnce :: Random.RandomGen g => ([a], g) -> ([a], g)
        splitOnce (xs, g) = let (i, g') = Random.randomR (1, length xs) g
                                (inits, tails) = L.splitAt i xs
                                xs' = tails ++ inits
                             in (xs', g')
