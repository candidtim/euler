module Lib
    ( factor
    , fibs
    ) where


-- |Predicate to verify if first argument is a factor of a second one
-- (that is, if second argument divides by first without remainder)
factor :: Int -> Int -> Bool
x `factor` n = n `mod` x == 0


-- |Fibonaci inifinite sequence
fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
