module Lib
    ( factor
    ) where


-- |Predicate to verify if first argument is a factor of a second one
-- (that is, if second argument divides by first without remainder)
factor :: Int -> Int -> Bool
x `factor` n = n `mod` x == 0
