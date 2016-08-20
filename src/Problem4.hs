module Problem4
    ( solveProblem4
    , digits
    , number
    , palindrome
    ) where


import Data.List as List
import Lib (digits, number)

solveProblem4 :: Int -> Int
solveProblem4 n =
  let multiples = [x*y | x <- [1..n], y <- [1..n]]
   in head . filter palindrome . reverse . List.sort $ multiples

palindrome :: Int -> Bool
palindrome n = (number.reverse.digits $ n) == n
