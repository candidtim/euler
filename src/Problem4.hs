module Problem4
    ( solveProblem4
    , digits
    , number
    , palindrome
    ) where


import Data.List as List


solveProblem4 :: Int
solveProblem4 =
  let n = 999
      multiples = [x*y | x <- [1..n], y <- [1..n]]
   in head . filter palindrome . reverse . List.sort $ multiples

palindrome :: Int -> Bool
palindrome n = (number.reverse.digits $ n) == n

digits :: Int -> [Int]
digits 0 = [0]
digits n = digits' n []
  where digits' 0 xs = xs
        digits' n xs = digits' (n `div` 10) (n `mod` 10 : xs)

number :: [Int] -> Int
number xs = number' xs 0
  where number' [] n = n
        number' (x:xs) n = number' xs (n*10+x)
