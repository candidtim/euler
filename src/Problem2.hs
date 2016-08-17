module Problem2
    ( solveProblem2
    ) where

solveProblem2 :: Int
-- solveProblem2 = fibsum 1 2 4000000
solveProblem2 = fibsum' 4000000

-- solution 1

fibsum :: Int -> Int -> Int -> Int
fibsum prev next cap
    | next > cap = evenOr0 prev
    | otherwise  = evenOr0 prev + fibsum next (prev+next) cap
    where evenOr0 n = if even n then n else 0

-- solution 2

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibsum' :: Int -> Int
fibsum' cap = sum $ filter even $ takeWhile (<cap) fibs
