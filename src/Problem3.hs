module Problem3
    ( solveProblem3
    , primes
    ) where


solveProblem3 :: Int
solveProblem3 =
    -- let n = 13195
    let n = 600851475143
        cap = ceiling.sqrt.fromIntegral $ n
    in  last $ filter (`factor` n) $ primes cap

primes :: Int -> [Int]
primes cap = sieve [2..cap]
    where sieve (x:xs) = x : (sieve $ filter (not.(factor x)) xs)
          sieve [] = []

factor :: Int -> Int -> Bool
x `factor` n = n `mod` x == 0
