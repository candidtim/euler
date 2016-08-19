module Problem3
    ( solveProblem3
    , primes
    ) where

import Lib (factor, primes)

solveProblem3 :: Int
solveProblem3 =
    -- let n = 13195
    let n = 600851475143
        cap = ceiling.sqrt.fromIntegral $ n
    in  last $ filter (`factor` n) $ primes cap
