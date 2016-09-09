module Problem21
  ( sumAmicables
  , amicable
  ) where


import Lib (properDivisors, perfect)


sumAmicables :: Int -> Int
sumAmicables cap = sum $ filter (not.perfect) $ filter amicable [1..cap]

amicable :: Int -> Bool
amicable n = ( sum . properDivisors . sum . properDivisors $ n) == n

