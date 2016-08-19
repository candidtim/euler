module Problem6
  ( sumSquareDifference
  , sumSquareDifference'
  ) where


import Lib (arithmeticProgressionSum, squareNumbersSum)


sumSquareDifference :: Int -> Int
sumSquareDifference cap =
  let apSum = arithmeticProgressionSum 1 cap cap
      sqSum = squareNumbersSum cap
   in apSum^2 - sqSum

sumSquareDifference' :: Int -> Int
sumSquareDifference' cap =
  let apSum = sum [1..cap]
      sqSum = sum $ map (^2) [1..cap]
   in apSum^2 - sqSum
