module Problem12
  ( divisibleTriangularNumber
  ) where


import Lib (factor, triangulars, divisors)

divisibleTriangularNumber :: Int -> Int
divisibleTriangularNumber n = head $ dropWhile ((<n).(length.divisors)) triangulars
