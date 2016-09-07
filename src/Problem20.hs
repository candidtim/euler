module Problem20
  ( sumFactorialDigits
  ) where


import Lib (factorials, digits)


sumFactorialDigits :: Int -> Integer
sumFactorialDigits n = sum.digits $ (factorials !! (n-1))
