module Problem16
  ( powerDigitSum
  ) where


import Lib (digits)


powerDigitSum :: Integer -> Integer -> Integer
powerDigitSum x pow = sum.digits $ x^pow
