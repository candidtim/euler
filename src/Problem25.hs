module Problem25
  ( fibIdxWithNDigits
  ) where

import Lib (fibs, digits)

fibIdxWithNDigits :: Int -> Int
fibIdxWithNDigits n = length $ takeWhile (\i -> length (digits i) < n) fibs
