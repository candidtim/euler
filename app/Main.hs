module Main where

import Lib
import Problem25


main :: IO ()
main = putStrLn . show $ fibIdxWithNDigits 1000
