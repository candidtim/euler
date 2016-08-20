module Main where

import Lib
import Problem9

main :: IO ()
main = putStrLn $ show $ product $ pythagorianTriplet' 1000
