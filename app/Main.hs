module Main where

import Lib
import Problem14


main :: IO ()
main = putStrLn $ show $ longestCollatz 1000000
