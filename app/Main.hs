module Main where

import Lib
import Problem22


main :: IO ()
main = do
  names <- namesFromFile "res/p022_names.txt"
  putStrLn . show $ sumNamesScores names
