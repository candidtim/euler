module Main where

import System.Random

import Lib
import Problem84


main :: IO ()
main = do
  g <- getStdGen
  putStrLn . show $ monopolyOddsToken g 100000
