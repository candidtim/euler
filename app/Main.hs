module Main where

import Lib
import Problem18


main :: IO ()
main = putStrLn $ show $ maxTrianglePathSum bigTriangle
