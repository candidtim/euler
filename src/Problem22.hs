module Problem22
  ( sumNamesScores
  , namesFromFile
  ) where


import Data.List (sort)
import System.IO

import Lib (words')


namesFromFile :: FilePath -> IO [String]
namesFromFile fileName = do
  contents <- readFile fileName
  let namesQuoted = words' (==',') contents
      names = map (init.tail) namesQuoted
  return names


sumNamesScores :: [String] -> Int
sumNamesScores names =
  let sortedNames = sort names
      weights = map weight sortedNames
      scores = zipWith (*) [1..] weights
   in sum scores


weight :: String -> Int
weight name = sum $ map (ordShift+) $ map fromEnum name
  where ordShift = - fromEnum 'A' + 1
