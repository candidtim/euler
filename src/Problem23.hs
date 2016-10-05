module Problem23
  ( nonTwoAbundantAddendsNumbers
  , hasTwoAbundantAddends
  , abundants
  ) where


import qualified Data.Set as Set

import Lib


-- By mathematical analysis, it can be shown that all integers greater
-- than 28123 can be written as the sum of two abundant numbers
problemCap = 28123

-- |A list of numbers that cannot be written as a sum of two abundant numbers
-- (have no idea how to rename this function to make it shorter)
nonTwoAbundantAddendsNumbers :: [Int]
nonTwoAbundantAddendsNumbers =
  let candidateNumbers = [1..problemCap]
   in filter (not.hasTwoAbundantAddends) candidateNumbers

-- |Predicate to determine if the given number can be written as a sum
-- of two abundant numbers
hasTwoAbundantAddends :: Int -> Bool
hasTwoAbundantAddends n = not.null $ filter hasAbundantAddend abundants
  where hasAbundantAddend m = Set.member (n-m) abundantsSet

abundants :: [Int]
abundants = filter abundant [1..problemCap]

abundantsSet :: Set.Set Int
abundantsSet = Set.fromList abundants
