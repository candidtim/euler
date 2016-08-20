module Problem9
  ( pythagorianTriplet
  , pythagorianTriplet'
  ) where


-- Solution 1

pythagorianTriplet :: Int -> [Int]
pythagorianTriplet n =
  let as = [1 .. n]
      bs = [1 .. n]
      cs = [1 .. n]
      combos = [[a,b,c] | a <- as, b <- bs, c <- cs]
      pythagorianTriplets = filter (\[a,b,c] -> testSqSum a b c) combos
      specialOnes = filter (\[a,b,c] -> testSum n a b c) pythagorianTriplets
   in head $ specialOnes


testSqSum :: Int -> Int -> Int -> Bool
testSqSum a b c = a^2 + b^2 == c^2

testSum :: Int -> Int -> Int -> Int -> Bool
testSum n a b c = a + b + c == n


-- Solution 2

pythagorianTriplet' :: Int -> [Int]
pythagorianTriplet' n =
  let c' = head $ dropWhile (not.(bexists n)) [1 .. n]
      b' = b n c'
      a' = a n b' c'
   in [a', b', c']

-- from following equations:
-- a^2 + b^2 = c^2
-- a + b + c = N
-- can define a as fn(b,c) , and b as fn(c)
-- so need to find only suitable c

a :: Int -> Int -> Int -> Int
a n b c = n - b - c

b :: Int -> Int -> Int
b n c = (n^2 - 2*n*c + 2*c^2) `div` (2*n - 2*c)

-- a b c should be natural numbers
bexists :: Int -> Int -> Bool
bexists n c = ((n^2 - 2*n*c + 2*c^2) `mod` (2*n - 2*c)) == 0
