module Problem17
  ( spellNumber
  , lettersInNumber
  , lettersInNumbersUpTo
  ) where


lettersInNumbersUpTo :: Int -> Int
lettersInNumbersUpTo x = sum $ map lettersInNumber [1..x]

lettersInNumber :: Int -> Int
lettersInNumber x = length $ filter (\c -> c /= ' ' && c /= '-') (spellNumber x)

spellNumber :: Int -> String
spellNumber 1 = "one"
spellNumber 2 = "two"
spellNumber 3 = "three"
spellNumber 4 = "four"
spellNumber 5 = "five"
spellNumber 6 = "six"
spellNumber 7 = "seven"
spellNumber 8 = "eight"
spellNumber 9 = "nine"
spellNumber 10 = "ten"
spellNumber 11 = "eleven"
spellNumber 12 = "twelve"
spellNumber 13 = "thirteen"
spellNumber 14 = "fourteen"
spellNumber 15 = "fifteen"
spellNumber 16 = "sixteen"
spellNumber 17 = "seventeen"
spellNumber 18 = "eighteen"
spellNumber 19 = "nineteen"
spellNumber 20 = "twenty"
spellNumber 30 = "thirty"
spellNumber 40 = "forty"
spellNumber 50 = "fifty"
spellNumber 60 = "sixty"
spellNumber 70 = "seventy"
spellNumber 80 = "eighty"
spellNumber 90 = "ninety"
spellNumber x
  | x < 100 = spellNumber ((x `div` 10)*10) ++ "-" ++ spellNumber (x `mod` 10)
  | x < 1000 && (x `mod` 100) == 0 = spellNumber (x `div` 100) ++ " hundred"
  | x < 1000 = spellNumber (x `div` 100) ++ " hundred and " ++ spellNumber (x `mod` 100)
  | x < 1000000 && (x `mod` 1000) == 0 = spellNumber (x `div` 1000) ++ " thousand"
  | otherwise = error $ "cannot spell " ++ show x
