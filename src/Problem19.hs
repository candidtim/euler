module Problem19
  ( countSundaysOnFirst
  , addDays
  , nextDay
  , leap
  ) where


type Day = Int
type Month = Int
type Year = Int
type Date = (Day,Month,Year)

jan = 1
feb = 2
mar = 3
apr = 4
may = 5
jun = 6
jul = 7
aug = 8
sep = 9
oct = 10
nov = 11
dec = 12


countSundaysOnFirst :: Int
countSundaysOnFirst = 0

addDays :: Date -> Int -> Date
addDays date days = iterate nextDay date !! days

nextDay :: Date -> Date
nextDay (day, month, year)
  | day == 28 && month == feb && (not.leap $ year) = (1, mar, year)
  | day == 28 && month == feb = (29, feb, year)
  | day == 29 && month == feb = (1, mar, year)
  | day == 30 && (month == apr || month == jun || month == sep || month == nov) = (1, month+1, year)
  | day == 31 && month == dec = (1, jan, year+1)
  | day == 31 = (1, month+1, year)
  | otherwise = (day+1, month, year)

leap :: Year -> Bool
leap year = year `mod` 400 == 0 || (year `mod` 4 == 0 && year `mod` 100 /= 0)
