module Problem19Tests
  ( problem19Tests
  ) where


import Test.HUnit

import Problem19


testLeapYear :: Test
testLeapYear = TestCase $ do
  assertBool "1900 was not leap (divides by 100 and not by 400)" (not.leap $ 1900)
  assertBool "1904 was leap" (leap 1904)
  assertBool "1980 was leap" (leap 1980)
  assertBool "2000 was leap (divides by 400)" (leap 2000)

testNextDay :: Test
testNextDay = TestCase $ do
  assertEqual "28/02/1999 -> 01/03/1999" (1, 3, 1999) (nextDay (28, 2, 1999))
  assertEqual "28/02/2000 -> 29/02/2000" (29, 2, 2000) (nextDay (28, 2, 2000))
  assertEqual "29/02/2000 -> 01/03/2000" (1, 3, 2000) (nextDay (29, 2, 2000))
  assertEqual "30/04/1999 -> 01/05/1999" (1, 5, 1999) (nextDay (30, 4, 1999))
  assertEqual "30/05/1999 -> 31/05/1999" (31, 5, 1999) (nextDay (30, 5, 1999))
  assertEqual "31/05/1999 -> 01/06/1999" (1, 6, 1999) (nextDay (31, 5, 1999))
  assertEqual "22/05/1999 -> 23/05/1999" (23, 5, 1999) (nextDay (22, 5, 1999))
  assertEqual "31/12/1999 -> 01/01/2000" (1, 1, 2000) (nextDay (31, 12, 1999))

testAddDays :: Test
testAddDays = TestCase $ do
  assertEqual "30/05/1999 + 2 days" (1, 6, 1999) (addDays (30, 5, 1999) 2)
  assertEqual "22/05/1997 + 365 days" (22, 5, 1998) (addDays (22, 5, 1997) 365)
  assertEqual "22/05/1999 + 365 days" (21, 5, 2000) (addDays (22, 5, 1999) 365)


problem19Tests = TestList [ testLeapYear, testNextDay, testAddDays ]
