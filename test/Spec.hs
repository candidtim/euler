import Test.HUnit

import LibTests
import Problem1Tests
import Problem4Tests
import Problem5Tests


main :: IO Counts
main = runTestTT $ TestList [libTests, problem1Tests, problem4Tests, problem5Tests]
