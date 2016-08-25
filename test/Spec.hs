import Test.HUnit

import LibTests
import Problem1Tests
import Problem4Tests
import Problem5Tests
import Problem6Tests
import Problem8Tests
import Problem11Tests
import Problem12Tests


main :: IO Counts
main = runTestTT $ TestList [ libTests, problem1Tests, problem4Tests, problem5Tests, problem6Tests, problem8Tests
                            , problem11Tests, problem12Tests ]
