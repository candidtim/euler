import Test.HUnit

import LibTests
import Problem1Tests
import Problem4Tests
import Problem5Tests
import Problem6Tests
import Problem8Tests
import Problem11Tests
import Problem12Tests
import Problem13Tests
import Problem15Tests
import Problem17Tests
import Problem18Tests
import Problem19Tests
import Problem21Tests


main :: IO Counts
main = runTestTT $ TestList [ libTests, problem1Tests, problem4Tests, problem5Tests, problem6Tests, problem8Tests
                            , problem11Tests, problem12Tests, problem13Tests, problem15Tests, problem17Tests
                            , problem18Tests , problem19Tests, problem21Tests ]
