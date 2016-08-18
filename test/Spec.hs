import Test.HUnit

import LibTests
import Problem4Tests
import Problem5Tests


main :: IO Counts
main = runTestTT $ TestList [libTests, problem4Tests, problem5Tests]
