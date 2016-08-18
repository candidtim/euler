import Test.HUnit

import Problem4Tests

main :: IO Counts
main = runTestTT $ TestList [problem4Tests]
