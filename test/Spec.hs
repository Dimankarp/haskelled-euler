import CountingSundays
import Test.HUnit
import TriangularNum

main :: IO ()
main = runTestTTAndExit tests

prob12Solution :: Int
prob12Solution = 76576500 :: Int

prob19Solution :: Int
prob19Solution = 171 :: Int

testTriangFold :: Test
testTriangFold = TestCase (assertEqual "for real answer comparison" (modularHighlyDivisibleTriangNum 500) prob12Solution)

testTriangRecursive :: Test
testTriangRecursive = TestCase (assertEqual "for real answer comparison" (recursiveHighlyDivisibleTriangNum 500) prob12Solution)

testTriangTail :: Test
testTriangTail = TestCase (assertEqual "for real answer comparison" (tailRecursionHighlyDivisibleTriangNum 500) prob12Solution)

testSundayRecursive :: Test
testSundayRecursive =
  TestCase (assertEqual "for real answer comparison" prob19Solution (recursiveCountSundays 1901 stWk 2001))
  where
    Date days = toDate 1901 1 1
    stWk = rem days 7

testSundayBrute :: Test
testSundayBrute = TestCase (assertEqual "for real answer comparison" prob19Solution (bruteforceCountSundays 1901 2000))

tests :: Test
tests =
  TestList
    [ TestLabel "#12 Fold solution" testTriangFold,
      TestLabel "#12 Recursive solution" testTriangRecursive,
      TestLabel "#12 Tail recursive solution" testTriangTail,
      TestLabel "#19 Recursive solution" testSundayRecursive,
      TestLabel "#19 Bruteforce solution" testSundayBrute
    ]
