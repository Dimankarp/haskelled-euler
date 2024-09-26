module Main (main) where
import TriangularNum (tailRecursionHighlyDivisibleTriangNum)
import CountingSundays(toDate)

main :: IO ()
main = print $ toDate 2024 9 27
