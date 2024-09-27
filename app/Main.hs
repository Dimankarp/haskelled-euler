module Main (main) where

import CountingSundays (Date (..), bruteforceCountSundays, recursiveCountSundays, toDate)
import TriangularNum (modularHighlyDivisibleTriangNum, recursiveHighlyDivisibleTriangNum, tailRecursionHighlyDivisibleTriangNum)

main :: IO ()
main = do
  putStrLn "#12 - Triangular nums"
  putStrLn $ "Fold: " ++ show (modularHighlyDivisibleTriangNum 500)
  putStrLn $ "Recursive: " ++ show (recursiveHighlyDivisibleTriangNum 500)
  putStrLn $ "Tail Recursive: " ++ show (tailRecursionHighlyDivisibleTriangNum 500)

  putStrLn "\n#19 - Sundays"
  putStrLn $ "Recursive: " ++ show (recursiveCountSundays 1901 stWk 2001)
  putStrLn $ "Bruteforce: " ++ show (bruteforceCountSundays 1901 2000)
  where
    Date days = toDate 1901 1 1
    stWk = rem days 7
