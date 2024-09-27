module Main (main) where

import CountingSundays (Date(..), countSundays, toDate)
import TriangularNum (tailRecursionHighlyDivisibleTriangNum)

main :: IO ()
main = print $ countSundays 1901 (rem days 7) 2001
  where
    (Date days) = toDate 1901 1 1
