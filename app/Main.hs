module Main (main) where

import CountingSundays (Date (..), recursiveCountSundays, toDate)

main :: IO ()
main = print $ recursiveCountSundays 1901 (rem days 7) 2001
  where
    (Date days) = toDate 1901 1 1
