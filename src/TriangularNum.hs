module TriangularNum (modularHighlyDivisibleTriangNum, countDivisorsFold) where

{-
\** ProjEuler №12:
What is the value of the first triangle
number to have over five hundred divisors?
-}

{-
\* Modular solution:
    Infinite sequence
    Folding
    Mapping
-}

triangularSeqInf :: (Integral a) => [a]
triangularSeqInf = 1 : [x + y | (x, y) <- zip triangularSeqInf [2 ..]]

{- Int is used everywhere instead of Integral because of sqrt
 and fromIntegral (Int fits into Double, but not every Integral)
 Embrace totality!
-}

intSqrt :: Int -> Double
intSqrt = sqrt . fromIntegral

flooredIntSqrt :: Int -> Int
flooredIntSqrt = floor . intSqrt

countDivisorsFold :: Int -> Int
countDivisorsFold x = foldr (\el acc -> if rem x el == 0 then acc + 1 else acc) 0 [1 .. flooredIntSqrt x]

modularHighlyDivisibleTriangNum :: Int
{- The number 251 is here, because each divisor below sqrt X
 has counterpart above sqrt X, albeit sqrt X itself.
 250 * 2 <= 500 w/ and w/o sqrt X inside
 251 * 2 > 500 w/ and w/o sqrt X inside -}
modularHighlyDivisibleTriangNum = fst $ head $ dropWhile ((< 251) . snd) $ map (\x -> (x, countDivisorsFold x)) triangularSeqInf

-- Modular solution end

{-
\* Tail recursion solution:
    Infinite sequence
    Folding
    Mapping
-}

