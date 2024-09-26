module TriangularNum (modularHighlyDivisibleTriangNum, recursiveHighlyDivisibleTriangNum, tailRecursionHighlyDivisibleTriangNum) where

{- ProjEuler №12:
What is the value of the first triangle
number to have over five hundred divisors?
-}

{- Modular solution:
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
countDivisorsFold x =
  (\c -> if rem x sqrtx == 0 then c + 1 else c) $
    (* 2) $
      foldr (\el acc -> if rem x el == 0 then acc + 1 else acc) 0 [1 .. sqrtx - 1]
  where
    sqrtx = flooredIntSqrt x

modularHighlyDivisibleTriangNum :: Int -> Int
modularHighlyDivisibleTriangNum x =
  fst $
    head $
      dropWhile ((<= x) . snd) $
        map (\y -> (y, countDivisorsFold y)) triangularSeqInf

-- Modular solution end

{- Regular recursion solution:
    Recusrive divisor counter
    Tail recursive triangular num
-}

countDivisorsRecursion :: Int -> Int
countDivisorsRecursion x = 2 * go x sqrtx - (if rem x sqrtx == 0 then 1 else 0)
  where
    sqrtx = flooredIntSqrt x
    go n divisor
      | divisor == 1 = 1
      | otherwise = (if rem n divisor == 0 then 1 else 0) + go n (divisor - 1) -- Losing tail of recursion

recursiveHighlyDivisibleTriangNum :: Int -> Int
recursiveHighlyDivisibleTriangNum x = go 1 2
  where
    go prev_triag next_ind = if countDivisorsRecursion new_triag > x then new_triag else go new_triag $ next_ind + 1
      where
        new_triag = prev_triag + next_ind

-- Regular recursion solution end

{- Tail recursion solution:
    Tail divisor counter (in a sense, it's a self-written fold)
    Tail recursive triangular num
-}

countDivisorsTailRecursion :: Int -> Int
countDivisorsTailRecursion x = 2 * go x sqrtx 0 - (if rem x sqrtx == 0 then 1 else 0)
  where
    sqrtx = flooredIntSqrt x
    go n divisor accum
      | divisor == 1 = accum + 1
      | otherwise = go n (divisor - 1) (if rem n divisor == 0 then accum + 1 else accum)

tailRecursionHighlyDivisibleTriangNum :: Int -> Int
tailRecursionHighlyDivisibleTriangNum x = go 1 2
  where
    go prev_triag next_ind = if countDivisorsTailRecursion new_triag > x then new_triag else go new_triag $ next_ind + 1
      where
        new_triag = prev_triag + next_ind

-- Regular recursion solution end
