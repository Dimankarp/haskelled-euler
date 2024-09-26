module CountingSundays(toDate) where

{- 1 Jan 1900 was a Monday.
Thirty days has September,
April, June and November.
All the rest have thirty-one,
Saving February alone,
Which has twenty-eight, rain or shine.
And on leap years, twenty-nine.
A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
-}

-- Representing date as offset by days from 1 Jan 1900
newtype (Integral a) => Date a = Date a deriving (Show, Eq)

toDate :: (Integral a) => a -> a -> a -> Date a
toDate y m d = Date days
  where
    year_offset = y - 1900
    month_offset = m - 1
    days_offset = d - 1
    days =
      quot year_offset 4 * (365 * 3 + 366)
        + rem year_offset 4 * 365
        + foldr (\a b -> monthToLen a y + b) 0 [1 .. month_offset]
        + days_offset

isYearLeap :: (Integral a) => a -> Bool
isYearLeap y = (rem y 4 == 0 && rem y 100 /= 0) || (rem y 400 == 0)

monthToLen :: (Integral a) => a -> a -> a
monthToLen m y = case m of
  4 -> 30
  6 -> 30
  9 -> 30
  11 -> 30
  2 -> if isYearLeap y then 29 else 28
  _ -> 31