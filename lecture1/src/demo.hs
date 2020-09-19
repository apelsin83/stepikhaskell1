module Demo where

f :: (Ord a, Num t, Num a) => a -> t

f x = if x > 0 then 1 else (-1)

g x = (if x > 0 then 1 else (-1)) + 3

sign :: (Ord a, Num a, Num t) => a -> t
sign x = if x > 0 then 1 else if x < 0 then -1 else 0

max5 :: (Ord a, Num a) => a -> a
max5 x = max 5 x

max5' :: Integer -> Integer
max5' = max 5

discount limit proc sumx = if sumx >= limit then sumx * (100 - proc) / 100 else sumx

standardDiscount = discount 1000 5