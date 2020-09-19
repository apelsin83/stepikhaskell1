{--
:t not
not :: Bool -> Bool

:t (&&)
(&&) :: Bool -> Bool -> Bool

-> is right assiciative

--}
module Types where

import Data.Char

discount :: Double -> Double -> Double -> Double
discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum

standardDiscount :: Double -> Double
standardDiscount = discount 1000 5 

test = isDigit '7'

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if isDigit x && isDigit y then digitToInt x * 10 + digitToInt y else 100

twoDigits22Int x y
    | isDigit x && isDigit y = digitToInt x * 10 + digitToInt y
    | otherwise = 100


dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt $ (fst p1 - fst p2) ^ 2 + (snd p1 - snd p2) ^ 2

dist2 :: (Double, Double) -> (Double, Double) -> Double
dist2 (x1,y1) (x2,y2) = sqrt $ sum $ fmap (^2) [x2 - x1, y2 - y1] 
