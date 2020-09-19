module Local where

roots :: Double 
      -> Double 
      -> Double 
      -> (Double, Double)


roots a b c = 
    (
        (-b - sqrt (b ^ 2 - 4 * a * c)) / (2 * a)
    ,
        (-b + sqrt (b ^ 2 - 4 * a * c)) / (2 * a)
    )

roots' :: Double 
      -> Double 
      -> Double 
      -> (Double, Double)
roots' a b c =
    let d = sqrt (b ^ 2 - 4 * a * c) in 
    ((-b -d) / (2 * a), (-b + d) / (2 * a) )


roots'' :: Double 
        -> Double 
        -> Double 
        -> (Double, Double)
roots'' a b c =
    let {d = sqrt (b ^ 2 - 4 * a * c);x1 = (-b -d) / (2 * a);x2 = (-b + d) / (2 * a)} in 
    (x1, x2)


roots''' :: Double 
         -> Double 
         -> Double 
         -> (Double, Double)
roots''' a b c =
    let 
        x1 = (-b - d) / aTwice
        x2 = (-b + d) / aTwice
        d = sqrt $ b ^ 2 - 4 * a * c
        aTwice = 2 * a
    in (x1, x2)


factorial6 :: (Ord p, Num p) => p -> p
factorial6 n
    | n >= 0 = let 
        helper acc 0 = acc
        helper acc n = helper (acc * n) (n - 1)
      in helper 1 n
    | otherwise = error "arg must be >= 0"

rootsDiff :: Double -> Double -> Double -> Double
rootsDiff a b c = let
    (x1, x2) = roots a b c
    in x2 -x1

seqA :: Integer -> Integer
seqA n
    | n >= 0 = let 
        helper a b c n 
            | n == 0 = a
            | otherwise = helper b c (c + b - 2 * a) ( n-1 )
      in helper 1 2 3 n
    | otherwise = error "arg must be >= 0"


roots'''' :: Floating b => b -> b -> b -> (b, b)
roots'''' a b c = (x1, x2) where
    x1 = (-b - d) / aTwice
    x2 = (-b + d) / aTwice
    d = sqrt $ b ^ 2 - 4 * a * c
    aTwice = 2 * a


factorial7 :: (Ord p, Num p) => p -> p
factorial7 n | n >= 0 = helper 1 n
             | otherwise = error "arg must be >= 0"
    where
        helper acc 0 = acc
        helper acc n = helper (acc * n) (n - 1)

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x | x >= 0 = helper x 0 0
              | otherwise = helper (-x) 0 0 
    where
        helper n sm cnt | n < 10 = (n + sm, cnt + 1)
                        | otherwise = helper (n `div` 10) (sm + mod n 10) (cnt + 1)


integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = helper (a + step) 0 total
    where
        total = 1000
        step = (b - a) / total
        helper start sm n | n == 1 = step * ((f a + f b) / 2 + sm)
                          | otherwise = helper (start + step) (sm + f start) (n - 1)