module Recursion where

-- factorial :: Int -> Int


factorial n = if n == 0 then 1 else n * factorial (n - 1)

--  pattern matching
-- for excetion error and undefined

factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

factorial'' 0 = 1
factorial'' n = if n < 0 then error "arg must be >= 0" else n * factorial'' (n - 1)

-- guraded 

factorial''' 0 = 1
factorial''' n | n < 0 = error "arg must be >= 0" 
               | n > 0 = n * factorial''' (n - 1)

factorial4 :: Integer -> Integer

factorial4 n | n == 0 = 1
             | n > 0 = n * factorial4 (n - 1)
             | otherwise = error "arg must be >= 0" 

-- with accumulator

factorial5 n | n >= 0 = helper 1 n
             | otherwise = error "arg must be >= 0" 

helper acc 0 = acc
helper acc n = helper (acc * n) (n - 1)


fibonacci :: Integer -> Integer
fibonacci n = fH 0 1 n

fH :: Integer -> Integer -> Integer -> Integer
fH first second n | n > 0 = fH second (first + second) (n - 1)
                  | n < 0 = fH (second - first) first (n + 1)
                  | otherwise = first


-- | n > 1     = fibonacci2 (n - 1) + fibonacci2 (n - 2)
--              | n < 0     = fibonacci2 (n + 2) - fibonacci2 (n + 1)
--              | otherwise = n

doubleFact :: Integer -> Integer
doubleFact 0 = 1
doubleFact n = if n < 3 then n else n * doubleFact (n - 2)

doubleFact1 :: Integer -> Integer
doubleFact1 n = if n <= 0 then 1 else n * doubleFact1 (n - 2)


fibonacci1 :: Integer -> Integer
fibonacci1 n | n >= 0 = fibonacciHelper n
            | otherwise =  fibonacciHelper (-n) * (-1) ^ ((-n) + 1)

fibonacciHelper :: Integer -> Integer
fibonacciHelper 0 = 0
fibonacciHelper 1 = 1
fibonacciHelper n = fibonacciHelper (n - 1) + fibonacciHelper (n - 2) 


fibonacci2 :: Integer -> Integer
fibonacci2 n | n > 1     = fibonacci2 (n - 1) + fibonacci2 (n - 2)
             | n < 0     = fibonacci2 (n + 2) - fibonacci2 (n + 1)
             | otherwise = n