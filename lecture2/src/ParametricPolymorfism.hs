module Poly where

-- id :: p -> p
-- id x = x -- is parametric Polymorphism - same function regarding of type
-- (id id) 4

--  let k x y = x

import Data.Function (on)
getSecondFrom :: t -> t1 -> t2 -> t1
getSecondFrom a b c = b 

f :: a -> a -> b -> a -> a
f x x1 y x2 = undefined


mono :: Char -> Char
mono x = x

semiMono :: Char -> a -> Char
--semiMono :: p1 -> p2 -> p1 - haskell tries to get the most wide type
-- algotithm 
semiMono x y = x


-- HOF
apply2 :: (t -> t) -> t -> t
apply2 f x = f (f x)
-- apply2 (+5) 22
-- apply2 (++ "AB") "CD"

-- standard library
-- flip f y x = f x y
-- standard library
-- on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
-- on op f x y = f x `op` f y

sumSquares = (+) `on` (^2)

multSecond = g `on` h

g = (*)
h = snd
-- h (_, x) = x

-- 2 * x + 7
-- let f x = 2 * x + 7
-- lambda function
-- \x -> 2 * x + 7
-- \x -> 2 * x + 7


-- lenVec x y = sqrt $ x ^ 2 + y ^ 2
-- lenVec x = \y -> sqrt $ x ^ 2 + y ^ 2
-- lenVec = \x -> \y -> sqrt $ x ^ 2 + y ^ 2
-- lenVec = \x y -> sqrt $ x ^ 2 + y ^ 2

-- let p1 = ((1,2), (3,4))
-- let p2 = ((5,6), (7,8))
-- fst $ fst p1

sumFstFst = (+) `on` helper
    where helper pp = fst $fst pp

sumFstFst' :: ((Integer, b1), b2) -> ((Integer, b1), b2) -> Integer
sumFstFst' = (+) `on` \pp -> fst $fst pp

on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)

-- let sum3squares = (\x y z -> x+y+z) `on3` (^2)

-- on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
-- on op f x y = f x `op` f y

--  Просто применение функций в Haskell (и в lambda calculus вообще) левоассоциативно: f x f y f z эквивалентно ((((f x) f) y)  f) z, при право-ассоциативности было бы f (x (f (y (f z))), но никак не op (f x) (f y) (f z). Правила ассоциативности и служат для вывода типа выражения, можно написать просто
-- on3 op f x y z = op (f x) (f y) (f z) и тип функции будет автоматически выведен как (t1 -> t1 -> t1 -> t2) -> (t3 -> t1) -> t3 -> t3 -> t3 -> t2

-- То есть, в решении можно опустить явное определение типа on3 которое там вставлено по умолчанию.