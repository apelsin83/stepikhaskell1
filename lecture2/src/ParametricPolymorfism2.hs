module Poly2 where
import Data.Function (on)
--Composition
--f :: b -> c
--g :: a -> b
--x :: a
-- \x f (g x) :: a -> c

-- compose :: (t1 -> t2) -> (t3 -> t1) -> t3 -> t2
-- compose f g = \x -> f (g x)
-- in haskell (.)

sumFstFst' = (+) `on` \pp -> fst $fst pp


sumFstFst'' = (+) `on` (fst . fst)

{-|
doIt x = f ( g (h x))
doIt x = f . g . h
-}


doItYourself = f . g . h

f = logBase 2

g = (^ 3)

h = max 42

-- f = \x -> logBase 2 x

-- g = \x -> x ^ 3

-- h = \x -> if x > 42 then x else 42 

-- lists :t []   :t (++)  :t (:)

-- tuples
-- (True, 3) mixfix style or  (,) True 3
-- (,) :: a -> b -> (a, b)

-- let dup x = (x, x)
-- :t dup   t -> (t, t)

-- currying
{- 
fst (1,2)
:t on
:t fst `on` (^2)
error

but 
:t curry fst `on` (^2)
-}

{-
avg :: (Double, Double) -> Double
avg p = (fst p )
-}

myCurry :: ((a, b) -> t) -> a -> b -> t
myCurry f x y = f (x, y)

myUncurry :: (t1 -> t2 -> t3) -> (t1, t2) -> t3
myUncurry f (x, y) = f x y

-- mx ::  (a -> b -> a)
{-
:t const a -> b -> a
:t flip (a -> b -> c) -> b -> a -> c

:t uncurry (flip const) ->>> (b, c) -> c
-}


mySwap = f' ( g' h')
-- левоассоциативно: f x f y f z эквивалентно ((((f x) f) y)  f) z,


f' :: (a -> b -> c) -> (a, b) -> c
f' = uncurry
g' :: (a -> b -> c) -> b -> a -> c
g' = flip
h' :: a -> b -> (a, b)
h' = (,)

{-
h is a -> b -> (a, b)
now 
g is (a -> b -> c) -> b -> a -> c

g h is


f is (a -> b -> c) -> (a, b) -> c

-}


-- curry uncurry flip (,) const
--
-- curry :: ((a, b) -> c) -> a -> b -> c
-- uncurry :: (a -> b -> c) -> (a, b) -> c
-- const :: a -> b -> a
-- flip (a -> b -> c) -> b -> a -> c
-- (,) :: a -> b -> (a, b)


-- swap (1,'A')