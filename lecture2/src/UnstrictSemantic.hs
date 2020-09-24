module Unstrict where

sumIt :: Int -> Int -> Int
sumIt x y = x + y


{- 
let x = 2 + 3
in haskell lazy stratagy
sumIt (2+3) 4
-> (2+3) + 4
-> 5 + 4
-> 9
redex - what we can simlify

also exists proactive strategy

sumIt (2+3) 4
-> sumIt 5 4
-> 5 + 4
-> 9
-}

-- const $ const (4 + 5) $ max 42

add7 :: Int -> Int -> Int
add7 x y = x + 7

{-
add7 1 (2+3)
lazy
-> 1 + 7
-> 8


not lazy
-> add7 1 5
-> 1 + 7
-> 8
-}

dup :: Int -> (Int, Int)
dup x = (x, x)
{-
dup (2+3)
lazy
-> (2+3, 2+3)
-> (5 , 2+3)
-> (5, 5)

mechanizm razdelenia
dup (2+3)
lazy
-> (p, p)  p = 2+3
-> (5 , p) p = 5
-> (5, 5)

not lazy
-> dup 5
-> (5, 5)
-}


{-
Сколько шагов редукции потребуется, чтобы вычислить значение функции value, если используется ленивая стратегия вычислений с механизмом разделения?

bar x y z = x + y
foo a b = bar a a (a + b)
value = foo (3 * 10) (5 - 2)

Примечание. Подстановку тела функции value вместо value не считайте.


-}
const42 :: a -> Int
const42 = const 42

{-
const42 undefined
unstrict function esli est' rashodyacheesya znachenie no ono ne vychislyatsa i rezultat u nas zhacnenie
-}



-- Отметьте функции, которые не могут привести к расходимости ни на каком корректном наборе аргументов.

-- foo a = a

-- bar = const foo
-- -- (const foo) 1 2 = 
--     -- ((\x y -> x) foo) 1 2 = ((\x -> \y -> x) foo) 1 2 = (\y -> foo) 1 2 = foo 2 = 2

-- baz x = const True

-- quux = let x = x in x

-- corge = "Sorry, my value was changed"

-- grault x 0 = x
-- grault x y = x

-- garply = grault 'q'

-- waldo = foo

{-
noraml form when no redex in expression
weak head normal form
-}

{-
NF
42
(3,4)
\x -> x + 4

not NF
"Hello " ++ "world"
(\x -> x + 4) 5
(3, 1+5)

WHNF
3 types
1. Any lambda abstraction
\x -> x + 2 * 3 (inside lambda body)
2. Data constructor
(3, 1+5)
(,) (4*5)
3. partially aplied vstroennaya funkciya
(+) (2*7)
-}

{-

we use to prevent laziness and tells calculate value
:t seq
seq :: a -> b -> b
seq _|_ b = _|_
seq a b = b

_|_ rashodimost'

seq 1 2
2

seq undefined 2
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:80:14 in base:GHC.Err
  undefined, called at <interactive>:24:5 in interactive:Ghci2

seq (id undefined) 2
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:80:14 in base:GHC.Err
  undefined, called at <interactive>:25:9 in interactive:Ghci2

but seq not forces WHNF
seq (undefined, undefined) 2
-}



-- При вычислении каких из перечисленных ниже функций использование seq предотвратит нарастание количества невычисленных редексов при увеличении значения первого аргумента:

foo 0 x = x
foo n x = let x' = foo (n - 1) (x + 1)
          in x' `seq` x'

bar 0 f = f
bar x f = let f' = \a -> f (x + a)
              x' = x - 1
          in f' `seq` x' `seq` bar x' f'

baz 0 (x, y) = x + y
baz n (x, y) = let x' = x + 1
                   y' = y - 1
                   p  = (x', y')
                   n' = n - 1
               in p `seq` n' `seq` baz n' p

quux 0 (x, y) = x + y
quux n (x, y) = let x' = x + 1
                    y' = y - 1
                    p  = (x', y')
                    n' = n - 1
                in x' `seq` y' `seq` n' `seq` quux n' p


{-
sec analogue is call by value ($!)
($!) :: (a -> b) -> a -> b
f $! x = x `seq` f x


const 42 undefined
42

const 42 $! undefined
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:80:14 in base:GHC.Err
  undefined, ca
-}

factorial :: Integer -> Integer
factorial n | n >= 0 = helper 1 n
            | otherwise = error "arg must be >=0"
    where
        helper acc 0 = acc
        helper acc n = (helper $! (acc * n)) (n -1)

{-

Ниже определены функции mySum и goSum. Вызов goSum может выглядеть, к примеру, так:  goSum 15. Выберите верные утверждения, описывающие процесс вычисления подобного выражения.


-}

mySum :: (Eq t, Num t) => (t, ()) -> t -> (t, ())
mySum acc 0 = acc
mySum (result, ()) n = (mySum $! (result + n, ())) $ n - 1

goSum = mySum (0, ())