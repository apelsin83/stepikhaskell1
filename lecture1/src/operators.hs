module Demo where

{-
max 6 7
6 + 7

6 `max` 7

(+) 6 7

- 7
(-) 5 3

max (-5) 5

-- priority 1 .. 10
-- function has priority 10 It runs first
-- (-) is left associative operator for it infixl
--  right associative operator for it infixr
-- not associative infix
-- example: 
-- infixr 8 ^, `logBase`   -- here 8 is priority
-- infixl 7 *, /, `div`, `mod`
-- infix 4 ==, /=, >, >=, <, <=x


Symbols to define operators of your own

! # $ & * + . / < = > ? @ \ ^ | - ~
-}

infixl 6 *+*

a *+* b = a ^ 2 + b ^ 2

-- (*+*) a b = a ^ 2 + b ^ 2

infixl 6 |-|

(|-|) x y = if x > y then x - y else y - x

-- partial (/) 2  could be (2 /)


---logBase 4 (min 20 (9 + 7))  equivalent to  ----- logBase 4 $ min 20 $ 9 + 7  $-sets priority to 0 