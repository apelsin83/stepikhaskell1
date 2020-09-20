module Standard where

{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}
-- class Eq1 a where
--     (==), (/=) :: a -> a -> Bool
--     x /= y = not (x Standard.== y)
--     x == y = not (x Standard./= y)


-- -- extension class type
-- class (Eq1 a) => Ord1 a where
--     (<=), (>=), (<), (>) :: a -> a -> Bool
--     max, min :: a -> a -> a
--     compare :: a -> a -> Ordering




{- Ord Minimal complete definition: either compare or <= -}

-- miltiple extension

-- class (Eq1 a, Printable a) => MyClass where

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a

    stompOrStab a | doesEnrageGork a && doesEnrageMork a = (stomp . stab) a
                  | doesEnrageGork a = stab a
                  | doesEnrageMork a = stomp a
                  | otherwise = a

{-

        stompOrStab a = 
        let 
            stompStab (True, True) = stomp . stab
            stompStab (True, _   ) = stomp
            stompStab (_   , True) = stab
            stompStab _            = id
        in stompStab (doesEnrageMork a, doesEnrageGork a) a


        stompOrStab a = (g . f) a
                   where
                   f = if doesEnrageGork a then stab else id
                   g = if doesEnrageMork a then stomp else id

        stompOrStab a = f doesEnrageMork stomp $ f doesEnrageGork stab a
        where f p g = if p a then g else id

-}

{-
type Show
show

show :: Show a => a -> String

show 5
"5"

:t read
read :: Read a => String -> a

if response polymorph we need add type
read "5" :: Int
5

read "[1,2]" :: [Double]
[1.0,2.0]

read "5 rings" :: Int
*** Exception: Prelude.read: no parse

reads "5 rings" :: [(Int,String)]
[(5," rings")]

reads "c5 rings" :: [(Int,String)]
[]

-}

{-
Имея функцию ip = show a ++ show b ++ show c ++ show d определите значения a, b, c, d так, чтобы добиться следующего поведения:
GHCi> ip
"127.224.120.12"
-}

a = 127.2
b = 2
c = 4.12
d = 0.12
ip = show a ++ show b ++ show c ++ show d

{-


newtype AddDotAtEnd = AddDotAtEnd Int

instance Show AddDotAtEnd where
  show (AddDotAtEnd s) = show s ++ "."

a = AddDotAtEnd 127
b = AddDotAtEnd 224
c = AddDotAtEnd 120
d = 12



data Signal = Red | Yellow | Green
	deriving (Eq)
instance Show Signal where
	show Yellow = "127."
	show Red = "224."
	show Green = "120."
a = Yellow
b = Red
c = Green
d = 12





data Helper = Helper String
instance Show Helper where
    show (Helper x) = x

a = Helper "127."
b = Helper "224."
c = Helper "120."
d = Helper "12"


-}

class Enum1 a where 
    succ1, pred1 :: a -> a
    toEnum1 :: Int -> a
    fromEnum1 :: a -> Int

{-

fro Enum class
succ 4
5

pred 4
4

pred 'c'
'b'

fromEnum 'z'
122

succ True
*** Exception: Prelude.Enum.Bool.succ: bad argument

toEnum 122 :: Int
122
toEnum 122 :: Char
'z'
-}


class Bounded1 a where
    minBound1, maxBound1:: a


{-
minBound :: Bool
False
minBound :: Int
-9223372036854775808
-}

class (Bounded a, Eq a, Enum a) => SafeEnum a where
  ssucc :: a -> a
  ssucc a
    | maxBound == a = minBound
    | otherwise = succ a

  spred :: a -> a
  spred a
    | minBound == a = maxBound
    | otherwise = pred a

{-
instance SafeEnum Int
ssucc 5 :: Int
-}

-- class Num1 a where 
--     (+), (-), (*) :: a -> a -> a
--     negate :: a -> a
--     signum :: a -> a
--     abs :: a -> a
--     fromInteger :: Integer -> a

--     x - y = x + negate y
--     negate x = 0 - x

{-
LAW abs x * signum x == x
sub classes Real -> Integral and Fractional


:i Integral
class (Real a, Enum a) => Integral a where
  quot :: a -> a -> a
  rem :: a -> a -> a
  div :: a -> a -> a
  mod :: a -> a -> a
  quotRem :: a -> a -> (a, a)
  divMod :: a -> a -> (a, a)
  toInteger :: a -> Integer
  {-# MINIMAL quotRem, toInteger #-}
        -- Defined in ‘GHC.Real’
instance Integral Word -- Defined in ‘GHC.Real’
instance Integral Integer -- Defined in ‘GHC.Real’
instance Integral Int -- Defined in ‘GHC.Real’


:i Fractional
class Num a => Fractional a where
  (/) :: a -> a -> a
  recip :: a -> a
  fromRational :: Rational -> a
  {-# MINIMAL fromRational, (recip | (/)) #-}
        -- Defined in ‘GHC.Real’
instance Fractional Float -- Defined in ‘GHC.Float’
instance Fractional Double -- Defined in ‘GHC.Float’



class Fractional a => Floating a where
  pi :: a
  exp :: a -> a
  log :: a -> a
  sqrt :: a -> a
  (**) :: a -> a -> a
  logBase :: a -> a -> a
  sin :: a -> a
  cos :: a -> a
  tan :: a -> a
  asin :: a -> a
  acos :: a -> a
  atan :: a -> a
  sinh :: a -> a
  cosh :: a -> a
  tanh :: a -> a
  asinh :: a -> a
  acosh :: a -> a
  atanh :: a -> a
  GHC.Float.log1p :: a -> a
  GHC.Float.expm1 :: a -> a
  GHC.Float.log1pexp :: a -> a
  GHC.Float.log1mexp :: a -> a
  {-# MINIMAL pi, exp, log, sin, cos, asin, acos, atan, sinh, cosh,
              asinh, acosh, atanh #-}
        -- Defined in ‘GHC.Float’
instance Floating Float -- Defined in ‘GHC.Float’
instance Floating Double -- Defined in ‘GHC.Float’


:i RealFrac
class (Real a, Fractional a) => RealFrac a where
  properFraction :: Integral b => a -> (b, a)
  truncate :: Integral b => a -> b
  round :: Integral b => a -> b
  ceiling :: Integral b => a -> b
  floor :: Integral b => a -> b
  {-# MINIMAL properFraction #-}
        -- Defined in ‘GHC.Real’
instance RealFrac Float -- Defined in ‘GHC.Float’
instance RealFrac Double -- Defined in ‘GHC.Float’


:i RealFloat
class (RealFrac a, Floating a) => RealFloat a where
  floatRadix :: a -> Integer
  floatDigits :: a -> Int
  floatRange :: a -> (Int, Int)
  decodeFloat :: a -> (Integer, Int)
  encodeFloat :: Integer -> Int -> a
  exponent :: a -> Int
  significand :: a -> a
  scaleFloat :: Int -> a -> a
  isNaN :: a -> Bool
  isInfinite :: a -> Bool
  isDenormalized :: a -> Bool
  isNegativeZero :: a -> Bool
  isIEEE :: a -> Bool
  atan2 :: a -> a -> a
  {-# MINIMAL floatRadix, floatDigits, floatRange, decodeFloat,
              encodeFloat, isNaN, isInfinite, isDenormalized, isNegativeZero,
              isIEEE #-}
        -- Defined in ‘GHC.Float’
instance RealFloat Float -- Defined in ‘GHC.Float’
instance RealFloat Double -- Defined in ‘GHC.Float’
-}



avg :: Int -> Int -> Int -> Double
avg a b c = fromInteger (toInteger a + toInteger b + toInteger c) / 3