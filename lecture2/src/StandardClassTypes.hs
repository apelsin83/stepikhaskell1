module Standard where

class Eq1 a where
    (==), (/=) :: a -> a -> Bool
    x /= y = not (x Standard.== y)
    x == y = not (x Standard./= y)


-- extension class type
class (Eq1 a) => Ord1 a where
    (<=), (>=), (<), (>) :: a -> a -> Bool
    max, min :: a -> a -> a
    compare :: a -> a -> Ordering

{- Ord Minimal complete definition: either compare or <= -}

-- miltiple extension

-- class (Eq1 a, Printable a) => MyClass where