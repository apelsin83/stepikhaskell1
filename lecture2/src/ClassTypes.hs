module Types where

-- :t (+) :: Num a => a -> a -> a
-- here Num a => is interface limitation

{-

:t (>)
(>) :: Ord a => a -> a -> Bool

:t (> 7)
(> 7) :: (Ord a, Num a) => a -> Bool


:t (> (7,3))
(> (7,3)) :: (Ord a, Ord b, Num a, Num b) => (a, b) -> Bool
-}

class Eq1 a where
    (==), (/=) :: a -> a-> Bool
    x /= y = not (x Types.== y)
    -- (==) :: a -> a-> Bool
    -- (/=) :: a -> a-> Bool
    x == y = not (x Types./= y) -- cycle we can override in instances




-- :t (==)
-- (==) :: Eq a => a -> a -> Bool
-- :t (== 42)
-- (== 42) :: (Eq a, Num a) => a -> Bool
-- :t (== 'c')
-- (== 'c') :: Char -> Bool
-- :t elem
-- elem :: (Foldable t, Eq a) => a -> t a -> Bool

instance Eq1 Bool where
    True  == True = True
    False == False = False
    _     == _     = False
    -- can override
    -- x /= y = not (x Types.== y)

class Printable a where
    toString :: a -> [Char]

instance Printable Bool where
    toString a = if a then "true" else "false"

instance Printable () where
    toString a = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
    toString (a, b) = "(" ++ toString a ++ "," ++ toString b ++ ")"

instance (Eq1 a, Eq1 b) => Eq1 (a, b) where
    p1 == p2 = fst p1 Types.== fst p2 && snd p1 Types.== snd p2