module Streams
where
import Prelude hiding (head, tail, repeat, map, zip, take, sum)
-- If you happen to need any of these hidden functions just write
-- Prelude.head, Prelude.tail, etc.


data Stream elem  =  Cons { head :: elem, tail :: Stream elem }
-- Note: this notation creates a constructor
--   Cons :: elem -> Stream elem -> Stream elem
-- and the two functions
--   head :: Stream elem -> elem
--   tail :: Stream elem -> Stream elem


-- Since we do not want to write Cons in prefix notation all the time,
-- let us introduce the infix operator .:
infixr 5 .:
(.:) :: elem -> Stream elem -> Stream elem
a .: s = Cons a s

-- Example usage:
from :: Integer -> Stream Integer
from n = n .: from (n + 1)

-- You probably want to see streams on the GHCi console,
-- so here is an instance for Show that returns the first 16 elements:
limit :: Integer
limit = 16 -- you can adjust this constant when necessary
instance Show elem => Show (Stream elem) where
    showsPrec _ s = showString "⟨" . h limit s where
        h 0 _ = showString "...⟩"
        h n t = shows (head t) . showString ", " . h (n-1) (tail t)


---------------------------------------------------------------------
-- a)
repeat :: a -> Stream a
repeat x  = x .: (repeat x)

map :: (a -> b) -> (Stream a -> Stream b)
map f (Cons x xs) = f x .: (map f xs)


zip :: (a -> b -> c) -> (Stream a -> Stream b -> Stream c)
zip f (Cons x xs) (Cons y ys) = f x y .: zip f xs ys


---------------------------------------------------------------------
-- b)
-- As a small service, here are all functions you need to implement
-- for the Num typeclass. Replace 'undefined' with something useful.

instance (Ord elem, Num elem) => Num (Stream elem) where
    (+) (Cons x xs) (Cons y ys)        = (x + y) .: ((+)xs ys)
    (-) (Cons x xs) (Cons y ys)        = (x - y) .: ((-)xs ys)
    (*) (Cons x xs) (Cons y ys)        = (x * y) .: ((*)xs ys)
    negate (Cons x xs)     = (0 - x) .: negate xs
    abs (Cons x xs) | (x > 0) = x .: abs xs
                    |otherwise = (0-x) .: abs xs
    signum (Cons x xs) | (x < 0) = -1 .: signum xs
                       | (x == 0) = 0 .: signum xs
                       | otherwise   = 1 .: signum xs
    fromInteger n = fromInteger n .: fromInteger n


nat, fib :: Stream Integer
nat = 0 .: nat + 1
fib = 0 .: 1 .: fib + tail fib


---------------------------------------------------------------------
-- c)
take :: Integer -> Stream elem -> [elem]
take n _      | n <= 0 =  []
take n (Cons x xs)          =  x  : take (n-1) xs


---------------------------------------------------------------------
-- d)

-- The given function:
diff :: (Ord elem, Num elem) => Stream elem -> Stream elem
diff s = tail s - s

-- The function you should implement:
sum :: (Ord elem, Num elem) => Stream elem -> Stream elem
sum s = 0 .: sum s + s

-- Specification: diff (sum s) = s and head (sum s) = 0

-- Don't forget to try out your functions, there are no test cases this time!
