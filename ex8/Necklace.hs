{-# LANGUAGE RecursiveDo #-}
module Necklace where
import Data.IORef

type NecklaceRef elem = IORef (Necklace elem)
data Necklace elem = Cons elem (NecklaceRef elem)

example :: IO (NecklaceRef Integer)
example = mdo
    x <- newIORef (Cons 2 y)
    y <- newIORef (Cons 3 z)
    z <- newIORef (Cons 5 x)
    return z

example2 :: IO (NecklaceRef Integer)
example2 = mdo
    x <- newIORef (Cons 7 y)
    y <- newIORef (Cons 11 x)
    return y

toList :: NecklaceRef elem -> IO [elem]
toList last = do
    Cons _ first <- readIORef last
    helper first first where
        helper first p = do
            Cons x next <- readIORef p
            if next == first then do return [x]
            else do
                xs <- helper first next
                return (x:xs)


-- a)
rotate :: NecklaceRef elem -> IO (NecklaceRef elem)
rotate = undefined

-- Test your implementation in ghci:
-- example >>= rotate >>= toList (should be [3,5,2])
-- example >>= rotate >>= rotate >>= toList (should be [5,2,3])
-- example >>= rotate >>= rotate >>= rotate >>= toList (should be [2,3,5])


-- b)
append :: NecklaceRef elem -> NecklaceRef elem -> IO ()
append = undefined


-- Use this function to test your implementation in ghci:
-- appendCheck example example2 (should be ([7,11,2,3,5],[2,3,5,7,11]))
appendCheck :: IO (NecklaceRef elem) -> IO (NecklaceRef elem) -> IO ([elem], [elem])
appendCheck x y = do
    a <- x
    b <- y
    append a b
    xs <- toList a
    ys <- toList b
    return (xs, ys)
