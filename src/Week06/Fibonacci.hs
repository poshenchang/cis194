{-# LANGUAGE FlexibleInstances #-}

module Week06.Fibonacci where

-- Exercise 1

-- define Fibonacci numbers by direct recursion
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- infinite list of Fibonacci's sequence using fib
fibs1 :: [Integer]
fibs1 = map fib [0..]
-- fibs1 = [fib i | i <- [0..]]

-- Exercise 2

-- O(n) implementation of infinite Fibonacci's sequence
fibs2 :: [Integer]
fibs2 = map fst $ iterate (\(x, y) -> (y, x+y)) (0, 1)

-- Exercise 3

-- new data type of polymorphic streams, representing lists 
-- that must be infinite.
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
    show = ("[" ++) . util 20
        where util n (Cons x xs)
                | n == 0    = "...]"
                | otherwise = show x ++ "," ++ util (n-1) xs

-- Exercise 4

-- generates a stream containing infinitely many copies of the 
-- given element
streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

-- applies function to every element of a Stream
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

-- generates Stream from with an initial value, then repeatedly apply
-- the given function.
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- Exercise 5

-- Stream Integer of natural numbers
nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- Stream Integer of ruler function
-- implemented recursively by interleaving Streams
ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap (+1) ruler)

-- alternates the elements from two streams
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

-- Exercise 6

-- implement Stream Integer as an instance of class Num, with operations
-- resembling that of generating functions.
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger n = Cons n (streamRepeat 0)
    negate (Cons x xs) = Cons (-x) (negate xs)
    Cons x xs + Cons y ys = Cons (x+y) (xs + ys)
    Cons x xs * ys'@(Cons y ys)
        = Cons (x*y) (streamMap (*x) ys + xs*ys')

instance Fractional (Stream Integer) where
    Cons x xs / Cons y ys
        = Cons (x `div` y) (streamMap (`div` y) (xs - q*ys))
        where q = Cons x xs / Cons y ys

-- generating function for Fibonacci's sequence
fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- Exercise 7

newtype Matrix = Matrix ((Integer, Integer), (Integer, Integer))
    deriving (Show, Eq)
type Vector = (Integer, Integer)

instance Num Matrix where
    fromInteger n = Matrix ((n, 0), (0, n))
    Matrix ((a1, b1), (c1, d1)) + Matrix ((a2, b2), (c2, d2))
        = Matrix ((a1+a2, b1+b2), (c1+c2, d1+d2))
    Matrix ((a1, b1), (c1, d1)) * Matrix ((a2, b2), (c2, d2))
        = Matrix ((a1*a2+b1*c2, a1*b2+b1*d2), (c1*a2+d1*c2, c1*b2+d1*d2))
    negate (Matrix ((a, b), (c, d))) = Matrix ((-a, -b), (-c, -d))

fib4 :: Integer -> Integer
fib4 = (\(Matrix ((_, x), (_, _))) -> x) . (f^)
    where f = Matrix ((1, 1), (1, 0))