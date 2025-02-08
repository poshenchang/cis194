module Week04.Main where

import Data.List((\\))

-- Exercise 1

-- fun1 :: [Integer] -> Integer
-- fun1 [] = 1
-- fun1 (x:xs)
--   | even x = (x - 2) * fun1 xs
--   | otherwise = fun1 xs
fun1 :: [Integer] -> Integer
fun1 = product . map (-2 +) . filter even

-- fun2 :: Integer -> Integer
-- fun2 1 = 0
-- fun2 n | even n = n + fun2 (n ‘div‘ 2)
--        | otherwise = fun2 (3 * n + 1)
fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (/= 1)
       . iterate (\n -> if even n then n `div` 2 else 3*n+1)

-- Exercise 2

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- generates a balanced binary tree from a list of values
-- foldTree "ABCDEFGHIJ" ==
--   Node 3
--     (Node 2
--       (Node 0 Leaf 'F' Leaf)
--       'I'
--       (Node 1 (Node 0 Leaf 'B' Leaf) 'C' Leaf))
--     'J'
--     (Node 2
--       (Node 1 (Node 0 Leaf 'A' Leaf) 'G' Leaf)
--       'H'
--       (Node 1 (Node 0 Leaf 'D' Leaf) 'E' Leaf))
foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

-- insert nodes into the binary tree balancedly
insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node d left y right)
  | depth left < depth right = nt (insert x left) y right
  | otherwise                = nt left y (insert x right)
    where nt l x r = Node (max (depth l) (depth r) + 1) l x r

-- outputs the depth of a given Tree
-- Leaves have depth -1
depth :: Tree a -> Integer
depth (Node d _ _ _) = d
depth Leaf = -1

-- Exercise 3

-- returns True if and only if there are an odd number of True values
-- contained in the input list. 
xor :: [Bool] -> Bool
xor = foldr (\x y -> (x || y) && not (x && y)) False

-- implement map with foldr.
-- the middle function is of type a -> [b] -> [b], and we can see that
-- ((:) . f) x ys = (:) (f x) ys = (f x) : ys
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

-- implement foldl with foldr.
-- foldr :: (a -> b -> b) -> b -> [a] -> b
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f x = foldr (flip f) x . reverse

-- Exercise 4

-- given an integer n, generates all odd prime numbers up to 2n + 2
-- using the sieve of Sundaram.
-- sieveSundaram :: Integer -> [Integer]
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1).(*2)) $ [1..n] \\
                  filter (<= n) (map f (cartProd [1..n] [1..n]))
    where f (x, y) = x + y + 2*x*y

-- computes the Cartesian product of two lists
-- cartProd [1,2] [’a’,’b’] == [(1,’a’),(1,’b’),(2,’a’),(2,’b’)]
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]