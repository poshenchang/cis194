module Week01.Hanoi where

-- Exercise 5

-- Solves the Towers of Hanoi recursively:
-- 1. move n-1 discs from a to c using b as temporary storage
-- 2. move the top disc from a to b
-- 3. move n-1 discs from c to b using a as temporary storage
-- hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n > 0     = hanoi (n-1) a c b ++ ((a, b) : hanoi (n-1) c b a)
  | otherwise = []

-- Exercise 6

-- Solves the 4-pegs-version Towers of Hanoi recursively:
-- 1. let S = 1 + ... + k be the largest triangular number not greater than n
-- 2. move n-k discs from a to c using b and d as temporary storage
-- 3. move k discs from a to b using d as temporary storage
-- 4. move n-k discs from c to b using a and d as temporary storage
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n a b c d
  | n > 0 = hanoi4 (n-k) a c b d ++ hanoi k a b d ++ hanoi4 (n-k) c b a d
  | otherwise = []
    where 
        k = floor $ ((sqrt . fromIntegral $ 8*n+1) - 1) / 2
-- k = floor((sqrt(8*c + 1) - 1) / 2)