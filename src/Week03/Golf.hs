module Week03.Golf where

-- Exercise 1

-- The output of skips is a list of lists.
-- The nth list in the output should contain
-- every nth element from the input list.
-- skips "ABCD" == ["ABCD", "BD", "C", "D"]
-- skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
-- skips [1] == [[1]]
-- skips [True,False] == [[True,False], [False]]
-- skips [] == []
-- implemented by list comprehension
skips :: [a] -> [[a]]
skips xs = [everyK 1 i xs | i <- [1..toInteger(length xs)]]

-- outputs list obtained by picking every k-th element
-- implements by keeping a counter, initial value is also an argument
everyK :: Integer -> Integer -> [a] -> [a]
everyK _ _ [] = []
everyK n k (x:xs) = y $ everyK (n+1) k xs
    where y | n `mod` k == 0 = (x:)
            | otherwise      = id

-- Exercise 2

-- A local maximum of a list is an element of the list which is strictly
-- greater than both the elements immediately before and after it
-- filters all the local maxima in the input list
-- localMaxima [2,9,5,6,1] == [9,6]
-- localMaxima [2,3,4,1,5] == [4]
-- localMaxima [1,2,3,4,5] == []
localMaxima :: [Integer] -> [Integer]
localMaxima (x:ys@(y:z:zs)) = f $ localMaxima ys
    where f | y > z && y > x = (y:)
            | otherwise      = id
localMaxima _ = []

-- Exercise 3

-- takes as input a list of Integers between 0 and 9, 
-- and outputs a vertical histogram showing how many of each number
-- were in the input list.
-- 
-- histogram [1,1,1,5] ==
-- *
-- *
-- * *
-- ==========
-- 
-- 0123456789
-- histogram [1,4,5,4,6,6,3,4,2,4,9] ==
-- *
-- *
-- * *
-- ****** *
-- ==========
-- 0123456789
histogram :: [Integer] -> String
histogram xs = unlines [util i d | i <- [m,m-1..1]]
               ++ "==========\n0123456789\n"
    where d = [count i xs | i <- [0..9]]
          m = maximum d

-- utility for getting the histogram at height n
util :: Int -> [Int] -> String
util n = map f
    where f m | m >= n    = '*'
              | otherwise = ' '

-- counts the number of instances of a given input in a list
count :: (Eq a) => a -> [a] -> Int
count x y = length $ filter (== x) y