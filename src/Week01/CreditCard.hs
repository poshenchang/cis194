module Week01.CreditCard where

-- Exercise 1

-- Converts positive Integers to a list of digits.
-- For 0 or negative inputs, toDigits should return the empty list.
-- toDigits 1234 == [1,2,3,4]
-- toDigits 0 == []
-- toDigits (-17) == []
toDigits :: Integer -> [Integer]
toDigits n
  | n > 0     = toDigits (n `div` 10) ++ [n `mod` 10]
  | otherwise = []

-- Converts positive Integers to a reversed list of digits.
-- toDigits 1234 == [4,3,2,1]
toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

-- Exercise 2

-- Double every other number beginning from the right.
-- doubleEveryOther [8, 7, 6, 5] == [16, 7, 12, 5]
-- doubleEveryOther [1, 2, 3] == [1, 4, 3]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = case reverse xs of
  x : y : zs -> doubleEveryOther (reverse zs) ++ [2*y, x]
  _          -> xs

-- Exercise 3

-- Calculate the sum of all digits of all Integers in a list.
-- sumDigits [16, 7, 12, 5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
sumDigits :: [Integer] -> Integer
sumDigits (x : ys) = sum (toDigits x) + sumDigits ys
sumDigits []       = 0
-- Alternative solution:
-- sumDigits = foldr ((+) . sum . toDigits) 0
--   foldr :: (a -> b -> b) -> b -> [a] -> b
--   takes b and last item of [a] and applies the function, 
--   then takes the result and the second-to-last item of [a]
--   and applies the function, and so on.

-- Exercise 4

-- Indicates whether an Integer could be a valid credit card number.
-- validate 4012888888881881 = True
-- validate 4012888888881882 = False
validate :: Integer -> Bool
validate n = (sumDigits . doubleEveryOther . toDigits $ n) `mod` 10 == 0