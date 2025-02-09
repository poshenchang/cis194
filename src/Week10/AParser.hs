{- CIS 194 HW 10
   due Monday, 1 April
-}

module Week10.AParser where

import Control.Applicative

import Data.Char
import Control.Monad (void)

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- Exercise 1

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

-- if the Parser does not fail, the given function is then applied to the result
instance Functor Parser where
    fmap f (Parser g) = Parser $ fmap (first f) . g

-- Exercise 2


instance Applicative Parser where
    -- represents the parser consuming no input and always returns x successfully
    pure x = Parser (\s -> Just (x, s))
    -- the parser first runs p1, then passes the remaining input to p2,
    -- then returns the result of applying the function to the value.
    -- If either p1 or p2 fails, then the whole thing should also fail.
    Parser f1 <*> Parser f2 = Parser g
        where g s = case f1 s of
                        Nothing      -> Nothing
                        Just (h, s') -> first h <$> f2 s'

-- Exercise 3

-- expects to see the characters 'a' and 'b' right after.
abParser :: Parser (Char, Char)
abParser = (, ) <$> char 'a' <*> char 'b'

-- functions like abParser, except that it returns ().
abParser_ :: Parser ()
abParser_ = const () <$> abParser
-- alternatively, use void :: Functor f => f a -> f ()

-- reads two integer values separated by a space and returns the integer
-- values in a list.
intPair :: Parser [Integer]
intPair = (\x _ y -> [x, y]) <$> posInt <*> char ' ' <*> posInt

-- Exercise 4

instance Alternative Parser where
    -- represents the parser which always fails
    empty = Parser (const Nothing)
    -- represents the parser which first tries running p1, returns on success, 
    -- otherwise goes on and tries p2.
    Parser f <|> Parser g = Parser (\s -> f s <|> g s)

-- Exercise 5

-- parses either an integer value or an uppercase character, and fails otherwise
intOrUppercase :: Parser ()
intOrUppercase = void posInt <|> void (satisfy isUpper)