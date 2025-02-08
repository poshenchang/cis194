module Week07.JoinList where

import Week07.Buffer
import Week07.Sized
import Week07.Scrabble
import Week07.Editor

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

instance Buffer (JoinList (Score, Size) String) where
    toString Empty          = ""
    toString (Single m a)   = a
    toString (Append m x y) = toString x ++ "\n" ++ toString y

    fromString = foldl (\x s -> x +++ scoreLine' s) Empty . lines
        where scoreLine' s = Single (scoreString s, 1) s
    
    line = indexJ

    replaceLine n s x = takeJ n x +++ fromString s +++ dropJ (n+1) x

    numLines = getSize . snd . tag

    value = getScore . fst . tag

-- Exercise 1

-- append function for JoinLists, where the monoidal annotaion of the
-- new JoinList is derived from those of the two arguments.
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
x +++ y = Append (tag x <> tag y) x y

-- returns the annotation at the root of a JoinList
tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m a)   = m
tag (Append m x y) = m

-- Exercise 2

-- finds the JoinList element at the specified index, assuming that
-- the annotation represents the size of each subtree
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ n _ | n < 0                         = Nothing
indexJ n x | n >= (getSize . size $ tag x) = Nothing
indexJ _ Empty                             = Nothing
indexJ n (Single m a)                      = Just a
indexJ n (Append m x y) | n < sx           = indexJ n x
                        | otherwise        = indexJ (n-sx) y
                        where sx = getSize . size $ tag x

-- drops the first n elements from a JoinList
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n x | n <= 0                        = x
dropJ n x | n >= (getSize . size $ tag x) = Empty
dropJ n (Append m x y) | n < sx           = dropJ n x +++ y
                       | otherwise        = dropJ (n-sx) y
                       where sx = getSize . size $ tag x
dropJ _ _ = Empty

-- takes the first n elements of a JoinList
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n x | n <= 0                        = Empty
takeJ n x | n >= (getSize . size $ tag x) = x
takeJ n (Append m x y) | n <= sx          = takeJ n x
                       | otherwise        = x +++ takeJ (n-sx) y
                       where sx = getSize . size $ tag x
takeJ _ _ = Empty

-- Exercise 3

-- test functions for JoinLists annotated with scores
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- Exercise 4

-- main function for running the editor with JoinList as Buffer
main = runEditor editor (fromString test :: (JoinList (Score, Size) String))
    where test = unlines [ "This buffer is for notes you don't want to save, and for"
                         , "evaluation of steam valve coefficients."
                         , "To load a different file, type the character L followed"
                         , "by the name of the file."
                         ]