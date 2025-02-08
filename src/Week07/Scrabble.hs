module Week07.Scrabble where

import Data.Monoid
import Data.Char

-- Exercise 3

newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)

instance Semigroup Score where
    (<>) = (+)

instance Monoid Score where
    mempty = Score 0
    mappend = (<>)

score :: Char -> Score
score c | l `elem` ['a', 'e', 'i', 'l', 'o', 'r', 's', 't', 'u']
          = Score 1
        | l == 'd' || l == 'g'
          = Score 2
        | l `elem` ['b', 'c', 'm', 'p']
          = Score 3
        | l `elem` ['f', 'h', 'v', 'w', 'y']
          = Score 4
        | l == 'k'
          = Score 5
        | l == 'j' || l == 'x'
          = Score 8
        | l == 'q' || l == 'z'
          = Score 10
        | otherwise = Score 0
        where l = toLower c

scoreString :: String -> Score
scoreString = mconcat . map score

getScore :: Score -> Int
getScore (Score x) = x