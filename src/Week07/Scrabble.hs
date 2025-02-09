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
score c | l `elem` "aeilorstu"
          = Score 1
        | l `elem` "dg"
          = Score 2
        | l `elem` "bcmp"
          = Score 3
        | l `elem` "fhvwy"
          = Score 4
        | l `elem` "k"
          = Score 5
        | l `elem` "jx"
          = Score 8
        | l `elem` "qz"
          = Score 10
        | otherwise = Score 0
        where l = toLower c

scoreString :: String -> Score
scoreString = mconcat . map score

getScore :: Score -> Int
getScore (Score x) = x