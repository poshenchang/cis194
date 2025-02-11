{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Week12.Risk where

import Control.Monad.Random
import Control.Applicative
import Control.Monad
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Eq, Show)

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = battleUtil bf <$> multiDie (maxAtk bf) <*> multiDie (maxDef bf)

-- generates a list of n random die values
multiDie :: Army -> Rand StdGen [DieValue]
multiDie n = replicateM n die

-- return maximum allowed units to attack with
maxAtk :: Battlefield -> Army
maxAtk bf = min (attackers bf - 1) 3

-- return maximum allowed units to defend with
maxDef :: Battlefield -> Army
maxDef bf = min (defenders bf) 2

-- sort list by descending order
descSort :: Ord a => [a] -> [a]
descSort = sortBy (comparing Down)

-- return the number of units that died in battle given determinstic 
-- die values of attack and defense army (in that order) respectively
battleUtil :: Battlefield -> [DieValue] -> [DieValue] -> Battlefield
battleUtil bf xs ys = foldr f bf $ zip (descSort xs) (descSort ys)
    where f (x, y) (Battlefield a d) | x > y     = Battlefield a (d-1)
                                     | otherwise = Battlefield (a-1) d

-- simulate entire invasion attempt, that is, repeated battle until either
-- 1. no defenders remain, or
-- 2. fewer than two attackers remain.
invade :: Battlefield -> Rand StdGen Battlefield
invade bf | attackers bf <= 1 || defenders bf <= 0 = return bf
          | otherwise                              = battle bf >>= invade

-- simulation count for estimating successProb
simCount :: Int
simCount = 1000

-- runs invade for simCount times, and use the results to compute the
-- estimated probability that the attacking army will completely destroy
-- the defending army.
successProb :: Battlefield -> Rand StdGen Double
successProb bf = avg . map f <$> replicateM simCount (invade bf)
    where f (Battlefield a d) | d <= 0    = 1
                              | otherwise = 0

-- returns average of a list of Double
avg :: [Double] -> Double
avg xs = sum xs / fromIntegral (length xs)