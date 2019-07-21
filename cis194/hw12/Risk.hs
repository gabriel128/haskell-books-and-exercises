{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List

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

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving Show

die2 :: (RandomGen g) => Rand g Int
die2 = getRandomR (1, 6)

-- Ex 2 --

battle :: Battlefield -> Rand StdGen Battlefield
battle b = do
    let attackersQty = attackers b
    let defendersQty = defenders b
    as <- replicateM attackersQty die
    bs <- replicateM defendersQty die
    let sortedAdices = take defendersQty . sort $ as
    let sortedBdices = sort bs
    let results = zipWith (>) sortedAdices sortedBdices
    return (Battlefield (length . filter (==True) $ results) (length . filter (==False) $ results))


-- Ex 3 --
invade :: Battlefield -> Rand StdGen Battlefield
invade b = battle b >>= invasion
  where
    invasion bfld@(Battlefield as bs)
      | bs == 0 = return bfld
      | as < 2 = return bfld
      | otherwise = battle bfld >>= invasion

-- Ex 4 --
successProb :: Battlefield -> Rand StdGen Double
successProb b = do
  battles <- replicateM 1000 (invade b)
  let winCount =  fromIntegral . length . filter (\ b' -> defenders b' == 0) $ battles
  return (winCount / 1000)
