module Scrabble where

import Data.Char
import Data.Monoid

newtype Score = Score Integer deriving (Show, Eq, Ord)

instance Monoid Score where
  mempty = Score 0
  (Score a) `mappend` (Score b) = Score (a + b)

-- 2 points: D ×4, G ×3
-- 3 points: B ×2, C ×2, M ×2, P ×2
-- 4 points: F ×2, H ×2, V ×2, W ×2, Y ×2
-- 5 points: K ×1
-- 8 points: J ×1, X ×1
-- 10 points: Q ×1, Z ×1
score :: Char -> Score
score a
  | toLower a `elem` ['e', 'a', 'i', 'o', 'n', 'r', 't', 'l', 's', 'u'] = Score 1
  | toLower a `elem` ['b', 'c', 'm', 'p'] = Score 2
  | toLower a `elem` ['f', 'h', 'v', 'w', 'y'] = Score 4
  | toLower a == 'k' = Score 5
  | toLower a `elem` ['j', 'x'] = Score 8
  | toLower a `elem` ['q', 'z'] = Score 10
  | otherwise = Score 0

scoreString :: String -> Score
scoreString = foldMap score
