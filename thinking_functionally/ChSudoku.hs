module CHSudo where

import Data.List
import Data.Monoid
import Data.Foldable
import Data.Maybe
-- Exercise D Given a function sort :: (Ord a) => [a] -> [a] that sorts a list,
-- construct a definition of

nodups :: (Ord a) => [a] -> Bool
nodups = anyDup . sort
  where
    anyDup [] = True
    anyDup [_] = True
    anyDup (x:x':xs) = x /= x' && anyDup xs



-- Exercise E The function nub :: (Eq a) => [a] -> [a] removes duplicates from a list
-- (a version of this function is available in the library Data.List).
-- Define nub. Assuming the order of the elements in the result is not important, define
-- nub :: (Ord a) => [a] -> [a] so that the result is a more efficient function.

nub' :: (Ord a) => [a] -> [a]
nub' = go . sort
  where
    go [] = []
    go (x:xs)
      | x `elem` xs = go xs
      | otherwise = x : go xs


-- Exercise G Define minimum :: Ord a => [a] -> a.
newtype Min a = Min {getMin :: Maybe a}

instance Ord a => Monoid (Min a) where
    mempty = Min Nothing
    m `mappend` Min Nothing = m
    Min Nothing `mappend` n = n
    (Min m@(Just x)) `mappend` (Min n@(Just y))
      | x <= y    = Min m
      | otherwise = Min n


minimum' :: Ord a => [a] -> Maybe a
minimum' =  getMin . foldMap (Min . Just)
