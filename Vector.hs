module FiboVector where

import Data.List

data Vector4 a = Vector4 a a a a deriving (Show, Eq)

data Vector2 a = Vector2 a a deriving (Show, Eq)

vectMult :: Vector4 Int -> Vector2 Int -> Vector2 Int
vectMult (Vector4 a1 a2 a3 a4) (Vector2 b1 b2) = Vector2 (b1 * a1 + b2 * a2) (b1 * a3 + b1 * a4)

nextFib :: Vector2 Int -> Vector2 Int
nextFib = vectMult (Vector4 1 1 1 0)

fibsV :: Int -> [Vector2 Int]
fibsV i = unfoldr unreducer (0, Vector2 1 1)
  where
    unreducer (x, vect)
      | x == (i-1) = Nothing
      | otherwise = Just (vect, (x + 1, nextFib vect))

fibs :: Int -> [Int]
fibs n = foldr reducer [] (fibsV n)
  where
    reducer (Vector2 1 1) acc = 1 : 1 : acc
    reducer (Vector2 a1 _) acc = a1 : acc
