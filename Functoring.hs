module Functoring (Tree) where

data Tree a = Node a [Tree a] deriving (Show)

data Hello

instance Functor Tree where
    fmap f (Node x ts) = Node (f x) (fmap f <$> ts)

-- instance Functor Tree where
--   fmap f (Node a ts) = Node (f a) (fmap (fmap f) ts)
(+*+) :: Int -> Int -> Int -> Int
(+*+) x y z = x + y + z


a = 6 +*+ 8

f :: a -> b -> b
f a b = b

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

choose :: Int -> Int -> Int
choose n k
  | k < 0 || n < k = 0
  | k == n || k == 0 = 1
  | otherwise = (factorial n) `div` (factorial k * factorial (n-k))

indices :: Int -> Int -> [Int]
indices n m = [(n `choose` k) * (m `choose` j) | k <- [0..n], j <- [0..m]]

indices2 :: Int -> Int -> [Int]
indices2 n m = [(m `choose` j) * (n `choose` k-j) | k <- [0..m+n], j <- [0..k]]
