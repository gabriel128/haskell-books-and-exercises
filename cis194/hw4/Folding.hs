{-# LANGUAGE TemplateHaskell #-}
module Folding where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (\x acc -> (x - 2) * acc) 1 . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = foldr (+) 0 . filter even . takeWhile (>1) . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

-- foldTree "ABCDEFGHIJ" ==
-- Node 3
--   (Node 2
--      (Node 0 Leaf ’F’ Leaf)
--       ’I’
--      (Node 1 (Node 0 Leaf ’B’ Leaf) ’C’ Leaf))
--   ’J’
--   (Node 2
--     (Node 1 (Node 0 Leaf ’A’ Leaf) ’G’ Leaf)
--      ’H’
--     (Node 1 (Node 0 Leaf ’D’ Leaf) ’E’ Leaf))

-- Node 3
--   (Node 2
--      (Node 1 (Node 0 Leaf 'E' Leaf) 'H' Leaf)
--      'I'
--      (Node 1 (Node 0 Leaf 'A' Leaf) 'D' Leaf))
--   'J'
--   (Node 2
--      (Node 1 (Node 0 Leaf 'C' Leaf) 'F' Leaf)
--      'G'
--      (Node 0 Leaf 'B' Leaf))

data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

treeHeight :: Tree a -> Integer
treeHeight Leaf = 0
treeHeight (Node ht _ _ _) = ht

foldTree :: Eq a => [a] -> Tree a
foldTree = foldr insertNode Leaf

insertNode :: a -> Tree a -> Tree a
insertNode x Leaf = Node 0 Leaf x Leaf
insertNode x (Node _height left letter right)
    | treeHeight left > treeHeight right = Node (treeHeight right + 1) left letter (insertNode x right)
    | otherwise = Node (treeHeight left + 1) (insertNode x left) letter right

-- xor [False, True, False] == True
-- xor [False, True, False, False, True] == False
xor :: [Bool] -> Bool
xor = odd . foldr (\p acc -> if p then acc + 1 else acc) 0

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> (f x) : acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base = foldr (\x acc -> f acc x) base . reverse
