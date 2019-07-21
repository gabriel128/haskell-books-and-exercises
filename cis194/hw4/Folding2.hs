module Folding2 where

import Data.List
-- 1.
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

-- [1,2,3,4] = (2 - 2) * ((4 - 2) * ... )

fun1w :: [Integer] -> Integer
fun1w = product . map (\x -> x - 2) . filter even

-- 2.
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

-- fun2 4 = 4 + fun2 2
--        = 4 + (2 + fun2 1)
--        = 4 + (2 + 0)

-- fun2 5 = fun2 16
--        = 16 + ()


fun2w :: Integer -> Integer
fun2w = sum . filter even . takeWhile (>1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)


-- foldTree "ABCDEFGHIJ" ==
--   Node 3
--   (Node 2
--    (Node 0 Leaf ’F’ Leaf)
--    ’I’
--    (Node 1 (Node 0 Leaf ’B’ Leaf) ’C’ Leaf))
--   ’J’
--   (Node 2
--    (Node 1 (Node 0 Leaf ’A’ Leaf) ’G’ Leaf)
--    ’H’
--    (Node 1 (Node 0 Leaf ’D’ Leaf) ’E’ Leaf))

data Tree a =
  Leaf | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree ::  [a] -> Tree a
foldTree = foldr build Leaf
  where
    build x Leaf = Node 0 Leaf x Leaf
    build x (Node n Leaf root Leaf) = Node (n + 1) Leaf root (build x Leaf)
    build x (Node n Leaf root rhs) = Node n (build x Leaf) root rhs
    build x (Node n lhs root Leaf) = Node n lhs root (build x Leaf)
    build x (Node _ lhs@(Node p _ _ _) root rhs@(Node q _ _ _))
      | p < q = Node (q + 1) (build x lhs) root rhs
      | otherwise = Node ((+1) $ getHeight $ build x rhs) lhs root (build x rhs)


getHeight :: Tree a -> Integer
getHeight (Node n _ _ _) = n
getHeight _ = 0
-- foldTree ::  [a] -> Tree a
-- foldTree = foldr build Leaf
--   where
--     build x Leaf = Node 0 Leaf x Leaf
--     build x (Node _ lhs root rhs)
--       | getHeight lhs < getHeight rhs = Node (getHeight (build x lhs) + 1) (build x lhs) root rhs
--       | otherwise = Node (getHeight (build x rhs) + 1) lhs root (build x rhs)


-- getHeight :: Tree a -> Integer
-- getHeight (Node n _ _ _) = n
-- getHeight _ = -1

--   Node 3
--   (Node 2
--    (Node 0 Leaf ’F’ Leaf)
--    ’I’
--    (Node 1 (Node 0 Leaf ’B’ Leaf) ’C’ Leaf))
--   ’J’
--   (Node 2
--    (Node 1 (Node 0 Leaf ’A’ Leaf) ’G’ Leaf)
--    ’H’
--    (Node 1 (Node 0 Leaf ’D’ Leaf) ’E’ Leaf))

-- xor [False, True, False] == True
-- xor [False, True, False, False, True] == False
xor :: [Bool] -> Bool
xor = odd . foldr (\x acc -> if x then acc + 1 else acc) 0

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc ) []

-- foldr f z [x1, x2, ..., xn] == x1 ‘f‘ (x2 ‘f‘ ... (xn ‘f‘ z)...)
-- foldl f z [x1, x2, ..., xn] == (...((z ‘f‘ x1) ‘f‘ x2) ‘f‘...) ‘f‘ xn

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x acc -> f acc x) base xs

-- Start with a list of the integers from 1 to n. From this list, remove all numbers of the form i + j + 2ij where:
-- i,j in Nat, 1 <= i <= j /\ i + j + 2ij <= n
-- The remaining numbers are doubled and incremented by one

sieveNums :: Integer -> [Integer]
sieveNums n = [x | j <- [1..n], i <- [1..j], let x = i + j + 2 * i * j, x <= n]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = fmap (+1) . fmap (*2) . ([1..n] \\) . sieveNums $ n


geometricSeq :: Fractional a => a -> a -> [a]
geometricSeq a ratio = iterate (*ratio) a
