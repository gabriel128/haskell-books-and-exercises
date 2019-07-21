{-# LANGUAGE FlexibleInstances #-}
module JoinList where

import Editor
import Data.Maybe
import Buffer
import Data.Monoid
import Sized
import Scrabble

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

jtl :: JoinList (Product Integer) Char
jtl = Append (Product 210)
      (Append (Product 30)
       (Single (Product 5) 'y')
       (Append (Product 6)
        (Single (Product 2) 'e')
        (Single (Product 3) 'a')))
      (Single (Product 7) 'h')

test = (Single (Product 2) 'e') +++ (Single (Product 3) 'a')

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
jl1 +++ jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

jtl1 :: JoinList Size Char
jtl1 = (Append (Size 2)
        (Single (Size 1) 'e')
        (Single (Size 1) 'a'))

b :: JoinList Size String
b = (Append (Size 4)
      (Append (Size 2)
        (Single (Size 1) "moon")
        (Single (Size 1) "planet"))
      (Append (Size 2)
        (Single (Size 1) "star")
        (Single (Size 1) "galaxy")))
-- indexJ 3 b = "galaxy"
-- indexJ 3 b = indexJ 2 (Append (Size 2) (Single (Size 1) "star") (Single (Size 1) "galaxy"))

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ n _ | n < 0 = Nothing
indexJ _ (Single _ a) = Just a
indexJ 0 (Append s l _) | (getSize . size) s == 2 =  indexJ 0 l
indexJ 1 (Append s _ r) | (getSize . size) s == 2 = indexJ 0 r
indexJ n (Append m l r)
  | n >= (getSize . size) m = Nothing
  | n < ((getSize . size) m `div` 2) = indexJ n l
  | n >= ((getSize . size) m `div` 2) = indexJ (n-2) r
  | otherwise = Nothing

(!?) :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
n !? Empty    = Nothing
n !? (Single _ a)
  | n == 0    = Just a
  | otherwise = Nothing
n !? (Append m l r)
  | n < 0 || n > (getSize . size $ m) = Nothing
  | n < (getSize . size $ tag l)      = n !? l
  | otherwise                         = (n - (getSize . size $ tag l)) !? r
-- Ex 2 --

-- dropJ 1 b  = (Append (Size 2) Empty (Single (Size 1) "galaxy"))
-- dropJ 1 b = dropJ 1 (Append (Size 2) (Single (Size 1) "star") (Single (Size 1) "galaxy"))

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl | (n + 1) <= 0 = jl
dropJ n (Append m l r)
  | n > (getSize . size) m = Empty
  | n <= sizel = Append m (dropJ n l) r
  | otherwise = Append m Empty (dropJ (n - sizel) r)
    where
      sizel = getSize . size . tag $ l
dropJ _ _ = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 _ = Empty
takeJ n jl@(Append m l r)
  | n > (getSize . size) m = jl
  | n <= sizel = Append m (takeJ n l) Empty
  | otherwise = Append m l (takeJ (n - sizel) r)
    where
      sizel = getSize . size . tag $ l
takeJ _ a = a


testIndex i jl = (indexJ i jl) == (jlToList jl !!? i)
testDrop n jl = jlToList (dropJ n jl) == drop n (jlToList jl)
testTake n jl = jlToList (takeJ n jl) == take n (jlToList jl)

-- [1,2,3,4] !!? 2 = [2, 3, 4] !!? 1 = [3, 4] !!? 0 = 3
(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2


-- Ex 3

scoreLine :: String -> JoinList Score String
scoreLine xs = Single (scoreString xs) xs

-- Ex 4

instance Monoid (JoinList (Score, Size) String) where
  mempty = Empty
  a@(Single (x, y) _) `mappend` a'@(Single (x', y') _) = Append (x <> x', y <> y') a a'

instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . jlToList
  fromString = foldr ((+++) . (\word -> Single (scoreString word, Size 1) word)) Empty . lines
  line = (!?)
  replaceLine n l m = takeJ n m +++ fromString l +++ dropJ (n+1) m
  numLines               = getSize  . snd . tag
  value (Append (Score s, _) _ _) = fromInteger s
  value (Single (Score s, _) _) = fromInteger s
  value Empty = 0

safeIndex :: Int -> [a] -> Maybe a
safeIndex n _ | n < 0 = Nothing
safeIndex _ []        = Nothing
safeIndex 0 (x:_)     = Just x
safeIndex n (_:xs)    = safeIndex (n-1) xs

main = runEditor editor (fromString "test" :: (JoinList (Score, Size) String))
