module LectureHW5 where

f :: a -> a
f x = x

f' :: a -> b
f' x = undefined

f'' :: a -> b -> a
f'' x y = x

g :: [a] -> [a]
g x = x

g' :: (b -> c) -> (a -> b) -> (a -> c)
g' x y = x . y

g'' :: (a -> a) -> a -> a
g'' x y = x y
