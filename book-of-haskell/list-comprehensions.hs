{-# LANGUAGE ParallelListComp #-}

module Comprenh where

import Data.Bool

mySqr :: [Integer]
mySqr = [x^2 | x <- [1..5]]

myCube :: [Integer]
myCube = [y^3 | y <- [1..5]]

cubeSqr :: [(Integer, Integer)]
cubeSqr = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

waaa = [x^y | x <- [1..5], y <- [2, undefined]]

itIsMystery :: String -> [Bool]
itIsMystery xs = map (\x -> elem x "aeiou") xs

testBool :: [Int]
testBool = map (\x -> bool x (-x) (x == 3)) [1..10]

multOf3l :: Int
multOf3l = length . filter (\x -> x `mod` 3 == 0) $ [1..30]

myFilter :: String -> [String]
myFilter = filter (\x -> x /= "the" && x /= "a" ) . words

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f xs ys = [f x y | x <- xs | y <- ys]

myZip :: [a] -> [b] -> [(a, b)]
myZip xs ys = myZipWith (,) xs ys
