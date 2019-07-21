{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RankNTypes #-}
module CH10 where

import Data.Time
import Data.List

testing = foldr (&&) True [True, True]

testing2 = foldr (||) True [False, True]

testing3 = foldl (flip ((++) . show)) "9" [1..5]

t4 = foldr const 0 [1..5]

t5 = foldr const 'a' "tacos"

t6 = foldl (flip (const . show)) "z" [1..5]


data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr addUtcTime []
  where addUtcTime (DbDate utcTime) acc = utcTime : acc
        addUtcTime _ acc = acc

filterDbNumbers :: [DatabaseItem] -> [Integer]
filterDbNumbers = foldr addNumbers []
  where addNumbers (DbNumber number) acc = number : acc
        addNumbers _ acc = acc

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumbers

avgDb :: [DatabaseItem] -> Double
avgDb db = (realToFrac . sumDb) db / (realToFrac . length . filterDbNumbers) db

fibs = takeWhile (< 100) $ 1 : scanl (+) 1 fibs

stops = "pbtdkg"
vowels = "aeiou"

stopsAndVowels :: String -> String -> [(Char, Char, Char)]
stopsAndVowels ss vs = [(x, y, z) | x <- ss, y <- vs, z <- ss]

justPsFrom :: String -> String -> [(Char, Char, Char)]
justPsFrom ss vs = filter startsWithP (stopsAndVowels ss vs)
  where startsWithP ('p', _, _) = True
        startsWithP (_, _, _) = False


seekritFunc :: String -> Double
seekritFunc x = fromIntegral (sum (map length (words x))) / (fromIntegral (length (words x)))

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny p = foldr (\x acc -> p x || acc) False

myElem :: Eq a => a -> [a] -> Bool
myElem a = foldr (\x acc -> x == a || acc) False

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x acc -> if f x then x : acc else acc) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [a] = a
myMaximumBy f (x:xs)= foldl compareIt x xs
  where compareIt a b = if f a b == GT then a else b

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldl compareIt x xs
  where compareIt a b = if f a b == LT then a else b
