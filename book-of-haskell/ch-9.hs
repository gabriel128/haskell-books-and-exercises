module CH9 where

import Data.Char

capitalizeFirst :: String -> String
capitalizeFirst (x:xs) = toUpper x : xs

capitalizeItAll :: String -> String
capitalizeItAll [] = []
capitalizeItAll (x:xs) = toUpper x : capitalizeItAll xs

capitalHead :: String -> Char
capitalHead = toUpper . head

myOr :: [Bool] -> Bool
myOr [] = True
myOr (True:_) = True
myOr (_:xs) = myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = if f x then True else myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (x':xs) = if x == x' then True else myElem x xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f =  squish . map f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
myMaximumBy _ [] = Nothing
myMaximumBy _ [x] = Just x
myMaximumBy f (x:y:xs) = myMaximumBy f (greatest:xs)
  where greatest = case f x y of
          GT -> x
          LT -> y
          EQ -> x

myMinimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
myMinimumBy _ [] = Nothing
myMinimumBy _ [x] = Just x
myMinimumBy f (x:y:xs) = myMinimumBy f (greatest:xs)
  where greatest = case f x y of
          GT -> y
          LT -> x
          EQ -> x

myMaximum :: (Ord a) => [a] -> Maybe a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> Maybe a
myMinimum = myMinimumBy compare
