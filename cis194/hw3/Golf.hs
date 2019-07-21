module Golf where

import Data.List
import Data.Char

-- skips "ABCD" == ["ABCD", "BD", "C", "D"]
-- skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
-- skips [1] == [[1]]
-- skips [True,False] == [[True,False], [False]]
-- skips [] == []

filterSndByModZero :: (Int, a) -> [(Int, a)] -> [a]
filterSndByModZero (index, _) = map snd . filter (\y' -> fst y' `mod` index == 0)

skips :: [a] -> [[a]]
skips xs = foldr (\y acc -> filterSndByModZero y zipped : acc) [] zipped
  where zipped = zip [1..] xs

-- localMaxima [2,9,5,6,1] == [9,6]
-- localMaxima [2,3,4,1,5] == [4]
-- localMaxima [1,2,3,4,5] == []
localMaxima :: [Integer] -> [Integer]
localMaxima [_, _] = []
localMaxima [_] =  []
localMaxima [] = []
localMaxima (x:y:s:xs) = if y > x && y > s then y : localMaxima (y:s:xs) else localMaxima (y:s:xs)

-- histogram [1,1,1,5] ==
-- *
-- *
-- * *
-- ==========
-- 0123456789
-- histogram [1,4,5,4,6,6,3,4,2,4,9] ==
-- *
-- *
-- * *
-- ****** *
-- ==========
-- 0123456789

-- ["0123456789", "=========", ]
--

numberQuantities :: [Integer] -> [Int]
numberQuantities xs = map (\n -> length $ filter (== n) xs) [0..9]

buildLine :: Int -> [Integer] -> String
buildLine lineN theNumbers = map (\nLength -> if nLength >= lineN then '*' else ' ') (numberQuantities theNumbers)

buildLines :: [Integer] -> [String]
buildLines theNumbers = ["0123456789", "=========="] ++ map (\lineN -> buildLine lineN theNumbers) [1..9]

histogram :: [Integer] -> String
histogram = unlines . dropWhile (\line -> all isSpace line) . reverse . buildLines
