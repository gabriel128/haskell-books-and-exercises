module Recursion where

import Data.List
import Data.Char
import Unsafe.Coerce
import Data.Maybe

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"


-- Write a function that recursively sums all numbers from 1 to n,
-- n being the argument. So that if n was 5, you’d add 1 + 2 + 3 + 4
-- + 5 to get 15. The type should be (Eq a, Num a) => a -> a.

sumToN :: (Eq a , Num a) => a -> a
sumToN 1 = 1
sumToN n = n + sumToN(n-1)

-- Write a function that multiplies two integral numbers using
-- recursive summation. The type should be (Integral a) => a ->
-- a -> a.

multiplyBySum :: (Integral a) => a -> a -> a
multiplyBySum x 1 = x
multiplyBySum x n = x + multiplyBySum x (n - 1)

data DividedResult =
  Result { quotient :: Integer , rest :: Integer}
  | DividedByZero
  deriving Show

dividedBy :: Integer -> Integer -> DividedResult
dividedBy num denom = go num (abs denom) 0
  where go n d count
          | denom == 0 = DividedByZero
          | n < d && denom > 0 = Result {quotient = count, rest = n}
          | n < d && denom < 0 = Result (-count) n
          | otherwise = go (n - d) d (count + 1)

mc91 :: Int -> Int
mc91 n
  | n > 100 =  n - 10
  | otherwise = mc91 . mc91 $ n + 11

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = "donknow"

digits :: Int -> [Int]
digits n
  | n < 10 = []
  | otherwise = reverse $ (n `mod` 10) : digits (n `div` 10)

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits

doEft :: (Eq a, Enum a) => a -> a -> [a]
doEft x y
  | x == y = [y]
  | otherwise = x : doEft (succ x) y

eftBool :: Bool -> Bool -> [Bool]
eftBool = doEft

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = doEft

eftInt :: Int -> Int -> [Int]
eftInt = doEft

eftChar :: Char -> Char -> String
eftChar = doEft

y :: (a -> b) -> b
y =  \f -> (\x -> f (unsafeCoerce x x)) (\x -> f (unsafeCoerce x x))

fact :: Int -> Int
fact = \x -> if x == 0 then 1 else x * y (\_ -> x-1)

 ------------------------------------

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

-- putStrLn sentences -- should print
-- Tyger Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?
-- Implement this

myFold :: String -> Char -> [String]
myFold [] _ = []
myFold xs char
  | [char] `isPrefixOf` xs = myFold withoutTale char
  | otherwise = takeWhile (/=char) xs : myFold (dropWhile (/=char) xs) char
  where withoutTale = fromMaybe [char] (stripPrefix [char] xs)

myLines :: String -> [String]
myLines xs = myFold xs '\n'

myWords :: String -> [String]
myWords xs = myFold xs ' '


shouldEqual :: [String]
shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"]

main :: IO ()
main = print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)
