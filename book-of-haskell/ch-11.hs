{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module DataTypessss where

import Data.Function (on)
import Data.Int
import Data.Char
import Data.List.Split
import Data.List (sort, group, groupBy, elemIndex, intercalate)
import Data.Maybe
import Data.Foldable (maximumBy, Foldable)
import Data.Ord      (comparing)

data Doggies a = Husky a
               | Mastiff a
               deriving (Eq, Show)
data Size = Small | Big | Huge deriving (Eq, Show)

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini
                  | Tata
                  | Mazda
                  deriving (Eq, Show)

data Airline = PapuAir
             | CatapultsRUs
             | TakeYourChancesUnited
             deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir Huge

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _)= True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Maybe Manufacturer
getManu (Car manuf _) = Just manuf
getManu _ = Nothing

data Example = MakeExample deriving Show

data SuperExample = MakeSuperExample Int deriving Show

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany String where
  tooMany s = length s > 42

instance TooMany (Int, Int) where
  tooMany (n, n') = n + n' > 42

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) =  tooMany (x + y)

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

data NumberOrBool = Numba Int8 | BoolyBool Bool deriving (Eq, Show)

data OperatingSystem = GnuPlusLinux
                     | OpenBSDPlusNevermindJustBSDStill
                     | Mac
                     | Windows
                     deriving (Eq, Show)

data ProgrammingLanguage = Haskell
                         | Agda
                         | Idris
                         | PureScript
                         deriving (Eq, Show)

data Programmer = Programmer
                  { os :: OperatingSystem
                  , lang :: ProgrammingLanguage }
                deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [ GnuPlusLinux , OpenBSDPlusNevermindJustBSDStill , Mac , Windows]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [ Programmer { os = x, lang = y} | x <- allOperatingSystems, y <- allLanguages]

-- BinaryTree

data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"

----------------------------------

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = preorder left ++ [a] ++ preorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = preorder left ++ preorder right ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "postorder failed check"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f acc (Node left a right) = f a (foldTree f (foldTree f acc right) left)

 -------------

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] [] = True
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf word@(xw:restw) phrase@(xp:restp)
  | xw == xp = isSubsequenceOf restw phrase
  | otherwise = isSubsequenceOf word restp

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map doTupling . words
  where doTupling word@(x:xs) = (word, toUpper x : xs)
        doTupling [] = ("","")

capitalizeWord :: String -> String
capitalizeWord [] = ""
capitalizeWord (' ':xs) = capitalizeWord xs
capitalizeWord (x:xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph =  intercalate ". " . map capitalizeWord . splitOn "."

---------------------------------
data Digit =
  NumericDigit {alphanum :: Char , letters :: String}
  | Astersiq { alphanum :: Char, letters :: String}
  deriving (Eq, Show)

-- type Digit = Char

type Presses = Int

newtype DaPhone = DaPhone [Digit] deriving (Eq, Show)

phone :: DaPhone
phone = DaPhone [NumericDigit { alphanum = '1', letters = "1"},
                 NumericDigit { alphanum = '2', letters = "abc2"},
                 NumericDigit { alphanum = '3', letters = "def3"},
                 NumericDigit { alphanum = '4', letters = "ghi4"},
                 NumericDigit { alphanum = '5', letters = "jkl5"},
                 NumericDigit { alphanum = '6', letters = "mno6"},
                 NumericDigit { alphanum = '7', letters = "pqrs7"},
                 NumericDigit { alphanum = '8', letters = "tuv8"},
                 NumericDigit { alphanum = '9', letters = "wyxz9"},
                 NumericDigit { alphanum = '0', letters = "+ 0"},
                 NumericDigit { alphanum = '#', letters = ".,#"},
                 Astersiq { alphanum = '*', letters = ""}]


first :: [a] -> Maybe a
first [] = Nothing
first (x:_) = Just x

numericDigitByLetter :: Char -> DaPhone -> Maybe Digit
numericDigitByLetter letr (DaPhone digits)= first $ filter byLet digits
  where byLet NumericDigit{letters = xs} = letr `elem` xs
        byLet _ = False

letterFromDigit :: DaPhone -> (Char, Int) -> Char
letterFromDigit (DaPhone digits) (digit, presses) = (letters . head . filter (\x -> alphanum x == digit) $ digits) !! (presses - 1)

pressesFromDigit :: Char -> Maybe Digit -> Maybe Presses
pressesFromDigit _ Nothing = Nothing
pressesFromDigit letr (Just NumericDigit{letters = letrs}) =
  pure (+) <*> (elemIndex letr letrs) <*> Just 1
pressesFromDigit _ (Just _) = Nothing

reverseTaps :: DaPhone -> Char -> [(Char, Presses)]
reverseTaps daPhone letter
  | isUpper letter = [('*', 1),
                      (alphan . toLower $ letter,
                       fromJust . presses . toLower $ letter)]
  | otherwise = [(alphan letter, fromJust . presses $ letter)]
  where digitNumber x = numericDigitByLetter x daPhone
        presses x = pressesFromDigit x $ digitNumber x
        alphan = alphanum . fromJust . digitNumber

cellPhonesDead :: DaPhone -> String -> [(Char, Presses)]
cellPhonesDead thePhone = concatMap (reverseTaps thePhone)

testConvo :: [[(Char, Presses)]]
testConvo = map (\x -> cellPhonesDead phone x) convo

fingerTaps :: [(Char, Presses)] -> Presses
fingerTaps = foldr howManyPress 0
  where howManyPress (_, presses) acc =  acc + presses

mostPopularLetter :: DaPhone -> String -> Char
mostPopularLetter thePhone =
  letterFromDigit thePhone  .
  head . take 1 . foldr filterMax [] .
  groupBy tapsLetter . cellPhonesDead thePhone .
  map toLower
  where tapsLetter (letter, press) (let', press')= letter == let' && press == press'

convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol lol",
   "Lol ya",
   "Wow ur cool haha. Ur turn",
   "Ok. Do u think I am pretty Lol",
   "Lol ya",
   "Haha thanks just making sure rofl ur turn"]

filterMax :: Foldable t => t a -> t a -> t a
filterMax x acc = if length x > length acc then x else acc

coolestLtr :: [String] -> Maybe Char
coolestLtr = first . foldr filterMax [] . group . map toLower . sort . filter (/= ' ') . concat

coolestWord :: [String] -> Maybe String
coolestWord = first . foldr filterMax [] . group . sort . words . filter (/='.') . map toLower . unwords

----------------------
-- Hutton's Razor

data Expr =
  Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit val) = val
eval (Add lit lit') = eval lit + eval lit'

printExpr :: Expr -> String
printExpr (Lit val) = show val
printExpr (Add lit lit') = printExpr lit ++ " + "  ++ printExpr lit'
