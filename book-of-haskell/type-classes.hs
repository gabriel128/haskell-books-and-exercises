module TypeClasses where

import Data.List

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (TisAn a) == (TisAn a') = a == a'

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (Two a b) == (Two a' b') = a == a' && b == b'

data StringOrInt =
  TisAnInt Int | TisAString String

instance Eq StringOrInt where
  (TisAnInt a) == (TisAnInt a') = a == a'
  (TisAString a) == (TisAString a') = a == a'
  (TisAnInt _) == _ = False
  (TisAString _) == _ = False

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  (Pair a _) == (Pair a' _) = a' == a

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (Tuple a b) == (Tuple a' b') = a == a' && b == b'

data Which a =
  ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
  (ThisOne a) == (ThisOne a') = a == a'
  (ThatOne a) == (ThatOne a') = a == a'
  _ == _ = False

data EitherOr a b =
  Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (Hello a) == (Hello a') = a == a'
  (Goodbye a) == (Goodbye a') = a == a'
  _ == _ = False

data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood = Blah | Woot deriving (Show, Eq)

settleDown x = if x == Woot
  then Blah
  else x

type Subject = String
type Verb = String
type Object = String
data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"

s2 = Sentence "Julie" "loves" "dogs"


------------------------------

data Rocks = Rocks String deriving (Eq, Show, Ord)

data Yeah = Yeah Bool deriving (Eq, Show, Ord)

data Papu = Papu Rocks Yeah deriving (Eq, Show, Ord)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'

-------------------------------

f :: RealFrac a => a
f = 1.0

freud :: Int -> Int
freud x = x

myX = 1 :: Int

sigmund :: Int -> Int
sigmund x = myX


 ----------

jung :: [Int] -> Int
jung xs = head (sort xs)

young :: Ord a => [a] -> a
young xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)

-----------------------

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk fun a b = (fun a) == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f'' _ a = (f'' a) + 1
