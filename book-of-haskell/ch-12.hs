{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module SignalAdversity where

import Data.Char
import Data.List
import Control.Monad
import Data.Maybe (fromJust)

-- example GHCi session
-- above the functions
-- >>> notThe "the"
-- Nothing
-- >>> notThe "blahtheblah"
-- Just "blahtheblah"
-- >>> notThe "woot"
-- Just "woot"
notThe :: String -> Maybe String
notThe "the" =  Nothing
notThe a = Just a


words' :: String -> [String]
words' phrase = go phrase "" []
  where go [] acc final = final ++ [reverse acc]
        go (' ':rest) acc final = go rest "" (final ++ [reverse acc])
        go (a:rest) acc final = go rest (a:acc) final

-- >>> replaceThe "the cow loves us"
-- "a cow loves us"
replaceThe :: String -> String
replaceThe phrase = go (words' phrase)
  where
    go [] = []
    go (word:rest) = parseThe (notThe word) ++ " " ++ go rest
    parseThe (Just a) = a
    parseThe Nothing = "a"

isVowel :: Char -> Bool
isVowel ch = isInfixOf [(toLower ch)] "aeiou"

-- >>> countTheBeforeVowel "the cow"
-- 0
-- >>> countTheBeforeVowel "the evil cow"
-- 1
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel phrase = go (words' phrase)
  where
    go [] = 0
    go (word:rest) = (countThe (notThe word) (head rest)) + go rest
    countThe (Just _) _ = 0
    countThe _ [] = 0
    countThe Nothing (a:_) = if isVowel a then 1 else 0

countVowels :: String -> Integer
countVowels = foldr (\n acc -> if isVowel n then acc + 1 else acc) 0

countConsonats :: String -> Integer
countConsonats = foldr (\n acc -> if isVowel n then acc else acc + 1) 0

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord phrase = if countVowels phrase > countConsonats phrase then Nothing else Just(Word' phrase)

data Nat = Zero | Succ Nat deriving(Eq, Show)

-- >>> natToInteger Zero
-- 0
-- >>> natToInteger (Succ Zero)
-- 1
-- >>> natToInteger (Succ (Succ Zero))
-- 2
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

-- >>> integerToNat 0
-- Just Zero
-- >>> integerToNat 1
-- Just (Succ Zero)
-- >>> integerToNat 2
-- Just (Succ (Succ Zero))
-- >>> integerToNat (-1)
-- Nothing
integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0 = Nothing
  | x == 0 = Just Zero
  | otherwise = Just (Succ (fromJust (integerToNat (x - 1))))

-- >>> isJust (Just 1)
-- True
-- >>> isJust Nothing
-- False
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

-- >>> isNothing (Just 1)
-- False
-- >>> isNothing Nothing
-- True
isNothing :: Maybe a -> Bool
isNothing = not . isJust

-- >>> mayybee 0 (+1) Nothing
-- 0
-- >>> mayybee 0 (+1) (Just 1)
-- 2
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee acc _ Nothing = acc
mayybee _ f (Just x) = f x

-- >>> fromMaybe 0 Nothing
-- 0
-- >>> fromMaybe 0 (Just 1)
-- 1
-- Try writing it in terms
-- of the maybe catamorphism
fromMaybe :: a -> Maybe a -> a
fromMaybe a = mayybee a id

-- >>> listToMaybe [1, 2, 3]
-- Just 1
-- >>> listToMaybe []
-- Nothing
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (a:_) = Just a

-- >>> maybeToList (Just 1)
-- [1]
-- >>> maybeToList Nothing
-- []
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- >>> catMaybes [Just 1, Nothing, Just 2]
-- [1, 2]
-- >>> let xs = take 3 $ repeat Nothing
-- >>> catMaybes xs
-- []
catMaybes :: [Maybe a] -> [a]
catMaybes = concatMap maybeToList

-- >>> flipMaybe [Just 1, Just 2, Just 3]
-- Just [1, 2, 3]
-- >>> flipMaybe [Just 1, Nothing, Just 3]
-- Nothing
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs = let result = catMaybes xs in
                 if length result < length xs
                 then Nothing
                 else Just result

lefts' :: [Either a b] -> [a]
lefts' = foldr unwrapLeft []
  where unwrapLeft (Left a) acc = a : acc
        unwrapLeft _ acc = acc

rights' :: [Either a b] -> [b]
rights' = foldr unwrapRight []
  where unwrapRight (Right b) acc = b : acc
        unwrapRight _ acc = acc

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right a) = Just (f a)
eitherMaybe' _ (Left _) = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a)= f a
either' _ f (Right b)= f b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f x@(Right _)= Just (either' undefined f x)
eitherMaybe'' _ _ = Nothing

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x =
  case f x of
    Nothing -> []
    (Just (a, b)) -> a : myUnfoldr f b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\a -> Just (a, f a))

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f a =
  case f a of
    Nothing -> Leaf
    (Just (x,y,z)) -> Node (unfold f x) y (unfold f z)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\x -> if x == n then Nothing else Just (x+1, x, x+1)) 0
