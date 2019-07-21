{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Monoid where

import Data.Monoid
import Test.QuickCheck
-- import Text.Show.Functions

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only b) (Only a) = Only (b <> a)
  mappend Nada x = x
  mappend x Nada = x

-- ###################

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj =
  e <> "! he said " <>
  adv <> " as he jumped into his car " <>
  noun <> " and drove off with his " <>
  adj <> " wife."

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj =
  mconcat [e, "! he said ",adv ," as he jumped into his car ",
           noun, " and drove off with his ", adj ," wife."]

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' (Only a)) (First' (Only _)) = First' (Only a)
  mappend (First' Nada) x =  x
  mappend x (First' Nada) = x

firstMappend :: Monoid a => First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    frequency [(5, return (Only a)), (1, return Nada)]

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    return (First' a)

-- main :: IO ()
-- main = do
--   quickCheck (monoidAssoc :: FirstMappend)
--   quickCheck (monoidLeftIdentity :: FstId)
--   quickCheck (monoidRightIdentity :: FstId)

class Semigroup a where
  (<|>) :: a -> a -> a

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  Trivial <|> Trivial = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool
type TrivId = Identity Trivial -> Identity Trivial -> Identity Trivial -> Bool
type TwoTriv = Two Trivial Trivial -> Two Trivial Trivial -> Two Trivial Trivial -> Bool

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  Identity a <|> Identity b = Identity (a <|> b)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two a b <|> Two a' b' = Two (a <|> a') (b <|> b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj True <|> BoolConj True = BoolConj True
  BoolConj False <|> _ = BoolConj False
  _ <|> BoolConj False = BoolConj False

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj False <|> BoolDisj False = BoolDisj False
  _ <|> _ = BoolDisj True


data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  Fst _ <|> x = x
  Snd a <|> _ = Snd a

type OrInt = Or Int Int -> Or Int Int -> Or Int Int -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
     a <- arbitrary
     b <- arbitrary
     elements [Fst a, Snd b]

newtype Combine a b = Combine { unCombine :: a -> b }

-- instance Show (Combine a b) where
--   show (Combine f) = "Combine " ++ show f

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f) <|> (Combine g) = Combine $ \ x -> f x <|> g x

newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup a => Semigroup (Comp a) where
  Comp a <|> Comp b = Comp $ a . b

data Validation a b = Failure' a | Success' b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Validation a b) where
   Success' a <|> Success' b = Success' (a <|> b)
   Failure' a <|> Failure' b = Failure' (a <|> b)
   Failure' a <|> _ = Failure' a
   _ <|> Failure' a = Failure' a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Success' a, Failure' b]

type ValidationAssoc =
  Validation Trivial Trivial ->
  Validation Trivial Trivial ->
  Validation Trivial Trivial ->
  Bool

newtype AccumulateBoth a b = AccumulateBoth (Validation a b) deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  AccumulateBoth a <|> AccumulateBoth b = AccumulateBoth $ a <|> b

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = do
    a <- arbitrary
    return $ AccumulateBoth a

type AccumulateBothAssoc =
  AccumulateBoth Trivial Trivial ->
  AccumulateBoth Trivial Trivial ->
  AccumulateBoth Trivial Trivial ->
  Bool

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<|>)

instance (Monoid a, Semigroup a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<|>)

instance (Monoid a, Monoid b, Semigroup a, Semigroup b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<|>)

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<|>)

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<|>)

instance Arbitrary BoolConj where
  arbitrary = do
    a <- elements [True, False]
    return (BoolConj a)

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- elements [True, False]
    return (BoolDisj a)

instance (Monoid a, Monoid b) => Monoid (Combine a b) where
  mempty = Combine ( \ _ -> mempty )
  (Combine a) `mappend` (Combine b) = Combine ( \ x -> a x <> b x)

instance (Monoid a, Semigroup a) => Monoid (Comp a) where
  mempty = Comp id
  mappend = (<|>)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <|> (b <|> c)) == ((a <|> b) <|> c)

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- main :: IO ()
-- main = do
--   quickCheck (semigroupAssoc :: TrivId)
--   quickCheck (semigroupAssoc :: TwoTriv)
--   quickCheck (semigroupAssoc :: OrInt)
--   quickCheck (semigroupAssoc :: ValidationAssoc)
--   quickCheck (semigroupAssoc :: AccumulateBothAssoc)
--   quickCheck (monoidLeftIdentity :: Trivial -> Bool)
--   quickCheck (monoidRightIdentity :: Trivial -> Bool)
--   quickCheck (monoidLeftIdentity :: Identity Trivial -> Bool)
--   quickCheck (monoidRightIdentity :: Identity Trivial -> Bool)
--   quickCheck (monoidLeftIdentity :: Two Trivial Trivial -> Bool)
--   quickCheck (monoidRightIdentity :: Two Trivial Trivial -> Bool)
--   quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
--   quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
--   quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
--   quickCheck (monoidRightIdentity :: BoolDisj -> Bool)

newtype Mem s a = Mem {runMem :: s -> (a,s)}

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  (Mem f1) `mappend` (Mem f2) = Mem $
    \s -> let (a, b) = f1 s
              (a', b') = f2 b
          in (a <> a', b')

f' = Mem $ \s -> ("hi", s + 1)

main :: IO ()
main = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print $ (rmzero :: (String, Int)) -- ("", 0)
  print $ rmright -- ("hi", 1)
  print $ rmleft -- ("hi", 1)
  print $ rmleft == runMem f' 0 -- True
  print $ rmright == runMem f' 0 -- True
