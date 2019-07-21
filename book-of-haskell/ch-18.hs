{-# LANGUAGE TemplateHaskell #-}
module Monaddssss where

import Data.Monoid

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Monad


data Sum' a b = First' a | Second' b deriving (Eq, Show)

instance Functor (Sum' a) where
  fmap f (Second' b) = Second' (f b)
  fmap _ (First' a) = First' a

instance Monoid a => Applicative (Sum' a) where
  pure = Second'
  (First' a) <*> (First' a') = First' (a <> a')
  (Second' _) <*> (First' a') = First' a'
  (First' a) <*> (Second' _) = First' a
  (Second' f) <*> (Second' a) = Second' (f a)

instance (Monoid a) => Monad (Sum' a) where
  return = pure
  (First' a) >>= _ = First' a
  (Second' b) >>= f = f b

data Nope a = NopeDotJpg deriving (Show, Eq)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance (Eq a) => EqProp (Nope a) where
  (=-=) = eq

data Either' b a = Left' a | Right' b deriving (Show, Eq)

instance Functor (Either' b) where
  fmap _ (Right' b) = Right' b
  fmap f (Left' a) = Left' (f a)

instance (Monoid b) => Applicative (Either' b) where
  pure = Left'
  (Left' _) <*> (Right' b) = Right' b
  (Right' b) <*> _  = Right' b
  (Left' f) <*> (Left' a) = Left' (f a)

instance (Monoid b) => Monad (Either' b) where
  return = pure
  (Left' a) >>= f = f a
  (Right' b) >>= _  = Right' b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Either' b a) where
  arbitrary = oneof [Left' <$> arbitrary, Right' <$> arbitrary]

instance (Eq a, Eq b) => EqProp (Either' b a) where
  (=-=) = eq

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity b = Identity (f b)

instance Monad Identity where
  return = pure
  Identity a >>= f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

data List a = Nil | Cons a (List a) deriving (Show, Eq)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a list) = Cons (f a) (fmap f list)

instance Applicative List where
  pure = flip Cons Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f list) <*> (Cons a list') = Cons (f a) (list <*> list')

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  (Cons a list) >>= f = f a `append` (list >>= f)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons a list) list' = Cons a (append list list')

-- fold :: (a -> b -> b) -> b -> List a -> b
-- fold _ acc Nil = acc
-- fold f acc (Cons a list) = f a (fold f acc list)

-- concat' :: List (List a) -> List a
-- concat' = fold append Nil

-- flatMap :: (a -> List b) -> List a -> List b
-- flatMap f = concat' . fmap f

something :: Maybe String
something = do
  a <- Just "a"
  _ <- Nothing
  b <- Just "b"
  return (a ++ b)

main :: IO ()
main = do
  quickBatch . functor $ (NopeDotJpg :: Nope (String, String, String))
  quickBatch . applicative $ (NopeDotJpg :: Nope (String, String, String))
  quickBatch . monad $ (NopeDotJpg :: Nope (String, String, String))
  quickBatch . functor $ (undefined :: Either' (String, String, String) (String, String, String))
  quickBatch . applicative $ (undefined :: Either' (String, String, String) (String, String, String))
  quickBatch . monad $ (undefined :: Either' (String, String, String) (String, String, String))
  quickBatch . functor $ (undefined :: Identity (String, String, String))
  quickBatch . applicative $ (undefined :: Identity (String, String, String))
  quickBatch . monad $ (undefined :: Identity (String, String, String))
  -- quickBatch $ functor $ Cons ('a', 'b', 'c') Nil
  -- quickBatch $ applicative $ Cons ('a', 'b', 'c') Nil
  -- quickBatch $ monad $ Cons ('a', 'b', 'c') Nil


--  Write the following functions using the methods provided by
-- Monad and Functor. Using stuff like identity and composition is fine,
-- but it has to typecheck with types provided.

j :: Monad m => m (m a) -> m a
j = join
-- Expecting the following behavior:
-- Prelude> j [[1, 2], [], [3]]
-- [1,2,3]
-- Prelude> j (Just (Just 1))
-- Just 1
-- Prelude> j (Just Nothing)
-- Nothing
-- Prelude> j Nothing
-- Nothing

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f a b = pure f <*> a <*> b

au :: Monad m => m a -> m (a -> b) -> m b
au a mf = mf <*> a

-- Recursive
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = l2 (:) (f x) (meh xs f)

-- Reuse meh
flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs id
