module Chapter20 where

import Data.Monoid

-- use foldMap
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

-- use foldMap
product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = foldr (\n acc -> acc || n == x) False

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = getFirst . foldMap (First . Just)

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = getLast . foldMap (Last . Just)

null' :: (Foldable t) => t a -> Bool
null'  =  foldr (\_ _ -> False) True

length' :: (Foldable t) => t a -> Int
-- length' = foldr (\_ acc -> acc + 1) 0
length' = getSum . foldMap (const (Sum 1))

-- Some say this is all Foldable amounts to.
toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

-- Hint: use foldMap.
-- | Combine the elements
-- of a structure using a monoid.
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

 -- Define foldMap in terms of foldr.
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty

-- Write Foldable instances for the following datatypes.
data Constant a b = Constant b

instance Foldable (Constant a) where
  foldr f z (Constant b) = f b z

data Two a b = Two a b

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

data Three' a b = Three' a b b

instance Foldable (Three' a)  where
  foldMap f (Three' _ b b') = f b <> f b'

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' _ b b' b'') = f b <> f b' <> f b''

-- Thinking cap time. Write a filter function for Foldable types using foldMap.
filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF pf = foldMap (\x -> if pf x then pure x else mempty)
