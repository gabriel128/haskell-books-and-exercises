module MonoidFoldable where

import Data.Monoid

newtype First' a = First'  { getFirst :: Maybe a } deriving Show
newtype Last' a = Last'  { getLast :: Maybe a } deriving Show

instance Monoid (First' a) where
  mempty = First' Nothing
  a `mappend` _ = a

instance Monoid (Last' a) where
  mempty = Last' Nothing
  r `mappend` Last' Nothing = r
  _ `mappend` r = r

f1 :: [Bool] -> [Bool]
f1 = id

f2 :: Int -> Sum Int
f2 = Sum

f3 :: Double -> Product Double
f3 = Product

f4 :: Bool -> Any
f4 = Any

f5 :: Bool -> All
f5 = All

f6 :: Maybe String -> Maybe String
f6 = id

f7 :: Maybe String -> First String
f7 = First

-- *Exercise 13.4

-- Implement Foldable instances for each of the following types by providing implementations (only) for foldMap:

-- []
-- Maybe
-- Either a
-- (,) a

data List' a = Empty | Cons a (List' a) deriving Show

data Maybe' a = Nothing' | Just' a

data Either' a b = Left' a  | Right' b

instance Monoid (List' a) where
  mempty = Empty
  Empty `mappend` a = a
  (Cons a as) `mappend` b = Cons a (as <> b)

instance Foldable List' where
  foldMap _ Empty = mempty
  foldMap f (Cons a rest)= f a <> foldMap f rest

instance Foldable Maybe' where
  foldMap _ Nothing' = mempty
  foldMap f (Just' a) = f a

instance Foldable (Either' a) where
  foldMap _ (Left' _) = mempty
  foldMap f (Right' b) = f b

-- instance Foldable ((,) a) where
--   foldMap f (_, b) = f b

-- *Exercise 13.5

-- Define a new wrapper type for pairs called FstPair using newtype, and define a Foldable instance for it that corresponds to folding over the first element of pairs, rather than the second. Your solution should result in the following behavior:


--     > foldMap (++ "!") ("hello", "greetings")
--     "greetings!"
--     > foldMap (++ "!") (FstPair ("hello", "greetings"))
--     "hello!"

newtype FstPair b a = FstPair (a, b)

instance Foldable (FstPair a) where
  foldMap f (FstPair (a, _)) = f a



-- Exercise 13.6 It's often very nice to have functions that can convert from one data representation to another. For container types (like our BinaryTree), it's sometimes nice to be able repackage the contained elements into a list. The Foldable class includes a member function (with a default implementation) called


--     toList :: Foldable t => t a -> [a]
-- that does exactly this. Since we've created an instance of Foldable for BinaryTree, this means that


--     > toList $ (Leaf 1 `Node` Leaf 2) `Node` (Leaf 3 `Node` Leaf 4)
--     [1,2,3,4]
-- Works as expected. Provide your own implementation of toList, and compare it to the implementation in Data.Foldable.

toList :: Foldable t => t a -> [a]
toList = foldr (:) []
