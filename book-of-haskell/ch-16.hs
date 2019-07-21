{-# LANGUAGE FlexibleInstances #-}
module ChapterFunctors where

import Control.Applicative
import Data.List (elemIndex)

-- a =  fmap (+1) $ read "[1]" :: [Int]

-- b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- c = fmap (*2) (\x -> x - 2)

-- d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

-- e :: IO Integer
-- e = let ioi = readIO "1" :: IO Integer
--         changed = fmap (fmap read (fmap ("123"++) show))  ioi
--     in fmap (*3) changed

newtype Identity a = Identity a
data Pair a = Pair a a
data Two a b = Two a b
data Three a b c = Three a b c
data Three' a b = Three' a b b
data Four a b c d = Four a b c d
data Four' a b = Four' a a a b
data Trivial = Trivial

-- class Functor' functor where
--   fmap :: (a -> b) -> functor a -> functor b

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope =  LolNope
  fmap f (Yeppers x) = Yeppers (f x)

-- data Sum a b = First a | Second b deriving (Eq, Show)

-- instance Functor (Sum a) where
--   fmap _ (First a) = First a
--   fmap f (Second a) = Second (f a)

newtype Mu f = InF { outF :: f (Mu f) }

data Sum a b = First a | Second b

instance Functor (Sum e) where
  fmap f (First a) = First a
  fmap f (Second b) = Second (f b)

data Company a b c = DeepBlue a c | Something b

instance Functor (Company e e') where
  fmap _ (Something b) = Something b
  fmap f (DeepBlue a c) = DeepBlue (a) (f c)

data More b a = L a b a | R b a b deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

newtype K a b = K a

-- instance Functor (K a) where
--   fmap _ (K a) = K a

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip $ K $ f a

data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

data LiftItOut f a = LiftItOut (f a)

instance Functor functor => Functor (LiftItOut functor) where
  fmap f (LiftItOut a) = LiftItOut (fmap f a)

data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa funct funct') = DaWrappa (fmap f funct) (fmap f funct')

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething x y) = IgnoringSomething x (fmap f y)

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious a b c) = Notorious a b (fmap f c)

-------------------------

data List a = Nil | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a list) = Cons (f a) (fmap f list)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print str a) = Print str (f a)
  fmap f (Read fn) = Read (fmap f fn)

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- y :: Maybe Integer
-- y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

-- z :: Maybe Integer
-- z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

-- tupled :: Maybe (Integer, Integer)
-- tupled = liftA2 (,) y z

-- x :: Maybe Int
-- x = elemIndex 3 [1, 2, 3, 4, 5]

-- y :: Maybe Int
-- y = elemIndex 4 [1, 2, 3, 4, 5]

-- max' :: Int -> Int -> Int
-- max' = max

-- maxed :: Maybe Int
-- maxed = max' <$> x <*> y

xs = [1, 2, 3]
ys = [4, 5, 6]

x :: Maybe Integer
x = lookup 3 $ zip xs ys

y :: Maybe Integer
y = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> ((,) <$> x <*> y)
