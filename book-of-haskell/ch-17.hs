module ChapterApplicative where



import Data.Monoid
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

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

-- xs = [1, 2, 3]
-- ys = [4, 5, 6]

-- x :: Maybe Integer
-- x = lookup 3 $ zip xs ys

-- y :: Maybe Integer
-- y = lookup 2 $ zip xs ys

-- summed :: Maybe Integer
-- summed = sum <$> ((,) <$> x <*> y)

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure = const (Constant mempty)
  Constant x <*> Constant y = Constant $ x <> y

data Guy = Guy String Integer deriving Show

buildPerson :: Maybe Guy
buildPerson = Guy <$> Just "whaa" <*> Just 1

-- aaa = const <$> Just "Hello" <*> Just "World"

-- bbb :: Maybe (Integer, Integer, String, [Integer])
-- bbb = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Just [1, 2, 3]

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a list) = Cons (f a) (fmap f list)


instance Applicative List where
  pure = flip Cons Nil
  list <*> list' = flatMap (<$> list') list

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f = concat' . fmap f

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = oneof [Cons <$> arbitrary <*> arbitrary, return Nil]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

-- main :: IO ()
-- main = do
--   quickBatch . functor $ Cons ('a', 'b', 'c') Nil
--   quickBatch . applicative $ Cons ('a', 'b', 'c') Nil
  -- quickBatch $ functor $ ZipList' $ Cons ('a', 'b', 'c') Nil
  -- quickBatch $ applicative $ ZipList' $ Cons ('a', 'b', 'c') Nil

take' :: Int -> List a -> List a
take' n Nil = Nil
take' n (Cons a list) = Cons a (take' (n - 1) list)

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l

          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure = ZipList' . repeat'
  (<*>) = undefined

repeat' :: a -> List a
repeat' a = Cons a (repeat' a)

data Validation e a = Failure' e | Success' a deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure' e) = Failure' e
  fmap f (Success' a) = Success' $ f a

instance Monoid e => Applicative (Validation e) where
  pure = Success'
  (Success' f) <*> (Success' a) = Success' (f a)
  (Success' _) <*> (Failure' e) = Failure' e
  (Failure' e) <*> (Success' _) = Failure' e
  (Failure' e) <*> (Failure' e') = Failure' (e <> e')

instance (Arbitrary a, Arbitrary e) => Arbitrary (Validation e a) where
  arbitrary = oneof [Success' <$> arbitrary, Failure' <$> arbitrary]

instance (Eq a, Eq e) => EqProp (Validation e a) where
  (=-=) = eq

-- main :: IO ()
-- main =
--    -- quickBatch . functor $ Success' ('a', 'b', 'c')
--    quickBatch . applicative $ (Success' ('a', 'b', 'c') :: Validation String (Char, Char, Char))


data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure a = Pair a a
  (Pair f f') <*> (Pair a a') = Pair (f a) (f' a')

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b)  = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (Two a f) <*> (Two a' b) = Two (a <> a') (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = liftA2 Two arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (Three a b f) <*> (Three a' b' c) = Three (a <> a') (b <> b') (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Monoid a => Applicative (Three' a) where
  pure b = Three' mempty b b
  (Three' a f f') <*> (Three' a' b b') = Three' (a <> a') (f b) (f' b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = liftA3 Three' arbitrary arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

data Four a b c d = Four a b c d deriving (Show, Eq)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c $ f d

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (Four a b c f) <*> (Four a' b' c' d) = Four (a <> a') (b <> b') (c <> c') (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)  => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

data Four' a b = Four' a a a b deriving (Show, Eq)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Monoid a) => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (Four' a b c d) <*> (Four' a' b' c' d') = Four' (a <> a') (b <> b') (c <> c') (d d')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

-- Use liftA3 to combine all the strings
combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

main :: IO ()
main = do
  quickBatch . functor $ Pair ("a", "b", "c") ("a", "b", "c")
  quickBatch . applicative $ Pair ("a", "b", "c") ("a", "b", "c")
  quickBatch . functor $ Two ("a", "b", "c") ("a", "b", "c")
  quickBatch . applicative $ Two ("a", "b", "c") ("a", "b", "c")
  quickBatch . functor $ Three ("a", "b", "c") ("a", "b", "c") ("a", "b", "c")
  quickBatch . applicative $ Three ("a", "b", "c") ("a", "b", "c") ("a", "b", "c")
  quickBatch . functor $ Three' ("a", "b", "c") ("a", "b", "c") ("a", "b", "c")
  quickBatch . applicative $ Three' ("a", "b", "c") ("a", "b", "c") ("a", "b", "c")
  quickBatch . functor $ Four ("a", "b", "c") ("a", "b", "c") ("a", "b", "c") ("a", "b", "c")
  quickBatch . applicative $ Four ("a", "b", "c") ("a", "b", "c") ("a", "b", "c") ("a", "b", "c")
  quickBatch . functor $ Four' ("a", "b", "c") ("a", "b", "c") ("a", "b", "c") ("a", "b", "c")
  quickBatch . applicative $ Four' ("a", "b", "c") ("a", "b", "c") ("a", "b", "c") ("a", "b", "c")
