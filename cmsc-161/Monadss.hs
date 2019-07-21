module Monadss where

import Data.Monoid
import Control.Category hiding ((.), id)
import Control.Monad.State
{- *Laws

left identity:  return a >>= f is equivalent to f a
right identity:  ma >>= return is equivalent to ma
associativity:  (ma >>= f) >>= g is equivalent to ma >>= (\x -> f x >>= g)


    instance Monad Maybe where
        return x = Just x

        Just x  >>= f = f x
        Nothing >>= _ = Nothing

- Left Identity

return a >>= f
= Just a >>= f
= f a

- Right Id

Just a >>= return
= return a
= Just a

Nothing >>= return = Nothing

- Assoc

Case Just:

(Just a >>= f) >>= g
= (f a) >>= g
= (\x -> f x >>= g) a
= Just a >>= (\x -> f x >>= g)

Case Nothing is trivial

-}

{- Exercise 15.2 Write out an instance definition for Monad Either. Yeah, you can look it up in the source code, but do it yourself. -}

data Either' a b = Left' a | Right' b deriving (Show, Eq)

instance Functor (Either' a) where
  fmap f (Right' b) = Right' (f b)
  fmap _ (Left' a) = Left' a

instance Applicative (Either' a) where
  pure = Right'
  (Right' f) <*> (Right' b) = Right' (f b)
  (Right' _) <*> (Left' a) = Left' a
  (Left' a) <*> _ = Left' a


instance Monad (Either' a) where
  return = pure
  (Right' b) >>= f = f b
  (Left' a) >>= _ = Left' a

{-*

left identity:  return a >>= f is equivalent to f a
right identity:  ma >>= return is equivalent to ma
associativity:  (ma >>= f) >>= g is equivalent to ma >>= (\x -> f x >>= g)

- l.i

return a >>= f
= Right' a >>= f
= f a

- r.i

Right' a >> return
= return a
= Right' a


(Right' a >>= f) >>= g
= (f a) >>= g
= (\x -> f x >>= g) a
= Right' a >>= (\x -> f x >>= g)

-}

{-
Exercise 15.4 We saw earlier that every Applicative must be a Functor, and indeed that the link between Applicative and Functor is so strong that if we can define pure and (<*>) for a type M without mentioning fmap, then we implement the Functor instance “for free” via:


    instance Functor M where
        fmap f x = pure f <*> x
or even


    instance Functor M where
        fmap = (<*>) . pure
in much the same way, if we can implement return and (>>=) without using pure or (<*>), there's also a “free” instance implementation of Applicative available. Write it. Once you've done so, take a look at Control.Monad.ap and its implementation.

-}

join' :: Monad m => m (m a) -> m a
join' m = m >>= id


{- Proof
join' Just (Just x)
= Just (Just x) >>= id

= id (Just x)  - by {Just x  >>= f = f x}
= Just x

-}

buildIndexed :: String -> String
buildIndexed = unlines . zipWith (\x y -> show x <> ". " <> y) [1..10] . lines


main :: IO ()
main = readFile "numbers.txt" >>= (buildIndexed >>> putStrLn)



-- push :: a -> State [Int] ()
-- push a = state $ \s -> ((), (a:[]))
