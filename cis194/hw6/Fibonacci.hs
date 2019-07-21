{-# LANGUAGE FlexibleInstances #-}
module Fibonacci where

import Data.Monoid

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs :: [Integer]
fibs = map fib [0..]

fibs2 :: [Integer]
fibs2 = 0 : 1 : go 0 1
  where
    go x y = x + y : go y (x + y)

-- Ex 3

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show xs = doShow 10 xs
    where
      doShow 0 (Cons a _) = "Cons " <> show a <> "...)"
      doShow n (Cons a xs') = "Cons " <> show a <> " (" <> doShow (n-1) xs'

aStream :: Stream Integer
aStream = go 0
  where
    go n = Cons n (go (n + 1))

-- Ex 4

streamToList :: Stream a -> [a]
streamToList (Cons a xs) = a : streamToList xs

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a xs) = Cons (f a) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

-- Ex 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = interleveStreams (streamRepeat 0) rulerStream
  where
    theRuler :: Integer -> Integer
    theRuler n = last . takeWhile (\k -> (2*n `mod` 2^k) == 0) $ [0..]
    rulerStream = streamMap theRuler (streamFromSeed (+1) 1)

interleveStreams :: Stream a -> Stream a -> Stream a
interleveStreams (Cons a xs) s' = Cons a (interleveStreams s' xs)

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate = streamMap (*(-1))
  (Cons a xs) + (Cons b ys) = Cons (a+b) (xs + ys)
  (Cons a0 a') * b@(Cons b0 b') = Cons (a0*b0) (streamMap (*a0) b' + (a' * b))

-- Q = (a0/b0) + x((1/b0)(A' − QB'))
instance Fractional (Stream Integer) where
  (Cons a0 a') / (Cons b0 b') = q
    where
      q = Cons (a0 `div` b0) (streamMap (`div` b0) (a' - (q*b')))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- Ex 7

type TL = Integer
type TR = Integer
type BL = Integer
type BR = Integer

data Matrix = Matrix TL TR BL BR

instance Num Matrix where
  (Matrix a b c d) * (Matrix e f g h) =
    Matrix (a*e+b*g) (a*f+b*h) (c*e+d*g) (c*f+d*h)

fibs4 :: Integer -> Integer
fibs4 0 = 0
fibs4 n = extractFib ((Matrix 1 1 1 0)^n)
  where
    extractFib (Matrix _ fn _ _) = fn
