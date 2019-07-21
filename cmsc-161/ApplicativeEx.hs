module ApplicativeEx where
-- http://cmsc-16100.cs.uchicago.edu/2016/Lectures/14-applicative.php

import Control.Applicative

shaves :: Integer -> Integer -> Bool
1 `shaves` 1 = True
2 `shaves` 2 = False
0 `shaves` x = not (x `shaves` x)
_ `shaves` _ = False

liftAN :: Applicative f => ([a] -> b) -> f [a] -> f b
liftAN f a = f <$> a

-- [ (x,y,z) | x <- [1..3], y <- [1..3], z <- [1..3] ]

sameAsCompre :: [(Int,Int,Int)]
sameAsCompre = (,,) <$> [1..3] <*> [1..3] <*> [1..3]

{-

instance Applicative [] where
        pure x = [x]
        fs <*> xs = concat $ map (\f -> map f xs) fs

map (\f -> map f [1,2,3]) [(,,)] = [(1,,), (2,,), (3,,)]
map (\f -> map f [1,2,3]) [(1,,), (2,,), (3,,)] = [[(1,1,), (1,2,), (1,3,)], ..]

-}

{- *Exercise 14.3

Consider the following two, very similar looking calculations:


    > [(+),(*)] <*> pure 2 <*> pure 3
    [5,6]
    > ZipList [(+),(*)] <*> pure 2 <*> pure 3
    ZipList {getZipList = [5,6]}
The results of these computations (modulo syntactic noise around ZipList) are identical, but the computational patterns that produce these results are quite different. Explain the difference.
-}



{- *Exercise 14.4

There are several additional operators defined to improve readability when writing programs in applicative style:


    (<$)   :: Functor f => a -> f b -> f a
    (*>)   :: Applicative f => f a -> f b -> f b
    (<*)   :: Applicative f => f a -> f b -> f a
    (<**>) :: Applicative f => f a -> f (a -> b) -> f b
We won't often use them in our examples. But, similar to our discussion of foldMap and foldr last time, it can be helpful to think about how to implement such polymorphic functions based only on their types and what we know about the typeclasses that are mentioned in their constraints.

Try implementing these functions before peeking at them in the libraries.
-}

(<<$)   :: Functor f => a -> f b -> f a
a <<$ b =  fmap (const a) b

(*>>)   :: Applicative f => f a -> f b -> f b
_ *>> b = b

(<<*)   :: Applicative f => f a -> f b -> f a
a <<* _ = a

(<<**>>) :: Applicative f => f a -> f (a -> b) -> f b
a <<**>> b = b <*> a


{- *Exercise 14.5

The function mconcat :: Monoid a => [a] -> a combines a list of Monoids. For example:


    > getFirst . mconcat . map First $ [Just 1, Just 2]
    Just 1
    > getFirst . mconcat . map First $ [Just 1, Nothing]
    Just 1
    > getFirst . mconcat . map First $ [Nothing, Just 2]
    Just 2
Implement a function


    altconcat :: Alternative f => [f a] -> f a
that combines a list of Alternatives. Once defined, you will be able to use altconcat as follows:


    > altconcat [Just 1, Just 2]
    Just 1
    > altconcat [Just 1, Nothing]
    Just 1
    > altconcat [Nothing, Just 2]
    Just 2
-}

altconcat :: Alternative f => [f a] -> f a
altconcat = foldr (<|>) empty
