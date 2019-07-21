module ChLists where

-- Exercise B You want to produce an infinite list of all distinct pairs (x, y) of natural numbers.

allPairs :: [(Integer, Integer)]
allPairs = [(x,y) | x <- [0, 2..], y <- [1, 3..]]

-- C- Give a definition of the function disjoint :: (Ord a) => [a] -> [a] -> Bool
-- that takes two lists in ascending order, and determines whether or not they have an element in common.

disjoint :: (Ord a) => [a] -> [a] -> Bool
disjoint xs ys = not (null [(x,y) | x <- xs, y <- ys, x == y])

-- Exercise D Under what conditions do the following two list
-- comprehensions deliver the same result? [e | x <- xs, p x, y <- ys] [e | x <- xs, y <- ys, p x]
-- Compare the costs of evaluating the two expressions.

-- Always

--Exercise E ..it is the first number that can be
-- expressed as two cubes in essentially different ways: 1^3 + 12^3 = 9^3 + 10^3 = 1729.
--  Write a program to find the second such number. In fact, define a function that returns a list of all
-- essentially different quadruples (a, b, c, d) in the range 0 < a, b, c, d ≤ n such that a3 +b3 = c3 +d3.
-- I suggest using a list comprehension, but only after thinking carefully about what it means to say two quadruples are
-- essentially different. After all, a3 + b3 = c3 + d3 can be written in eight different ways.

raj :: Integer -> [(Integer, Integer, Integer, Integer)]
raj n = cuadruples
  where
    cuadruples :: [(Integer, Integer, Integer, Integer)]
    cuadruples = [(a,b,c,d) | a <- [1..n], b <- [a..n], c <- [(a+1)..n], d <- [c..n], a^3 + b^3 == c^3 + d^3]


-- Exercise F The dual view of lists is to construct them by adding elements to the end of the list:
-- data List a = Nil | Snoc (List a) a Snoc is, of course, Cons backwards. With this view of lists [1, 2, 3]
-- would be represented by Snoc (Snoc (Snoc Nil 1) 2) 3 Exactly the same information is provided by the two views but it is organised differently.
-- Give the definitions of head and last for the snoc-view of lists, and define two functions
-- toList :: [a] -> List a fromList :: List a -> [a] for converting efficiently from one view of lists to the other.
-- (Hint: reverse is efficient, taking linear time to reverse a list.)

data List a = Nil | Snoc (List a) a deriving (Eq, Show)

head' :: List a -> Maybe a
head' Nil = Nothing
head' (Snoc Nil a) = Just a
head' (Snoc a _) = head' a
-- head (Snoc (Snoc (Nil) a)) =  a

last' :: List a -> Maybe a
last' Nil = Nothing
last' (Snoc _ a) = Just a

toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = Snoc (toList xs) x

fromList :: List a -> [a]
fromList Nil = []
fromList (Snoc xs a) = reverse $ a : fromList xs

-- Exercise G How much space is required to evaluate length xs? Consider the following alternative definition of length:
-- length :: [a] -> Int length xs = loop (0,xs) where loop (n,[]) = n loop (n,x:xs) = loop (n+1,xs)
-- Does the space requirement change? Does it change if we switched to eager evaluation?
-- These questions are taken up in much more detail in Chapter 7.
length' :: [a] -> Int
length' xs = loop (0,xs)
  where
    loop (n, []) = n
    loop (n, x:xs) = loop (n+1,xs)


-- Exercise H The prelude function take n takes the first n elements of a list, while drop n drops the first n elements.
-- Give recursive definitions for these functions. What are the values of take 0 undefined take undefined [] according to your definition?
-- A more tricky question: can you find a definition in which both the above expressions have the value []? If not, why not?
-- Which of the following equations are valid for all integers m and n? You don’t have to justify your answers,
-- just try to understand what they claim to say.
--   take n xs ++ drop n xs = xs = true
-- take m . drop n = drop n . take (m+n) = true
-- take m . take n = take (m `min` n) = true
-- drop m . drop n = drop (m+n) = true
-- The standard prelude function splitAt n can be defined by splitAt n xs = (take n xs,drop n xs)
-- Though clear, the above definition is maybe a little inefficient as it involves processing xs twice.
-- Give a definition of splitAt that traverses the list only once

take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 _ = []
take' n (x:xs) = x : take' (n-1) xs

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 xs = xs
drop' n (_:xs) = drop' (n-1) xs

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' _ [] = ([], [])
splitAt' n xs = loop n xs ([], [])
  where
    loop 0 xs' (ys, _) = (reverse ys, xs')
    loop n (x':xs') (ys, ys') = loop (n-1) xs' (x' : ys, ys')


-- Exercise I Which of the following statements about the equation
-- map (f . g) xs = map f (map g xs) do you agree with, and which do you disagree with (again, no justification is required)?
-- 1.It’s not true for all xs; it depends on whether xs is a finite list or not.
-- 2.It’s not true for all f and g; it depends on whether f and g are strict functions or not.
-- 3.It’s true for all lists xs, finite, partial or infinite, and for all f and g of the appropriate type.
-- In fact map (f . g) = map f . map g is a much neater alternative.
-- 4.It looks true, but it has to be proved so from the definition of map and the definition of functional composition.
-- 5.Used right-to-left, it expresses a program optimisation: two traversals of a list are replaced by one.
-- 6.It’s not an optimisation under lazy evaluation because map g xs is not computed in its entirety before evaluation of map f on the result begins.
-- 7.Whether or not it is computed in pieces or as a whole, the right-hand side does produce an intermediate list, while the left-hand side doesn’t.
-- It is a rule for optimising a program even under lazy evaluation.

-- Agree with: 3, 4, 5


-- Exercise J Here are some equations; at least one of them is false. Which are the true ones, and which are false?
-- Once again, you do not have to provide any justification for your answers, the aim is just to
-- look at some equations and appreciate what they are saying.
-- map f . take n = take n . map f = T
-- map f . reverse = reverse . map f = T
-- map f . sort = sort . map f = false
-- map f . filter p  = map fst . filter snd . map (fork (f,p)) = T
-- filter (p . g) = map (invertg) . filter p . map g = T
-- reverse . concat = concat . reverse . map reverse = F
-- filter p . concat = concat . map (filter p) = T
-- In the fifth equation assume invertg satisfies invertg . g = id. The function fork in the fourth equation is defined by


-- Exercise K Define unzip and cross by
-- What are the types of these functions? Prove by simple equational reasoning that
-- cross (map f, map g) . unzip = unzip . map (cross (f,g))
-- You can use the functor laws of map and the following rules:
-- cross (f,g) . fork (h,k) = fork (f . h,g . k)
-- fork (f,g) . h = fork (f . h,g . h)
-- fst . cross (f,g) = f . fst
-- snd . cross (f,g) = g . snd

fork :: (a -> b,a -> c) -> a -> (b,c)
fork (f,g) x = (f x, g x)

unzip :: [(a, b)] -> ([a], [b])
unzip = fork (map fst, map snd)

cross :: (a -> b, c -> d) -> (a, c) -> (b, d)
cross (f,g) = fork (f . fst, g . snd)


-- Exercise L Continuing from the previous exercise, prove that
-- cross (f,g) . cross (h,k) = cross (f . h,g . k)
-- We also have cross (id,id) = id (Why?).
-- So it looks like cross has functor like properties, except that it takes a pair of functions. Yes, it’s a bifunctor.
-- That suggests a generalisation:
-- class Bifunctor p where
--    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
 --
-- The arguments to bimap are given one by one rather than paired.
-- Express cross in terms of bimap for the
-- instance Pair of Bifunctor, where type Pair a b = (a,b)
-- Now consider the data type data Either a b = Left a | Right b Construct the instance Either of Bifunctor.

class Bifunctor p where
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d

instance Bifunctor (,) where
  bimap f g (a, b) = (f a, g b)

instance Bifunctor Either where
  bimap f _ (Left a) = Left (f a)
  bimap _ g (Right b) = Right (g b)
