module Proofs where

{-|
Exercise A In Chapter 3 we defined multiplication on natural numbers. The following definition is slightly different:

  mult :: Nat -> Nat -> Nat
  mult Zero y = Zero
  mult (Succ x) y = mult x y + y

Prove that mult (x+y) z = mult x z + mult y z. You can use only the facts that x+0 = x and that (+) is associative.
That means a long think about which variable x, y or z is the best one on which to do the induction.


Proof.

x = 0 =>

mult (0+y) z = mult 0 z + mult y z
mult y z  = 0 + mult y z

mult (n+y) z = mult n z + mult y z => mult (n+1+y) z = mult (n+1) z + mult y z

LHS:

mult (n+1+y) z = mult (n+y) z + z  By Def


RHS:

mult (n+1) z + mult y z = mult n z + mult y z + z By Def

By assumption we know

 mult (n+y) z = mult n z + mult y z
={By adding z both sides}
 mult (n+y) z + z = mult n z + mult y z + z

so LHS = RHS
-}

{-|
Exercise B

Prove that reverse (xs ++ ys) = reverse ys ++ reverse xs for all finite lists xs and ys.

reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

-------

You may assume that (++) is associative.

base case, xs = []

reverse ([] ++ ys) = reverse ys ++ reverse []
={(++).1} = {reverse.1}
reverse ys = (reverse ys) ++ []
={k ++ [] = k}
reverse ys = reverse ys

Assume: reverse (xs ++ ys) = reverse ys ++ reverse xs
Prove: reverse ((x:xs) ++ ys) = reverse ys ++ reverse (x:xs)

So,

reverse ((x:xs) ++ ys) = reverse ys ++ reverse (x:xs)

= {(++).2} = {reverse.2}

reverse (x:(xs ++ ys)) = reverse ys ++ (reverse xs ++ [x])

={reverse.2} = {}

reverse (xs ++ ys) ++ [x] = reverse ys ++ (reverse xs ++ [x])

= {I.H} = {(++) is associative}

(reverse ys ++ reverse xs) ++ [x] = (reverse ys ++ reverse xs) ++ [x]


QED. Mathafucka

-}

{-|
Exercise C
Recall our friends Eager Beaver and Lazy Susan from
-}


{-|
Exercise D in Chapter 2.
Susan happily used the expression head . map f, while Beaver would probably prefer f . head.
Wait a moment! Are these two expressions equal? Carry out an induction proof to check.
Exercise D Recall the cartesian product function
cp :: [[a]] -> [[a]] from the previous chapter.

Give a definition of the form cp = foldr f e
for suitable f and e.
You can use a list comprehension for the definition of f if you like. The rest of this exercise concerns the proof of the identity length .
cp = product . map length
where product returns the result of multiplying a list of numbers.
1.Using the fusion theorem, express length.cp as an instance of foldr.
2.Express map length as an instance of foldr.
3.Using the fusion theorem again, express product . map length as an instance of foldr.
4.Check that the two results are identical. If they aren’t, your definition of cp was wrong.
-}

{-|
Exercise E

The first two arguments of foldr are replacements for the constructors
(:) :: a -> [a] -> [a]
[]:: [a] of lists.

A fold function can be defined for any data type: just give replacements for the constructors of the data type.

For example, consider data Either a b = Left a | Right b To define a fold for Either we have to give replacements for
Left :: a -> Either a b
Right :: b -> Either a b

That leads to

foldE :: (a -> c) -> (b -> c) -> Either a b -> c
foldE f g (Left x) = f x
foldE f g (Right x) = g x The

type Either is not a recursive data type and foldE is not a recursive function.
In fact foldE is a standard prelude function, except that it is called either not foldE.
Now define fold functions for

data Nat = Zero | Succ Nat
data NEList a = One a | Cons a (NEList a)

The second declaration introduces nonempty lists. What is wrong with the Haskell definition of foldr1?
-}

{-|
Exercise F

Prove that

foldl f e xs = foldr (flip f) e (reverse xs) for all finite lists xs.

Also prove that foldl (@) e xs = foldr (<>) e xs for all finite lists xs,
provided that (x <> y) @ z = x <> (y @ z) e @ x = x <> e
-}

{-|
Exercise G

Using foldl f e (xs ++ ys) = foldl f (foldl f e xs) ys
      foldr f e (xs ++ ys) = foldr f (foldr f e ys) xs

prove that

foldl f e . concat = foldl (foldl f) e
foldr f e . concat = foldr (flip (foldr f)) e
-}

{-|

Exercise H
Mathematically speaking, what is the value of sum (scanl (/) 1 [1..]) ?
-}

{-|
Exercise I Calculate the efficient definition of scanr from the specification scan r f e = map (foldr f e) . tails
-}

{-|

Exercise J Consider the problem of computing

mss :: [Int] -> Int
mss = maximum . map sum . subseqs

where subseqs returns all the subsequences of a finite list, including the list itself:

subseqs :: [a] -> [[a]]
subseqs []= [[]]
subseqs (x:xs) = xss ++ map (x:) xss
  where xss = subseqs xs

Find a more efficient alternative for mss.
-}

{-|

Exercise K

This question is in pieces.

1.The function takePrefix p applied to a list xs returns the longest initial segment of xs that satisfies p.
Hence
takePrefix :: ([a] -> Bool) -> [a] -> [a]

What are the values of the following expressions?

takePrefix nondec
[1,3,7,6,8,9]
takePrefix (all even)
[2,4,7,8]

Complete the right-hand side of
takePrefix (all p) = ...

Give a definition of takePrefix in terms of standard functions, including inits. We will return to takePrefix in the final part of this question.

2.The functions one and none are defined by the equations one x= [x] none x = []

Complete the right-hand side of the following identities:

none . f = ...
map f . none = ...
map f . one= ...

3.Recall that fork (f,g) x = (f x,g x).

Complete the identities

fst . fork (f,g) = ...
snd . fork (f,g) = ...
fork (f,g) . h= ...

4. Define

test p (f,g) x = if p x then f x else g x

Complete the right-hand sides of test
p (f,g) . h = ...
h . test p (f,g) = ...

The function filter can be defined by

filter p = concat . map (test p (one,none))

Using the identities above, together with other standard identities, prove using equational reasoning that
filter p = map fst . filter snd . map (fork (id,p))
(Hint: as always in calculations, start with the more complicated side.)

5.Recall the standard prelude functions curry and uncurry from the answer to Exercise K in Chapter 4:

curry :: ((a,b) -> c) -> a -> b -> c
curry f x y = f (x,y)

uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f (x,y) = f x y

Complete the right-hand side of

map (fork (f,g)) = uncurry zip . (??)

6. Returning to takePrefix, use equational reasoning to calculate an efficient program for the expression
takePrefix (p . foldl f e) that requires only a linear number of applications of f .
-}
