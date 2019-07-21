import Prelude hiding (sum, take, last)

main :: IO ()
main = putStrLn ""

-- 1. How does the recursive version of the factorial function behave if applied to a negative argument, such as (-1)?
--    Modify the definition to prohibit negative arguments by adding a guard to the recursive case.
fact :: Int -> Int
fact 1 = 1
fact n | n == 1 = 1
       | n < 0 = 0
       | otherwise = n * fact (n - 1)

-- 2.Define a recursive function sumdown :: Int -> Int that returns the sum of the non-negative integers from a given value down to zero. For example, sumdown 3 should return the result 3+2+1+0 = 6.
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

-- 3.Define the exponentiation operator ^ for non-negative integers using the same pattern of recursion as the multiplication operator *, and show how the expression 2 ^ 3 is evaluated using your definition.

(^>) :: Int -> Int -> Int
(^>) n 1 = n
(^>) n exponent = n * (n ^> (exponent - 1))

-- 4.Define a recursive function euclid :: Int -> Int -> Int that implements Euclid’s algorithm for calculating the greatest common divisor of two non-negative integers: if the two numbers are equal, this number is the result; otherwise, the smaller number is subtracted from the larger, and the same process is then repeated. For example: > euclid 6 27 3

euclid :: Int -> Int -> Int
euclid x y | x == y = x
           | x < y = euclid x (y - x)
           | x > y  = euclid (x- y) y

-- 5.Using the recursive definitions given in this chapter, show how length [1,2,3], drop 3 [1,2,3,4,5], and init [1,2,3] are evaluated.

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length'(xs)

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' n (_:xs) = drop' (n - 1) xs

init' :: [a] -> [a]
init' [] = []
init' (x:xs) = x : init' xs

-- 6.Without looking at the definitions from the standard prelude, define the following library functions on lists using recursion.
   -- a.Decide if all logical values in a list are True: and :: [Bool] -> Bool
   -- b.Concatenate a list of lists: concat :: [[a]] -> [a]
   -- c.Produce a list with n identical elements: replicate :: Int -> a -> [a]
   -- d.Select the nth element of a list: (!!) :: [a] -> Int -> a
   -- e.Decide if a value is an element of a list: elem :: Eq a => a -> [a] -> Bool Note: most of these functions are defined in the prelude using other library functions rather than using explicit recursion, and are generic functions rather than being specific to the type of lists.

-- a.
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x == and' xs
-- b.
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x !! 0 : concat' xs

-- c.
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x  = x : replicate' (n - 1) x

-- d.
(!!>) :: [a] -> Int -> a
(!!>) (x:xs) n | n == 0  = x
               | otherwise = (!!>) xs (n-1)

-- e.
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' n (x:xs) | n == x = True
               | otherwise = elem' n xs

-- 7.Define a recursive function merge :: Ord a => [a] -> [a] -> [a] that merges two sorted lists to give a single sorted list. For example: > merge [2,5,6] [1,3,4] [1,2,3,4,5,6] Note: your definition should not use other functions on sorted lists such as insert or isort, but should be defined using explicit recursion.

merge' :: Ord a => [a] -> [a] -> [a]
merge' [] _ = []
merge' _ [] = []
merge' (x:xs) (y:ys) | x < y = x : y : merge' xs ys
                     | y < x = y : x : merge' xs ys

-- 8.Using merge, define a function msort :: Ord a => [a] -> [a] that implements merge sort, in which the empty list and singleton lists are already sorted, and any other list is sorted by merging together the two lists that result from sorting the two halves of the list separately. Hint: first define a function halve :: [a] -> ([a],[a]) that splits a list into two halves whose lengths differ by at most one.

-- 9. Using the five-step process, construct the library functions that:
-- a.calculate the sum of a list of numbers;
-- b.take a given number of elements from the start of a list;
-- c.select the last element of a non-empty list.

sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs

take :: Int -> [a] -> [a]
take 0 xs = []
take n (x:xs) = x : take (n-1) xs

last :: [a] -> a
last (x:[]) = x
last (_:xs) = last xs
