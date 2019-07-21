-- 1 "What are the types of the following values?
--  [’a’,’b’,’c’] :: [Char]
--  (’a’,’b’,’c’) :: (Char, Char, Char)
--  [(False,’O’),(True,’1’)] :: [(Bool, Char)]
--  ([False,True],[’0’,’1’]) :: ([Bool], [Char])
--  [tail, init, reverse] :: [[a] -> [a]]

--  2.Write down definitions that have the following types; it does not matter what the definitions actually do as long as they are type correct.
bools :: [Bool]
bools = [False, True]

nums :: [[Int]]
nums = [[1, 2, 3, 4]]

add :: Int -> Int -> Int -> Int
add a b c = a + b + c

copy :: a -> (a,a)
copy a = (a, a)

apply :: (a -> b) -> a -> b
apply f a = f a

-- 3.What are the types of the following functions?
second :: [t] -> t
second xs = head (tail xs)

swap :: (t, t1) -> (t1, t)
swap (x,y) = (y,x)

pair :: t -> t1 -> (t, t1)
pair x y = (x,y)

double :: Num t => t -> t
double x = x*2

palindrome :: Eq t => [t] -> Bool
palindrome xs = reverse xs == xs

twice :: (t -> t) -> t -> t
twice f x = f (f x)
-- Hint: take care to include the necessary class constraints in the types if the functions are defined using overloaded operators.

--  4.Check your answers to the preceding three questions using GHCi.
-- done

--  5.Why is it not feasible in general for function types to be instances of the Eq class? When is it feasible?
--  Hint: two functions of the same type are

main :: IO ()
main = putStrLn "Hello, World"
