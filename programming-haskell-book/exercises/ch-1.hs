module Main where

f :: [Integer] -> [Integer]
f [] = []
f (x:xs) = f ys ++ [x] ++ f zs
  where
    ys = [a | a <- xs, a <= x]
    zs = [b | b <- xs, b > x]

sumAndRest :: Num a => a -> a -> (a, a)
sumAndRest x y = let sumResult = x + y
                     lessResult = x - y
                 in
                   (sumResult, lessResult)

sum1 :: [Integer] -> Integer
sum1 x = case x of [] -> 0
                   (h:hs) -> h + sum1 hs
-- sum1 (h:hs) = h + sum1 hs

prod1 :: [Integer] -> Integer
prod1 [] = 1
prod1 (n:ns) = n * (prod1 ns)

reverse1 :: [t] -> [t]
reverse1 [] = []
reverse1 (h:t) = reverse1 t ++ [h]

main :: IO ()
main = putStrLn "Hello, World"
