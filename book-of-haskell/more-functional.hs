module MoreFunctional where

dodgy :: Int -> Int -> Int
dodgy x y = x + y * 10

oneIsOne :: (Int -> Int)
oneIsOne = dodgy 1

oneIsTwo :: Int -> Int
oneIsTwo = flip dodgy 2

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9= 'A'
  | y >= 0.7 = 'C'
  | y >= 0.8 = 'B'
  | y >= 0.59 = 'D'
  | otherwise = 'F'
  where y = x / 100

pal :: Eq a => [a] -> Bool
pal xs
  | xs == reverse xs = True
  | otherwise = False

numbers :: (Ord a, Eq a, Num a) => a -> a
numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1
  | otherwise = 1

f5 :: a -> a
f5 x = x

tensDigit :: Integral a => a -> a
tensDigit x = fst $ x `divMod` 100

foldBool :: a -> a -> Bool -> a
foldBool x y p
  | p = x
  | otherwise = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)


roundTrip :: (Show a, Read b) => a -> b
roundTrip  = read . show

main :: IO ()
main = do
  print ((roundTrip 4) :: Int)
  print (id 4)
