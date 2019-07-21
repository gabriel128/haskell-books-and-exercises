module Sing where

-- fstString :: [Char] -> [Char]
-- fstString x = x ++ " in the rain"

-- sndString :: [Char] -> [Char]
-- sndString x = x ++ " over the rainbow"

-- sing = if (x < y) then fstString x else sndString y
--   where x = "Singin"
--         y = "Somewhere"

-- -- main :: IO ()
-- -- main = undefined


-- -- id2' :: a -> b -> b
-- -- id2' x y = y

-- -- -- (++) :: [a] -> [a] -> [a]

-- -- myMult :: Fractional a => a -> a
-- -- myMult x = (x / 3) * 5

-- -- myTake :: Int -> [Char]
-- -- myTake x = take x "hey you"

-- -- myCom :: Int -> Bool
-- -- myCom x = x > (length [1..10])

-- -- myAlph :: Char -> Bool
-- -- myAlph x = x < 'z'

-- -- example = 1

-- -- bigNum = (^) 5 $ 10
-- -- wahoo = bigNum * 10

-- -- x = print
-- -- y = print "woohoo!"
-- -- z = x "hello world"

-- -- a = 12 + b
-- -- b = 10000 * a

-- -- functionH :: [a] -> a
-- -- functionH (x:_) = x

-- -- functionC :: Ord a => a -> a -> Bool
-- -- functionC x y = if (x > y) then True else False

-- -- functionS :: (a, b) -> b
-- -- functionS (x, y) = y

-- -- myFunc :: (x -> y) -> (y -> z) -> c -> (a, x) -> (a, z)
-- -- myFunc xToY yToZ _ (a, x) = (a, (yToZ . xToY $ x))

-- -- i :: a -> a
-- -- i a = a

-- -- c :: a -> b -> a
-- -- c a _ = a

-- -- c'' :: b -> a -> b
-- -- c'' b _ = b

-- -- r :: [a] -> [a]
-- -- r = map (\x -> x)

-- -- co :: (b -> c) -> (a -> b) -> a -> c
-- -- co bToC aTob a = bToC . aTob $ a

-- -- a :: (a -> c) -> a -> a
-- -- a _ x = x

-- -- a' :: (a -> b) -> a -> b
-- -- a' f = f

-- f :: Int -> String
-- f = undefined

-- g :: String -> Char
-- g = undefined

-- h :: Int -> Char
-- h = g . f

-- data A

-- data B

-- data C

-- q :: A -> B
-- q = undefined

-- w :: B -> C
-- w = undefined

-- e :: A -> C
-- e = w . q

-- data X

-- data Y

-- data Z

-- xz :: X -> Z
-- xz = undefined

-- yz :: Y -> Z
-- yz = undefined

-- xform :: (X, Y) -> (Z, Z)
-- xform (x, y) = (xz x, yz y)

-- munge :: (x -> y) -> (y -> (w, z)) -> x -> w
-- munge xToY yToWZ = fst . yToWZ . xToY

-- addOneIfOdd :: Int -> Int
-- addOneIfOdd n = case odd n of
--   True -> f n
--   False -> n
--   where f = \n -> n + 1

-- addFive :: Int -> Int -> Int
-- -- addFive x y = (if x > y then y else x) + 5
-- addFive = \x -> \y -> (if x > y then y else x) + 5

-- mflip :: (a -> a -> b) -> a -> a -> b
-- mflip f y x = f y x

-- ft :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
-- ft (a, b, c) (d, e, f) = ((a,d), (c, f))

-- functionC :: Ord a => a -> a -> a
-- -- functionC x y = if (x > y) then x else y
-- functionC x y =
--   case (x > y) of
--     True -> x
--     False -> y

-- ifEvenAdd2 :: Int -> Int
-- -- ifEvenAdd2 n = if even n then (n+2) else n

-- ifEvenAdd2 n =
--   case even n of
--     True -> n + 2
--     False -> n

nums :: (Ord a, Num a) => a -> a
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0
