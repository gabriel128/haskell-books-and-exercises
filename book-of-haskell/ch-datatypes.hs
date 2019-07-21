main :: IO ()
main = putStrLn "wjaa"


isPalindrome :: String -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs x
  | x < 0 = x * (-1)
  | otherwise = x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

x = (+)

f' xs = w `x` 1
  where w = length xs

f'' = \x' -> x'

h (a, _) = a
