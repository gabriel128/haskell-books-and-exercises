module Arith3Broken where

main :: IO ()
main = do
  print(1 + 2)
  putStrLn(show 10)
  print (negate (-1))
  print ((+) 0 blah)
    where blah = negate 1

data Woot
data Blah

f :: Woot -> Blah
f = undefined

g :: (Blah, Woot) -> (Blah, Blah)
g (x, _) =  (x, x)
