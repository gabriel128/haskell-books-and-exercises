module Analysis where

data Nat = Zero | Succ Nat deriving (Show, Eq, Ord)

instance Num Nat where
  m + Zero = m
  Zero + m = m
  m + Succ n = Succ (m + n)
  m * Zero = Zero
  Zero * m = Zero
  m * Succ n = m * n + m
  abs n = n


seriesSum :: Integer -> (Integer, [Integer])
seriesSum n =  foldr (\x (s, sn) -> ((s + x), s : sn)) (0, []) [1..n]
