module ThinkingFunctionally where

type Interval = (Integer, Integer)

isqrt :: Float -> Integer
isqrt x = fst (until unit (shrink x) (bound x))
  where unit (m,n) = m+1 == n

shrink :: Float -> Interval -> Interval
shrink x (m,n) = if (p*p) `leq` x then (p,n) else (m,p)
  where p = (m+n) `div` 2

bound :: Float -> Interval
bound x = (0, until above (*2) 1)
  where above n = x `lt` (n*n)

leq :: Integer -> Float -> Bool
x `leq` y = fromInteger x <= y

lt :: Float -> Integer -> Bool
x `lt` y = x > fromInteger y


sqrt :: Float -> Float
sqrt x = until goodenough improve x
  where goodenough y = abs (y*y-x) < eps*x
        improve y = (y+x/y)/2
        eps= 0.000001

-- Give an explicit instance of Nat as a member of the type class Ord.
-- Hence construct a definition of divMod :: Nat -> Nat -> (Nat,Nat)

data Nat = Zero | Succ Nat deriving (Show, Eq)

instance Num Nat where
  m + Zero = m
  Zero + m = m
  m + Succ n = Succ (m + n)
  m * Zero = Zero
  Zero * m = Zero
  m * Succ n = m * n + m
  abs n = n

instance Ord Nat where
  Zero < Succ _ = True
  _ < Zero = False
  Succ m < Succ n = m < n

divMod' :: Nat -> Nat -> (Nat,Nat)
divMod' m n =  if m < n then (Zero, m) else (Succ q, r)
  where (q,r) = divMod' (m-n) n

congr :: [Integer]
congr = map ((+1) . (`mod` 5)) [0..100]

-- List Chapter
