{- A pedagogical implementation of the standard State monad -}

module State where

import Control.Applicative -- not needed with ghc 7.10ff

newtype State s a = State {runState :: s -> (a,s)}

instance Functor (State s) where
    fmap f ma = State $ \s ->
        let (a,t) = runState ma s
        in (f a,t)

instance Applicative (State s) where
    pure a = State $ \s -> (a,s)
    af <*> aa = State $ \s ->
        let (f,t) = runState af s
            (a,u) = runState aa t
        in (f a, u)

instance Monad (State s) where
    return = pure
    -- ma >>= f = State $ \s ->
    --     let (a,t) = runState ma s
    --         mb = f a
    --         (b,u) = runState mb t
    --     in (b,u)
    (State h) >>= f = State $ \s ->
        let (a,t) = h s
            (State g) = f a
        in  g t
    ma >> f = ma >>= const f

{- constructor -}

state :: (s -> (a,s)) -> State s a
state = State

{- primitive state manipulation functions -}

get :: State s s
get = State $ \s -> (s,s)

put :: s -> State s ()
put t = State $ \s -> ((),t)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((),f s)

{- evaluation of state values -}

evalState :: State s a -> s -> a
evalState ma s = fst (runState ma s)

execState :: State s a -> s -> s
execState ma s = snd (runState ma s)

initState :: State [Int] Int
initState = State $ const (0, [])

push' :: Int ->  State [Int] ()
push' x = State $ \xs -> ((), (x:xs))

pop' :: State [Int] Int
pop' = State $ \(x:xs) -> (x, xs)

type CalcState = State [Double]

pop :: CalcState Double
pop = do
    stk <- get
    case stk of
        [] -> return 0.0
        x:xs -> do
            put xs
            return x

push :: Double -> CalcState ()
push d = do
    stk <- get
    put (d:stk)


main2 = push 1 >> pop >> push 3 >> pop

main :: State [Int] Int
main = do
  push' 3
  pop'

{-

(3. []) = runState (push' 3 >> pop') []

RHS

=
runState q

let

q = (State $ \xs -> ((), (3:xs)) >> pop')
f = (\_ -> pop')
h = \xs -> ((), (3:xs)

So

q = State $ \xs -> ((), (3:xs)) >>= (\_ -> pop')

-- >>= def
(State h) >>= f = State $ \s ->
    let (a,t) = h s
        (State g) = f a
        (b,u) = g t
    in (b,u)

-- substituting by defs

State $ \s ->
    let (a,t) = \xs -> ((), (3:xs)) s
        (State g) = (\_ -> State $ \(x:xs) -> (x, xs))
        (b,u) = g t
    in (b,u)

-- Applying runState and then s = []

(1) let (a,t) = \xs -> ((), (3:xs)) []
      (State g) = State $ \(x:xs) -> (x, xs)
      (b,u) = g t
      in (b,u)

-- Since (a,t) = ((), 3:[]) = ((), [3])
-- and
  (State g) = (\_ -> State $ \(x:xs) -> (x, xs)) a
            = (\_ -> State $ \(x:xs) -> (x, xs)) ()
            = State $ \(x:xs) -> (x, xs)

Thus:

(1) => (a,t) = ((), [3])
      g = \(x:xs) -> (x, xs)
      (b,u) = g t = (\(x:xs) -> (x, xs)) [3] = (3,[])

-}


{-
push' 3 >>= push' 5

(State h) >>= f = State $ \s ->
    let (a,t) = h s
        (State g) = f a
    in  g t

f = push' 5 = State $ \xs -> ((), 5:xs)
h = \xs -> ((), (3:xs))

(a,t) = \xs -> ((), (3:xs)) [] = ((), [3])
(State g) = f a

-}
