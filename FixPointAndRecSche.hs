module FixPointAndRecSche where

-- fix'' :: ((b -> c) -> b -> c) -> b -> c
-- fix'' f = let x = f x in x
import Unsafe.Coerce
import Control.Monad.Free

cosfix = fix(\f b -> if cos b == b then b else f (cos b))


multfix = fix' (\f b -> if 2 * b == b then b else f (2 * b))

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

fix' :: (a -> a) -> a
fix' f = let x = f x in x

fix :: (a -> a) -> a
-- fix f = let x = f x in x
fix f = (\x -> unsafeCoerce x x ) (\x -> f (unsafeCoerce x x))

fixmap :: (a -> b) -> [a] -> [b]
fixmap g = fix' (\f b@(x:xs) -> if null b then [] else g x : f xs)



{-

fix' f = {let x = (\f b@(x:xs) -> if null b then [] else g x : f xs) x} in (\f b@(x:xs) -> if null b then [] else g x : f xs) x
=
(\f b@(x:xs) -> if null b then [] else g x : f xs) ((\f b@(x:xs) -> if null b then [] else g x : f xs) x)
=
(\b@(x:xs) -> if null b then [] else g x : ((\f b@(x:xs) -> if null b then [] else g x : <more Xs here> xs) x) xs)
= {add vals}
(\b@(x:xs) -> if null b then [] else g x : ((\f b@(x:xs) -> if null b then [] else g x : <more Xs here> xs) x) xs) [1]
=fix ("hello"++)
 if null b then [] else g x : ((\f b@(x:xs) -> if null b then [] else g 1 : <more Xs here> xs) x) [])
=
2 : ((\f b@(x:xs) -> if null b then [] else g x : <more Xs here> xs) x) [])
=
2 : []


fix (2*)
= {let x = 2 *  x} = 2 * x
= {let x = 2 *  x} = 2 * (2 * x)
= {let x = 2 *  x} = 2 * (2 * (2 * x))

-}

-- newtype Fix f = Fix (f (Fix f))

data FixE f e = Fix (f (FixE f e)) | Throw e

data Toy b next = Output b next | Bell next | Done deriving Show

catch :: (Functor f) => FixE f e1 -> (e1 -> FixE f e2) -> FixE f e2
catch (Fix x) f = Fix (fmap (flip catch f) x)
catch (Throw e) f = f e

instance Functor (Toy b) where
    fmap f (Output x next) = Output x (f next)
    fmap f (Bell     next) = Bell     (f next)
    fmap f  Done           = Done

data IncompleteException = IncompleteException

-- output 'A'
-- throw IncompleteException
-- subroutine = Fix (Output 'A' (Throw IncompleteException)) :: FixE (Toy Char) IncompleteException

-- try {subroutine}
-- catch (IncompleteException) {
--     bell
--     done
-- }
-- program = subroutine `catch` (\_ -> Fix (Bell (Fix Done)) :: FixE (Toy Char) e

data Terminal a
  = GetLine (String -> a)
  | PrintLine String a
  deriving Functor

type TerminalM = Free Terminal

liftF :: Functor f => f a -> Free f a
liftF = Free . fmap return

getLine' :: TerminalM String
getLine' = Free (GetLine return)

printLine :: String -> TerminalM ()
printLine str = liftF (PrintLine str ())

myProgram :: TerminalM ()
myProgram = do
  a <- getLine'
  b <- getLine'
  printLine (a ++ b)
