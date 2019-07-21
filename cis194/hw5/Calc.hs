{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Calc where

-- import ExprT
import Parser
import qualified StackVM as VM
import qualified Data.Map as M

newtype MinMax = MinMax Integer deriving (Eq, Show)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

class Expr a where
  mul :: a -> a -> a
  add :: a -> a -> a
  lit :: Integer -> a

-- instance Expr ExprT where
--   mul = Mul
--   add = Add
--   lit = Lit

instance Expr Integer where
  mul a b = a * b
  add a b = a + b
  lit a = a

instance Expr Bool where
  mul a b = a && b
  add a b = a || b
  lit a
    | a > 0 = True
    | otherwise = False

instance Expr MinMax where
  mul (MinMax a) (MinMax b) = MinMax $ min a b
  add (MinMax a) (MinMax b) = MinMax $ max a b
  lit = MinMax

instance Expr Mod7 where
  mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7
  add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
  lit = Mod7

-- eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20.
-- eval :: ExprT -> Integer
-- eval (Mul a b) = (eval a) * (eval b)
-- eval (Add a b) = (eval a) + (eval b)
-- eval (Lit a) = a

-- evalStr :: String -> Maybe Integer
-- evalStr xs =
--   case parseExp Lit Add Mul xs of
--     Just a -> Just $ eval a
--     Nothing -> Nothing

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7
-- testProg = testExp :: Maybe Program

-- Ex 5


instance Expr VM.Program where
  mul a b = a ++ b ++ [VM.Mul]
  add a b =  a ++ b ++ [VM.Add]
  lit a = [VM.PushI a]

compile :: String -> Maybe VM.Program
compile = parseExp lit add mul

compile2 :: String -> Either String VM.Program
compile2 xs =
  case compile xs of
    Just a -> Right a
    Nothing -> Left "error"



-- Ex 6

data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String
              deriving (Show, Eq)

class HasVars a where
  var :: String -> a

instance Expr VarExprT where
  mul = Mul
  add = Add
  lit = Lit

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

-- add (\_ -> Just 3) (M.lookup "x" 6)
instance Expr (M.Map String Integer -> Maybe Integer) where
  mul a b x = (*) <$> a x <*> b x
  add a b x = (+) <$> a x <*> b x
  lit a _ = Just a

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs expr = expr $ M.fromList vs
