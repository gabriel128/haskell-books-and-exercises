{-# LANGUAGE GADTs #-}

module Sty where

-- data Sty ty = SInt | SBool

-- data Sty ty where
--   SInt :: Sty Int
--   SBool :: Sty Bool

data Sty ty
  = (ty ~ Int) => SInt
  | (ty ~ Bool) => SBool

zero :: Sty ty -> ty
zero SInt = 0
zero SBool = False
