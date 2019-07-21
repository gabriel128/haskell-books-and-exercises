module Geometry (moveIt, Shape) where

import qualified Data.List as List

data Shape = Circle Float Float Float | Rectangle Float Float Float Float
  deriving Show

moveIt :: Shape -> Float  -> Shape
moveIt (Circle a b c) x = Circle (a + x) (b + x) c
moveIt (Rectangle a b c d) x = Rectangle (a + x) (b + x) c d
