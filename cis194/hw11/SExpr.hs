{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

----- Lecture

-- (*>) :: Applicative f => f a -> f b -> f b
-- _ *> fb = fb

mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA f = sequenceA' . fmap f

sequenceA' :: Applicative f => [f a] -> f [a]
sequenceA' [] = pure []
sequenceA' (x:xs) = (:) <$> x <*> sequenceA' xs

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA n = sequenceA' . replicate n

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

-- *AParser> runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh"
-- Just ("","abcdeFGh")
-- *AParser> runParser (oneOrMore (satisfy isUpper)) "abcdeFGh"
-- Nothing

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> (oneOrMore p <|> pure [])

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

-- *AParser> runParser ident "foobar baz"
-- Just ("foobar"," baz")
-- *AParser> runParser ident "foo33fA"
-- Just ("foo33fA","")
-- *AParser> runParser ident "2bad"
-- Nothing
-- *AParser> runParser ident ""
-- Nothing
ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)


------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

-- 5
-- foo3
-- (bar (foo) 3 5 874)
-- (((lambda x (lambda y (plus x y))) 3) 5)
-- ( lots of ( spaces in ) this ( one ) )

parseAtom :: Parser Atom
parseAtom = (N <$> posInt) <|> (I <$> ident)


parseSExpr :: Parser SExpr
parseSExpr = spaces *> ((A <$> parseAtom) <|> parseComb) <* spaces
  where
    parseComb = Comb <$> (spaces *> char '(' *> spaces *> zeroOrMore parseSExpr <* spaces <* char ')')

exercise3 = do
  print $ runParser parseSExpr "5"
  print $ runParser parseSExpr "foo3"
  print $ runParser parseSExpr "(bar (foo) 3 5 874)"
  print $ runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)"
  print $ runParser parseSExpr "(   lots  of   (  spaces   in  )  this ( one ) )"
