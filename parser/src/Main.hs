module Main where

import Data.Char
import Data.Monoid
import Safe (readMay)

data Expression

data Operator = Plus | Minus | Times | Div deriving (Show, Eq)

data Token = TokOp Operator
           | TokAssign
           | TokLParen
           | TokRParen
           | TokIdent String
           | TokNum Double
           | TokEnd
    deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:cs)
  | elem c "+-*/" = TokOp (operator c) : tokenize cs
  | c == '(' = TokLParen : tokenize cs
  | c == ')' = TokRParen : tokenize cs
  | c == '=' = TokAssign : tokenize cs
  | isDigit c = number c cs
  | isAlpha c = identifier c cs
  | isSpace c = tokenize cs
  | otherwise = error $ "Cannot tokenize: " ++ [c]

number :: Char -> String -> [Token]
number c cs =
  let (digs, cs') = span isDigit cs
  in TokNum (read (c : digs)) : tokenize cs'

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Times
           | c == '/' = Div

identifier :: Char -> String -> [Token]
identifier c cs =
  let (str, cs') = span isAlphaNum cs
  in TokIdent (c:str) : tokenize cs'

parse :: [Token] -> Expression
parse = undefined

evaluate :: Expression -> Double
evaluate = undefined

opToChar :: Operator -> Char
opToChar Plus = '+'
opToChar Minus = '-'
opToChar Times = '*'
opToChar Div = '/'

main :: IO ()
main = do
    line <- getLine
    putStrLn line
    main

squares :: [Int] -> Int
squares = foldl (\acc x -> acc + (x*x)) 0

rev :: [a] -> [a]
rev = foldl (\acc x -> x : acc) []

add xs ys = do
    x <- xs
    if x < 0 then []
    else do
        y <- ys
        if y < 0 then []
        else return (x+y)

data Test = Test Integer Integer Integer deriving Show


vals :: [Maybe Int]
vals = [readMay "ignored", readMay "1", readMay "5", readMay "10"]

test = do
    print . mconcat $ map First vals
    print . mconcat $ map Last vals
