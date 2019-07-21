module Chapter13 where

import Control.Monad
import Data.Char
import System.Exit (exitSuccess)

isPalindrome :: String -> Bool
isPalindrome phrase = phrase == (reverse . map toLower $ phrase)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case isPalindrome line1 of
    True -> putStrLn "it is"
    False -> do
      putStrLn "it is not"
      exitSuccess

type Name = String
type Age = Integer
data Person = Person Name Age deriving Show

data PersonInvalid =
  NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
                "Name was: " ++ show name ++
                " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Enter Name"
  name <- getLine
  putStrLn "Enter age"
  age <- getLine
  case mkPerson name (read age) of
    (Right (Person aName pAge)) ->
      putStrLn $
      "Yay! Successfully got a person: " ++ aName ++ " "
      ++ show pAge
    (Left NameEmpty) -> putStrLn "Name empty"
    (Left AgeTooLow) -> putStrLn "Age too low"
    (Left _) -> putStrLn "plain wrong"

main :: IO ()
main = gimmePerson
