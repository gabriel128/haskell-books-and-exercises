module Person (adultPerson) where

import Geometry

data Person = Person { firstName :: String , lastName :: String , age :: Int}
  | Person2 { firstName :: String }
  deriving Show

adultPerson :: String -> String -> Int -> Maybe Person
adultPerson name lastN age = if age >= 21 then Just (Person {firstName = name, lastName = lastN, age = age})
  else Nothing

isJustPerson :: Maybe Person -> Bool
isJustPerson (Just (Person {firstName = _, lastName = _, age = _})) = True
isJustPerson Nothing = False
