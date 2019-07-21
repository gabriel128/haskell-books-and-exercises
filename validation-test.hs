module SomeValidations where

type FromAddress = Address
type ToAddress = Address

data Address = Address String
data Body = Body String
data Email = Email FromAddress ToAddress Body


makeAddress :: String -> Maybe Address
makeAddress address = fmap Address (validateContains '@' address)

makeBody :: String -> Maybe Body
makeBody body = fmap Body (validateNonEmpty body)

makeEmail :: String -> String -> String -> Maybe Email
makeEmail from to body = Email <$> makeAddress from <*> makeAddress to <*> makeBody body

validateContains :: Char -> String -> Maybe String
validateContains x xs
  | elem x xs = Just xs
  | otherwise = Nothing

validateNonEmpty :: String -> Maybe String
validateNonEmpty [] = Nothing
validateNonEmpty xs = Just xs


data Pepe = Pepe String String String

buildPepe :: Maybe String -> Maybe String -> Maybe String -> Maybe Pepe
buildPepe a b c = Pepe <$> a <*> b <*> c

