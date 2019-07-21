module Cipher where

import Data.Char
import Data.List

type Shift = Int
type UnShift = Int
type EncodeOp = (Int -> Int -> Int)
type Code = String

shiftedChar :: EncodeOp -> Char -> Shift -> Int
shiftedChar encodeOp x shift
  | isUpper x = (ord x `encodeOp` shift - ord 'A') `mod` 26 + ord 'A'
  | otherwise = (ord x `encodeOp` shift - ord 'a') `mod` 26 + ord 'a'

chartToShift :: Char -> Int
chartToShift x
  | isUpper x = (ord x - ord 'A') `mod` 26
  | otherwise = (ord x - ord 'a') `mod` 26

caesar :: Shift -> String -> String
caesar shift = map (chr . (\x -> shiftedChar (+) x shift))

uncaesar :: UnShift -> String -> String
uncaesar unshift = map (chr . (\x -> shiftedChar (-) x unshift))

repeatedCode :: Code -> String
repeatedCode = intercalate "" . repeat

replaceLetters :: String -> Code -> [String] -> String
replaceLetters word repeatedCode acc = drop previousWordsLength . take (length word + previousWordsLength) $ repeatedCode
  where previousWordsLength = sum . map length $ acc

applyCode :: String -> Code -> [String]
applyCode phrase code = reverse . foldl f [] $ words phrase
  where f acc word = [replaceLetters word (repeatedCode code) acc] ++ acc

wordCodeShifts :: [String] -> [[Int]]
wordCodeShifts = map f
  where f = map chartToShift

vigenere :: String -> Code -> String
vigenere phrase code =  unwords . map concat $ zipWith f (words phrase) (wordCodeShifts $ applyCode phrase code)
  where f = zipWith (\word shift -> caesar shift [word])

main :: IO ()
main = do
  putStrLn "Put a phrase"
  aWord <- getLine
  putStrLn "Put a code"
  code <- getLine
  putStrLn $ vigenere aWord code
