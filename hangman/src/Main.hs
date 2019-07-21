module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

newtype WordList = WordList [String] deriving (Eq, Show)

minWordLenght :: Int
minWordLenght = 5

maxWordLenght :: Int
maxWordLenght = 9

allWords :: IO WordList
allWords = do
  dict <- readFile "data/cracklib-small"
  return $ WordList (lines dict)

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length w
          in l >= minWordLenght && l < maxWordLenght

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, (length wl) - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    intersperse ' ' $ fmap renderPuzzleChar discovered
    ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle x = Puzzle x (map (const Nothing) x) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) aChar = elem aChar word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) aChar = elem aChar guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just aChar) = aChar

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c = Puzzle word newFilledInSoFar (c : s)
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar
        newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess , alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that\
               \ character, pick \
               \ something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the\
               \ word, filling in the word\
               \ accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in\
               \ the word, try again."
      return (fillInCharacter puzzle guess)

fromDiscovered :: Maybe Char -> Char
fromDiscovered Nothing = ' '
fromDiscovered (Just aChar) = aChar

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess discovered guessed) =
  if (length wrongGuesses) > 7 then
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess
  else return ()
  where
    notElement guessChar = notElem guessChar . map fromDiscovered $ discovered
    wrongGuesses = filter notElement guessed

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter mothefucka: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
