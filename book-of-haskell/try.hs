module Main where

main :: IO ()
main = do
  putStrLn "Let's use IO as an applicative:"
  multiple <- twoIntsMultipliedInApplicative getInt getInt
  putStrLn ("The multiple is " ++ show multiple)
  putStrLn ""
  putStrLn "Okay, now let's use Maybe as an applicative:"
  putStrLn "Make the numbers greater than 10 to get a result..."
  nums <- sequenceA [getInt, getInt]
  let
    [maybeNum1, maybeNum2] = fmap maybeGT10 nums
    resultString = case twoIntsMultipliedInApplicative maybeNum1 maybeNum1 of
      Nothing -> "No result because the numbers have to be greater than 10"
      Just r -> "The result is " ++ show r
  putStrLn resultString
  putStrLn ""

getInt :: IO Int
getInt =  do
  putStr "Hey, give me a number? "
  line <- getLine
  return (read line :: Int)

multiplyInApplicative :: (Applicative f) => f (Int -> Int -> Int)
multiplyInApplicative = pure (*)

twoIntsMultipliedInApplicative :: (Applicative f) => f Int -> f Int -> f Int
twoIntsMultipliedInApplicative applicInt1 applicInt2 = multiplyInApplicative <*> applicInt1 <*> applicInt2

maybeGT10 :: Int -> Maybe Int
maybeGT10 num = if num > 10 then Just num else Nothing
