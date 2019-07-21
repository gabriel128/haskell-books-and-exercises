{-# LANGUAGE FlexibleInstances #-}
module Party where

import Data.Monoid
import Data.Tree
import Employee
import Data.List

glCons :: Employee -> GuestList -> GuestList
glCons x (GL xs fun) = GL (x : xs) (fun + empFun x)

instance Monoid GuestList where
  mempty = GL [] 0
  (GL xs fun) `mappend` (GL ys fun') = GL (xs ++ ys) (fun + fun')

moreFun :: GuestList -> GuestList -> GuestList
moreFun x@(GL _ fun) y@(GL _ fun')
  | fun > fun' = x
  | otherwise = y

-- data Tree a = Node {
--   rootLabel :: a, -- label value
--   subForest :: [Tree a] -- zero or more child trees
--   } deriving (Show, Eq)

-- treeFold glCons (GL [] 0) testCompany
treeFold :: (a -> b -> b) -> b -> Tree a -> b
treeFold f z (Node a xs) = a `f` subTree xs
  where
    subTree [] = z
    subTree (x:_) = rootLabel x `f` subTree (subForest x)

treeFold' :: (a -> [b] -> b) -> Tree a -> b
treeFold' f (Node a xs) = a `f` (fmap (treeFold' f) xs)

tr :: Tree Integer
tr = Node 1 [Node 3 [Node 3 [Node 4 []]]]

nextLevel :: Employee
          -> [(GuestList, GuestList)]
          -> (GuestList, GuestList)
nextLevel boss gls = (withTheBoss, withoutTheBoss)
  where
    withoutTheBoss = maximum . map fst $ gls
    withTheBoss = glCons boss . maximum . map snd $ gls

-- nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
-- nextLevel boss gls = (withBoss, withoutBoss)
--   where
--     withoutBoss = foldMap (uncurry moreFun) gls
--     withBoss = glCons boss . foldMap snd $ gls

testBoss = Emp "Bob" 3
testGls = [(GL [Emp "Stan" 9] 10, GL [Emp "Jack" 3] 3), (GL [Emp "Stan1" 9] 9, GL [Emp "Jack1" 3] 3)]
-- Ex 4

-- funEmp :: Tree Employee -> Integer
-- funEmp = treeFold (<>) (Emp "" 0)

empSumFun :: [Employee] -> Integer
empSumFun = foldr sumfun 0
  where
    sumfun (Emp _ fun) acc = fun + acc

maxFun :: Tree Employee -> GuestList
maxFun employees = GL employeesList (empSumFun employeesList)
  where
    employeesList = treeFold' (\x acc -> x : concat acc) employees

computeContent :: String -> GuestList
computeContent =  maxFun . read

parsedEmployees :: String -> [Employee]
parsedEmployees = undefined

instance Ord Employee where
  compare Emp{empName = x} Emp{empName = y} = compare x y

empToLines :: [Employee] -> String
empToLines = unlines . fmap (show . empName) . sort

main :: IO ()
main = do
  content <- readFile "company.txt"
  let (GL emps fun) = computeContent content
  putStrLn ("Total fun: " <> show fun)
  putStrLn (empToLines emps)
