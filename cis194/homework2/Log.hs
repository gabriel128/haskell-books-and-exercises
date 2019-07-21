module Log where

import Data.Maybe



parseMessage :: String -> LogMessage
parseMessage strings = case words strings of
                         ("E":t:n:rest) -> LogMessage (Error (read t)) (read n) (unwords rest)
                         ("I":t:rest) -> LogMessage Info (read t) (unwords rest)
                         ("W":t:rest) -> LogMessage Warning (read t) (unwords rest)
                         _ -> Unknown "This is not in the right format"

parse :: String -> [LogMessage]
parse = foldr (\x acc -> parseMessage x : acc) [] . lines

data MessageType = Info | Warning | Error Int | MessageUnknown String deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String | Unknown String deriving (Show, Eq)

data MessageTree = Leaf | Node MessageTree LogMessage MessageTree deriving (Show)

insert :: LogMessage -> MessageTree -> MessageTree
insert newLog Leaf = Node Leaf newLog Leaf
insert newLog@(LogMessage _ newTimestamp _) (Node leftTree oldLog@(LogMessage _ timestamp _) rightTree)
  | newTimestamp > timestamp = Node leftTree oldLog (insert newLog rightTree)
  | otherwise = Node (insert newLog leftTree) oldLog rightTree
insert _ messageTree = messageTree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftN logMessage rightN) = (inOrder leftN) ++ [logMessage] ++ (inOrder rightN)

filterMapSeverityErrors :: LogMessage -> Maybe String
filterMapSeverityErrors (LogMessage (Error severity) _ message)
  | severity >= 50 = Just message
  | otherwise = Nothing
filterMapSeverityErrors _ = Nothing

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = catMaybes . map filterMapSeverityErrors . inOrder . build

testBuild :: String -> IO MessageTree
testBuild filePath = do
  contents <- readFile filePath
  return (build . parse $ contents)

testParse :: (String -> [LogMessage]) -> Int -> String -> IO [LogMessage]
testParse f numberOfLines filePath = do
  contents <- readFile filePath
  return (f . unlines . take numberOfLines . lines $ contents)
