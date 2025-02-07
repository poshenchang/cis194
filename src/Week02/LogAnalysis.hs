{-# OPTIONS_GHC -Wall #-}
module Week02.LogAnalysis where

import Week02.Log
import Text.Read (readMaybe)

-- Exercise 1

-- Converts String to Int.
-- Negative outputs are regarded as failed conversion.
readsInt :: String -> Int
readsInt s = case readMaybe s :: Maybe Int of
    Just n  -> n
    Nothing -> -1

-- Parses log message String into LogMessage.
-- LogMessage types are defined in "Log.hs".
-- parseMessage "E 2 562 help help" 
--   == LogMessage (Error 2) 562 "help help"
-- parseMessage "I 29 la la la"
--   == LogMessage Info 29 "la la la"
-- parseMessage "This is not in the right format"
--   == Unknown "This is not in the right format"
parseMessage :: String -> LogMessage
parseMessage msg = case words msg of
    "E" : xs : ys : zs | err >= 0 && ts >= 0
        -> LogMessage (Error err) ts (unwords zs)
        where
            err = readsInt xs
            ts = readsInt ys
    [c] : xs : ys
        | ts >= 0 && c == 'I' -> LogMessage Info ts (unwords ys)
        | ts >= 0 && c == 'W' -> LogMessage Warning ts (unwords ys)
        where
            ts = readsInt xs
    _ -> Unknown msg

-- Parses entire log files.
parse :: String -> [LogMessage]
parse file = map parseMessage (lines file)

-- Exercise 2

-- Insert a LogMessage into a MessageTree(sorted BST).
insert :: LogMessage -> MessageTree -> MessageTree
insert newMsg@(LogMessage _ newts _)
       (Node left msg@(LogMessage _ ts _) right)
    | newts > ts = Node left msg (insert newMsg right)
    | otherwise  = Node (insert newMsg left) msg right
insert msg Leaf = Node Leaf msg Leaf
insert _ tree = tree

-- Exercise 3

-- Build a Message Tree from a list of LogMessage by repeated insertion.
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4

-- Extracts sorted list of LogMessage from a MessageTree(sorted BST).
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ (msg : inOrder right)

-- Exercise 5

-- Determines whether a LogMessage is relevant.
-- A LogMessage is relevant if error messages with severity >= 50.
isRelevant :: LogMessage -> Bool
isRelevant (LogMessage (Error err) _ _) = err >= 50
isRelevant _ = False

-- Extract relevant LogMessages.
relevantLogs :: [LogMessage] -> [LogMessage]
relevantLogs = filter isRelevant

-- Extract the message String from LogMessage.
extractMsg :: LogMessage -> String
extractMsg (LogMessage _ _ msg) = msg
extractMsg (Unknown msg) = msg

extractMsgList :: [LogMessage] -> [String]
extractMsgList = map extractMsg

-- Takes an unsorted list of LogMessages,
-- and returns a list of relevant error messages, sorted by timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = extractMsgList . inOrder . build . relevantLogs