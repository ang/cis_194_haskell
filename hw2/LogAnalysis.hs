{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- parseMessage "E 2 562 help help"
-- == LogMessage (Error 2) 562 "help help"
-- cis 194: homework 2 3
-- parseMessage "I 29 la la la"
-- == LogMessage Info 29 "la la la"
-- parseMessage "This is not in the right format"
-- == Unknown "This is not in the right format"

-- "words" function will parse a string into a list of words, separated by
-- whitespace
-- "read" function casts a string into another type
-- "unwords" is the opposite of "words", will put a list of words into a string
parseMessage :: String -> LogMessage
parseMessage log = case words log of
  ("I":timestamp:msg) -> LogMessage Info (read timestamp) (unwords msg)
  ("W":timestamp:msg) -> LogMessage Warning (read timestamp) (unwords msg)
  ("E":errno:timestamp:msg) -> LogMessage (Error (read errno)) (read timestamp) (unwords msg)
  _ -> Unknown log

-- List comprehension to the rescue
-- [method(variable) | variable <- [x0, x1, x2...], some filter]
parse :: String -> [LogMessage]
parse logFile = [parseMessage(logLine) | logLine <- lines(logFile)]

-- QUESTION: I wrote out (LogMessage messageType timestamp string) when I needed
-- to use one of its components, which required me to rewrite it all again. Is
-- there a better way to do this?
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logMessage Leaf = Node Leaf logMessage Leaf
insert (LogMessage mType ts str) (Node leftTree (LogMessage nMType nTS nStr) rightTree)
  | ts < nTS = Node (insert (LogMessage mType ts str) leftTree) (LogMessage nMType nTS nStr) rightTree
  | otherwise = Node leftTree (LogMessage nMType nTS nStr) (insert (LogMessage mType ts str) rightTree)
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (message:rest) = insert message (build rest)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree logMessage rightTree) = inOrder(leftTree) ++ [logMessage] ++ inOrder(rightTree)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = stringsFromLogs (inOrder (build [message | message <- messages, parseErrorsAbove message 50]))

-- Returns errors above a certain serverity level
parseErrorsAbove :: LogMessage -> Int -> Bool
parseErrorsAbove (LogMessage (Error serverity) _ts _str) threshold = serverity >= threshold
parseErrorsAbove _ _ = False

stringsFromLogs :: [LogMessage] -> [String]
stringsFromLogs strings = [str | (LogMessage _ _ str) <- strings]

-- 1. Filter to only have errors greater than 50
-- 2. Put it into tree
-- 3. Return sorted list
-- 4. Get only messages from sorted list

-- Test data
-- logs = ["I 2 blah",
--   "I 1 blah",
--   "W 4 blah",
--   "I 6 blah",
--   "I 4 blah",
--   "I 10 blah",
--   "I 300 blah",
--   "I 3 blah",
--   "I 5 blah"]
--
-- logMessages = [parseMessage(log) | log <- logs]
-- tree = build logMessages
-- sortedMessages = inOrder tree
