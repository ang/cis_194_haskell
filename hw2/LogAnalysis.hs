{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

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
whatWentWrong messages = stringsFromLogs (inOrder (build [message | message <- messages, filterErrorsAboveSeverity message 50]))

-- Returns errors above a certain serverity level
filterErrorsAboveSeverity :: LogMessage -> Int -> Bool
filterErrorsAboveSeverity (LogMessage (Error serverity) _ts _str) threshold = serverity >= threshold
filterErrorsAboveSeverity _ _ = False

stringsFromLogs :: [LogMessage] -> [String]
stringsFromLogs strings = [str | (LogMessage _ _ str) <- strings]

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
