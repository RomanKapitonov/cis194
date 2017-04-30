{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

-- Exercise 1 The first step is figuring out how to parse an individual
-- message. Define a function
-- parseMessage :: String -> LogMessage
-- parseMessage "E 2 562 help help"
--   == LogMessage (Error 2) 562 "help help"
-- parseMessage "I 29 la la la"
--   == LogMessage Info 29 "la la la"
-- parseMessage "This is not in the right format"
--   == Unknown "This is not in the right format"

parseMessage :: String -> LogMessage
parseMessage message = case words message of
  ("E":sev:tm:rest) -> LogMessage (Error (read sev)) (read tm) (unwords rest)
  ("W":tm:rest)     -> LogMessage Warning (read tm) (unwords rest)
  ("I":tm:rest)     -> LogMessage Info (read tm) (unwords rest)
  _                 -> Unknown message

-- Define a function
-- parse :: String -> [LogMessage]
-- which parses an entire log file at once and returns its contents as a
-- list of LogMessages.
parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- Exercise 2
-- Define a function
-- insert :: LogMessage -> MessageTree -> MessageTree
-- which inserts a new LogMessage into an existing MessageTree, producing
-- a new MessageTree. insert may assume that it is given a
-- sorted MessageTree, and must produce a new sorted MessageTree
-- containing the new LogMessage in addition to the contents of the
-- original MessageTree.
-- However, note that if insert is given a LogMessage which is
-- Unknown, it should return the MessageTree unchanged.

insert :: LogMessage -> MessageTree -> MessageTree
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ tm _) (Node left m'@(LogMessage _ tm' _) right)
  | tm <= tm' = Node (insert m left) m' right
  | otherwise = Node left m' (insert m right)
insert _ tree = tree

-- Exercise 3
-- Once we can insert a single LogMessage into a MessageTree,
-- we can build a complete MessageTree from a list of messages. Specifi-
-- cally, define a function
-- build :: [LogMessage] -> MessageTree
-- which builds up a MessageTree containing the messages in the list,
-- by successively inserting the messages into a MessageTree (beginning
-- with a Leaf).

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4
-- Finally, define the function
-- inOrder :: MessageTree -> [LogMessage]
-- which takes a sorted MessageTree and produces a list of all the
-- LogMessages it contains, sorted by timestamp from smallest to biggest.
-- (This is known as an in-order traversal of the MessageTree.)
-- With these functions, we can now remove Unknown messages and
-- sort the well-formed messages using an expression such as:
-- inOrder (build tree)
-- [Note: there are much better ways to sort a list; this is just an exercise
-- to get you working with recursive data structures!]

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left m right) = inOrder left ++ [m] ++ inOrder right

-- Exercise 5
-- Now that we can sort the log messages, the only thing
-- left to do is extract the relevant information. We have decided that
-- “relevant” means “errors with a severity of at least 50”.
-- Write a function
-- whatWentWrong :: [LogMessage] -> [String]
-- which takes an unsorted list of LogMessages, and returns a list of the
-- messages corresponding to any errors with a severity of 50 or greater,
-- sorted by timestamp. (Of course, you can use your functions from the
-- previous exercises to do the sorting.)
-- whatWentWrong xs = map (\(LogMessage _ m _) -> m) $ filter important sorted

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = map message $ filter important sorted
  where sorted = inOrder $ build xs
        important (LogMessage (Error sev) _ _) = sev > 50
        important _ = False
        message (LogMessage _ _ m) = m
        message (Unknown _) = []
