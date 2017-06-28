{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Data.List.Split

parseMessage :: String -> LogMessage
parseMessage n = let x = words n
                 in case x of
                  ("I":timestamp:rest)          -> LogMessage Info (read timestamp) (unwords rest)
                  ("E":severity:timestamp:rest) -> LogMessage (Error (read severity)) (read timestamp) (unwords rest)
                  ("W":timestamp:rest)          -> LogMessage Warning (read timestamp) (unwords rest)
                  _ -> Unknown n

parse :: String -> [LogMessage]
parse n = foldl (\xs x -> (parseMessage x) : xs) [] ys
          where
            ys = splitOn "\n" n

insert :: LogMessage -> MessageTree -> MessageTree
insert m t = case m of
             (LogMessage _ s _) -> case t of
                                   Leaf -> Node Leaf m Leaf
                                   (Node l c@(LogMessage _ d _) r)
                                     | s > d -> Node l c (insert m r)
                                     | s < d -> Node (insert m l) c r
                                     | _     -> t
                              _ -> t
