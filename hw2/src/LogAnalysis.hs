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
             (Unknown _)        -> t
             (LogMessage _ s _) -> case t of
                                   Leaf -> Node Leaf m Leaf
                                   (Node _ (Unknown _) _) -> t
                                   (Node l c@(LogMessage _ d _) r)
                                     | s > d     -> Node l c (insert m r)
                                     | s < d     -> Node (insert m l) c r
                                     | otherwise -> t

build :: [LogMessage] -> MessageTree
build xs = foldl (\x y -> insert y x) Leaf xs

inOrder :: MessageTree -> [LogMessage]
inOrder n = case n of
            Leaf         -> []
            (Node l m r) -> (inOrder l) ++ [m] ++ (inOrder r)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = ((\xs -> map (\(LogMessage _ _ t) -> t) xs) . inOrder . build . (filter (\x -> case x of
                                                                                                 (LogMessage (Error s) _ _)
                                                                                                   | s >= 50   -> True
                                                                                                   | otherwise -> False
                                                                                                 _ -> False))) xs
