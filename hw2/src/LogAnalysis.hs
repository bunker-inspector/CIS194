{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Data.List.Split
import Log

splitBy :: String -> Char -> [String]
splitBy (x:xs) d


parseMessage :: String -> LogMessage
parseMessage n =
                let x = splitOn " " n
                in case x of
                _  | ("I":timestamp:message)            -> LogMessage Info second message
                   | ("E":severity:timestamp:message)   -> LogMessage (Error second) third rest
                   | ("W":timestamp:message)            -> LogMessage Warning second message
                   | otherwise                          -> Unknown n
