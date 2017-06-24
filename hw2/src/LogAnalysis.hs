{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage [] = Unknown ""
parseMessage [_] = Unknown ""
parseMessage [_,_] = Unknown ""
parseMessage n = let x = words n
                 in case x of
                  (code:second:third:rest)
                    | code == "I" -> LogMessage Info (read second :: Int) (unwords (third : rest))
                    | code == "E" -> LogMessage (Error (read second :: Int)) (read third :: Int) (unwords rest)
                    | code == "W" -> LogMessage Warning (read second :: Int) (unwords (third : rest))
                    | otherwise   -> Unknown n

