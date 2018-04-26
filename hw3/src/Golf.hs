module Golf where

import Data.List.Split

skips :: [a] -> [[a]]
skips xs = case xs of
             []           -> []
             [x]          -> [[x]]
             (first:rest) -> map (\x -> (\n -> map snd . filter ((==n) . fst) . zip (cycle [1..n])) x xs) [1 .. (length xs)]

localMaxima :: [Integer] -> [Integer]
localMaxima xs = case xs of
                   (first:second:rest) -> foldl (\x y -> let xs' = (take 3 (drop y xs))
                                                         in case xs' of
                                                           (first:second:rest)
                                                             | second > first && second > (head rest) -> x ++ [second]
                                                           _ -> x) [] [0 .. ((length xs) - 1)]
                   _ -> xs
