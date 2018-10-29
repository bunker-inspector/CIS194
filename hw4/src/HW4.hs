module HW4 where

import Data.Function
import Data.List

fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' xs = foldr (*) 1 $ map (\y -> y - 2) $ filter even xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = foldr (\x y -> if even x then x + y else y)  0
    . takeWhile(/= 1)
    . iterate (\y -> if even y then y `div` 2 else 3*y+1)


data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

termNode v = Node 0 Leaf v Leaf

insert' :: e -> Tree e -> Tree e
insert' i Leaf = termNode i
insert' i (Node s l v r) =
    case (l, r) of
      (Leaf, Leaf)                   -> Node 1 (termNode i) v r
      (Node _ _ _ _, Leaf)           -> Node s l v (termNode i)
      (Leaf, Node _ _ _ _)           -> Node s (termNode i) v r
      (Node ls _ _ _, Node rs _ _ _) ->
          if ls < rs
            then Node (s + 1) (insert' i l) v r
            else Node (s + 1) l v (insert' i r)

foldTree :: [a] -> Tree a
foldTree = foldr insert' Leaf

xor :: [Bool] -> Bool
xor = odd . foldr (\x y -> if x == True then y + 1 else y) 0

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> (f x) : y) []

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram n =
    let relevantRange = [1 .. n]
    in map (\y -> 2*y+1)
     . (relevantRange \\)
     . filter (< n)
     . map (\x -> let i = fst x
                      j = snd x
                 in i + j + 2*i*j
           ) $ cartProd relevantRange relevantRange
