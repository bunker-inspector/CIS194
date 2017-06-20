module CreditCardValidate where

toDigits :: Integral t => t -> [t]
toDigits n
 | n < 1 = []
 | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integral t => t -> [t]
toDigitsRev n = (foldl (flip(:)) []) (toDigits n)

doubleEveryOther :: Integral t => [t] -> [t]
doubleEveryOther = zipWith ($) (cycle [id, (*2)])

sumDigits :: [Integer] -> Integer
sumDigits n = sum (map sum (map toDigits n))

validate = (((==) 0) . (\x -> mod x 10) . sumDigits . doubleEveryOther . toDigitsRev)
