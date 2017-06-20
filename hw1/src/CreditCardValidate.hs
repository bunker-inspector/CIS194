module CreditCardValidate where

toDigits :: Integer -> [Integer]
toDigits n
  | n >= 10   = [1, (n - 10)]
  | otherwise = [n]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = (foldl (flip(:)) []) (toDigits n)
