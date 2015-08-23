module Digit where

digits :: Integer -> [Integer]
digits x | x < 10 = [x]
digits x = (x `rem` 10):(digits $ x `div` 10)

nrDigits :: Integer -> Integer
nrDigits x | x < 10 = 1
nrDigits x = 1 + (nrDigits $ x `div` 10)

fromDigits :: [Integer] -> Integer
fromDigits = go . reverse
  where
    go [] = 0
    go (x:xs) = x + 10 * go xs
