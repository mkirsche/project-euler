module Power where

powers :: Integer -> [Integer]
powers n = map (^n) [1..]

squares :: [Integer]
squares = powers 2
