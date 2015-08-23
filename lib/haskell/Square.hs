module Square where

isSquare :: Integer -> Bool
isSquare = isInt . intSquare . fromInteger
  where isInt x = x == fromInteger (round x)

intSquare :: Integer -> Integer
intSquare 0 = 0
intSquare 1 = 1
intSquare n = head $ dropWhile (not . isRoot) iters
  where
    twopows = iterate (^2) 2
    (lowerRoot, lowerN) = last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
    newtonStep x = div (x + div n x) 2
    iters = iterate newtonStep (intSquare (div n lowerN) * lowerRoot)
    isRoot r  =  r^2 <= n && n < (r+1)^2
