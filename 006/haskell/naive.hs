main :: IO ()
main = readLn >>= print . solve

solve :: Int -> Int
solve p = squareOfSum p - sumOfSquares p

-- The sum of squares of the first n integers
sumOfSquares :: Int -> Int
sumOfSquares n = sum $ map (^2) [1..n]

-- The square of the sum of the first n integers
squareOfSum :: Int -> Int
squareOfSum n = (sum [1..n])^2

