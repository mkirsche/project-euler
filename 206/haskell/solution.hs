import           Digit  (digits, fromDigits)
import           Square (intSquare, isSquare)

main :: IO ()
main = readLn >>= print . solve

solve :: Integer -> Integer
solve = intSquare . head . dropWhile (not . isSquare) . possibilities . intersperceUnknown . conceiled

type Conceiled = [D]
data D = Unknown
       | Lit Integer
  deriving (Show, Eq)

conceiled :: Integer -> Conceiled
conceiled = map Lit . digits

intersperceUnknown :: Conceiled -> Conceiled
intersperceUnknown [] = []
intersperceUnknown [x] = [x]
intersperceUnknown (x:xs) = x:Unknown:intersperceUnknown xs

data Zipper a = Z ([a], a, [a])
instance Functor Zipper where
  fmap f (Z (as, b, cs)) = Z (map f as, f b, map f cs)
type DZipper = Zipper D

toZipper :: Conceiled -> DZipper
toZipper []     = error "no zipper possible over empty list"
toZipper [x]    = Z ([], x, [])
toZipper (x:xs) = Z ([], x, xs)

possibilities :: Conceiled -> [Integer]
possibilities = go . toZipper
  where
    go :: DZipper -> [Integer]
    go (Z (as, l@(Lit x), []))      = [fromDigits . reverse $ map (\(Lit y) -> y) (l:as)]
    go (Z (as, b@(Lit x), (c:cs)))  = go $ Z (b:as, c, cs)
    go (Z (as, b@Unknown, cs))      = do
        let lits = map Lit [1..9]
        concat $ map (\x -> go (Z (as, x, cs))) lits
