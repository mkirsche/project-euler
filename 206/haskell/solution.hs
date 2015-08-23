import           Power (squares)

main :: IO ()
main = print solve

solve :: Integer
solve = fst . head . dropWhile (not . hasProp . snd) $ zip [1..] squares

hasProp :: Integer -> Bool
hasProp n = go $ show n
  where
    go ('1':_:'2':_:'3':_:'4':_:'5':_:'6':_:'7':_:'8':_:'9':_:'0':[]) = True
    go _ = False
