import Data.Char

match :: Char -> Char -> Bool
match a aa = (abs $ (ord a) - (ord aa)) == 32

myZip :: [Char] -> [Char]
myZip xs = myZip' [] xs
  where
    myZip' ys [] = ys
    myZip' [] (x:xs) = myZip' [x] xs 
    myZip' ys (x:xs)
      | match (last ys) x = myZip' (init ys) xs
      | otherwise = myZip' (ys ++ [x]) xs

-- after python giving the same wrong answer, I took a correct algorithm from reddit
part1 :: String -> Int
part1 = length . foldr step ""
  where
    step x (y:ys) | x /= y && toUpper x == toUpper y = ys
    step x ys                                        = x : ys


main = do
  raw <- readFile "input_0.txt"
  print $ length raw
  let res = myZip raw
  print $ length res
  let res = myZip raw
  print $ length res
  let res = myZip raw
  print $ length res
