module Day1_0 (myFix, toInt)
    where

-- Haskell modules are alphanumeric and must begin with an uppercase letter.
-- The module name must match the filename without the .hs extension.

-- runhaskell 1_0.hs

input_file = "input_0.txt"

myFix :: String -> String
myFix (x:xs)
    | x == '+' = xs
    | otherwise = x:xs

toInt :: String -> Integer
toInt a = read a :: Integer

-- mySum :: Integer -> String -> Integer
-- mySum ai bs = ai + (read (myFix bs) :: Integer)

main' = do
    fstr <- readFile input_file
    let ln = lines fstr
    let mp = map myFix ln
    let mi = map toInt mp
    let sm = sum mi
    print sm

main'' = do
    fstr <- readFile input_file
    let ln = lines fstr
    let mi = map (toInt . myFix) ln
    let sm = sum mi
    print sm

foldHelper :: Integer -> String -> Integer
foldHelper ai bs = ((toInt . myFix) bs) + ai

main = do
    fstr <- readFile input_file
    let ln = lines fstr
    let res = foldl foldHelper 0 ln
    print res
