import Data.Char

data Id = Id Int deriving (Show, Read)
data Point = Point Int Int deriving (Show, Read)
data Square = Square Id Point Point deriving (Show, Read)

parse :: String -> [Int]
parse chs = [read [x] | x <- chs, [x] `elem` (map show [1..9])]

genSquare :: [Int] -> Square
genSquare lst = Square (Id a) (Point b c) (Point (b+d-1) (c+e-1))
    where
        a = lst !! 0 
        b = lst !! 1
        c = lst !! 2
        d = lst !! 3
        e = lst !! 4

main = do
    raw <- readFile "input_1.txt"
    let tmp = map (genSquare . parse) (lines raw)
    print tmp
    print 42
