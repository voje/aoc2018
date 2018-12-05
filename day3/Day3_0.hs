import qualified Data.Map as Map
import Data.Char
import Data.List
import Debug.Trace

data Id = Id Int deriving (Show, Read)
data Point = Point Int Int deriving (Show, Read, Eq, Ord)
data Square = Square Id Point Point deriving (Show, Read)

-- test
p1 = Point 2 3
p2 = Point 7 5
raw = "#1 @ 1,3: 43x4"

-- this is ugly but I want to go to bed...
parse :: String -> [Int]
parse chs = do
  let s0 = dropWhile (not . isDigit) chs
  let x0 = read $ takeWhile isDigit s0

  let s1 = dropWhile (not . isDigit) $ dropWhile isDigit s0
  let x1 = read $ takeWhile isDigit s1

  let s2 = dropWhile (not . isDigit) $ dropWhile isDigit s1
  let x2 = read $ takeWhile isDigit s2

  let s3 = dropWhile (not . isDigit) $ dropWhile isDigit s2
  let x3 = read $ takeWhile isDigit s3

  let s4 = dropWhile (not . isDigit) $ dropWhile isDigit s3
  let x4 = read $ takeWhile isDigit s4

  -- I'm so sorry...
  [x0,x1,x2,x3,x4]

-- doesn't work for double digits
-- parseBadly :: String -> [Int]
-- parseBadly chs = [read [x] | x <- chs, [x] `elem` (map show [1..9])]

genSquare :: [Int] -> Square
genSquare lst = Square (Id a) (Point b c) (Point (b+d-1) (c+e-1))
    where
        a = lst !! 0 
        b = lst !! 1
        c = lst !! 2
        d = lst !! 3
        e = lst !! 4

area :: Square -> [Point]
area(Square _ p1 p2) = area' p1 p2
  where
    area' :: Point -> Point -> [Point]
    area' (Point p11 p12) (Point p21 p22) = do
      let tmp = [Point x y | x <- [p11..p21], y <- [p12..p22]]
      -- trace ("\n" ++ show tmp ++ "\n") tmp
      tmp

main = do
    raw <- readFile "input_0.txt"
    let tmp = map (area . genSquare . parse) (lines raw)
    -- print tmp
    let flat = foldr (++) [] tmp
    let mp = Map.fromListWith (+) (zip flat $ repeat 1)
    -- print mp
    let fields = [(x,y) | (x,y) <- Map.toList mp, y > 1]
    -- print fields
    putStr "Number of intersections: "
    print $ length fields
    print $ parse raw
