-- Voronoi diagram or Delaunay circumcircles
import qualified Data.Map as Map
import Data.List

-- brute force first :P Delaunay if there's time (probably after Christmas)

data Point = Point Int Int deriving (Show, Eq)

instance Ord Point where
    compare (Point a b) (Point c d) = compare (a,b) (c,d)

makeField :: [Point] -> [Point]
makeField x = makeField' (maximum x) (minimum x)
    where
        makeField' (Point mxa mxb) (Point mna mnb) = [(Point a b) | a <- [mna..mxa], b <-[mnb..mxb]]

-- brute force variant: loop through points in field
-- if point not in points, find closest in points, increase accumulator
accum :: [Point] -> [Point] -> Map.Map Point Integer
accum field points = Map.fromListWith (+) [((Point 1 2),1)] -- TODO

closest :: Point -> [Point] -> Point
closest p points = closest' (elemIndices (minimum dst) dst) points
    where
        dst = map(mhDist p) points
        closest' [] x = Point (-2) (-1)
        closest' [idx] x = x!!idx
        closest' idc x = Point (-3) (-1)

mhDist :: Point -> Point -> Int
mhDist (Point a b) (Point c d) = abs (a - c) + abs (b - d)

parsePoint :: String -> Point
parsePoint st = (Point a b)
    where
        [a, b] = read $ "[" ++ st ++ "]"

maxTri :: [Point] -> Int
maxTri _ = 42

main = do
    raw <- readFile "input_1.txt"
    let points = map parsePoint (lines raw)
    print points
    putStrLn "---"
    let field = makeField points
    print field
    putStrLn "---"
    let mapped = accum field points
    print mapped
    print $ closest (Point 1 4) points
    print 42

