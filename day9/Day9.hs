import Data.Char
import qualified Data.Map as Map

parseInput :: String -> [Int]
parseInput str = map read $ filter (isDigit . head) (words str)

main = do
  raw <- readFile "input_0.txt"
  print $ parseInput raw
  let lst = [1,2,3,4,5,6,7]

  print 42
