import Data.List
import Debug.Trace

-- cheated with python
-- twos:  248
-- threes:  19
-- mul:  4712

filename = "input_0.txt"

tests = ["aakhem", "okayy"]

-- same string can contain a 2 sequence and a 3 sequence !
longestSeq :: (Eq a) => [a] -> Int
longestSeq (x:xs) = longestSeq' (x:xs) 1 1
    where
        longestSeq' :: (Eq a) => [a] -> Int -> Int -> Int
        longestSeq' [x] buff lngst = max buff lngst
        longestSeq' (x:xs) buff lngst =
            if x == (head xs) then longestSeq' xs (buff + 1) lngst
            else longestSeq' xs 0 (max buff lngst)

seqln :: (Eq a) => [a] -> Int
seqln [] = 0
seqln [x] = 1
seqln (x:xs) = seqln' (x:xs) 1 
    where
        seqln' :: (Eq a) => [a] -> Int -> Int
        seqln' [x] cnt = cnt
        seqln' (x:xs) cnt
            | x == (head xs) = seqln' xs (cnt + 1)
            | otherwise = cnt

-- 1 true, 0 false
hasseq :: (Eq a) => [a] -> Int -> Int
hasseq x tarsl
    | (length x) < tarsl = 0
    | sl == tarsl = 1
    | otherwise = hasseq (drop sl x) tarsl
    where sl = seqln x 

hasthree :: (Eq a) => [a] -> Int
hasthree x = hasseq x 3
hastwo :: (Eq a) => [a] -> Int
hastwo x = hasseq x 2

checksum :: (Eq a, Show a) => [[a]] -> Int
checksum strings = (foldl (+) 0 (map hasthree strings)) * (foldl (+) 0 (map hastwo strings))

main = do
    raw <- readFile filename
    let strings = (map sort (lines raw))
    -- print (foldl (+) 0 (map hasthree strings)) -- ok
    print (foldl (+) 0 (map hastwo strings)) -- 
    print (checksum strings)
