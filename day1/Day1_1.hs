module Main
    where

import Day1_0
import Data.List -- intersect

input_file = "input_0.txt"

-- dumb brute force
-- CPU intensive, we'll have to use dynamic programming
-- gives an anwser after cca 10 minutes

myRec :: [Integer] -> [Integer] -> Integer
myRec rl (x:xs)
    | elem next rl = next
    | otherwise = myRec (rl ++ [next]) xs
    where next = (last rl) + x

main' = do
    fstr <- readFile input_file
    let ints = map (toInt . myFix) (lines fstr)
    let res = myRec [0] (cycle ints)
    -- print (ints!!2)
    print res

accSum :: [Integer] -> [Integer]
accSum a = accSum' [0] a
    where 
        accSum' :: [Integer] -> [Integer] -> [Integer]
        accSum' r (x:xs) = accSum' (r ++ [last r + x]) xs
        accSum' r [] = tail r -- remove the initial 0 element

rekIter :: [Integer] -> Integer -> Int -> Integer
rekIter rlist incr initlen
    | interlist /= [] = head interlist
    | otherwise = rekIter (rlist ++ newlist) incr initlen
    where
        newlist = map (+incr) (drop (length rlist - initlen) rlist)
        interlist = intersect newlist rlist

main = do
    fstr <- readFile input_file
    let ints = map (toInt . myFix) (lines fstr) 
    print ints
    let rlist = accSum ints
    -- print rlist
    let incr = last rlist
    -- print incr
    let res = rekIter rlist incr (length ints)
    print res






