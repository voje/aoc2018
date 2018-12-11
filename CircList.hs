tlist = "abcdefghijkl" 

spin :: Int -> [a] -> [a]
spin n (x:xs)
    | n == 0 = (x:xs)
    | n > 0 = spin (n-1) (xs ++ [x])
    | n < 0 = 
        let l = (length (x:xs))
            nn = (n `mod` l) + l
        in spin (nn-1) (xs ++ [x])
