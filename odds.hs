sumFirstOdds :: Int -> Int
sumFirstOdds 0 = 0
sumFirstOdds n = ((n * 2) - 1) + (sumFirstOdds (n-1))
