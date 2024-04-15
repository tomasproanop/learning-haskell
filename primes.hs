divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0 

ld n = ldf 2 n

ldf k n | divides k n = k
ldf k n | k^2 > n = n
ldf k n = ldf (k+1) n

prime0 n | n < 1      = error "not a positive integer"
         | n == 1     = False
         | otherwise  = ld == n 


average :: [Int] -> Float 
average [] = error "empty list"
average xs = fromInt (sum xs) / fromInt (length xs) --promedio de una lista 

{-
sum’ :: [Int] -> Int
sum’ [] = 0
sum’ (x:xs) = x + sum’ xs -- homemade version of sum 

length’ :: [a] -> Int
length’ [] = 0
length’ (x:xs) = 1 + length’ xs
-}