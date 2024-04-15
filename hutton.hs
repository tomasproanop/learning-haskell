-- https://www.youtube.com/watch?v=uBEPFkuPrcU&list=PLF1Z-APd9zK7usPMx3LGMZEHrECUGodd3&index=5&ab_channel=GrahamHutton


--FP 3\

--sorts characters, logical values, numbers,etc.

qsort :: Ord a => [a] -> [a]
qsort []      = []
qsort (x:xs)  = qsort ys ++ [x] ++ qsort zs
                where 
                    ys = [a | a <- xs, a <= x]
                    zs = [b | b <- xs, b > x]

--FP 4
{-
2.7
head (first), tail (todo menos la primera), take (primeras n), drop, length, sum
product, reverse (++)
-}

double x = x + x 

quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns

n = a `div` length xs 
      where 
        a  = 10
        xs = [1,2,3,4,5]

last xs = head (reverse xs)

initial xs = take (length xs -1) xs

--more haskelly way:

init xs = reverse (tail (reverse xs))


