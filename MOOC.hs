{-Haskell MOOC

Example higher-order functions
map length ["abc","abcdef", "ac"]
[3,6]


--Structure of a program

module Gold where -- 1 module per file

-- The golden ratio
phi :: Double
phi = (sqrt 5 + 1) / 2

polynomial :: Double -> Double
polynomial x = x^2 - x - 1

f x = polynomial (polynomial x)

main = do
  print (polynomial phi)
  print (f phi)

1.9.4 Pattern Matching-}

greet :: String -> String -> String 
greet "Germany" name = "Hallo, " ++ name
greet "Ecuador" name = "Que mas ve, " ++ name
greet _         name = "Hi, " ++ name


--1.9.5 Recursion

factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n-1)


-- compute the sum 1^2+2^2+3^2+...+n^2
squareSum 0 = 0
squareSum n = n^2 + squareSum (n-1)


summe :: (Num a) => [a] -> a  --summe de todos los elementos de una lista
summe [] = 0 
summe (x:xs) = x + summe xs

--(++) :: [a] -> [a] -> [a]
--(++) [] ys = ys
--(++) (x:xs) ys = x:( (++) xs ys)

dreheUm :: [a] -> [a]
dreheUm [] = []
dreheUm (x:xs) = dreheUm ++ [x]


dreheUm1 xs = help [] xs
   where
      help acc [] = acc
      help acc (x:xs) = help (x:acc) xs
