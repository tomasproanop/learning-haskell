f a b
  | mod (a+b) 2 == 0 = first + (b-1)
  | otherwise = first + (a-1)
   where
     first = (div ((a+b-1)*(a+b-2)) 2)
