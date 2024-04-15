data Pet = Bicho | Cielo | Bebecita String

hello :: Pet -> String
hello x = 
    case x of 
        Bicho -> "hola bicho"
        Cielo -> "hola cielo"
        Bebecita name -> "hola anarco bebecita " ++ name

{-
maxhelper :: Int -> [Int] -> Int 
maxhelper x [] = x
maxhelper x (y:ys) = maxhelper
    (if x>y then x else y) ys

maxfromlist :: [Int] -> Maybe Int
maxfromlist [] = Nothing
maxfromlist (x:xs) = Just (maxhelper  xs)

-}

-- filter (even) [2,6,0,2,3]
--[2,6,0,2]

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 f [] = []
filter1 f (x:xs)
    | f x = x : filter1 f xs
    | otherwise = filter f xs 


length1 :: [a] -> Int
length1 xs = foldl (\x y -> succ x) 0 xs
-- que cuente elementos de una lista


reverse1 :: [a] -> [a]
--reverse1 [1,2,3]
--    -> [3,2,1]
reverse1 xs = foldl (flip (:)) [] xs
--reverse1  [1,2,3]

flip (:) [] 1 
1 : [] -> [1]

flip (:) [1] 2 
2 : [1] -> [2,1]

flip (:) [2,1] 3
3: [2,1] -> [3,2,1]

flip (:) [3,1,2,3] 2 
2: [3,2,1,3] -> [2,3,1,2,3]