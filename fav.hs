-- /show

data Thing = Shoe 
            | Ship
            | King
            | SealingWax
            | Cabbage
    deriving Show

-- Store person's name, age and favourite thing

data Person = Person String Int Thing
    deriving Show

--show

checkFav :: Person -> String
checkFav (Person n _ SealingWax) = n ++ ", you're my kind of person!"
checkFav (Person n _ _ ) = n ++ ",your favourite thing is lame."

main = putStrLn (checkFav (Person "Alice" 30 SealingWax))