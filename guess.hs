--Guessing game: Starman

check :: String -> String -> Char -> (Bool, String)
-- check whether a single char is in the mystery word
check word display c = (c `elem` word, [if x==c 
    then c 
    else y | (x,y) <- zip word display])

turn :: String -> String -> Int -> IO ()
-- single turn for user
turn word display n =
    do if n == 0
        then putStrLn "You lose!"
        else if word == display
            then putStrLn "You win!"
            else mkguess word display n

mkguess :: String -> String -> Int -> IO ()
-- user inputs a single char (first on the line)
mkguess word display n = 
    do putStrLn (display ++ "  " ++ take n (repeat '*'))
       putStr "  Enter your guess: "
       q <- getLine
       let (correct, display') = check word display (q!!0)
       let n' = if correct then n else n-1
       turn word display' n'

starman :: String -> Int -> IO ()
starman word n = turn word ['-' | x <- word] n