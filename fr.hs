firstLetter :: String -> String
firstLetter [] = "Ein leerer String hat keinen ersten Buchstaben"
firstLetter text@(x:xs) = "Der erste Buchstabe von " ++ text ++ " ist: " ++ [x] 