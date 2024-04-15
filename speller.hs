makeSentence :: String -> String
makeSentence word = take 1 word ++ " is for " ++ word

speller :: [String] -> String
speller []  = ""
speller [x] = makeSentence (head [x])
speller wordlst =
    do
        let sentencelst = map makeSentence (reverse wordlst)
        let last_item = ["and " ++ head sentencelst]
        let rest = map (++", ") (tail sentencelst)
        let book = last_item ++ rest
        concat (reverse book)