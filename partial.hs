--no son aplicables a todos los tipos de datos. 

--fromJust

--no: 

if isNothing mx
    then gcdelse h (fromJust mx)

--si: 

case mx of
    Nothing -> g
    Just -> x h x 
 
 --o:

 maybe g h mx 

--head, tail

--no: 

if null xs
    then g
    else h (head xs) (tail xs)

--si:



