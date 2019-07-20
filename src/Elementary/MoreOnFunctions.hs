module Elementary.MoreOnFunctions where
    
-- map f xs where f x = x * 2 + 3
ex1a xs = map (\x -> x * 2 + 3) xs

-- let f x y = read x + y in foldr f 1 xs
ex1b xs = foldr (\x y -> read x + y) xs

-- (4+) => (\x -> 4 + x)
-- (1 `elem`) => (\x -> 1 `elem` x)
-- (`notElem` "abc") => (\x -> x `notElem` "abc")