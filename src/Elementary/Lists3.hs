module Elementary.Lists3 where

recurSum :: Num a => [a] -> a
recurSum [] = 0
recurSum (x:xs) = x + recurSum xs

recurProduct :: Num a => [a] -> a
recurProduct [] = 1
recurProduct (x:xs) = x * recurProduct xs

recurConcat :: [[a]] -> [a]
recurConcat [] = []
recurConcat (x:xs) = x ++ recurConcat xs

sum' :: [Int] -> Int
sum' = foldr (+) 0

product' :: [Int] -> Int
product' = foldr (*) 1

concat' :: [[a]] -> [a]
concat' = foldr (++) []

recurAnd' :: [Bool] -> Bool
recurAnd' [] = True
recurAnd' (x:xs) = x && recurAnd' xs

and' :: [Bool] -> Bool
and' = foldr (&&) True

recurOr' :: [Bool] -> Bool
recurOr' [] = False
recurOr' (x:xs) = x || recurOr' xs

or' :: [Bool] -> Bool
or' = foldr (||) False

maximum' :: Ord a => [a] -> a
maximum' = foldr1 max

minimum' :: Ord a => [a] -> a
minimum' = foldr1 min

{-|Must check wikibook answer
reverse' :: [a] -> [a]
reverse' = foldl (\x xs -> [x] ++ xs) []
-}

recurScanr' :: (a -> b -> b) -> b -> [a] -> [b]
recurScanr' _ acc [] = [acc]
recurScanr' f acc (x:xs) = (f x acc) : recurScanr' f (f x acc) xs

scanr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' _ acc [] = [acc]
scanr' f acc (x:xs) = (f x acc) : [foldr f (f x acc) xs]

factList :: Integer -> [Integer]
factList n = scanl (*) 1 [2..n]

returnDivisible :: Int -> [Int] -> [Int]
returnDivisible n = filter (\x -> (mod x n) == 0)

choosingTails :: [[Int]] -> [[Int]]
choosingTails xs = [ls | (l:ls) <- xs, l > 5]

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = [x | x <- xs, (f x)]

map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

doubleOfFirstForEvenSeconds :: [(Int, Int)] -> [Int]
doubleOfFirstForEvenSeconds ps = map ((2*) . fst) $ filter (isEven . snd) ps
  where
    isEven n = (mod n 2) == 0

-- showDate :: Anniversary -> String