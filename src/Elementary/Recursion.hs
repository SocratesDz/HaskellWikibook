module Elementary.Recursion where 

doublefactorial :: Int -> Int
doublefactorial n 
  | n <= 0 = 1
  | otherwise = n * doublefactorial (n-2)

power x y
  | y <= 0 = 1
  | otherwise = (power x (y-1)) * x

addition x y
  | x < 0 && y < 0 = (-(addition (-x) (-y)))
  | x == 0 = y
  | y == 0 = x
  | y < 0 = addition (plusOne y) (x-1)
  | otherwise = addition (plusOne x) (y-1)
  where
    plusOne x = x + 1

log2 1 = 0
log2 x = log2' x 0 0
  where 
    log2' x y prev
      | x == 2**y = y
      | x < 2**y = prev
      | otherwise = log2' x (y+1) y

replicate' :: Int -> a -> [a]
replicate' n x = 
  reply n x []
  where
    reply 0 _ l = l
    reply 1 x [] = [x]
    reply n x ls = reply (n-1) x (x:ls)

get :: [a] -> Int -> a
get ls i = get' 0 ls
  where
    get' acc (x:xs)
      | acc == i = x
      | otherwise = get' (acc+1) xs

zip' :: [a] -> [b] -> [(a, b)]
zip' cola colb = zipIn' cola colb []
  where
    zipIn' [] _ acc = acc
    zipIn' _ [] acc = acc
    zipIn' (x:xs) (y:ys) acc = zipIn' xs ys (acc ++ [(x, y)])

length' :: [a] -> Int
length' xs = length'' xs 0
  where
    length'' [] acc = acc
    length'' (x:xs) acc = length'' xs (acc+1)