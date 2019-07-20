module Elementary.Lists2 where

import Data.List

takeInt :: Int -> [a] -> [a]
takeInt _ [] = []
takeInt n (x:xs) = takeInt' n (x:xs) []
  where
    takeInt' 0 _ acc = acc
    takeInt' n (l:ls) acc = takeInt' (n-1) ls (acc ++ [l])

-- Book solution:
takeInt'          :: Int -> [a] -> [a]
takeInt' 0 _      = []
takeInt' _ []     = []
takeInt' n (x:xs) = x : takeInt' (n-1) xs

dropInt :: Int -> [a] -> [a]
dropInt 0 xs = xs
dropInt _ [] = []
dropInt n (x:xs) = dropInt (n-1) xs

sumInt :: Num a => [a] -> a
sumInt [] = 0
sumInt (x:xs) = x + (sumInt xs)

scanSum :: Num a => [a] -> [a]
scanSum [] = []
scanSum xs = scan' xs [] 0
  where
    scan' [] acc _ = acc
    scan' [l] acc prev = acc ++ [l + prev]
    scan' (l:ls) acc prev = scan' ls (acc ++ [l+prev]) (l+prev)

diffs :: Num a => [a] -> [a]
diffs [] = []
diffs xs = diffs' xs []
  where
    diffs' [] acc = acc
    diffs' [y] acc = acc ++ [y]
    diffs' (x:y:[]) acc = acc ++ [abs (x-y)]
    diffs' (x:y:xs) acc = diffs' (y:xs) (acc ++ [abs (x-y)])

--{ Finally using maps }--

divisorList :: [Integer] -> [[Integer]]
divisorList xss = map divisors xss
  where
    divisors p = [f | f <- [1..p], p `mod` f == 0]

encode :: [Char] -> [(Int, Char)]
encode chars = map encfunc $ group chars
  where
    encfunc chars = (length chars, head chars)

decode :: [(Int, Char)] -> [Char]
decode encoded = concat $ map decfunc encoded
  where
    decfunc (n, x) = replicate n x

last' :: [a] -> a
last' [] = error "Empty list"
last' [x] = x
last' (_:xs) = last' xs

init' :: [a] -> [a]
init' [] = error "Empty list"
init' [x] = []
init' (x:xs) = x : init' xs