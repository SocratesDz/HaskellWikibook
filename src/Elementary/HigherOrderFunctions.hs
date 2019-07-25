module Elementary.HigherOrderFunctions where

import Data.Char (toLower)

usual :: (Ord a) => a -> a -> Ordering
usual = compare

descending x y = compare y x

insensitive :: [Char] -> [Char] -> Ordering
insensitive x y = compare (toLower' x) (toLower' y)
  where toLower' = map toLower

quickSort' :: (Ord a) => (a -> a -> Ordering) -> [a] -> [a]
quickSort' _ [] = []
quickSort' c (x : xs) = (quickSort' c less) ++ (x : equal) ++ (quickSort' c more)
  where
    less = filter (\y -> y `c` x == LT) xs
    equal = filter (\y -> y `c` x == EQ) xs
    more = filter (\y -> y `c` x == GT) xs

for :: a -> (a -> Bool) -> (a -> a) -> (a -> IO ()) -> IO ()
for i p f job
  | (not . p) i = return ()
  | otherwise = do 
      job i
      for (f i) p f job

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x, y) = f x y

const' :: a -> b -> a
const' a _ = a

-- (uncurry const) :: (a, b) -> c
-- (curry fst) :: a -> b -> c
-- (curry swap) :: a -> b -> (b, a)

-- TODO: Review foldr and foldl functions
-- fold'' = foldr
