module Elementary.HigherOrderFunctions where

import Data.Char (toLower)

usual :: (Ord a) => a -> a -> Ordering
usual = compare

descending x y = compare y x

insensitive x y = compare (toLower x) (toLower y)

quickSort' :: (Ord a) => (a -> a -> Ordering) -> [a] -> [a]
quickSort' _ [] = []
quickSort' c (x : xs) = (quickSort' c less) ++ (x : equal) ++ (quickSort' c more)
  where
    less = filter (\y -> y `c` x == LT) xs
    equal = filter (\y -> y `c` x == EQ) xs
    more = filter (\y -> y `c` x == GT) xs

for :: a -> (a -> Bool) -> (a -> a) -> (a -> IO ()) -> IO ()
for i p f job
  | p i = return ()
  | otherwise = do 
      job i
      for (f i) p f job