module Basics where

absolute x
  | x < 0 = -x
  | otherwise = x

tuplerize l = (head l, tail l)

getFifth l = fifth l 4
  where 
  fifth [] _ = Nothing
  fifth ls n
          | n <= 0 = Just(head ls)
          | otherwise = fifth (tail ls) (n-1)

-- 1.
area b h = b*h/2

-- main = do
--   putStrLn "The base?"
--   base <- getLine
--   putStrLn "The height?"
--   height <- getLine
--   putStrLn ("The area of that triangle is " ++ (show $ area (read base) (read height)))

-- tuplerize :: [a] -> (a, [a])
-- getFifth :: [a] -> Maybe a
-- h :: (Int a) => a -> b -> c -> Char
