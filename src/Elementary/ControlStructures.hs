module Elementary.ControlStructures where

fakeIf :: Bool -> a -> a -> a
fakeIf q x y = case q of 
  True -> x
  False -> y

doGuessing num = do
  putStrLn "Enter your guess:"
  guess <- getLine
  case compare (read guess) num of
    LT -> do putStrLn "Too low!"
             doGuessing num
    GT -> do putStrLn "Too high!"
             doGuessing num
    EQ -> do putStrLn "You Win!"
