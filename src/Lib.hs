module Lib where

import Data.Foldable

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- problem 1
myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs

-- problem 2
myButLast :: [a] -> a
myButLast (x:[_]) = x
myButLast (_:xs) = myButLast xs

-- problem 3
elementAt :: [a] -> Int -> a
-- elementAt xs i = xs !! (i-1)
-- elementAt = flip $ (last .) . take
elementAt (x:_) 1 = x
elementAt (_:xs) i = elementAt xs (i-1)

-- problem 4
myLength :: [a] -> Int
myLength = foldl' (\b _ -> b+1) 0
