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

-- problem 5
myReverse :: [a] -> [a]
myReverse = foldl' (\b a -> a:b) []

myReverse' :: [a] -> [a]
myReverse' xs = myReverse'' xs []
  where
    myReverse'' [] ys = ys
    myReverse'' (x:xs) ys = myReverse'' xs (x:ys)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == myReverse xs 

data NestedList a = Elem a | List' [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List' x) = concatMap flatten x

compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = x : compress (dropWhile (==x) xs)
