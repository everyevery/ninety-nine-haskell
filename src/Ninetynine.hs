 {-# LANGUAGE ViewPatterns #-}

module Ninetynine where


someFunc :: IO ()
someFunc = putStrLn "someFunc"

myLast :: [a] -> a
myLast (a:[]) = a
myLast (a:as) = myLast as
myLast _ = error "empty list"

myButLast :: [a] -> a
myButLast (a:b:[]) = a
myButLast (a:as) = myButLast as
myButLast _ = error "empty list"

elementAt :: [a] -> Int -> a
elementAt (a:_) 1 = a
elementAt (_:as) n = elementAt as (n-1)

myLength :: [a] -> Int
myLength [] = 0
myLength (a:as) = 1 + myLength as

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []
