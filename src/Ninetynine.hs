 {-# LANGUAGE ViewPatterns #-}

module Ninetynine where

import Data.List


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

isPalindrome :: Eq a => [a] -> Bool
isPalindrome a = a == (myReverse a)

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List as) = concat $ map flatten as

compress [] = []
compress xs = map head (group xs)

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack xss@(x:xs) = takeWhile (== x) xss : (pack $ dropWhile (== x) xss)


encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode xs = map (\x -> (length x, head x)) $ pack xs

data Encoded a = Multiple Int a | Single a deriving (Show, Eq)

encodeModified :: Eq a => [a] -> [Encoded a]
encodeModified = map convertToEncoded  . encode
                    where convertToEncoded (1, c) = Single c
                          convertToEncoded (n, c) = Multiple n c

decodeModified :: [Encoded a] -> [a]
decodeModified = concatMap helper 
                    where helper (Single c) = [c]
                          helper (Multiple n c) = replicate n c 

dupli :: [a] -> [a]
dupli xs  = concatMap (\x -> [x,x]) xs

repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = let (f, s) =  splitAt n' xs
                     n' = n - 1
                     s' = if length s == 0 then [] else (tail s)
                 in f ++ (dropEvery s' n)

split' :: [a] -> Int -> ([a], [a])
split' xs n = (take n xs, drop n xs)
