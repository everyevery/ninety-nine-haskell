module Main where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate, bracket)

import Lib

beforeHook :: IO ()
beforeHook = putStrLn "before"

afterHook :: IO ()
afterHook = putStrLn "after"

aroundHook :: IO () -> IO ()
aroundHook action = bracket (putStrLn "pre") (const $ putStrLn "post") (const action)
  

main :: IO ()
main = hspec $ before_ beforeHook $ do
  describe "myLast" $ after_ afterHook $ 
    it "returns the last element of a list" $ 
      myLast [23..100] `shouldBe` (100 :: Int)
  describe "myButLast" $ after_ afterHook $ around_ aroundHook $ 
    it "returns last but one element of a list" $ 
      myButLast [23..100] `shouldBe` (99 :: Int)
  describe "elementAt" $ 
    it "returns k'th elemenf of a list" $
      elementAt [1..100] 2  `shouldBe` (2 :: Int)
  describe "myLength" $
    it "return the number of elements of a list" $
      myLength [1..10] `shouldBe` (10 :: Int)
  describe "myReverse" $
    it "reverse a list" $
      myReverse [1,2,3,4] `shouldBe` [4,3,2,1]
  describe "isPalindrome" $ do
    it "find out whether a list is a palindrome" $ do
      isPalindrome [1,2,3] `shouldBe` False
      isPalindrome [1,2,2,1] `shouldBe` True
  describe "flatten" $
    it "tlatten a nested list structure" $ do
      flatten (List' [Elem 2, List' [Elem 3, Elem 4, List' [Elem 5], List' [Elem 6]]]) `shouldBe` [2,3,4,5, 6]
      flatten (Elem 3) `shouldBe` [3]
  describe "compress" $
    it "eleminate consecutive duplicates of list elements" $
      compress "aaaabccaadeeee" `shouldBe` "abcade"
