module Main where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Lib

main :: IO ()
main = hspec $ do
  describe "myLast" $ 
    it "returns the last element of a list" $ 
      myLast [23..100] `shouldBe` (100 :: Int)
  describe "myButLast" $ 
    it "returns last but one element of a list" $ 
      myButLast [23..100] `shouldBe` (99 :: Int)
  describe "elementAt" $ 
    it "returns k'th elemenf of a list" $
      elementAt [1..100] 2  `shouldBe` (2 :: Int)
  describe "myLength" $
    it "return the number of elements of a list" $
      myLength [1..10] `shouldBe` (10 :: Int)
