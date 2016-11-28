module NinetynineSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Ninetynine

spec :: Spec
spec = do
  describe "myLast" $ do
    it "find the last element of a list" $ do
      myLast [1,2,3,4] `shouldBe` 4

  describe "myButLast" $ do
    it "find the last but one element of a list" $ do
      myButLast [1,2,3,4] `shouldBe` 3

  describe "myReverse" $ do
    it "reverse a list" $ property $
      \xs -> (myReverse . myReverse) xs == (xs :: [Int])

  describe "isPalindrome" $ do
    it "Find out whether a list is a palindrome" $ do
      isPalindrome [1,2,3] `shouldBe` False

  describe "flatten" $ do
    it "Flatten a nested list structure" $ do
      flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) `shouldBe` [1,2,3,4,5]
--      flatten (List []) `shouldBe` []
--      flatten (Elem 5) `shouldBe` [5]

