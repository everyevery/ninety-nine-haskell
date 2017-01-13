 {-# OPTIONS_GHC -Wno-type-defaults #-}

module NinetynineSpec (test, spec) where

import Test.Hspec
import Test.QuickCheck

import Control.Exception

import Ninetynine

-- Hspec: http://hspec.github.io/index.html
-- QuickCheck: http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html

test :: IO ()
test = hspec spec

spec :: Spec
spec = parallel $ do
  describe "hspec tutorial" $ do
    it "can parse integers" $ do
      read "10" `shouldBe` (10 :: Int)
    it "pending test" $ do
      pending
      pendingWith "need to fix with message"
    it "expecting equality" $ do
      (+1) 0 `shouldBe` 1
    it "require that a predicate holds" $ do
      1 `shouldSatisfy` (== 1)
    it "expecting exceptions" $ do
      -- anyException, anyErrorCall, anyIOException, anyArithException
      evaluate (head []) `shouldThrow` anyException
      evaluate (1 `div` 0) `shouldThrow` anyArithException


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
      flatten (Elem 5) `shouldBe` [5]
    it "Flatten a empty list structure" $ do
      (flatten (List []) :: [Integer]) `shouldBe` ([] :: [Integer])

  describe "compress" $ do
    it "Eliminate consecutive duplicates of list elements" $ do
      compress "aaaabccaadeeee" `shouldBe` "abcade"
  describe "pack" $ do
    it "Packconsecutive duplicates of list elements into sublists" $ do
      pack "aaabbcaabcc" `shouldBe` ["aaa","bb","c","aa","b","cc"]
  describe "encode" $ do
    it "Run-length encoding of a list" $ do
      encode "aaaabccaadeeee" `shouldBe` [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
      
  describe "encodeModified" $ do
    it "Run-length encoding of a list. If there is non duplication, just copy" $ do
      encodeModified "aaaabccaadeeee" `shouldBe` [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']

  describe "decodeModified" $ do
    it "Decode a run-length encoded list" $ do
      decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e'] `shouldBe` "aaaabccaadeeee" 

  describe "dupli" $ do 
    it "Duplicate the elements of a list." $ do
      dupli [1,2,3] `shouldBe` [1,1,2,2,3,3]

  describe "repli" $ do 
    it "Replicate the elements of a list a given number of times." $ do
      repli [1,2,3] 3 `shouldBe` [1,1,1,2,2,2,3,3,3]
      
  describe "dropEvery" $ do 
    it "Drop every N'th element from a list." $ do
      dropEvery "abcdefghik" 3 `shouldBe` "abdeghk"

  describe "slice'" $ do
    it "Extract a slice from a list." $ do
      slice' [1,2,3,4,5,6,7, 8, 9] 4 7 `shouldBe` [4, 5, 6, 7]
    
  describe "rotate" $ do
    it "Rotate a list N places to the lieft" $ do
      rotate ['a','b','c','d','e','f','g','h'] 3 `shouldBe` "defghabc"
      rotate ['a','b','c','d','e','f','g','h'] (-2) `shouldBe` "ghabcdef"

  describe "remove the K'th element" $ do
    it "Remove the K'th element from a list." $ do
      remove_at [1,2,3,4] 2 `shouldBe` [1,3,4]

  describe "InsertAt" $ do
    it "Insert an element a a given position" $ do
      insert_at 4 [1,2,3,5] 4 `shouldBe` [1,2,3,4,5] 
