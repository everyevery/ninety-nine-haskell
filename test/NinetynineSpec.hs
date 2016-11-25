module NinetynineSpec (spec) where

import Test.Hspec
import Ninetynine

spec :: Spec
spec = do
  describe "myLast" $ do
    it "find the last element of a list" $ do
      myLast [1,2,3,4] `shouldBe` 4
  describe "myButLast" $ do
    it "find the last but one element of a list" $ do
      myButLast [1,2,3,4] `shouldBe` 3
