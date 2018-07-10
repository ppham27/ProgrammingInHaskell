module Chapter02Spec where

import Test.Hspec
import Test.QuickCheck

import Chapter02

main = spec

spec :: Spec
spec = describe "Chapter 2" $ do
  lastSpec

lastSpec :: Spec
lastSpec = describe "last" $ do
  it "should retrieve the last element of a list" $
    let lastProperty xs = last' (xs :: [Int]) == last xs in
      forAll (listOf1 arbitrary) lastProperty

  it "should throw an error on an empty list" $
    last' [] `shouldThrow` errorCall "empty list"
