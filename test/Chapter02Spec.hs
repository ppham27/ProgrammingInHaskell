module Chapter02Spec where

import Test.Hspec
import Test.QuickCheck

import Chapter02

main = spec

spec :: Spec
spec = describe "Chapter 2" $ do
  expressionsSpec
  divideSpec
  lastSpec

expressionsSpec :: Spec
expressionsSpec = describe "expressions" $ do
  it "should compute 2^3*4" $
    2^3*4 `shouldBe` (2^3)*4

  it "should compute 2*3+4*5" $
    2*3+4*5 `shouldBe` (2*3)+(4*5)

  it "should compute 2+3*4^5" $
    2+3*4^5 `shouldBe` 2+(3*(4^5))

divideSpec :: Spec
divideSpec = describe "divide" $
  it "should do division" $ do
    let a = 10
    let xs = [1,2,3,4,5] in
      a `div` length xs `shouldBe` 2

lastSpec :: Spec
lastSpec = describe "last" $ do
  it "should retrieve the last element of a list" $
    let lastProperty xs = last' (xs :: [Int]) == last xs in
      forAll (listOf1 arbitrary) lastProperty

  it "should throw an error on an empty list" $
    last' [] `shouldThrow` errorCall "empty list"
