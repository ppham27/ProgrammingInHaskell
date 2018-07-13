module Chapter02Spec where

import Control.Exception (evaluate)
import qualified Data.List.NonEmpty as NonEmpty
import Test.Hspec
import Test.QuickCheck

import Chapter02

main = spec

spec :: Spec
spec = describe "Chapter 2" $ do
  expressionsSpec
  divideSpec
  lastSpec
  initSpec

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

  it "should always work with non-empty lists" $
    property $ \(Test.QuickCheck.NonEmpty xs) ->
    let ys = NonEmpty.nonEmpty (xs :: [Int]) in
      case ys of
        Just zs -> NonEmpty.last zs == last' xs
        Nothing -> False

initSpec :: Spec
initSpec = describe "init" $ do
  it "should be drop the last element" $
    let initProperty xs = init' (xs :: [Int]) == reverse (tail (reverse xs)) in
      forAll (listOf1 arbitrary) initProperty

  it "should throw an error on an empty list" $
    evaluate (init' []) `shouldThrow` errorCall "empty list"
