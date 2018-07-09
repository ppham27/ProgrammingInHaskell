module Chapter01Spec where

import Data.List
import Test.Hspec
import Test.QuickCheck

import Chapter01

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Chapter 1" $ do
  doubleSpec
  sumSpec
  productSpec
  qsortSpec
  qsortReverseSpec

doubleSpec :: Spec
doubleSpec = describe "double" $
  it "should double numbers" $
    double (double 2) `shouldBe` 8

sumSpec :: Spec
sumSpec = describe "sum" $
  it "should sum a single number" $
    property $ \x -> sum [x] == (x :: Int)  

productSpec :: Spec
productSpec = describe "product" $ do
  it "computes the product of an empty list" $
    product' [] `shouldBe` (1 :: Int)
  
  it "computes the product of a list of numbers" $
    product' [3, 4, 5] `shouldBe` (60 :: Int)

qsortSpec :: Spec
qsortSpec = describe "qsort" $ do
  it "should sort a list with repeats" $
    qsort [2,2,3,1,1] `shouldBe` ([1,1,2,2,3] :: [Int])
      
  it "should sort lists" $
    property $ \xs -> qsort (xs :: [Int]) == sort xs

-- qsortReverseSpec :: Spec
qsortReverseSpec = describe "qsortReverse" $ do
  it "should sort a list with repeats in reverse" $
    qsortReverse [2,2,3,1,1] `shouldBe` ([3,2,2,1,1] :: [Int])

  it "should sort lists in descening order" $
    property $ \xs -> qsortReverse (xs :: [Int]) == reverse (sort xs)
