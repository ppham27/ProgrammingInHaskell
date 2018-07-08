module Chapter01Spec where

import Test.Hspec
import Test.QuickCheck

import Chapter01

main :: IO ()
main = hspec spec

spec :: Spec
spec =  
  describe "Chapter 1" $ do
    productSpec
    sumSpec



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
