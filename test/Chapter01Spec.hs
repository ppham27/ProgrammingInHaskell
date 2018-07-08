module Chapter01Spec where

import Test.Hspec
import Test.QuickCheck

import Chapter01

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Chapter 1" $ do
    it "computes the product of a list of numbers" $
      product' [3, 4, 5] `shouldBe` (60 :: Int)

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)
