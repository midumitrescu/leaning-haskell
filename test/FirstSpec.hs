module FirstSpec where

import Test.Hspec
import Prelude
import Lib

main :: IO ()

alwaysFive :: Int
alwaysFive = 5

somePrimes :: [Int]
somePrimes = [3, 5, 7, 11]

main = hspec spec

spec :: Spec

spec = do
  describe "Working with List" $ do
    it "returns the first element of a list" $ head [23,24 ..] `shouldBe` 23
    it "summs all elements of a list" $ sum [1 .. 1000] `shouldBe` 500500
    it "concatenates 2 lists" $ somePrimes ++ [13, 17] `shouldBe` [3, 5, 7, 11, 13, 17]
    it "constructs the list using :" $ 3 : 5 : 7 : 11 : [] `shouldBe` [3, 5, 7, 11]
    it "can prefix a list with cons" $ 2 : somePrimes `shouldBe` [2, 3, 5, 7, 11]
    it "computes the correct length" $ length somePrimes `shouldBe` 4
    it "reverses correctly" $ reverse somePrimes `shouldBe` [11, 7, 5, 3]
    it "recognizes correctly if the list is empty" $ do
      null somePrimes `shouldNotBe` True
      True `shouldBe` True
      null (tail [1]) `shouldBe` True
    it "correctly extracts elements" $ do
      head somePrimes `shouldBe` 3
      somePrimes !! 1 `shouldBe` 5
      somePrimes !! 2 `shouldBe` 7
    it "extracts head and last" $ do
      head somePrimes `shouldBe` 3
      last somePrimes `shouldBe` 11
    it "extracts head and last" $ init somePrimes `shouldBe` [3, 5, 7]
    it "extracts first elements" $ take 2 somePrimes `shouldBe` [3, 5]
    it "extracts first elements" $ drop 2 somePrimes `shouldBe` [7, 11]
    it "identifies correctly if an element is in a list" $ do
      2 `elem` somePrimes `shouldBe` False
      3 `elem` somePrimes `shouldBe` True
    it "identifiex max correctly" $ maximum somePrimes `shouldBe` 11
    it "generates lists" $ [1 .. 5] `shouldBe` [1, 2, 3, 4, 5]
    it "generates lists in steps" $ [2,4 .. 10] `shouldBe` [2, 4, 6, 8, 10]
    it "can generate an infinite list" $ [0,2 ..] !! 100 `shouldBe` 200
  describe "Working with number" $ do
    it "returns the max long" $ maxBound `shouldBe` (9223372036854775807 :: Int)
    it "should substract infix" $ 5 - 4 `shouldBe` 1
    it "allows conversion from numericals" $ sqrt (fromIntegral 9) `shouldBe` (3.0 :: Float)
    it "accesses the correct constant vale" $ alwaysFive `shouldBe` 5
  describe "modulo works" $ do
    it "in prefix" $ mod 5 4 `shouldBe` 1
    it "in infix" $ 5 `mod` 4 `shouldBe` 1
