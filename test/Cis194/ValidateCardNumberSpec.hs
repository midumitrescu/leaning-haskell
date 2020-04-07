module Cis194.ValidateCardNumberSpec where

import           Cis194.ValidateCardNumber
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "toDigits" $ do
    it "covertsCorrectly 0 to null" $ toDigits 0 `shouldBe` []
    it "covertsCorrectly -17 to null" $ toDigits (-17) `shouldBe` []
    it "covertsCorrectly 1234" $ toDigits 1234 `shouldBe` [1, 2, 3, 4]
  describe "toDigitsRev" $ do
    it "covertsCorrectly 0 to null" $ toDigitsRev 0 `shouldBe` []
    it "covertsCorrectly -17 to null" $ toDigitsRev 0 `shouldBe` []
    it "covertsCorrectly 1234" $ toDigitsRev 1234 `shouldBe` [4, 3, 2, 1]
  describe "double every second digit" $ do
    it "doubles correctly" $ do
      doubleEveryOther [] `shouldBe` []
      doubleEveryOther [1] `shouldBe` [1]
      doubleEveryOther [13] `shouldBe` [13]
      doubleEveryOther [1, 2] `shouldBe` [2, 2]
      doubleEveryOther [13, 7] `shouldBe` [26, 7]
      doubleEveryOther [1, 2, 3] `shouldBe` [1, 4, 3]
      doubleEveryOther [1, 2, 3] `shouldBe` [1, 4, 3]
      doubleEveryOther [13, 7, 24] `shouldBe` [13, 14, 24]
      doubleEveryOther [1, 2, 3, 4] `shouldBe` [2, 2, 6, 4]
      doubleEveryOther [13, 7, 24, 15] `shouldBe` [26, 7, 48, 15]
      doubleEveryOther [8, 7, 6, 5] `shouldBe` [16, 7, 12, 5]
    describe "add digits" $ do
      it "of empty" $ sumDigits [] `shouldBe` 0
      it "of digit" $ do
        sumDigits [0] `shouldBe` 0
        sumDigits [1] `shouldBe` 1
        sumDigits [9] `shouldBe` 9
      it "two digit numbers" $ do
        sumDigits [10] `shouldBe` 1
        sumDigits [11] `shouldBe` 2
        sumDigits [12] `shouldBe` 3
        sumDigits [26] `shouldBe` 8
      it "array of only digits" $ do
        sumDigits [1, 2, 3] `shouldBe` 6
        sumDigits [1 .. 9] `shouldBe` 45
        sumDigits [5 .. 9] `shouldBe` 35
      it "array of two digits" $ do
        sumDigits [11, 12, 13] `shouldBe` 9
        sumDigits [11 .. 19] `shouldBe` 54
        sumDigits [26 .. 28] `shouldBe` 27
        sumDigits [16, 7, 12, 5] `shouldBe` 22
    describe "validating card" $ do
      it "returns true for valid examples" $
        map validate [4012888888881881, 4916286237921762] `shouldMatchList` [True, True]
      it "returns false for 4012888888881882 example" $
        map validate [4012888888881882, 4916286237921763] `shouldMatchList` [False, False]
