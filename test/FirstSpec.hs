module FirstSpec where

import           Lib
import           Prelude
import           Test.Hspec

main :: IO ()
alwaysFive :: Int
alwaysFive = 5

somePrimes :: [Int]
somePrimes = [3, 5, 7, 11]

main = hspec spec

repeated :: String -> Int -> [String]
repeated string n
  | n == 0 = []
  | otherwise = string : repeated string (n - 1)

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
    it "can work with mathematical deffinitions of lists" $ [x * 2 | x <- [1 .. 10]] `shouldBe` [2,4 .. 20]
    it "can add predicates in list generation" $ [x * 2 | x <- [1 .. 10], x `mod` 3 == 0] `shouldBe` [6, 12, 18]
  describe "Working with number" $ do
    it "returns the max long" $ maxBound `shouldBe` (9223372036854775807 :: Int)
    it "should substract infix" $ 5 - 4 `shouldBe` 1
    it "allows conversion from numericals" $ sqrt (fromIntegral 9) `shouldBe` (3.0 :: Float)
    it "accesses the correct constant vale" $ alwaysFive `shouldBe` 5
  describe "modulo works" $ do
    it "in prefix" $ mod 5 4 `shouldBe` 1
    it "in infix" $ 5 `mod` 4 `shouldBe` 1
  describe "texas lists" $ it "converts from list of chars to string " $ ['a' .. 'd'] `shouldBe` "abcd"
  describe "comparison in lists" $ do
    it "compares first element first" $ [3, 10] > [2, 11] `shouldBe` True
    it "compares first element first" $ [3, 10] <= [2, 11] `shouldBe` False
    it "compares the second element, if the first is equal" $ [3, 10] > [3, 11] `shouldBe` False
    it "continues to the third element" $ [3, 15, 2] > [3, 15, 2, 10] `shouldBe` False
    it "then goed to the firth element, etc" $ [3, 15, 2, 10, -1] > [3, 15, 2, 10] `shouldBe` True
  describe "mathematical list declarations" $ do
    it "can work as an operator" $ do
      let boomBangs xs =
            [ if x < 10
              then "BOOM!"
              else "BANG!"
            | x <- xs
            , odd x
            ]
      boomBangs [5 .. 13] `shouldBe` ("BOOM!" `repeated` 3 ++ "BANG!" `repeated` 2)
    it "can combine multiple generators" $ do
      [(x ^ 2) + x * y | x <- [2, 5, 10], y <- [1, 2, 3]] `shouldBe` [6, 8, 10, 30, 35, 40, 110, 120, 130]
      
  describe "Implementations from Chapter 3" $ do
    describe "Minima" $ do
      it "applies correctly on a list of ints" $ do
        minima 1 [3, 1, 10] `shouldBe` [1]
        minima 2 [3, 1, 10] `shouldBe` [1, 3]
        minima 0 [3, 1, 10] `shouldBe` []
      it "applies correctly on a list of chars" $ do
        minima 1 "qwerty" `shouldBe` ['e']
        minima 2 "qwerty" `shouldBe` "eq"
        minima 0 "qwerty" `shouldBe` ""
    describe "remove uppercase strigs" $
      it "parses strings correctly" $ do
        removeUpperCases "aaaBBA" `shouldBe` "aaa"
        removeUpperCases "Mihai" `shouldBe` "ihai"
    describe "an ecuality operator" $
      it "can be defined infix" $ do
        1 =? 1 `shouldBe` True
        1 =? 2 `shouldBe` False
        "a" =? "a" `shouldBe` True
        "a" =? "b" `shouldBe` False