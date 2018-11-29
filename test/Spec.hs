import Test.Hspec
import Prelude
import Lib

main :: IO ()

alwaysFive :: Int
alwaysFive = 5

somePrimes :: [Int]
somePrimes = [3, 5, 7, 11]

main = hspec $ do

  describe "Working with List" $ do
    it "returns the first element of a list" $ do
     head [23, 24 ..] `shouldBe` (23 :: Int)

    it "summs all elements of a list" $ do
      sum [1..1000] `shouldBe` (500500 :: Int)

    it "concatenates 2 lists" $ do
      somePrimes ++ [13, 17] `shouldBe` [3, 5, 7, 11, 13, 17 :: Int]

    it "constructs the list using :" $ do
       3 :  5 :  7 :  11 : [] `shouldBe` [3, 5, 7, 11 :: Int]

    it "can prefix a list with cons" $ do
      2 : somePrimes `shouldBe` [2, 3, 5, 7, 11 :: Int]

    it "computes the correct length" $ do
      length somePrimes `shouldBe` 4

    it "reverses correctly" $ do
      reverse somePrimes `shouldBe` 11 : 7 : 5 : 3 : []

    it "recognizes correctly if the list is empty" $ do
      null somePrimes `shouldNotBe` True
      null [] `shouldBe` True
      null (tail [1 :: Int]) `shouldBe` True

    it "correctly extracts elements" $ do
      somePrimes !! 0  `shouldBe` (3 :: Int)
      somePrimes !! 1  `shouldBe` (5 :: Int)
      somePrimes !! 2  `shouldBe` (7 :: Int)

    it "extracts head and last" $ do
      head somePrimes `shouldBe` (3 :: Int)
      last somePrimes `shouldBe` (11 :: Int)

    it "extracts head and last" $ do
      init somePrimes `shouldBe` [3, 5, 7:: Int]

    it "extracts first elements" $ do
      take 2 somePrimes `shouldBe` [3, 5:: Int]

    it "extracts first elements" $ do
      drop 2 somePrimes `shouldBe` [7, 11:: Int]

    it "identifies correctly if an element is in a list" $ do
      2 `elem` somePrimes `shouldBe` False
      3 `elem` somePrimes `shouldBe` True

    it "identifiex max correctly" $ do
     maximum somePrimes `shouldBe` (11 :: Int)

    it "generates lists" $ do
      [1..5] `shouldBe` [1, 2, 3, 4, 5::Int]
    it "generates lists in steps" $ do
      [2, 4..10] `shouldBe` [2, 4, 6, 8, 10::Int]

    it "can generate an infinite list" $ do
      [0, 2..] !! 100 `shouldBe` (200:: Int)


  describe "Working with number" $ do
    it "returns the max long" $ do
      maxBound `shouldBe` ( 9223372036854775807 :: Int)
    it "should substract infix" $ do
      5 - 4 `shouldBe` (1 :: Int)
    it "allows conversion from numericals" $ do
      sqrt  (fromIntegral(9 :: Int)) `shouldBe` (3.0 :: Float)


    it "accesses the correct constant vale" $ do
      alwaysFive `shouldBe` (5 :: Int)

  describe "modulo works" $ do
    it "in prefix" $ do
      mod 5 4 `shouldBe` (1 :: Int)
    it "in infix" $ do
      5 `mod` 4 `shouldBe` (1 :: Int)

  describe "Working with monads" $ do
    it "differs between Just and Nothing" $ do
     _lift 3  `shouldBe` JustMon 3
     _lift 3  `shouldNotBe` NothingMon

    it "correctly binds between containers" $ do
     _lift 3  Lib.>>= (\x -> _lift (x + 3))  `shouldBe` JustMon 6
     _lift 3  Lib.>>= (\x -> _lift (x ** x))  `shouldBe` JustMon 27
     NothingMon  Lib.>>= (\x -> _lift (x ** x))  `shouldBe` NothingMon

    it "can add a number in a Monad" $ do
       3 `plus` (_lift 4) `shouldBe`  JustMon 7

