module Mit.Hw1Spec where

import Control.Monad (unless)
import Mit.Hw1
import Test.Hspec

main :: IO ()
main = hspec spec

between :: Float -> Float -> Float -> Bool
between x po dx = abs (po - x) <= dx

expectTrue :: HasCallStack => String -> Bool -> Expectation
expectTrue msg b = unless b (expectationFailure msg)

shouldBeAround :: (HasCallStack) => Float -> (Float, Float) -> Expectation
v `shouldBeAround` (x, dx) =
  expectTrue
    ("value " ++ show v ++ " not in interval (" ++ show (x - abs dx) ++ "," ++ show (x + abs dx) ++ ")")
    ((v, x) `closerThan` dx)

closerThan :: (Float, Float) -> Float -> Bool
(x, y) `closerThan` dx = abs (x - y) < dx

spec :: Spec
spec = do
  describe "computing derivatives for liniar functions" $ do
    it "computes correctly for f(x) = x + 3" $ diff (\x -> x + 3) 1.0e-2 1 `shouldBeAround` ((\_ -> 1) $ 1, 1.0e-3)
    it "computes correctly for f(x) = -x" $ diff (\x -> x * (-1)) 1.0e-2 1 `shouldBeAround` ((\_ -> -1) $ 1, 1.0e-3)
    it "computes correctly for f(x) = 3x" $ diff (\x -> 3 * x) 1.0e-3 1 `shouldBeAround` ((\_ -> 3) $ 1, 1.0e-3)
    {-
    describe "computing derivatives for polinomial functions functions" $ do
      it "computes correctly for f(x) = x² + 4" $
        diff (\x -> x ^ 2 + 34) 1.0e-3 1 `shouldBeAround` ((\x -> 2 * x) $ 1, 1.0e-3)
      it "computes correctly for f(x) = x³ + 3x² + 6" $
        diff (\x -> x ^^ 3 + 3 * (x ^^ 2) + 6) 1.0e-3 1 `shouldBeAround` ((\x -> 3 * (x ^^ 2) + 6) $ 1, 1.0e-3)
      -}
  describe "looking for solutions for sin (x) " $ do
    it "converges quickly to 0" $ newton_iter sin 0.5 0.01 2 `shouldBeAround` (0, 0.001)
    it "converges quickly to 0" $ newton_iter sin 0.5 0.01 3 `shouldBeAround` (0, 0.001)
    it "converges quickly to 0" $ newton_iter sin 0.5 0.01 7 `shouldBeAround` (0, 0.001)
    it "converges quickly to 0" $ newton_iter sin 0.5 0.01 8 `shouldBeAround` (0, 0.001)
    it "converges quickly to 0" $ newton_iter sin 0.5 0.01 9 `shouldBeAround` (0, 0.001)
    it "converges quickly to 0" $ newton_iter sin 0.5 0.01 10 `shouldBeAround` (0, 0.001)
  describe "x³ - 328 * x² -1999 x - 1670  " $ do
    let f x = x ^^ 3 - 328 * x ^^ 2 - 1999 * x - 1670
    it "converges quickly to -1" $ newton_iter f 100 0.01 11 `shouldBeAround` (-1, 0.001)
  describe "working with intervals" $
    it "checks correctly for interval " $ interval 3 5 `filter` [1 .. 10] `shouldBe` [3, 4, 5]
  describe "looking for relative primes" $ do
    it "returns true for simple cases" $
      map (uncurry isRelativePrime) [(3, 1), (4, 1), (10, 1), (3, 2), (10, 3), (4, 3), (3, 4), (15, 14)] `shouldBe`
      [True, True, True, True, True, True, True, True]
    it "returns false clear multiples" $
      map (uncurry isRelativePrime) [(3, 3), (4, 2), (21, 14)] `shouldBe` [False, False, False]
    it "can build the set of relative primes to 30" $ do
      relativePrimesOf 30 `filter` [1 .. 66] `shouldBe`
        [1, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 49, 53, 59, 61]
      setComplement (relativePrimesOf 30) `filter` [1 .. 10] `shouldBe` [2, 3, 4, 5, 6, 8, 9, 10]
  describe "working with operations on sets" $ do
    it "computes correctly union of 2 sets" $
      interval 3 5 `setUnion` interval 10 12 `filter` [1 .. 15] `shouldBe` [3, 4, 5, 10, 11, 12]
    it "computes intersection of 2 sets" $ do
      interval 3 5 `setIntersection` interval 10 12 `filter` [1 .. 15] `shouldBe` []
      interval 3 8 `setIntersection` interval 5 12 `filter` [1 .. 15] `shouldBe` [5, 6, 7, 8]
    it "computes adding to a sets" $ do
      7 `addToSet` interval 10 12 `filter` [1 .. 15] `shouldBe` [7, 10, 11, 12]
      7 `addToSet` interval 3 8 `filter` [1 .. 15] `shouldBe` [3 .. 8]
      7 `addToSet` emptySet `filter` [1 .. 15] `shouldBe` [7]
    it "computes adding to a sets" $ (setComplement (interval 3 5)) `filter` [1 .. 15] `shouldBe` [1, 2] ++ [6 .. 15]

