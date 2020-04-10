module Cis194.Hw1.TowersOfHanoiSpec where

import           Cis194.Hw1.TowersOfHanoi
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solving it for 1 block" $
    it "dirrectly returns one step solutin" $ hanoi 1 "a" "b" "c" `shouldBe` [("a", "b")]
  describe "solving it for 2 block" $
    it "returns 3 moves" $ hanoi 2 "a" "b" "c" `shouldBe` [("a", "c"), ("a", "b"), ("c", "b")]
  describe "solving it for 3 block" $
    it "returns 7 moves" $
    hanoi 3 "a" "b" "c" `shouldBe` [("a", "b"), ("a", "c"), ("b", "c"), ("a", "b"), ("c", "a"), ("c", "b"), ("a", "b")]
  describe "solving it for 15 blocks" $
    it "returns in 32767 moves" $ length (hanoi 15 "a" "b" "c") `shouldBe` 32767
