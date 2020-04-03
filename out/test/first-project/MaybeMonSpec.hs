module MaybeMonSpec where

import           Lib
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Working with monads" $ do
    it "differs between Just and Nothing" $ do
      _lift 3 `shouldBe` JustMon (3 :: Int)
      _lift 3 `shouldNotBe` NothingMon
    it "correctly binds between containers" $ do
      _lift 3 Lib.>>= (\x -> _lift (x + 3)) `shouldBe` JustMon 6
      _lift 3 Lib.>>= (\x -> _lift (x ^ x)) `shouldBe` JustMon 28
      NothingMon Lib.>>= (\x -> _lift (x ^ x)) `shouldBe` NothingMon
    it "can add a number in a Monad" $ 3 `plus` (_lift 4) `shouldBe` JustMon 7
