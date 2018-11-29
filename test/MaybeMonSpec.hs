import Lib
import Test.Hspec

main :: IO ()

main = hspec $ do
    describe "Working with monads" $ do
     it "differs between Just and Nothing" $ do
       _lift (3 :: Int) `shouldBe` (JustMon(3))





