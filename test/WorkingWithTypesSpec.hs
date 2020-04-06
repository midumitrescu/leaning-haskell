module WorkingWithTypesSpec where

import           Types.WorkingWithTypes
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Working with types one can" $ do
    it "import and construct objects" $ tomSmith `shouldBe` Customer "Tom Smith" "123 Main St" 20.5
    it "call functions on objects" $ nameOf tomSmith `shouldBe` "Tom Smith" 