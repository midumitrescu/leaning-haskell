module WorkingWithTypesSpec where

import           Test.Hspec
import           Types.WorkingWithTypes

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Working with types one can" $ do
    it "import and construct objects" $ tomSmith `shouldBe` Customer "Tom Smith" "123 Main St" 20.5
    it "call functions on objects" $ nameOf tomSmith `shouldBe` "Tom Smith"
    it "Easily extracts name from a marked field" $ name samSmith `shouldBe` "Sam"
    it "Can call the contructor nicely" $
      name Employee {name = "Mihai", position = "Student", idNum = 124} `shouldBe` "Mihai"
    it "Can call the contructor in any order" $
      name Employee {position = "Student", idNum = 124, name = "Mihai"} `shouldBe` "Mihai"

