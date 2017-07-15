import Test.Hspec
import Lib
import Data

main :: IO ()
main = hspec $ do
  describe "formatGrid" $ do
    it "should concatenate every line with a newline" $ do
      (formatGrid ["abc", "def", "ghi"]) `shouldBe` "abc\ndef\nghi\n"

  describe "findWord" $ do
    it "should find the supplied word if it is present in the grid" $ do
      findWord grid "HASKELL" `shouldBe` Just "HASKELL"
      findWord grid "RUBY" `shouldBe` Just "RUBY"
    it "should not find the supplied word if it is not present in the grid" $ do
      findWord grid "PURESCRIPT" `shouldBe` Nothing

  describe "findWords" $ do
    it "should find all the words in the supplied list that are present the grid" $ do
      findWords grid languages `shouldBe` languages
    it "should not find any words in the supplied list that are not present the grid" $ do
      findWords grid ["FRENCH", "ENGLISH", "GERMAN"] `shouldBe` []
