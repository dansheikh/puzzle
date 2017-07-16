import Test.Hspec
import Lib
import Data

gwc = gridWithCoords grid

testFindWord word =
  let (Just result) = findWord gwc word
      string = map cellToChar result
  in string `shouldBe` word

main :: IO ()
main = hspec $ do
  describe "formatGrid" $ do
    it "should concatenate every line with a newline" $ do
      (formatGrid (gridWithCoords ["abc", "def", "ghi"])) `shouldBe` "abc\ndef\nghi\n"

  describe "findWord" $ do
    it "should find the supplied word if it is present in the grid" $ do
      testFindWord "HASKELL"
      testFindWord "PERL"
    it "should not find the supplied word if it is not present in the grid" $ do
      findWord gwc "PURESCRIPT" `shouldBe` Nothing

  describe "findWords" $ do
    it "should find all the words in the supplied list that are present the grid" $ do
      let found = findWords gwc languages
          foundString = map (map cellToChar) found
      foundString `shouldBe` languages
    it "should not find any words in the supplied list that are not present the grid" $ do
      findWords gwc ["FRENCH", "ENGLISH", "GERMAN"] `shouldBe` []
