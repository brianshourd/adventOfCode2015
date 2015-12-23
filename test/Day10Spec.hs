module Day10Spec (spec) where

import Day10
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "lookAndSay" $ do
    it "1 becomes 11 (1 copy of digit 1)." $ do
      lookAndSay "1" == "11"

    it "11 becomes 21 (2 copies of digit 1)." $ do
      lookAndSay "11" == "21"

    it "21 becomes 1211 (one 2 followed by one 1)." $ do
      lookAndSay "21" == "1211"

    it "1211 becomes 111221 (one 1, one 2, and two 1s)." $ do
      lookAndSay "1211" == "111221"

    it "111221 becomes 312211 (three 1s, two 2s, and one 1)." $ do
      lookAndSay "111221" == "312211"
