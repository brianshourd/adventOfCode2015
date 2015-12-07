module Day5Spec (spec) where

import Day5
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "day5" $ do
    it "ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), a double letter (...dd...), and none of the disallowed substrings." $ do
      day5 "ugknbfddgicrmopn" == 1

    it "aaa is nice because it has at least three vowels and a double letter, even though the letters used by different rules overlap." $ do
      day5 "aaa" == 1

    it "jchzalrnumimnmhp is naughty because it has no double letter." $ do
      day5 "jchzalrnumimnmhp" == 0

    it "haegwjzuvuyypxyu is naughty because it contains the string xy." $ do
      day5 "haegwjzuvuyypxyu" == 0

    it "dvszwmarrgswjxmb is naughty because it contains only one vowel." $ do
      day5 "dvszwmarrgswjxmb" == 0

