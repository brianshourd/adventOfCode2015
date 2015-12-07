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

    it "handles multiple lines of input." $ do
      day5 (unlines ["ugknbfddgicrmopn", "aaa", "jchzalrnumimnmhp", "haegwjzuvuyypxyu", "dvszwmarrgswjxmb"]) == 2

  describe "day5'" $ do
    it "qjhvhtzxzqqjkmpb is nice because is has a pair that appears twice (qj) and a letter that repeats with exactly one letter between them (zxz)." $ do
      day5' "qjhvhtzxzqqjkmpb" == 1

    it "xxyxx is nice because it has a pair that appears twice and a letter that repeats with one between, even though the letters used by each rule overlap." $ do
      day5' "xxyxx" == 1

    it "uurcxstgmygtbstg is naughty because it has a pair (tg) but no repeat with a single letter between them." $ do
      day5' "uurcxstgmygtbstg" == 0

    it "ieodomkazucvgmuy is naughty because it has a repeating letter with one between (odo), but no pair that appears twice." $ do
      day5' "ieodomkazucvgmuy" == 0

    it "handles multiple lines of input" $ do
      day5' (unlines ["qjhvhtzxzqqjkmpb", "xxyxx", "uurcxstgmygtbstg", "ieodomkazucvgmuy"]) == 2
