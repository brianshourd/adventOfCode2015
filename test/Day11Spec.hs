module Day11Spec (spec) where

import Data.Bits
import Day11
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "requirement1" $ do
    it "succeeds on hijklmmn" $ do
      requirement1 "hijklmmn" == True

    it "fails on abbceffg" $ do
      requirement1 "abbceffg" == False

    it "fails on abbcegjk" $ do
      requirement1 "abbcegjk" == False

  describe "requirement2" $ do
    it "fails on hijklmmn" $ do
      requirement2 "hijklmmn" == False

    it "succeeds on abbceffg" $ do
      requirement2 "abbceffg" == True

    it "succeeds on abbcegjk" $ do
      requirement2 "abbcegjk" == True

  describe "requirement3" $ do
    it "fails on hijklmmn" $ do
      requirement3 "hijklmmn" == False

    it "succeeds on abbceffg" $ do
      requirement3 "abbceffg" == True

    it "fails on abbcegjk" $ do
      requirement3 "abbcegjk" == False

    it "succeeds on abccccde" $ do
      requirement3 "abccccde" == True

  describe "day11" $ do
    it "The next password after abcdefgh is abcdffaa." $ do
      day11 "abcdefgh" == "abcdffaa"

    it "The next password after ghijklmn is ghjaabcc, because you eventually skip all the passwords that start with ghi..., since i is not allowed." $ do
      day11 "ghijklmn" == "ghjaabcc"

  describe "day11'" $ do
    it "is defined" $ do
      day11' "something" /= undefined
