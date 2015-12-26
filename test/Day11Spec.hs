module Day11Spec (spec) where

import Day11
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "requirement1" $ do
        it "succeeds on hijklmmn" $ do
            requirement1 "hijklmmn" `shouldBe` True

        it "fails on abbceffg" $ do
            requirement1 "abbceffg" `shouldBe` False

        it "fails on abbcegjk" $ do
            requirement1 "abbcegjk" `shouldBe` False

    describe "requirement2" $ do
        it "fails on hijklmmn" $ do
            requirement2 "hijklmmn" `shouldBe` False

        it "succeeds on abbceffg" $ do
            requirement2 "abbceffg" `shouldBe` True

        it "succeeds on abbcegjk" $ do
            requirement2 "abbcegjk" `shouldBe` True

    describe "requirement3" $ do
        it "fails on hijklmmn" $ do
            requirement3 "hijklmmn" `shouldBe` False

        it "succeeds on abbceffg" $ do
            requirement3 "abbceffg" `shouldBe` True

        it "fails on abbcegjk" $ do
            requirement3 "abbcegjk" `shouldBe` False

        it "succeeds on abccccde" $ do
            requirement3 "abccccde" `shouldBe` True

    describe "day11" $ do
        it "The next password after abcdefgh is abcdffaa." $ do
            day11 "abcdefgh" `shouldBe` "abcdffaa"

        it "The next password after ghijklmn is ghjaabcc, because you eventually skip all the passwords that start with ghi..., since i is not allowed." $ do
            day11 "ghijklmn" `shouldBe` "ghjaabcc"
