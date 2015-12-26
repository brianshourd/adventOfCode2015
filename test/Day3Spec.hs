module Day3Spec (spec) where

import Day3
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "day3" $ do
        it "delivers to 2 houses on '>'" $ do
            day3 ">" `shouldBe` 2

        it "delivers to 4 houses on '^>v<'" $
            day3 "^>v<" `shouldBe` 4

        it "delivers to 2 houses on '^v^v^v^v^v'" $
            day3 "^v^v^v^v^v" `shouldBe` 2

    describe "day3'" $ do
        it "delivers to 3 houses on '^v'" $ do
            day3' "^v" `shouldBe` 3

        it "delivers to 3 houses on '^>v<'" $
            day3' "^>v<" `shouldBe` 3

        it "delivers to 11 houses on '^v^v^v^v^v'" $
            day3' "^v^v^v^v^v" `shouldBe` 11
