module Day2Spec (spec) where

import Day2
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "day2" $ do
        it "A present with dimensions 2x3x4 requires 2*6 + 2*12 + 2*8 = 52 square feet of wrapping paper plus 6 square feet of slack, for a total of 58 square feet." $ do
            day2 "2x3x4" `shouldBe` 58

        it "A present with dimensions 1x1x10 requires 2*1 + 2*10 + 2*10 = 42 square feet of wrapping paper plus 1 square foot of slack, for a total of 43 square feet." $ do
            day2 "1x1x10" `shouldBe` 43

        it "Handles multiple lines of input" $ do
            day2 (unlines ["1x1x10", "2x3x4"]) `shouldBe` 43 + 58

    describe "day2'" $ do
        it "A present with dimensions 2x3x4 requires 2+2+3+3 = 10 feet of ribbon to wrap the present plus 2*3*4 = 24 feet of ribbon for the bow, for a total of 34 feet." $ do
            day2' "2x3x4" `shouldBe` 34

        it " A present with dimensions 1x1x10 requires 1+1+1+1 = 4 feet of ribbon to wrap the present plus 1*1*10 = 10 feet of ribbon for the bow, for a total of 14 feet." $ do
            day2' "1x1x10" `shouldBe` 14

        it "Handles multiple lines of input" $ do
            day2' (unlines ["2x3x4", "1x1x10"]) `shouldBe` 34 + 14

