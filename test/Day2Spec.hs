module Day2Spec (spec) where

import Day2
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "day2" $ do
    it "A present with dimensions 2x3x4 requires 2*6 + 2*12 + 2*8 = 52 square feet of wrapping paper plus 6 square feet of slack, for a total of 58 square feet." $ do
      day2 "2x3x4" == 58

    it "A present with dimensions 1x1x10 requires 2*1 + 2*10 + 2*10 = 42 square feet of wrapping paper plus 1 square foot of slack, for a total of 43 square feet." $ do
      day2 "1x1x10" == 43

    it "Handles multiple lines of input" $ do
      day2 (unlines ["1x1x10", "2x3x4"]) == 43 + 58


  describe "day2'" $ do
    it "is defined" $ do
      day2' "" == 1

