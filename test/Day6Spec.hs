module Day6Spec (spec) where

import Day6
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "day6" $ do
        it "turn on 0,0 through 999,999 would turn on (or leave on) every light." $ do
            day6 "turn on 0,0 through 999,999" `shouldBe` 1000000

        it "turn on 20,30 through 39,49 would turn on 400 lights." $ do
            day6 "turn on 20,30 through 39,49" `shouldBe` 400

        it "toggle 0,0 through 999,0 would toggle the first line of 1000 lights, turning off the ones that were on, and turning on the ones that were off." $ do
            day6 "toggle 0,0 through 999,0" `shouldBe` 1000

        it "toggle 0,0 through 999,0 would toggle the first line of 1000 lights, turning off the ones that were on, and turning on the ones that were off." $ do
            (day6 . unlines $
                [ "toggle 0,0 through 999,0"
                , "toggle 0,0 through 499,0"
                ]) `shouldBe` 500

        it "turn off 499,499 through 500,500 would _turn_ off the middle four lights." $ do
            (day6 . unlines $
                [ "turn on 0,0 through 999,999"
                , "turn off 499,499 through 500,500"
                ]) `shouldBe` 1000000 - 4

        it "turn off 499,499 through 500,500 would _leave_ off the middle four lights." $ do
            day6 "turn off 499,499 through 500,500" `shouldBe` 0

    describe "day6'" $ do
        it "turn on 0,0 through 0,0 would increase the total brightness by 1." $ do
            day6' "turn on 0,0 through 0,0" `shouldBe` 1

        it "toggle 0,0 through 999,999 would increase the total brightness by 2000000" $ do
            day6' "toggle 0,0 through 999,999" `shouldBe` 2000000

        it "turn off 499,499 through 500,500 would _turn_ off the middle four lights." $ do
            (day6' . unlines $
                [ "turn on 0,0 through 999,999"
                , "turn off 499,499 through 500,500"
                ]) `shouldBe` 1000000 - 4

        it "repeatedly increases brightness" $ do
            (day6' . unlines $
                [ "turn on 0,0 through 999,999"
                , "turn on 0,0 through 999,999"
                , "turn off 499,499 through 500,500"
                ]) `shouldBe` 2000000 - 4

        it "never has the brightness drop below 0" $ do
            day6' "turn off 0,0 through 999,999" `shouldBe` 0

