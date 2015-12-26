{-# LANGUAGE OverloadedStrings #-}
module Day14Spec (spec) where

import Day14
import Test.Hspec

main :: IO ()
main = hspec spec

rawInput :: String
rawInput = unlines
    [ "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds."
    , "Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."
    ]

parsedInput :: [Reindeer]
parsedInput =
    [ Reindeer "Comet" 14 10 127
    , Reindeer "Dancer" 16 11 162
    ]

spec :: Spec
spec = do
    describe "day14 parser" $ do
        it "Parses the provided input properly" $ do
            parseInput rawInput `shouldBe` Right parsedInput

    describe "day14" $ do
        it "Given the input, after 1000 seconds, Comet is in the lead at 1120 km" $ do
            leadReindeer parsedInput 1000 `shouldBe` 1120

    describe "day14'" $ do
        it "Given the input, after 1000 seconds, Comet has 312 points and Dancer has 689 points" $ do
            let results = pointResults parsedInput 1000
            results !! 0 `shouldBe` 312
            results !! 1 `shouldBe` 689
