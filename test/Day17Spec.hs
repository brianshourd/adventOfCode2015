{-# LANGUAGE OverloadedStrings #-}
module Day17Spec (spec) where

import Test.Hspec

import Day17

main :: IO ()
main = hspec spec

rawInput :: String
rawInput = unlines ["20", "15", "10", "5", "5"]

parsedInput :: [Container]
parsedInput = map Container [20, 15, 10, 5, 5]

spec :: Spec
spec = do
    describe "day17 parser" $ do
        it "Parses the provided input properly" $ do
            parseInput rawInput `shouldBe` parsedInput

    describe "day17" $ do
        it "there are 4 ways to store 25 liters in containers 20, 15, 10, 5, and 5" $ do
            (length $ validCombinations 25 parsedInput) `shouldBe` 4

    describe "day17'" $ do
        it "there are 3 minimal ways to store 25 liters in containers 20, 15, 10, 5, and 5" $ do
            (length $ minimalCombinations 25 parsedInput) `shouldBe` 3

