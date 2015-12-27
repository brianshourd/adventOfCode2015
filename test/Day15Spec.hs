{-# LANGUAGE OverloadedStrings #-}
module Day15Spec (spec) where

import Data.List (sort)

import Test.Hspec

import Day15

main :: IO ()
main = hspec spec

rawInput :: String
rawInput = unlines
    [ "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8"
    , "Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"
    ]

parsedInput :: [Ingredient]
parsedInput =
    [ Ingredient "Butterscotch" (-1) (-2) 6 3 8
    , Ingredient "Cinnamon" 2 3 (-2) (-1) 3
    ]

spec :: Spec
spec = do
    describe "day15 parser" $ do
        it "Parses the provided input properly" $ do
            parseInput rawInput `shouldBe` Right parsedInput

    describe "partitions" $ do
        it "There are 11 partitions of 10 of size 2" $ do
            (length $ partitions 2 10) `shouldBe` 11

        it "There are 66 partitions of 10 of size 3" $ do
            (length $ partitions 3 10) `shouldBe` 66

        it "The partitions of 3 of size 2 are calculated" $ do
            partitions 2 3 `shouldMatchList` [[0,3],[1,2],[2,1],[3,0]]

    describe "day15" $ do
        it "Given the input, the highest total score is 62842880" $ do
            highestCookieScore parsedInput `shouldBe` 62842880

    describe "day15'" $ do
        it "Given the input, the highest total score is 57600000" $ do
            highestCookieScore' parsedInput `shouldBe` 57600000
