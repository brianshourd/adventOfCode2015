{-# LANGUAGE OverloadedStrings #-}
module Day15Spec (spec) where

import Day15
import Test.Hspec

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

    describe "day15" $ do
        it "Given the input, the highest total score is 62842880" $ do
            highestCookieScore parsedInput `shouldBe` 62842880
