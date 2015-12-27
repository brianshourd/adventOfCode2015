{-# LANGUAGE OverloadedStrings #-}
module Day16Spec (spec) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (fromList)
import Data.Set (Set)
import qualified Data.Set as Set (delete, fromList)
import Data.List (sort)

import Test.Hspec

import Day16

main :: IO ()
main = hspec spec

rawInput :: String
rawInput = unlines
    [ "Sue 1: cars: 9, akitas: 3, goldfish: 0"
    , "Sue 2: akitas: 9, children: 3, samoyeds: 9"
    , "Sue 3: trees: 6, cars: 6, children: 4"
    , "Sue 4: trees: 4, vizslas: 4, goldfish: 9"
    ]

parsedInput :: Set SueMemory
parsedInput =
    Set.fromList
    . map (\(id, clues) -> SueMemory id (Map.fromList clues))
    $   [ (1, [("cars", 9), ("akitas", 3), ("goldfish", 0)])
        , (2, [("akitas", 9), ("children", 3), ("samoyeds", 9)])
        , (3, [("trees", 6), ("cars", 6), ("children", 4)])
        , (4, [("trees", 4), ("vizslas", 4), ("goldfish", 9)])
        ]

spec :: Spec
spec = do
    describe "day16 parser" $ do
        it "Parses the provided input properly" $ do
            parseInput rawInput `shouldBe` Right parsedInput

    describe "applyClue" $ do
        it "removes Sues who conflict with the clue" $ do
            applyClue ("children", 4) parsedInput
                `shouldBe`
                Set.delete (SueMemory 2 $ Map.fromList [("akitas", 9), ("children", 3), ("samoyeds", 9)]) parsedInput
