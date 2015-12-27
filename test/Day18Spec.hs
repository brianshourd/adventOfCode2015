{-# LANGUAGE OverloadedStrings #-}
module Day18Spec (spec) where

import Data.Set (Set)
import qualified Data.Set as Set (fromList)

import Test.Hspec

import Day18

main :: IO ()
main = hspec spec

rawInput :: String
rawInput = unlines
    [ ".#.#.#"
    , "...##."
    , "#....#"
    , "..#..."
    , "#.#..#"
    , "####.."
    ]

parsedInput :: Grid
parsedInput = Grid
    (Boundary (0, 0) (5, 5))
    (Set.fromList
        [ (1, 0), (3, 0), (5, 0)
        , (3, 1), (4, 1)
        , (0, 2), (5, 2)
        , (2, 3)
        , (0, 4), (2, 4), (5, 4)
        , (0, 5), (1, 5), (2, 5), (3, 5)
        ])

givenSteps :: [String]
givenSteps = rawInput : map unlines
    [ [ "..##.."
      , "..##.#"
      , "...##."
      , "......"
      , "#....."
      , "#.##.."
      ]
    , [ "..###."
      , "......"
      , "..###."
      , "......"
      , ".#...."
      , ".#...."
      ]
    , [ "...#.."
      , "......"
      , "...#.."
      , "..##.."
      , "......"
      , "......"
      ]
    , [ "......"
      , "......"
      , "..##.."
      , "..##.."
      , "......"
      , "......"
      ]
    ]

spec :: Spec
spec = do
    describe "day18 parser" $ do
        it "Parses the provided input properly" $ do
            parseInput rawInput `shouldBe` parsedInput

    describe "Grid show" $ do
        it "Should show a grid properly" $ do
            show parsedInput `shouldBe` rawInput

    describe "willBeOn" $ do
        let onSet = gridOn parsedInput
        it "off lights should turn on if they have exactly 3 on neighbors" $ do
            willBeOn onSet (4, 2) `shouldBe` True
            willBeOn onSet (3, 3) `shouldBe` False  -- Too few nearby
            willBeOn onSet (1, 4) `shouldBe` False  -- Too many nearby
        it "on lights should stay on if they have exactly 2 or 3 on neighbors" $ do
            willBeOn onSet (3, 1) `shouldBe` True   -- 2 nearby
            willBeOn onSet (2, 5) `shouldBe` True   -- 3 nearby
            willBeOn onSet (2, 3) `shouldBe` False  -- Too few nearby
            willBeOn onSet (4, 1) `shouldBe` False  -- Too many nearby

    describe "countLightsOn" $ do
        it "correctly counts the number of lights that are on" $ do
            countLightsOn parsedInput `shouldBe` 15

    describe "conwayStep" $ do
        it "Progresses according to problem description" $ do
            let calculatedSteps = map show . iterate conwayStep $ parsedInput
            calculatedSteps !! 0 `shouldBe` givenSteps !! 0
            calculatedSteps !! 1 `shouldBe` givenSteps !! 1
            calculatedSteps !! 2 `shouldBe` givenSteps !! 2
            calculatedSteps !! 3 `shouldBe` givenSteps !! 3
            calculatedSteps !! 4 `shouldBe` givenSteps !! 4
