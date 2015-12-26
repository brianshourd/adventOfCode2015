module Day14Spec (spec) where

import Day14
import Test.Hspec

main :: IO ()
main = hspec spec

input :: String
input = unlines
    [ "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds."
    , "Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."
    ]

spec :: Spec
spec = do
    describe "day14 parser" $ do
        it "Parses the provided input properly" $ do
            parseInput input == Right
                [ Reindeer "Comet" 14 10 127
                , Reindeer "Dancer" 16 11 162
                ]
    describe "day14" $ do
        it "Given the input, after 1000 seconds, Comet is in the lead at 1120 km" $ do
            leadReindeer rs 1000 == 1120
              where
                rs =
                    [ Reindeer "Comet" 14 10 127
                    , Reindeer "Dancer" 16 11 162
                    ]
