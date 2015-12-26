module Day12Spec (spec) where

import Day12
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "day12" $ do
        it "[1,2,3] has a sum of 6." $ do
            day12 "[1,2,3]" `shouldBe` 6

        it "{\"a\":2,\"b\":4} has a sum of 6." $ do
            day12 "{\"a\":2,\"b\":4}" `shouldBe` 6

        it "[[[3]]] has a sum of 3." $ do
            day12 "[[[3]]]" `shouldBe` 3

        it "[-1,{\"a\":1}] has a sum of 0." $ do
            day12 "[-1,{\"a\":1}]" `shouldBe` 0

        it "[] has a sum of 0." $ do
            day12 "[]" `shouldBe` 0

        it "{} has a sum of 0." $ do
            day12 "{}" `shouldBe` 0

    describe "day12'" $ do
        it "[1,2,3] still has a sum of 6." $ do
            day12' "[1,2,3]" `shouldBe` 6

        it "[1,{\"c\":\"red\",\"b\":2},3] now has a sum of 4, because the middle object is ignored." $ do
            day12' "[1,{\"c\":\"red\",\"b\":2},3]" `shouldBe` 4

        it "{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5} now has a sum of 0, because the entire structure is ignored." $ do
            day12' "{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}" `shouldBe` 0

        it "[1,\"red\",5] has a sum of 6, because \"red\" in an array has no effect." $ do
            day12' "[1,\"red\",5]" `shouldBe` 6
