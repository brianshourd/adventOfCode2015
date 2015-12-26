module Day1Spec (spec) where

import Day1
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "day1" $ do
        it "(()) and ()() both result in floor 0" $ do
            day1 "(())" `shouldBe` 0
            day1 "()()" `shouldBe` 0

        it "((( and (()(()( both result in floor 3" $ do
            day1 "(((" `shouldBe` 3
            day1 "(()(()(" `shouldBe` 3

        it "))((((( also results in floor 3" $ do
            day1 "))(((((" `shouldBe` 3

        it "()) and ))( both result in floor -1 (the first basement level)" $ do
            day1 "())" `shouldBe` -1
            day1 "))(" `shouldBe` -1

        it "))) and )())()) both result in floor -3" $ do
            day1 ")))" `shouldBe` -3
            day1 ")())())" `shouldBe` -3

    describe "day1'" $ do
        it ") causes him to enter the basement at character position 1." $ do
            day1' ")" `shouldBe` 1

        it "()()) causes him to enter the basement at character position 5." $ do
            day1' "()())" `shouldBe` 5
