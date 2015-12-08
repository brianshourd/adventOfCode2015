module Day7Spec (spec) where

import Day7
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "day7" $ do
    it "is defined" $ do
      day7 "something" /= undefined

  describe "day7'" $ do
    it "is defined" $ do
      day7' "something" /= undefined
