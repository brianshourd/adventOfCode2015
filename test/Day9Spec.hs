module Day9Spec (spec) where

import Data.Bits
import Day9
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "day9" $ do
    it "Given the following distances:\nLondon to Dublin = 464\nLondon to Belfast = 518\nDublin to Belfast = 141\nThe shortest possible route is London -> Dublin -> Belfast = 605, and so the answer is 605 in this example." $ do
      day9 (unlines
        [ "London to Dublin = 464"
        , "London to Belfast = 518"
        , "Dublin to Belfast = 141"
        ]) == 605

  describe "day9'" $ do
    it "is defined" $ do
      day9' "" /= undefined
