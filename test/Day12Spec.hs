module Day12Spec (spec) where

import Day12
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "day12" $ do
    it "[1,2,3] has a sum of 6." $ do
      day12 "[1,2,3]" == 6

    it "{\"a\":2,\"b\":4} has a sum of 6." $ do
      day12 "{\"a\":2,\"b\":4}" == 6

    it "[[[3]]] has a sum of 3." $ do
      day12 "[[[3]]]" == 3

    it "[-1,{\"a\":1}] has a sum of 0." $ do
      day12 "[-1,{\"a\":1}]" == 0

    it "[] has a sum of 0." $ do
      day12 "[]" == 0

    it "{} has a sum of 0." $ do
      day12 "{}" == 0
