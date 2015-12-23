module Day13Spec (spec) where

import Day13
import Test.Hspec

main :: IO ()
main = hspec spec

input :: String
input = unlines
  [ "Alice would gain 54 happiness units by sitting next to Bob."
  , "Alice would lose 79 happiness units by sitting next to Carol."
  , "Alice would lose 2 happiness units by sitting next to David."
  , "Bob would gain 83 happiness units by sitting next to Alice."
  , "Bob would lose 7 happiness units by sitting next to Carol."
  , "Bob would lose 63 happiness units by sitting next to David."
  , "Carol would lose 62 happiness units by sitting next to Alice."
  , "Carol would gain 60 happiness units by sitting next to Bob."
  , "Carol would gain 55 happiness units by sitting next to David."
  , "David would gain 46 happiness units by sitting next to Alice."
  , "David would lose 7 happiness units by sitting next to Bob."
  , "David would gain 41 happiness units by sitting next to Carol."
  ]

spec :: Spec
spec = do
  describe "day13" $ do
    it "Given the input, resolves to 330" $ do
      day13 input == 330
