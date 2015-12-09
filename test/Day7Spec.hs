module Day7Spec (spec) where

import Data.Bits
import Day7
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "day7" $ do
    it "123 -> x means that the signal 123 is provided to wire x." $ do
      day7 "123 -> a" == 123

    it "123 -> y then y -> x means that the signal 123 is provided to wire x." $ do
      day7 (unlines ["123 -> y", "y -> a"]) == 123

    it "x AND y -> z means that the bitwise AND of wire x and wire y is provided to wire z." $ do
      day7 (unlines ["123 -> x", "456 -> y", "x AND y -> a"]) == 123 .&. 456

    it "1 AND y -> z means that the bitwise AND of 1 and wire y is provided to wire z." $ do
      day7 (unlines ["456 -> y", "1 AND y -> a"]) == 1 .&. 456

    it "p LSHIFT 2 -> q means that the value from wire p is left-shifted by 2 and then provided to wire q." $ do
      day7 (unlines ["123 -> p", "p LSHIFT 2 -> a"]) == shiftL 123 2

    it "NOT e -> f means that the bitwise complement of the value from wire e is provided to wire f." $ do
      day7 (unlines ["123 -> e", "NOT e -> a"]) == complement 123

    it "NOT e -> f means that the bitwise complement of the value from wire e is provided to wire f." $ do
      day7 (unlines
        [ "1 -> b"
        , "1 -> c"
        , "c RSHIFT 2 -> d"
        , "d AND c -> e"
        , "e OR b -> f"
        , "NOT f -> a"
        ]) == complement 1

  describe "day7'" $ do
    it "is defined" $ do
      day7' "something" /= undefined
