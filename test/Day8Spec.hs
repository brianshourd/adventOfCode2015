module Day8Spec (spec) where

import Day8
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "day8" $ do
    it "\"\" is 2 characters of code (the two double quotes), but the string contains zero characters." $ do
      day8 "\"\"" == 2

    it "\"abc\" is 5 characters of code, but 3 characters in the string data." $ do
      day8 "\"abc\"" == 2

    it "\"aaa\"aaa\" is 10 characters of code, but the string itself contains six 'a' characters and a single, escaped quote character, for a total of 7 characters in the string data." $ do
      day8 "\"aaa\\\"aaa\"" == 3

    it "\"\x27\" is 6 characters of code, but the string itself contains just one - an apostrophe ('), escaped using hexadecimal notation." $ do
      day8 "\"\\x27\"" == 5

    it "\"\x27ac\" is 8 characters of code, but the string itself contains 3 - an apostrophe ('), escaped using hexadecimal notation, followed by \"ac\"" $ do
      day8 "\"\\x27ac\"" == 5

    it "handles multiple lines of input" $ do
      day8 (unlines
        [ "\"\""
        , "\"abc\""
        , "\"aaa\\\"aaa\""
        , "\"\\x27\""
        ]) == 2 + 2 + 3 + 5

  describe "day8'" $ do
    it "\"\" encodes to \"\\\"\\\"\", an increase from 2 characters to 6." $ do
      day8' "\"\"" == 4

    it "\"abc\" encodes to \"\\\"abc\\\"\", an increase from 5 characters to 9." $ do
      day8' "\"abc\"" == 4

    it "\"aaa\\\"aaa\" encodes to \"\\\"aaa\\\\\\\"aaa\\\"\", an increase from 10 characters to 16." $ do
      day8' "\"aaa\\\"aaa\"" == 6

    it "\"\\x27\" encodes to \"\\\"\\\\x27\\\"\", an increase from 6 characters to 11." $ do
      day8' "\"\\x27\"" == 5

