module Day4Spec (spec) where

import Day4
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "day4" $ do
    it "If your secret key is abcdef, the answer is 609043, because the MD5 hash of abcdef609043 starts with five zeroes (000001dbbfa...), and it is the lowest such number to do so." $ do
      day4 "abcdef" == 609043

    it "If your secret key is pqrstuv, the lowest number it combines with to make an MD5 hash starting with five zeroes is 1048970; that is, the MD5 hash of pqrstuv1048970 looks like 000006136ef...." $ do
      day4 "pqrstuv" == 1048970
