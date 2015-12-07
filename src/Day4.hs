module Day4 (day4, day4', run) where

import Crypto.Hash.MD5 (hash)
import Data.ByteString.Char8 (pack)
import Data.ByteString (ByteString, unpack)
import Data.List (isPrefixOf)
import Data.Word (Word8)

day4 :: String -> Int
day4 input = fst . head . filter (startsWithZeros 5 . snd) . map (applyHash input) $ [1..]

applyHash :: String -> Int -> (Int, ByteString)
applyHash prefix x = (x, hash . pack $ prefix ++ (show x))

startsWithZeros :: Int -> ByteString -> Bool
startsWithZeros n b = startsWithZeros' n $ unpack b
    where
        startsWithZeros' :: Int -> [Word8] -> Bool
        startsWithZeros' n [] = False
        startsWithZeros' 1 (x:xs) = x < 16
        startsWithZeros' 2 (x:xs) = x == 0
        startsWithZeros' n (x:xs) = x == 0 && startsWithZeros' (n - 2) xs

day4' :: String -> Int
day4' input = fst . head . filter (startsWithZeros 6 . snd) . map (applyHash input) $ [1..]

-- Input
run :: IO ()
run = do
    --putStrLn "Day 4 results: "
    --let input = "yzbqklnj"
    --putStrLn $ "  " ++ show (day4 input)
    --putStrLn $ "  " ++ show (day4' input)
    putStrLn "Day 4 results (cached): "
    putStrLn $ "  " ++ show 282749
    putStrLn $ "  " ++ show 9962624
