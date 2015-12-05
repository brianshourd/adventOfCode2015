module Day4 (day4, day4', run, applyHash) where

import Crypto.Hash.MD5 (hash)
import Data.ByteString.Char8 (pack)
import Data.ByteString (ByteString, unpack)
import Data.List (isPrefixOf)
import Data.Word (Word8)

day4 :: String -> Int
day4 input = fst . head . filter (startsWith5Zeros . snd) . map (applyHash input) $ [1..]

applyHash :: String -> Int -> (Int, ByteString)
applyHash prefix x = (x, hash . pack $ prefix ++ (show x))

startsWith5Zeros :: ByteString -> Bool
startsWith5Zeros b = case unpack b of
    (x:y:z:rest) -> x == 0 && y == 0 && z < 16
    _            -> False

day4' :: String -> Int
day4' input = fst . head . filter (startsWith6Zeros . snd) . map (applyHash input) $ [1..]

-- Input
run :: IO ()
run = do
    putStrLn "Day 4 results: "
    let input = "yzbqklnj"
    putStrLn $ "  " ++ show (day4 input)
    putStrLn $ "  " ++ show (day4' input)

startsWith6Zeros :: ByteString -> Bool
startsWith6Zeros b = case unpack b of
    (x:y:z:rest) -> x == 0 && y == 0 && z == 0
    _            -> False
