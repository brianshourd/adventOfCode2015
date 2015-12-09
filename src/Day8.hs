{-# LANGUAGE OverloadedStrings #-}
module Day8 (day8, day8', run) where

import Data.Attoparsec.Text
import Data.Text (Text, pack, unpack)

import Prelude hiding (take)

day8 :: String -> Int
day8 input = case fmap sum . sequence . map difference . lines $ input of
    (Right i) -> i
    _         -> -1

difference :: String -> Either String Int
difference s = do
    short <- parseOnly parseString (pack s)
    return $ (length s) - (length short)

parseString :: Parser String
parseString = do
    char '\"'
    results <- many' parseCharacter
    char '\"'
    return results

parseCharacter :: Parser Char
parseCharacter = choice [parseBackslash, parseQuote, parseHex, notChar '\"']

parseBackslash :: Parser Char
parseBackslash = do
    string "\\\\"
    return '\\'

parseQuote :: Parser Char
parseQuote = do
    string "\\\""
    return '\"'

parseHex :: Parser Char
parseHex = do
    string "\\x"
    code <- unpack <$> take 2
    return 'a' -- The content doesn't actually matter

day8' :: String -> Int
day8' = sum . map encodeCost . lines

encodeCost :: String -> Int
encodeCost s = length (show s) - length s

-- Input
run :: IO ()
run = do
    putStrLn "Day 8 results: "
    input <- readFile "inputs/day8.txt"
    putStrLn $ "  " ++ show (day8 input)
    putStrLn $ "  " ++ show (day8' input)
