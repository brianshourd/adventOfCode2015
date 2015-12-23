{-# LANGUAGE OverloadedStrings #-}
module Day13 (day13, day13', run) where

import Data.Attoparsec.Text

-- Begin actual implementation
day13 :: String -> Int
day13 = undefined

day13' :: String -> Int
day13' = undefined

-- Input
run :: IO ()
run = do
    putStrLn "Day 13 results: "
    input <- readFile "inputs/day13.txt"
    putStrLn $ "  " ++ show (day13 input)
    putStrLn $ "  " ++ show (day13' input)
