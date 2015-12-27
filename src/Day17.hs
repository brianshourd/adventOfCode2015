{-# LANGUAGE OverloadedStrings #-}
module Day17 (day17, day17', run, Container(..), minimalCombinations, parseInput, validCombinations) where

import Data.Function (on)
import Data.List (delete, groupBy, sortBy)
import Data.Maybe (mapMaybe)
import Data.Text (Text, pack)

data Container = Container Int deriving (Show, Eq)

containerSize :: Container -> Int
containerSize (Container s) = s

parseInput :: String -> [Container]
parseInput = map (Container . read) . lines

validCombinations :: Int -> [Container] -> [[Container]]
validCombinations 0 _  = [[]]
validCombinations _ [] = []
validCombinations t (c:cs)
    | t < 0     = []
    | otherwise = prependedCombinations ++ validCombinations t cs
  where
    prependedCombinations :: [[Container]]
    prependedCombinations = map (c :) $ validCombinations (t - containerSize c) cs

minimalCombinations :: Int -> [Container] -> [[Container]]
minimalCombinations t cs =
    head
    . groupBy ((==) `on` length)
    . sortBy (compare `on` length)
    $ validCombinations t cs

day17 :: String -> Int
day17 = length . validCombinations 150 . parseInput

day17' :: String -> Int
day17' = length . minimalCombinations 150 . parseInput

-- Input
run :: IO ()
run = do
    putStrLn "Day 17 results: "
    input <- readFile "inputs/day17.txt"
    putStrLn $ "  " ++ show (day17 input)
    putStrLn $ "  " ++ show (day17' input)
