{-# LANGUAGE OverloadedStrings #-}
module Day14 (day14, day14', run, Reindeer(..), leadReindeer, parseInput) where

import Data.Attoparsec.Text
    ( Parser(..)
    , char
    , decimal
    , endOfLine
    , isHorizontalSpace
    , many'
    , option
    , parseOnly
    , string
    , takeTill
    )

data Reindeer = Reindeer
    !String  -- Name
    !Int     -- Speed
    !Int     -- Flying time
    !Int     -- Resting time
    deriving (Show, Eq)

reindeerName (Reindeer n _ _ _) = n
reindeerSpeed (Reindeer _ s _ _) = s
reindeerFlyTime (Reindeer _ _ f _) = f
reindeerRestTime (Reindeer _ _ _ r) = r

parseInput :: String -> Either String [Reindeer]
parseInput = undefined

leadReindeer :: [Reindeer] -> Int -> Int
leadReindeer = undefined

day14 :: String -> Int
day14 = undefined

day14' :: String -> Int
day14' = undefined

-- Input
run :: IO ()
run = do
    putStrLn "Day 14 results: "
    input <- readFile "inputs/day14.txt"
    putStrLn $ "  " ++ show (day14 input)
    putStrLn $ "  " ++ show (day14' input)
