{-# LANGUAGE OverloadedStrings #-}
module Day14 (day14, day14', run, Reindeer(..), leadReindeer, parseInput, pointResults) where

import Control.Applicative (optional)
import Data.Attoparsec.Text
    ( Parser(..)
    , decimal
    , endOfLine
    , isHorizontalSpace
    , many'
    , parseOnly
    , string
    , takeTill
    )
import Data.List (transpose)
import Data.Text (Text, pack)

data Reindeer = Reindeer
    !Text  -- Name
    !Int   -- Speed
    !Int   -- Flying time
    !Int   -- Resting time
    deriving (Show, Eq)

reindeerName (Reindeer n _ _ _) = n
reindeerSpeed (Reindeer _ s _ _) = s
reindeerFlyTime (Reindeer _ _ f _) = f
reindeerRestTime (Reindeer _ _ _ r) = r

parseReindeer :: Parser Reindeer
parseReindeer = do
    name <- takeTill isHorizontalSpace
    string " can fly "
    speed <- decimal
    string " km/s for "
    flyTime <- decimal
    string " seconds, but then must rest for "
    restTime <- decimal
    string " seconds."
    optional endOfLine
    return $ Reindeer name speed flyTime restTime

parseInput :: String -> Either String [Reindeer]
parseInput = parseOnly (many' parseReindeer) . pack

leadReindeer :: [Reindeer] -> Int -> Int
leadReindeer rs t = maximum . map (runFor t) $ rs

runFor
    :: Int       -- How many seconds
    -> Reindeer  -- Reindeer
    -> Int       -- Distance travelled
runFor t (Reindeer _ speed flyTime restTime) =
    snd . head . filter ((== 0) . fst) $ iterate cycle (t, 0)
  where
    cycle = go 0 restTime . go speed flyTime
    go rate time (secondsLeft, dist)
        | secondsLeft <= time = (0, secondsLeft * rate + dist)
        | otherwise           = (secondsLeft - time, time * rate + dist)

day14 :: String -> Int
day14 input = case parseInput input of
    (Left _)   -> -1
    (Right rs) -> leadReindeer rs 2503

pointResults :: [Reindeer] -> Int -> [Int]
pointResults rs t =
    map sum
    . transpose
    . map toPoints
    . transpose
    . map rPositions
    $ rs
  where
    rPositions :: Reindeer -> [Int]
    rPositions (Reindeer _ speed flyTime restTime) =
        take t
        . tail
        . scanl (+) 0
        . concat
        . repeat
        $ replicate flyTime speed ++ replicate restTime 0
    toPoints :: [Int] -> [Int]
    toPoints positions = map (\p -> if p == maxPos then 1 else 0) positions
      where
        maxPos = maximum positions

day14' :: String -> Int
day14' input = case parseInput input of
    (Left _)   -> -1
    (Right rs) -> maximum $ pointResults rs 2503

-- Input
run :: IO ()
run = do
    putStrLn "Day 14 results: "
    input <- readFile "inputs/day14.txt"
    putStrLn $ "  " ++ show (day14 input)
    putStrLn $ "  " ++ show (day14' input)
