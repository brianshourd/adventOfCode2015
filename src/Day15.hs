{-# LANGUAGE OverloadedStrings #-}
module Day15 (day15, day15', run, Ingredient(..), parseInput, highestCookieScore) where

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
import Data.Text (Text, pack)

data Ingredient = Ingredient
    { name       :: Text
    , capacity   :: Int
    , durability :: Int
    , flavor     :: Int
    , texture    :: Int
    , calories   :: Int
    } deriving (Show, Eq)

parseInput :: String -> Either String [Ingredient]
parseInput = parseOnly (many' parseIngredient) . pack

parseIngredient :: Parser Ingredient
parseIngredient = undefined

highestCookieScore :: [Ingredient] -> Int
highestCookieScore = undefined

day15 :: String -> Int
day15 = undefined

day15' :: String -> Int
day15' = undefined

-- Input
run :: IO ()
run = do
    putStrLn "Day 15 results: "
    input <- readFile "inputs/day15.txt"
    putStrLn $ "  " ++ show (day15 input)
    putStrLn $ "  " ++ show (day15' input)
