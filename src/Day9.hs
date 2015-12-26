{-# LANGUAGE OverloadedStrings #-}
module Day9 (day9, day9', run) where

import Data.Attoparsec.Text
    ( Parser
    , char
    , decimal
    , endOfLine
    , isHorizontalSpace
    , many'
    , string
    , parseOnly
    , takeTill
    )
import Data.Function (on)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (fromList)
import Data.Monoid (Sum(..))
import Data.Ord (comparing)
import Data.Text (Text, pack, unpack)

import Graph

-- Parsing types and type synonyms for the problem at hand
type City = Text
type Distance = Sum Int
data Source = Source
    { start    :: City
    , end      :: City
    , distance :: Distance
    } deriving Show
type SourceData = EdgeWeightMap City Distance

compileMap :: [Source] -> SourceData
compileMap = Map.fromList . map toPair
    where
        toPair (Source s e d) = (Edge (Node s) (Node e), d)

parseSourceData :: Parser SourceData
parseSourceData = do
    sources <- many' parseSource
    return $ compileMap sources

parseSource :: Parser Source
parseSource = do
    a <- takeTill isHorizontalSpace
    string " to "
    b <- takeTill isHorizontalSpace
    string " = "
    dist <- decimal
    endOfLine
    return $ Source a b (Sum dist)

-- Begin actual implementation
day9 :: String -> Int
day9 = day9Impl (compare `on` negate)

day9' :: String -> Int
day9' = day9Impl compare

day9Impl :: (Sum Int -> Sum Int -> Ordering) -> String -> Int
day9Impl comp input = case parseOnly parseSourceData . pack $ input of
    (Left _) -> -1
    (Right sourceData) -> case bestTraversal sourceData comp of
        Nothing -> -2
        (Just t) -> getSum . traversalWeight $ t

-- Input
run :: IO ()
run = do
    putStrLn "Day 9 results: "
    input <- readFile "inputs/day9.txt"
    putStrLn $ "  " ++ show (day9 input)
    putStrLn $ "  " ++ show (day9' input)
