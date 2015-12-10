{-# LANGUAGE OverloadedStrings #-}
module Day9 (day9, day9', run, day9Test) where

import Data.Attoparsec.Text
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text, pack, unpack)

day9 :: String -> Int
day9 = undefined

day9Test :: String -> Either String SourceData
day9Test = parseOnly parseSourceData . pack

type City = Text
data Source = Source
    { start    :: City
    , end      :: City
    , distance :: Int
    } deriving Show
type Route = [City]
type SourceData = Map City [(City, Int)]

compileMap :: [Source] -> SourceData
compileMap sources = M.fromSet (getAllConnections allSources) cities
    where
        cities = S.fromList . map start $ allSources
        allSources = sources ++ (map (\(Source a b d) -> Source b a d) sources)

getAllConnections :: [Source] -> City -> [(City, Int)]
getAllConnections sources city = map (\s -> (end s, distance s))
    . filter (\s -> city == (start s))
    $ sources

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
    return $ Source a b dist

shortestPath :: Set City -> (Route, Int)
shortestPath = undefined

shortestPaths :: Set City -> [(Route, Int)]
shortestPaths cities = undefined

day9' :: String -> Int
day9' = undefined

-- Input
run :: IO ()
run = do
    putStrLn "Day 9 results: "
    putStrLn "  In progress"
    --input <- readFile "inputs/day9.txt"
    --putStrLn $ "  " ++ show (day9 input)
    --putStrLn $ "  " ++ show (day9' input)
