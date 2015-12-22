{-# LANGUAGE OverloadedStrings #-}
module Day9 (day9, day9', run) where

import Data.Attoparsec.Text
import Data.List (minimumBy)
import Data.Maybe (catMaybes)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text, pack, unpack)
import Safe

day9 :: String -> Int
day9 input = case parseOnly parseSourceData . pack $ input of
    (Left _) -> -1
    (Right sourceData) -> minimum . map snd . shortestRoutes sourceData $ (allCities sourceData)

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

allCities :: SourceData -> Set City
allCities = S.fromList . M.keys

-- The shortest route starting here and visiting the provided cities
shortestRoute :: SourceData -> City -> Set City -> Maybe (Route, Int)
shortestRoute sourceData start rest = if (null rest)
    then Just ([start], 0)
    else minimumByMay (comparing snd)
        . catMaybes
        . map (prependRoute sourceData start)
        $ shortestRoutes sourceData rest

prependRoute :: SourceData -> City -> (Route, Int) -> Maybe (Route, Int)
prependRoute sourceData x (ys, dst) = do
    firstCity <- headMay ys
    distances <- M.lookup x sourceData
    distanceToFirst <- lookup firstCity distances
    return (x:ys, dst + distanceToFirst)

shortestRoutes :: SourceData -> Set City -> [(Route, Int)]
shortestRoutes sourceData cities = catMaybes . map (\city -> shortestRoute sourceData city (S.delete city cities)) $ S.elems cities

day9' :: String -> Int
day9' = undefined

-- Input
run :: IO ()
run = do
    putStrLn "Day 9 results: "
    input <- readFile "inputs/day9.txt"
    putStrLn $ "  " ++ show (day9 input)
    --putStrLn $ "  " ++ show (day9' input)
