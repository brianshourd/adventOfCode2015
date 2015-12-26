{-# LANGUAGE OverloadedStrings #-}
module Day13 (day13, day13', run) where

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
import Data.Function (on)
import Data.List (groupBy, nub, sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (empty, fromList, keys, union)
import Data.Monoid (Sum(..))
import Data.Set (Set)
import qualified Data.Set as Set (delete, fromList)
import Data.Text (Text, pack)

import Graph

-- Parsing types and type synonyms for the problem at hand
type Person = Text
type Happiness = Sum Int
data Source = Source (Edge Person) Happiness deriving (Show, Eq)
type SourceData = EdgeWeightMap Person Happiness

sourceEdge :: Source -> Edge Person
sourceEdge (Source e _) = e

sourceHappiness :: Source -> Happiness
sourceHappiness (Source _ h) = h

compileMap :: [Source] -> SourceData
compileMap [] = Map.empty
compileMap xs =
    Map.fromList
    . map collapseSameSources
    . groupBy ((==) `on` sourceEdge)
    . sortBy (compare `on` sourceEdge)
    $ xs

collapseSameSources :: [Source] -> (Edge Person, Happiness)
collapseSameSources [] =
    undefined
collapseSameSources xs@(x:_) =
    (sourceEdge x, mconcat . map sourceHappiness $ xs)

parseSourceData :: Parser SourceData
parseSourceData = do
    sources <- many' parseSource
    return $ compileMap sources

parseSource :: Parser Source
parseSource = do
    a <- takeTill isHorizontalSpace
    string " would "
    gainOrLose <- takeTill isHorizontalSpace
    char ' '
    h <- decimal
    string " happiness units by sitting next to "
    b <- takeTill (== '.')
    char '.'
    option () endOfLine
    let happiness = if gainOrLose == "gain" then h else (-h)
    return $ Source (Edge (Node a) (Node b)) (Sum happiness)

allPeople :: SourceData -> [Person]
allPeople = nub . concatMap (\(Edge (Node a) (Node b)) -> [a, b]) . Map.keys

bestFullCycle :: SourceData -> Maybe (Traversal Person Happiness)
bestFullCycle g = case allPeople g of
    []    -> Just $ Traversal [] mempty
    (x:_) -> bestTraversal' g compare start start rest
        where
            start = Just $ Node x
            rest = Set.delete (Node x) (Set.fromList . map Node . allPeople $ g)

day13 :: String -> Int
day13 input = case parseOnly parseSourceData . pack $ input of
    (Left _) -> -1
    (Right sourceData) -> case bestFullCycle sourceData of
        Nothing -> -2
        (Just t) -> getSum . traversalWeight $ t

addSelf :: SourceData -> SourceData
addSelf g = Map.union (Map.fromList selfEdges) g
  where
    self = Node "Self"
    selfEdges = map (\p -> (Edge self (Node p), 0)) $ allPeople g

day13' :: String -> Int
day13' input = case parseOnly parseSourceData . pack $ input of
    (Left _) -> -1
    (Right sourceData) -> case bestFullCycle $ addSelf sourceData of
        Nothing -> -2
        (Just t) -> getSum . traversalWeight $ t

-- Input
run :: IO ()
run = do
    putStrLn "Day 13 results: "
    input <- readFile "inputs/day13.txt"
    putStrLn $ "  " ++ show (day13 input)
    putStrLn $ "  " ++ show (day13' input)
