{-# LANGUAGE OverloadedStrings #-}
module Day16 (day16, day16', run, Clue(..), SueMemory(..), applyClue, parseInput) where

import Control.Applicative (optional)
import Data.Attoparsec.Text
    ( Parser(..)
    , decimal
    , endOfLine
    , isHorizontalSpace
    , many'
    , parseOnly
    , sepBy1'
    , string
    , takeTill
    )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (fromList, lookup)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set (filter, fromList, toList)
import Data.Text (Text, pack)

data SueMemory = SueMemory
    { sueMemoryId    :: !Int
    , sueMemoryClues :: !(Map Text Int)
    } deriving (Show, Eq, Ord)
type Clue = (Text, Int)

parseInput :: String -> Either String (Set SueMemory)
parseInput = fmap Set.fromList . parseOnly (many' parseSueMemory) . pack

parseSueMemory :: Parser SueMemory
parseSueMemory = do
    string "Sue "
    id <- decimal
    string ": "
    clues <- parseClue `sepBy1'` string ", "
    optional endOfLine
    return $ SueMemory id (Map.fromList clues)

parseClue :: Parser Clue
parseClue = do
    ct <- takeTill (== ':')
    string ": "
    v <- decimal
    return (ct, v)

applyClue :: Clue -> Set SueMemory -> Set SueMemory
applyClue (clueText, clueVal) = applyClueImpl (clueText, (== clueVal))

applyClueImpl :: (Text, Int -> Bool) -> Set SueMemory -> Set SueMemory
applyClueImpl (clueText, cluePred) = Set.filter doesNotConflict
  where
    doesNotConflict :: SueMemory -> Bool
    doesNotConflict (SueMemory _ cs) = fromMaybe True $ do
        memVal <- Map.lookup clueText cs
        return $ cluePred memVal

playGuessWho :: [Clue] -> Set SueMemory -> Maybe Int
playGuessWho cs ms = playGuessWho' (map (\(ct, cv) -> (ct, (== cv))) cs) ms

playGuessWho' :: [(Text, Int -> Bool)] -> Set SueMemory -> Maybe Int
playGuessWho' cs ms = case Set.toList . foldr applyClueImpl ms $ cs of
    [m] -> Just $ sueMemoryId m
    _   -> Nothing

allClues :: [Clue]
allClues =
    [ ("children", 3)
    , ("cats", 7)
    , ("samoyeds", 2)
    , ("pomeranians", 3)
    , ("akitas", 0)
    , ("vizslas", 0)
    , ("goldfish", 5)
    , ("trees", 3)
    , ("cars", 2)
    , ("perfumes", 1)
    ]

day16 :: String -> Int
day16 input = case parseInput input of
    (Left _)   -> -1
    (Right ms) -> fromMaybe (-2) $ playGuessWho allClues ms

allClues' :: [(Text, Int -> Bool)]
allClues' =
    [ ("children", (== 3))
    , ("cats", (> 7))
    , ("samoyeds", (== 2))
    , ("pomeranians", (< 3))
    , ("akitas", (== 0))
    , ("vizslas", (== 0))
    , ("goldfish", (< 5))
    , ("trees", (> 3))
    , ("cars", (== 2))
    , ("perfumes", (== 1))
    ]

day16' :: String -> Int
day16' input = case parseInput input of
    (Left _)   -> -1
    (Right ms) -> fromMaybe (-2) $ playGuessWho' allClues' ms

-- Input
run :: IO ()
run = do
    putStrLn "Day 16 results: "
    input <- readFile "inputs/day16.txt"
    putStrLn $ "  " ++ show (day16 input)
    putStrLn $ "  " ++ show (day16' input)
