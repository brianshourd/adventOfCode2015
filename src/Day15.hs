{-# LANGUAGE OverloadedStrings #-}
module Day15 (day15, day15', run, Ingredient(..), parseInput, partitions, highestCookieScore, highestCookieScore') where

import Control.Applicative (optional)
import Data.Attoparsec.Text
    ( Parser(..)
    , decimal
    , endOfLine
    , isHorizontalSpace
    , many'
    , parseOnly
    , signed
    , string
    , takeTill
    )
import Data.List (transpose)
import Data.Text (Text, pack)

data Ingredient = Ingredient
    { name       :: !Text
    , capacity   :: !Int
    , durability :: !Int
    , flavor     :: !Int
    , texture    :: !Int
    , calories   :: !Int
    } deriving (Show, Eq)

parseInput :: String -> Either String [Ingredient]
parseInput = parseOnly (many' parseIngredient) . pack

parseIngredient :: Parser Ingredient
parseIngredient = do
    name <- takeTill (== ':')
    string ": capacity "
    capacity <- signed decimal
    string ", durability "
    durability <- signed decimal
    string ", flavor "
    flavor <- signed decimal
    string ", texture "
    texture <- signed decimal
    string ", calories "
    calories <- signed decimal
    optional endOfLine
    return $ Ingredient name capacity durability flavor texture calories

highestCookieScore :: [Ingredient] -> Int
highestCookieScore = highestCookieScoreImpl (const True)

highestCookieScore' :: [Ingredient] -> Int
highestCookieScore' = highestCookieScoreImpl (== 500)

-- There's only 4 ingredients, so only 176851 possible combinations
-- We can brute-force that!
-- Here pred is a predicate applied to the number of calories which must be true
-- of any recipe to be considered
highestCookieScoreImpl :: (Int -> Bool) -> [Ingredient] -> Int
highestCookieScoreImpl pred is =
    maximum
    . map gradeRecipe
    . filter (pred . head)
    . map calcRecipe
    $ allCombinations
  where
    gradeRecipe :: [Int] -> Int
    gradeRecipe = product . tail
    calcRecipe :: [Int] -> [Int]
    calcRecipe =
        map (truncZero . sum)
        . transpose
        . zipWith scaleIngredient is
    scaleIngredient :: Ingredient -> Int -> [Int]
    scaleIngredient (Ingredient _ c d f t l) s = map (* s) [l, c, d, f, t]
    truncZero :: Int -> Int
    truncZero x | x < 0     = 0
                | otherwise = x
    allCombinations :: [[Int]]
    allCombinations = partitions (length is) 100

-- Calculate partitions, where each partition is 0 or more
partitions
    :: Int      -- n: How many pieces in the partition
    -> Int      -- t: Total the partition should sum to
    -> [[Int]]  -- List of partitions of n integers summing to t
partitions 1 t = [[t]]
partitions _ 0 = [[0]]
partitions n t = concatMap recurse [0..t]
  where
    recurse t' = map (t' :) $ partitions (n - 1) (t - t')

day15 :: String -> Int
day15 input = case parseInput input of
    (Left _)   -> -1
    (Right is) -> highestCookieScore is

day15' :: String -> Int
day15' input = case parseInput input of
    (Left _)   -> -1
    (Right is) -> highestCookieScore' is

-- Input
run :: IO ()
run = do
    putStrLn "Day 15 results: "
    input <- readFile "inputs/day15.txt"
    putStrLn $ "  " ++ show (day15 input)
    putStrLn $ "  " ++ show (day15' input)
