{-# LANGUAGE OverloadedStrings #-}
module Day6 (day6, day6', run) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (pack)

day6 :: String -> Int
day6 input = case parseOnly parseInstructions (pack input) of
    (Right instructions) -> length . filter (== On) . map (flip applyInstructions $ instructions) $ [ (x, y) | x <- [0..999], y <- [0..999] ]
    _ -> -1

test input = case parseOnly parseInstructions (pack input) of
    (Right instructions) -> length [ (x, y) | x <- [0..999], y <- [0..999] ]
    _ -> undefined

data Instruction = Instruction Operation BulbRange deriving (Show, Eq)
data Operation = TurnOn | TurnOff | Toggle deriving (Show, Eq)
data BulbRange = BulbRange (Int, Int) (Int, Int) deriving (Show, Eq)

parseInstructions :: Parser [Instruction]
parseInstructions = many' $ parseInstruction <* option () endOfLine

parseInstruction :: Parser Instruction
parseInstruction = do
    operation <- parseOperation
    range <- parseBulbRange
    return $ Instruction operation range

parseOperation :: Parser Operation
parseOperation =
    (string "turn on " >> return TurnOn) <|>
    (string "turn off " >> return TurnOff) <|>
    (string "toggle " >> return Toggle)

parseBulbRange :: Parser BulbRange
parseBulbRange = do
    start <- parseTuple
    string " through "
    end <- parseTuple
    return $ BulbRange start end

parseTuple :: Parser (Int, Int)
parseTuple = do
    x <- decimal
    char ','
    optional $ char ' '
    y <- decimal
    return (x, y)

type Position = (Int, Int)
data State = On | Off deriving (Show, Eq)

applyInstructions :: Position -> [Instruction] -> State
applyInstructions p = foldl (flip $ applyOperation . getOperation) Off . filter (inRange p . getRange)

getOperation :: Instruction -> Operation
getOperation (Instruction o _) = o

getRange :: Instruction -> BulbRange
getRange (Instruction _ r) = r

inRange :: Position -> BulbRange -> Bool
inRange (x, y) (BulbRange (x1, y1) (x2, y2)) = x1 <= x && y1 <= y && x <= x2 && y <= y2

applyOperation :: Operation -> State -> State
applyOperation TurnOff _   = Off
applyOperation TurnOn  _   = On
applyOperation Toggle  On  = Off
applyOperation Toggle  Off = On

day6' :: String -> Int
day6' = undefined

-- Input
run :: IO ()
run = do
    putStrLn "Day 6 results: "
    input <- readFile "inputs/day6.txt"
    putStrLn $ "  377891 (cached)"
    --putStrLn $ "  " ++ show (day6 input)
    --putStrLn $ "  " ++ show (day6' input)
