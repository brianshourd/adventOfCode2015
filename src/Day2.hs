module Day2 (day2, day2', run) where

import Control.Monad ((>=>))
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (pack)
import Data.List.Split
import Safe (readMay)

day2 :: String -> Int
day2 = parseAndApply paperPerBox

parseAndApply :: (Box -> Int) -> String -> Int
parseAndApply f input = case parseOnly parseBoxes (pack input) of
    (Right boxes) -> sum . map f $ boxes
    _ -> -1

data Box = Box Int Int Int
    deriving Show

parseBox :: Parser Box
parseBox = do
  length <- decimal
  char 'x'
  width <- decimal
  char 'x'
  height <- decimal
  return $ Box length width height

parseBoxes :: Parser [Box]
parseBoxes = many' $ parseBox <* option () endOfLine

paperPerBox :: Box -> Int
paperPerBox (Box x y z) = let areas = [x * y, y * z, x * z] in
    (2 * sum areas) + (minimum areas)

day2' :: String -> Int
day2' = parseAndApply ribbonPerBox

ribbonPerBox :: Box -> Int
ribbonPerBox (Box x y z) = let perimiters = [2 * (x + y), 2 * (y + z), 2 * (x + z)] in
    (x * y * z) + (minimum perimiters)

-- Input
run :: IO ()
run = do
    putStrLn "Day 2 results: "
    input <- readFile "inputs/day2.txt"
    putStrLn $ "  " ++ show (day2 input)
    putStrLn $ "  " ++ show (day2' input)
