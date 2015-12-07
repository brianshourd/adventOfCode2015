module Day5 (day5, day5', run, contains3Vowels, containsDouble) where

import Text.Regex.TDFA

day5 :: String -> Int
day5 = length . filter isNice . lines

isNice :: String -> Bool
isNice input = and $ map ($ input) [contains3Vowels, containsDouble, isntBlacklisted]

contains3Vowels :: String -> Bool
contains3Vowels input = input =~ "[aeiou].*[aeiou].*[aeiou]"

containsDouble :: String -> Bool
--Why isn't this working?
--containsDouble input = input =~ "(.)\\1"
containsDouble [] = False
containsDouble (_:[]) = False
containsDouble (x:y:rest) = x == y || containsDouble (y:rest)

isntBlacklisted :: String -> Bool
isntBlacklisted input = not $ input =~ "(ab|cd|pq|xy)"

day5' :: String -> Int
day5' = undefined

-- Input
run :: IO ()
run = do
    putStrLn "Day 5 results: "
    input <- readFile "inputs/day5.txt"
    putStrLn $ "  " ++ show (day5 input)
    --putStrLn $ "  " ++ show (day5' input)
