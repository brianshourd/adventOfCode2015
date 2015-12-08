module Day5 (day5, day5', run) where

import Text.Regex.TDFA

day5 :: String -> Int
day5 = length . filter isNice . lines

isNice :: String -> Bool
isNice input = and $ map ($ input) [contains3Vowels, containsDouble, isntBlacklisted]

contains3Vowels :: String -> Bool
contains3Vowels input = input =~ "[aeiou].*[aeiou].*[aeiou]"

containsDouble :: String -> Bool
containsDouble = any (uncurry (==)) . adjacentPairs

adjacentPairs :: [a] -> [(a, a)]
adjacentPairs [] = []
adjacentPairs s  = zip s $ tail s

isntBlacklisted :: String -> Bool
isntBlacklisted input = not $ input =~ "(ab|cd|pq|xy)"

day5' :: String -> Int
day5' = length . filter isNice' . lines

isNice' :: String -> Bool
isNice' input = and $ map ($ input) [containsPairTwice, containsRepeatBetween]

containsPairTwice :: String -> Bool
containsPairTwice = matchSkipLoop elem . adjacentPairs

containsRepeatBetween :: String -> Bool
containsRepeatBetween = matchSkipLoop equalsHead
    where
        equalsHead :: Char -> [Char] -> Bool
        equalsHead _ [] = False
        equalsHead x (y:ys) = x == y

matchSkipLoop :: (a -> [a] -> Bool) -> [a] -> Bool
matchSkipLoop f []         = False
matchSkipLoop f (x:[])     = False
matchSkipLoop f (x:y:rest) = (f x rest) || matchSkipLoop f (y:rest)

-- Input
run :: IO ()
run = do
    putStrLn "Day 5 results: "
    input <- readFile "inputs/day5.txt"
    putStrLn $ "  " ++ show (day5 input)
    putStrLn $ "  " ++ show (day5' input)
