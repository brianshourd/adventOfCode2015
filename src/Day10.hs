module Day10 (day10, day10', run, lookAndSay) where

import Data.List (group)

lookAndSay :: String -> String
lookAndSay = concat . map processGroup . group

processGroup :: String -> String
processGroup []     = []
processGroup (x:xs) = (show (1 + length xs)) ++ [x]

day10 :: String -> Int
day10 = length . (!! 40) . iterate lookAndSay

day10' :: String -> Int
day10' = length . (!! 50) . iterate lookAndSay

-- Input
run :: IO ()
run = do
    putStrLn "Day 10 results: "
    let input = "1113122113"
    putStrLn $ "  " ++ show (day10 input)
    putStrLn $ "  " ++ show (day10' input)
