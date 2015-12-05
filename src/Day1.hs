module Day1 (day1, day1', run) where

day1 :: String -> Int
day1 = foldl move 0

move :: Int -> Char -> Int
move x c = case c of
    '(' -> x + 1
    ')' -> x - 1
    _   -> x

day1' :: String -> Int
day1' = day1'' 0 0

day1'' :: Int -> Int -> String -> Int
day1'' (-1)         index _            = index
day1'' _            _     []           = -1
day1'' currentFloor index (input:rest) =
    day1'' (move currentFloor input) (index + 1) rest

-- Input
run :: IO ()
run = do
    putStrLn "Day 1 results: "
    input <- readFile "inputs/day1.txt"
    putStrLn $ "  " ++ show (day1 input)
    putStrLn $ "  " ++ show (day1' input)
