module Day3 (day3, day3', run) where

import Data.Set (Set, insert, empty, size, union)

-- Given a string such as "><<>", return the number of houses visited
day3 :: String -> Int
day3 = size . snd . foldl move ((0, 0), insert (0, 0) empty)

type Position = (Integer, Integer)
type State = (Position, Set Position)

move :: State -> Char -> State
move (p, ps) dir = let q = go dir p in (q, insert q ps)

go :: Char -> Position -> Position
go dir (x, y) = case dir of
    '>' -> (x + 1, y)
    '<' -> (x - 1, y)
    '^' -> (x, y + 1)
    'v' -> (x, y - 1)
    _   -> (x, y)


-- Second half
day3' :: String -> Int
day3' = size . merge . foldl move' (startState, startState)
    where
        startState = ((0, 0), insert (0, 0) empty)
        merge :: TwoState -> Set Position
        merge ((_, ps), (_, qs)) = ps `union` qs

type TwoState = (State, State)

move' :: TwoState -> Char -> TwoState
move' ((p, ps), other) dir = let q = go dir p in (other, (q, insert q ps))

-- Actual input
run :: IO ()
run = do
    putStrLn "Day 3 results: "
    input <- readFile "inputs/day3.txt"
    putStrLn $ "  " ++ show (day3 input)
    putStrLn $ "  " ++ show (day3' input)

