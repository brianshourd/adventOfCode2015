module Day11 (day11, day11', run, requirement1, requirement2, requirement3) where

import Data.Char (chr, ord)
import Data.List (group)

-- Passwords must include one increasing straight of at least three letters,
-- like abc, bcd, cde, and so on, up to xyz. They cannot skip letters; abd
-- doesn't count.
requirement1 :: String -> Bool
requirement1 []           = False
requirement1 [x]          = False
requirement1 [x, y]       = False
requirement1 (x:y:z:rest) =
    ((ord y == ord x + 1) && (ord z == ord y + 1)) || requirement1 (y:z:rest)

-- Passwords may not contain the letters i, o, or l, as these letters can be
-- mistaken for other characters and are therefore confusing.
requirement2 :: String -> Bool
requirement2 input = all (not . flip elem input) ['i', 'o', 'l']

-- Passwords must contain at least two different, non-overlapping pairs of
-- letters, like aa, bb, or zz.
requirement3 :: String -> Bool
requirement3 = (>= 2) . snd . foldl f (Nothing, 0)
    where
        f :: (Maybe Char, Int) -> Char -> (Maybe Char, Int)
        f (Nothing, count) c = (Just c, count)
        f (Just d, count) c = if c == d then (Nothing, count + 1) else (Just c, count)

-- Incrementing is just like counting with numbers: xx, xy, xz, ya, yb, and so
-- on. Increase the rightmost letter one step; if it was z, it wraps around to
-- a, and repeat with the next letter to the left until one doesn't wrap around.
increment :: String -> String
increment = reverse . increment' . reverse
    where
        increment' [] = []
        increment' (x:xs) = if x == 'z' then 'a' : increment' xs else (chr . (+1) . ord $ x):xs

validPasswordsAfter :: String -> [String]
validPasswordsAfter =
    filter (\pw -> all ($ pw) [requirement2, requirement1, requirement3])
    . iterate increment

day11 :: String -> String
day11 = head . validPasswordsAfter

day11' :: String -> String
day11' = head . tail . validPasswordsAfter

-- Input
run :: IO ()
run = do
    putStrLn "Day 11 results: "
    let input = "vzbxkghb"
    putStrLn $ "  " ++ show (day11 input)
    putStrLn $ "  " ++ show (day11' input)
