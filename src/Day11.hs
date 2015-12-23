module Day11 (day11, day11', run, requirement1, requirement2, requirement3) where

-- Passwords must include one increasing straight of at least three letters,
-- like abc, bcd, cde, and so on, up to xyz. They cannot skip letters; abd
-- doesn't count.
requirement1 :: String -> Bool
requirement1 input = undefined

-- Passwords may not contain the letters i, o, or l, as these letters can be
-- mistaken for other characters and are therefore confusing.
requirement2 :: String -> Bool
requirement2 input = undefined

-- Passwords must contain at least two different, non-overlapping pairs of
-- letters, like aa, bb, or zz.
requirement3 :: String -> Bool
requirement3 input = undefined

-- Incrementing is just like counting with numbers: xx, xy, xz, ya, yb, and so
-- on. Increase the rightmost letter one step; if it was z, it wraps around to
-- a, and repeat with the next letter to the left until one doesn't wrap around.
increment :: String -> String
increment = undefined

day11 :: String -> String
day11 = head
    . filter (\pw -> all ($ pw) [requirement1, requirement2, requirement3])
    . iterate increment

day11' :: String -> String
day11' = undefined

-- Input
run :: IO ()
run = do
    putStrLn "Day 11 results: "
    let input = "vzbxkghb"
    putStrLn $ "  " ++ show (day11 input)
    putStrLn $ "  " ++ show (day11' input)
