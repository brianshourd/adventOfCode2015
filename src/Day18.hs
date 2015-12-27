{-# LANGUAGE OverloadedStrings #-}
module Day18
    ( day18
    , day18'
    , run
    , Boundary(..)
    , Grid(..)
    , Location
    , conwayStep
    , countLightsOn
    , listLocations
    , parseInput
    , willBeOn
    ) where

import Data.Set (Set)
import qualified Data.Set as Set
    ( empty
    , fromList
    , insert
    , member
    , size
    , toList
    , union
    )

type Location = (Int, Int)
data Boundary = Boundary !Location !Location deriving (Show, Eq)
data Grid = Grid
    { gridBoundary :: !Boundary
    , gridOn       :: !(Set Location)
    } deriving (Eq)

instance Show Grid
  where
    show g = unlines . map (\y -> map (\x -> toChar (x, y)) [x1..x2]) $ [y1..y2]
      where
        Boundary (x1, y1) (x2, y2) = gridBoundary g
        toChar p = if Set.member p (gridOn g) then '#' else '.'

parseInput :: String -> Grid
parseInput input = Grid boundary onLights
  where
    rows = lines input
    boundary = Boundary
        (0, 0)
        ((maximum . map length $ rows) - 1, length rows - 1)
    onLightsRow :: Int -> String -> Set Location
    onLightsRow y = foldrWithIndex
        (\c i acc -> maybeInsert (c == '#') (i, y) acc)
        Set.empty
    maybeInsert True x  = Set.insert x
    maybeInsert False _ = id
    onLights = foldrWithIndex
        (\r j acc -> Set.union acc (onLightsRow j r))
        Set.empty
        rows
    foldrWithIndex :: (a -> Int -> b -> b) -> b -> [a] -> b
    foldrWithIndex f z = foldr (uncurry f) z . flip zip [0..]

listLocations :: Boundary -> [Location]
listLocations (Boundary (x1, y1) (x2, y2))
    = [(x, y) | x <- [x1..x2], y <- [y1..y2]]

conwayStep :: Grid -> Grid
conwayStep (Grid b onSet) = Grid b onSet'
  where
    onSet' = Set.fromList . filter (willBeOn onSet) . listLocations $ b

willBeOn
    :: Set Location  -- Currently on
    -> Location      -- Location to check
    -> Bool          -- True if this location will be on
willBeOn onSet l
    | Set.member l onSet = numOnNearby == 2 || numOnNearby == 3
    | otherwise          = numOnNearby == 3
  where
    numOnNearby = length . filter (`Set.member` onSet) $ nearbyLocations l
    nearbyLocations (x, y) =
        [ (x - 1, y - 1), (x, y - 1), (x + 1, y - 1)
        , (x - 1, y    ),             (x + 1, y    )
        , (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)
        ]

countLightsOn :: Grid -> Int
countLightsOn = Set.size . gridOn

day18 :: String -> Int
day18 = countLightsOn . (!! 100) . iterate conwayStep . parseInput

day18' :: String -> Int
day18' = countLightsOn
    . (!! 100)
    . iterate (forceCornersOn . conwayStep)
    . forceCornersOn
    . parseInput
  where
    forceCornersOn (Grid b onSet) = Grid b $ Set.union onSet (corners b)
    corners (Boundary (x1, y1) (x2, y2))
        = Set.fromList [(x1, y1), (x1, y2), (x2, y1), (x2, y2)]

-- Input
run :: IO ()
run = do
    putStrLn "Day 18 results: "
    input <- readFile "inputs/day18.txt"
    putStrLn $ "  " ++ show (day18 input)
    putStrLn $ "  " ++ show (day18' input)
