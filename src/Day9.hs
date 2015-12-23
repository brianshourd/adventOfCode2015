{-# LANGUAGE OverloadedStrings #-}
module Day9 (day9, day9', run) where

import Data.Attoparsec.Text
import Data.Function (on)
import Data.List (maximumBy, minimumBy, sort)
import Data.Maybe (catMaybes)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Monoid (Sum(..))
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text, pack, unpack)
import Safe

-- Defining data types for an undirected, weighted graph, with the additional
-- constraint that there are no lone nodes, so that the entirety of the graph
-- can be represented by the Edge map
-- Expectations: Ord t
data Node t = Node t deriving (Show, Eq, Ord)
data Edge t = Edge
    { edgeStart :: Node t
    , edgeEnd   :: Node t
    } deriving Show
-- Undirected edges
instance Ord t => Eq (Edge t) where
    (Edge s1 e1) == (Edge s2 e2) = sort [s1, e1] == sort [s2, e2]
instance Ord t => Ord (Edge t) where
    compare (Edge s1 e1) (Edge s2 e2) = compare (sort [s1, e1]) (sort [s2, e2])
type EdgeWeightMap t w = Map (Edge t) w
type NodeSet t = Set (Node t)
data Traversal t w = Traversal
    { traversalNodes  :: [Node t]
    , traversalWeight :: w
    }

-- Basic graph functionality
nodes :: Ord t => EdgeWeightMap t w -> NodeSet t
nodes = S.fromList . concat . map (\(Edge s1 e1) -> [s1, e1]) . M.keys

-- Prepend a node to a traversal, increasing the weight
prependNode :: (Ord t, Monoid w)
    => EdgeWeightMap t w
    -> Node t
    -> Traversal t w
    -> Maybe (Traversal t w)
prependNode g n (Traversal [] w)     = Just $ Traversal [n] w
prependNode g n (Traversal (x:xs) w) =
    fmap (Traversal (n:x:xs) . mappend w) $ M.lookup (Edge x n) g

-- Find the best complete traversal, if one exists
bestTraversal :: (Ord t, Monoid w)
    => EdgeWeightMap t w    -- source data
    -> (w -> w -> Ordering) -- comparison function
    -> Maybe (Traversal t w)
bestTraversal sourceData comp =
    maximumByMay (comp `on` traversalWeight) . bestTraversals sourceData comp $ nodes sourceData

-- Find the best traversal that starts at the given node and visits every one of
-- the other nodes provided, if one exists
bestTraversal' :: (Ord t, Monoid w)
    => EdgeWeightMap t w    -- source data
    -> (w -> w -> Ordering) -- comparison function
    -> Node t               -- start node
    -> NodeSet t            -- other nodes to visit
    -> Maybe (Traversal t w)
bestTraversal' sourceData comp s ns =
    maximumByMay (comp `on` traversalWeight)
    . catMaybes
    . map (prependNode sourceData s)
    $ bestTraversals sourceData comp ns

-- Find the best complete traversals for just the nodes provided, one for each
-- starting node
bestTraversals :: (Ord t, Monoid w)
    => EdgeWeightMap t w    -- source data
    -> (w -> w -> Ordering) -- comparison function
    -> NodeSet t            -- nodes to visit
    -> [Traversal t w]
bestTraversals sourceData comp ns =
    if null ns
    then [Traversal [] mempty]
    else catMaybes . map bestFrom . S.elems $ ns
    where
        bestFrom n = bestTraversal' sourceData comp n (S.delete n ns)

-- Parsing types and type synonyms for the problem at hand
type City = Text
type Distance = Sum Int
data Source = Source
    { start    :: City
    , end      :: City
    , distance :: Distance
    } deriving Show
type SourceData = EdgeWeightMap City Distance

compileMap :: [Source] -> SourceData
compileMap = M.fromList . map toPair
    where
        toPair (Source s e d) = (Edge (Node s) (Node e), d)

parseSourceData :: Parser SourceData
parseSourceData = do
    sources <- many' parseSource
    return $ compileMap sources

parseSource :: Parser Source
parseSource = do
    a <- takeTill isHorizontalSpace
    string " to "
    b <- takeTill isHorizontalSpace
    string " = "
    dist <- decimal
    endOfLine
    return $ Source a b (Sum dist)

-- Begin actual implementation
day9 :: String -> Int
day9 = day9Impl (compare `on` negate)

day9' :: String -> Int
day9' = day9Impl compare

day9Impl :: (Sum Int -> Sum Int -> Ordering) -> String -> Int
day9Impl comp input = case parseOnly parseSourceData . pack $ input of
    (Left _) -> -1
    (Right sourceData) -> case (bestTraversal sourceData comp) of
        Nothing -> -2
        (Just t) -> getSum . traversalWeight $ t

-- Input
run :: IO ()
run = do
    putStrLn "Day 9 results: "
    input <- readFile "inputs/day9.txt"
    putStrLn $ "  " ++ show (day9 input)
    putStrLn $ "  " ++ show (day9' input)
