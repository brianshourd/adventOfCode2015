module Graph
    ( Node(Node)
    , Edge(Edge)
    , EdgeWeightMap
    , Traversal(Traversal)
    , traversalWeight
    , NodeSet
    , nodes
    , bestTraversal
    , bestTraversal'
    ) where

import Data.Function (on)
import Data.Maybe (mapMaybe)
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Safe

-- Defining data types for an undirected, weighted graph, with the additional
-- constraint that there are no lone nodes, so that the entirety of the graph
-- can be represented by the Edge map
-- Expectations: Ord t
data Node t = Node t deriving (Show, Eq, Ord)
data Edge t = Edge (Node t) (Node t) deriving Show
-- Undirected edges
instance Ord t => Eq (Edge t) where
    (Edge s1 e1) == (Edge s2 e2) = sort [s1, e1] == sort [s2, e2]
instance Ord t => Ord (Edge t) where
    compare (Edge s1 e1) (Edge s2 e2) = compare (sort [s1, e1]) (sort [s2, e2])
type EdgeWeightMap t w = Map (Edge t) w
type NodeSet t = Set (Node t)
data Traversal t w = Traversal [Node t] w deriving Show

-- Basic graph functionality
traversalWeight :: Traversal t w -> w
traversalWeight (Traversal _ w) = w

nodes :: Ord t => EdgeWeightMap t w -> NodeSet t
nodes = S.fromList . concatMap (\(Edge s1 e1) -> [s1, e1]) . M.keys

-- Prepend a node to a traversal
prependNode :: (Ord t, Monoid w)
    => EdgeWeightMap t w
    -> Maybe (Node t)
    -> Traversal t w
    -> Maybe (Traversal t w)
prependNode _ Nothing  t                    = Just t
prependNode g (Just n) (Traversal [] w)     = Just $ Traversal [n] w
prependNode g (Just n) (Traversal (x:xs) w) =
    (Traversal (n:x:xs) . mappend w) <$> M.lookup (Edge x n) g

-- Append a node to a traversal
appendNode :: (Ord t, Monoid w)
    => EdgeWeightMap t w
    -> Maybe (Node t)
    -> Traversal t w
    -> Maybe (Traversal t w)
appendNode g n (Traversal xs w) =
    let t = prependNode g n (Traversal (reverse xs) w) in
        case t of
            Nothing -> Nothing
            Just (Traversal ys w') -> Just $ Traversal (reverse ys) w'

-- Find the best complete traversal, if one exists
bestTraversal :: (Ord t, Monoid w)
    => EdgeWeightMap t w    -- source data
    -> (w -> w -> Ordering) -- comparison function
    -> Maybe (Traversal t w)
bestTraversal g comp =
    bestTraversal' g comp Nothing Nothing (nodes g)

bestTraversal' :: (Ord t, Monoid w)
    => EdgeWeightMap t w    -- source data
    -> (w -> w -> Ordering) -- comparison function
    -> Maybe (Node t)       -- optional start node
    -> Maybe (Node t)       -- optional end node
    -> NodeSet t            -- nodes to visit
    -> Maybe (Traversal t w)
bestTraversal' g comp s e ns =
    if null ns
    then appendNode g e (Traversal [] mempty) >>= prependNode g s
    else
        maximumByMay (comp `on` traversalWeight)
        . mapMaybe
            ( (>>= prependNode g s)
            . (\n -> bestTraversal' g comp (Just n) e (S.delete n ns))
            )
        . S.elems
        $ ns
