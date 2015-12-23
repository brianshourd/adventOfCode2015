module Graph
    ( Node(Node)
    , Edge(Edge)
    , EdgeWeightMap
    , Traversal(Traversal)
    , traversalWeight
    , NodeSet
    , nodes
--    , prependNode
    , bestTraversal
    ) where

import Data.Function (on)
import Data.List (sort)
import Data.Maybe (catMaybes)
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
data Traversal t w = Traversal [Node t] w

-- Basic graph functionality
traversalWeight :: Traversal t w -> w
traversalWeight (Traversal _ w) = w

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

