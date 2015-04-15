-- Ryan Forsyth
-- 04/14/2015

module Graph where

import Data.List

-- Vertex is a type synonym for the String that labels this vertex
type Vertex = String
  
-- An edge goes from 'source' to 'destination' and has weight 'weight'                      
data Edge = Edge { source :: Vertex
                 , destination :: Vertex
                 , weight :: Int}
            deriving (Show, Eq)

-- A graph is a list of edges
data Graph = Graph { edges :: [Edge]}
             deriving (Show, Eq)


-- Insert into a directed graph
insertDirected :: Edge -> Graph -> Graph
insertDirected newEdge (Graph edges) = Graph (newEdge:edges)

-- Insert into a undirected graph
insertUndirected :: Edge -> Graph -> Graph
insertUndirected (Edge source destination weight) (Graph edges) =
    Graph $ (Edge source destination weight) : (Edge destination source weight) : edges

-- Remove an edge from a graph
removeDirected :: Edge -> Graph -> Graph
removeDirected oldEdge (Graph edges) = Graph (delete oldEdge edges)

-- Remove an edge from an undirected graph
removeUndirected :: Edge -> Graph -> Graph
removeUndirected (Edge source destination weight) (Graph edges) =
    let oldEdge = Edge source destination weight
        oldEdge' = Edge destination source weight
    in Graph (delete oldEdge' $ delete oldEdge edges)
    
-- Search for an edge
containsEdge :: Edge -> Graph -> Bool
containsEdge edge (Graph edges) = edge `elem` edges

-- Search for a vertex
containsVertex :: Vertex -> Edge -> Bool
containsVertex vertex (Edge source destination _) =
    vertex == source || vertex == destination
    
-- Search for a vertex in a graph
graphContainsVertex :: Vertex -> Graph-> Bool
graphContainsVertex vertex (Graph edges) =
                            any (== True) $ map (containsVertex vertex) edges
 
