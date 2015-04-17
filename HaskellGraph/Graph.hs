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
            deriving (Eq)

-- A graph is a list of edges
data Graph = Graph { edges :: [Edge]}
             deriving (Show, Eq)


instance Show Edge where
    show (Edge source destination weight) = show source ++ " " ++
                                            show destination ++ " " ++
                                            show weight


-- Insertion

-- Insert into a directed graph
insertDirected :: Edge -> Graph -> Graph
insertDirected newEdge (Graph edges) = Graph (newEdge:edges)

-- Insert into a undirected graph
insertUndirected :: Edge -> Graph -> Graph
insertUndirected (Edge source destination weight) (Graph edges) =
    Graph $ (Edge source destination weight) : (Edge destination source weight) : edges
    
    
    
-- Removal

-- Remove an edge from a graph
removeDirected :: Edge -> Graph -> Graph
removeDirected oldEdge (Graph edges) = Graph (delete oldEdge edges)

-- Remove an edge from an undirected graph
removeUndirected :: Edge -> Graph -> Graph
removeUndirected (Edge source destination weight) (Graph edges) =
    let oldEdge = Edge source destination weight
        oldEdge' = Edge destination source weight
    in Graph (delete oldEdge' $ delete oldEdge edges)
    
    
    
-- Search

-- Search for a vertex in an edge
containsVertex :: Vertex -> Edge -> Bool
containsVertex vertex edge = sourceVertex vertex edge || destinationVertex vertex edge
    
-- Search for a vertex in a graph
graphContainsVertex :: Vertex -> Graph-> Bool
graphContainsVertex vertex (Graph edges) =
                            any (== True) $ map (containsVertex vertex) edges
    
-- Search for an edge in a graph
containsEdge :: Edge -> Graph -> Bool
containsEdge edge (Graph edges) = edge `elem` edges



-- Getters

-- Get the source vertex in an edge
getSourceVertex :: Edge -> Vertex
getSourceVertex (Edge source _ _) = source

-- Get the destination vertex in an edge
getDestinationVertex :: Edge -> Vertex
getDestinationVertex (Edge _ destination _) = destination

-- Get the source vertices of a graph, with repeats
getSourceVertices :: Graph -> [Vertex]
getSourceVertices (Graph edges) = map getSourceVertex edges

-- Get the destination vertices of a graph, with repeats
getDestinationVertices :: Graph -> [Vertex]
getDestinationVertices (Graph edges) = map getDestinationVertex edges

-- Get all the vertices of a graph, with repeats
getVertices :: Graph -> [Vertex]
getVertices graph = getSourceVertices graph ++ getDestinationVertices graph

-- Get the source vertices of a graph, without repeats
getSourceVerticesNub :: Graph -> [Vertex]
getSourceVerticesNub graph = nub $ getSourceVertices graph

-- Get the destination vertices of a graph, without repeats
getDestinationVerticesNub :: Graph -> [Vertex]
getDestinationVerticesNub graph = nub $ getDestinationVertices graph

-- Get all the vertices of a graph, without repeats
getVerticesNub :: Graph -> [Vertex]
getVerticesNub graph = nub $ getVertices graph

-- Get the number of vertices, that is, the order of the graph
getOrder :: Graph -> Int
getOrder graph = length $ getVerticesNub graph

-- Get the in-degree of a vertex
getInDegree :: Vertex -> Graph -> Int
getInDegree vertex graph = length $ filter (== vertex) $ getDestinationVertices graph

-- Get the out-degree of a vertex
getOutDegree :: Vertex -> Graph -> Int
getOutDegree vertex graph = length $ filter (== vertex) $ getSourceVertices graph

-- Get a list of vertices with the same in-degree as out-degree
getSameInOut :: Graph -> [Vertex]
getSameInOut graph = filter (\x -> sameInOut x graph) $ getVerticesNub graph



-- Bool functions

-- Check if a vertex is the source vertex in an edge
sourceVertex :: Vertex -> Edge -> Bool
sourceVertex vertex (Edge source _ _) = vertex == source

-- Check if a vertex is the destination vertex in an edge
destinationVertex :: Vertex -> Edge -> Bool
destinationVertex vertex (Edge _ destination _) = vertex == destination
                     
-- Check if a vertex has same in-degree as out-degree
sameInOut :: Vertex -> Graph -> Bool
sameInOut vertex graph = getInDegree vertex graph == getOutDegree vertex graph
                            
-- Check if an Eulerian Cycle exists in a directed graph
eulerianCycleExistsDirected :: Graph -> Bool
eulerianCycleExistsDirected graph = let num = length $ getSameInOut graph
                           in num == getOrder graph

-- Check if an Eulerian Path exists in a directed graph
eulerianPathExistsDirected :: Graph -> Bool
eulerianPathExistsDirected graph = let num = length $ getSameInOut  graph
                           in num == (getOrder graph) - 2 ||
                              num == getOrder graph
                              
-- Check if in-degree of vertex mod n == 0
inMod :: Graph -> Int -> Vertex -> Bool
inMod graph n vertex = (getInDegree vertex graph) `mod` n == 0
                              
-- Check if an Eulerian Cycle exists in an undirected graph
eulerianCycleExistsUndirected :: Graph -> Bool
eulerianCycleExistsUndirected graph = all (inMod graph 2) $ getVerticesNub graph

-- Check if an Eulerian Path exists in an undirected graph
eulerianPathExistsUndirected :: Graph -> Bool
eulerianPathExistsUndirected graph = (eulerianCycleExistsUndirected graph) ||
                                    (2 == (length $ filter (inMod graph 2) $ getVerticesNub graph))
