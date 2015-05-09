# Data-Structures
Repository for various data structures, implemented in various languages.

* PythonGraph
   * Seven Bridges of Konigsberg
   * UMD Course Pre-Requisites
* HaskellGraph
* PythonTree
* HaskellTree

## PythonGraph
Graph data structure implemented in Python using an adjacency matrix. The graph is unweighted, but
multiple edges are allowed between nodes.

One interesting method is the to_dot method which creates a dot file from the current graph object.
GraphViz allows us to actually see the graph we are working with.

To create a png file from the dot file:

`neato -Tpng file_name.dot -o file_name.png`

One graph we can draw is the graph for the Seven Bridges of Konigsberg problem. See the section below.

### Seven Bridges of Konigsberg:
![seven_bridges](https://cloud.githubusercontent.com/assets/8814511/7099938/463bf616-dfd3-11e4-8972-1ea99b039a41.png)

This problem was very influential in the study of Graph Theory. The problem is this:

You want to cross all seven bridges exactly one time. Can you do this? You do not have to end up where you started.

Euler proved that you cannot do this. The graph corresponding to this problem has no Eulerian path - a path in which every edge is visited once and only once. More information can be found at the Wikipedia page 
[Seven Bridges of Konigsberg](http://en.wikipedia.org/wiki/Seven_Bridges_of_Königsberg).

### UMD Course Pre-Requisites
There's a cool API for UMD data at [umd.io](http://umd.io).

Using data from that API and the graph data structure in graph.py we can draw the following graph of course pre-requisites
for the Computer Science department by calling `umd.pre_reqs_graph("CMSC")`
![cs](https://cloud.githubusercontent.com/assets/8814511/7126176/2732b3ca-e205-11e4-9a00-5ac2a954dde5.png)

There's a few remaining issues with umd.py:

1. MATH115's prereqs include "eligibility of MATH115" which the program reads in as the course having itself as a prereq.

2. Sometimes a course may end up having multiple arrows to its prereqs. For instance, it looks like CMSC430 has two arrows
   going to CMSC351.

3. There's still a few issues in having the right prereqs drawn. For instance, CMSC456 has no arrow to CMSC351, MATH240 has        no arrow to MATH141, and CMSC427 has no arrow to CMSC420.

## HaskellGraph

There are many ways to implement graphs. I used an adjacency matrix for representing a graph with Python.
In Haskell, it made more sense to keep a list of all the edges.

## PythonTree

Binary Search Tree implemented in Python.

## HaskellTree

Binary Search Tree implemented in Haskell.
