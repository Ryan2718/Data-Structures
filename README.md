# Data-Structures
Repository for various data structures.

## PythonGraph
Graph data structure implemented in Python using an adjacency matrix. The graph is unweighted, but
multiple edges are allowed between nodes.

One interesting method is the to_dot method which createsa dot file from the current graph object.
GraphViz allows us to actually see the graph we are working with.

To create a png file from the dot file:

`neato -Tpng file_name.dot -o file_name.png`

One graph we can draw is the graph for the Seven Bridges of Konigsberg problem. See the section below.

### Seven Bridges of Konigsberg:
![seven_bridges](https://cloud.githubusercontent.com/assets/8814511/7099938/463bf616-dfd3-11e4-8972-1ea99b039a41.png)

This problem was very influential in the study of Graph Theory. The problem is this:

You want to cross all seven bridges exactly one time. Can you do this? You do not have to end up where you started.

Euler proved that you cannot do this. The graph corresponding to this problem has no Eulerian path - a path in which every edge is visited once and only once. More information can be found at the Wikipedia page
[wikipedia page](http://en.wikipedia.org/wiki/Seven_Bridges_of_Königsberg).
