# Ryan Forsyth
# 04/10/2015

class Graph:
    """
    Defines a Graph.
    The Graph is stored as an adjacency matrix.

    The number in the i-jth entry denotes the number of edges from
    vertex i to vertex j.  

    That is, if vertex i has 1 edge to vertex j,
    then the matrix will have a 1 in the i-jth entry.

    Thus, for an undirected graph, the matrix will be symmetric.
    """
    def __init__(self, adjacency_matrix = [[]]):
        """
        Constructor for a Graph object.

        Arguments:
        
        adjacency-matrix -- 
        List of lists
        The adjacency matrix for this graph.
        If no adjacency matrix is given, it will default to an
        empty matrix.
        """
        
        # Data Fields
        self.matrix = adjacency_matrix
        self.numVertices = len(adjacency_matrix)
        
        # Make the sure the adjacency matrix is square.
        for row in self.matrix:
            if len(row) != self.numVertices:
                raise Exception("Adjacency Matrix must be square!")

    def __repr__(self):
        """
        Representation of the graph to use when debugging.
        """

        # Header (Column Numbers)
        string = "Col #  "
        for j in range(1, self.numVertices + 1):
            string += str(j - 1)
            if j != self.numVertices:
                string += " "
        string += "\n"

        for i in range(1, self.numVertices + 1):
            for j in range(0, self.numVertices + 1):
                if j == 0:
                    string += "Row #" + str(i - 1) # Print the row number
                else:
                    string += str(self.matrix[i - 1][j - 1])
                if j != self.numVertices:
                    string += " "
            string += "\n"
        string += "numVertices: " + str(self.numVertices) + "\n"
        return string


    def __str__(self):
        """
        String version of the graph - i.e. the adjacency matrix
        """

        string = ""
        for i in range(0, self.numVertices):
            for j in range(0, self.numVertices):
                string += str(self.matrix[i][j])
                if j != self.numVertices - 1:
                    string += " "
            string += "\n"
        return string

    def to_dot(self, file_name = "graph.dot", graph_name="graph"):
        """
        Create a dot (i.e. Graphviz) file of the current Graph object.
        
        Arguments:
        
        file_name --
        The name of the file. By convention, should end in '.dot'.
        Defaults to 'graph.dot'

        graph_name --
        The name of the graph. Defaults to 'graph'
        """
        
        # Determine if the graph is directed
        directed = self.is_directed()

        # Initial Information
        file = open(file_name, "w")
        if directed:
            file.write("digraph")
            edge_op = " -> "
        else:
            file.write("graph")
            edge_op = " -- "
        file.write(" \"" + graph_name + "\" {\n")
        file.write("    graph [overlap = scale]\n")
        
        # The Edges
        for i in range(0, self.numVertices):
            if directed:
                col_range = self.numVertices
            else:
                col_range = i + 1
            for j in range(0, col_range):
               if self.matrix[i][j] > 0:
                   # Account for ALL of the edges from vertex i to vertex j
                   for k in range(0, self.matrix[i][j]):
                       file.write("    \"node " + str(i) + "\"")
                       file.write(edge_op + "\"node " + str(j) + "\"\n")
        
        # End
        file.write("}\n")

    def is_directed(self):
        """
        Determine if the current Graph object is directed.
        It is directed if and only if the adjacency matrix is NOT symmetric.
        """
        
        return self.matrix != self.transpose().matrix

    def transpose(self):
        """
        Tranpose the adjacency matrix. That is, reverse all the arrows
        on the directed graph.
        """

        transposed_matrix  = []
        for j in range(self.numVertices):
            edges = []
            for i in range(self.numVertices):
                edges.append(self.matrix[i][j])
            transposed_matrix.append(edges)
        return Graph(transposed_matrix)
