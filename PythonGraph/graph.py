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
    def __init__(self, adjacency_matrix = [], vertex_names = []):
        """
        Constructor for a Graph object.

        Arguments:
        
        adjacency-matrix -- 
        List of lists of numbers
        The adjacency matrix for this graph.
        If no adjacency matrix is given, it will default to an
        empty matrix.
        
        vertex_names --
        List of strings
        The name of each vertex
        If no list of names is given, it will default to an empty list
        """
        
        # Data Fields
        self.matrix = adjacency_matrix
        self.num_vertices = len(adjacency_matrix)
        self.vertex_names = vertex_names
        
        # Make the sure the adjacency matrix is square.
        for row in self.matrix:
            if len(row) != self.num_vertices:
                raise Exception("Adjacency Matrix must be square!")
            
        # Make sure all the vertices are named.
        if len(self.vertex_names) != self.num_vertices:
            raise Exception("Not all vertices are named!")

    def longest_vertex_name(self):
        """
        Determine the longest vertex name.
        
        Return:
        String
        The longest vertex name.
        """
        
        name = ""
        for vertex in self.vertex_names:
            if len(vertex) > len(name):
                name = vertex
        return name
    
    def pad(self, length):
        """
        Return a string consisting of length space characters
        
        Arguments:
        
        length --
        Number
        The length of the string
        
        Return:
        String
        The string of spaces
        """
        
        string = ""
        for i in range(0, length):
            string += " "
        return string

    def __repr__(self):
        """
        Representation of the graph to use when debugging.
        
        Return:
        String
        The representation.
        """

        space = self.pad(len(self.longest_vertex_name()))

        # Header
        string = space + " "
        for j in range(0, self.num_vertices):
            string += str(self.vertex_names[j])
            if j != self.num_vertices:
                string += " "
        string += "\n"

        for i in range(0, self.num_vertices):
            for j in range(0, self.num_vertices + 1):
                if j == 0:
                    string += self.vertex_names[i]
                else:
                    string += str(self.matrix[i][j - 1])
                if j != self.num_vertices:
                    string += " "
                    if j != 0:
                        string += space
            string += "\n"
            
        string += "num_vertices: " + str(self.num_vertices) + "\n"
        return string


    def __str__(self):
        """
        String version of the graph - i.e. the adjacency matrix
        
        Return:
        String
        The string version
        """

        string = ""
        for i in range(0, self.num_vertices):
            for j in range(0, self.num_vertices):
                string += str(self.matrix[i][j])
                if j != self.num_vertices - 1:
                    string += " "
            string += "\n"
        return string

    def to_dot(self, file_name = "graph.dot", graph_name="graph"):
        """
        Create a dot (i.e. Graphviz) file of the current Graph object.
        
        Arguments:
        
        file_name --
        String
        The name of the file. By convention, should end in '.dot'.
        Defaults to 'graph.dot'

        graph_name --
        String
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
        for i in range(0, self.num_vertices):
            if directed:
                col_range = self.num_vertices
            else:
                col_range = i + 1
            for j in range(0, col_range):
                if self.matrix[i][j] > 0:
                    # Account for ALL of the edges from vertex i to vertex j
                    for k in range(0, self.matrix[i][j]):
                        file.write("    \"" + self.vertex_names[i] + "\"")
                        file.write(edge_op + "\"" + self.vertex_names[j] + "\"\n")
        
        # End
        file.write("}\n")
        file.close()

    def is_directed(self):
        """
        Determine if the current Graph object is directed.
        It is directed if and only if the adjacency matrix is NOT symmetric.
        
        Return:
        Boolean
        True if the graph is directed and false otherwise.
        """
        
        return self.matrix != self.transpose().matrix

    def transpose(self):
        """
        Tranpose the adjacency matrix. That is, reverse all the arrows
        on the directed graph.
        
        Return:
        Graph
        The graph with the transposed adjacency matrix.
        """

        transposed_matrix  = []
        for j in range(self.num_vertices):
            edges = []
            for i in range(self.num_vertices):
                edges.append(self.matrix[i][j])
            transposed_matrix.append(edges)
        return Graph(transposed_matrix, self.vertex_names) # names will still have the same order
    
    def order(self, vertex):
        """
        Finds the order of the vertex, that is how many edges connect to it
        
        Argument:
        
        vertex_name --
        String
        The vertex name
        
        Return:
        Number
        The order
        """
        
        num = self.vertex_names.index(vertex)
        order = 0
        
        for col in range(0, self.num_vertices):
            order += self.matrix[num][col]
            
        return order
    
    def eulerian_path_exists(self, cycle = False):
        """
        Determine if an Eulerian path exists in the graph.
        
        Argument:
        cycle --
        Boolean
        Set to True if you would like to see if an Eulerian cycle exists,
        defaults to False.
        
        Return:
        Boolean
        True if there exists an Eulerian path and false otherwise
        """
        odd = 0
        
        for i in range(0, self.num_vertices):
            if (self.order(self.vertex_names[i]) % 2 != 0):
                odd += 1
        if odd == 0:                    # If all vertices have even order, we have an Eulerian cycle and by extension path
            return True
        elif odd == 2 and cycle == False: # All vertices except 2 have even order => Eulerian path exists
            return True
        else:
            return False
        

    def insert(self, vertex, connections = []):
        """
        If vertex is new, add a new vertex to the graph.
        Add any connections to the graph.

        Arguments:
        
        vertex --
        Number
        The number of the vertex being considered. 
        If it is new, add it to the graph.

        connections --
        List
        The list of vertex numbers corresponding to vertices this
        vertex has an edge to. Defaults to an empty list
        (i.e. if you just want to add a vertex and no edge)
        """
        
        if not (vertex in self.vertex_names):
            self.num_vertices += 1
            self.vertex_names.append(vertex)
            for row in self.matrix:
                row.append(0)                           # Add right col
            self.matrix.append([0] * self.num_vertices) # Add bottom row
            
        length = len(connections)
        
        if length > 0:
            i = self.vertex_names.index(vertex)
            for k in range(0, length):
                j = self.vertex_names.index(connections[k])
                self.matrix[i][j] += 1
