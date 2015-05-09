"""
Binary Search Tree module

Ryan Forsyth
05/09/2015

"""

class Node():
    """Represents a node in the tree."""
    
    def __init__(self, data=None):
        """
        Initialize the node
        
        Keyword Arguments:
        data -- The element stored in the node (default None)
        """
    
        self.data = data
        self.left = None
        self.right = None
            
    def __repr__(self):
        """Representation of the node."""
        return repr(self.data)

    def __str__(self):
        """Return a string representation of the node."""
        return str(self.data)


class BinarySearchTree():
    """Binary Search Tree class"""
        
    def __init__(self):
        """Initialize the tree with an empty root node."""
        self.root = None
        
    def __repr__(self):
        """Reprsentation of the tree."""
        return str(self.in_order())
        
    def __str__(self):
        """Return a string representation of the tree by in-order traversal."""
        return str(self.in_order())
    
    def insert(self, data):
        """
        Insert an element into the tree.
        
        Arguments:
        data -- The element to be inserted
        """
        
        new_node = Node(data)
        
        previous = None
        current = self.root
        
        while current != None:
            previous = current
            if data < current.data:
                current = current.left
            else:
                current = current.right
        
        if previous != None:
            if data < previous.data:
                previous.left = new_node
            else:
                previous.right = new_node
        else:
            self.root = new_node
            
    def insert_multiple(self, data_list):
        """
        Insert multiple elements into the tree
        
        Arguments:
        data_list -- The list of elements to add
        """
        
        for element in data_list:
            self.insert(element)
            
    def in_order(self):
        """Return an in-order traversal of the tree."""
        def helper(node):
            if node == None:
                return []
            else:
                return helper(node.left) + [node.data] + helper(node.right)
        return helper(self.root)
    
    def pre_order(self):
        """Return a pre-order traversal of the tree."""
        def helper(node):
            if node == None:
                return []
            else:
                return [node.data] + helper(node.left) + helper(node.right)
        return helper(self.root)
    
    def post_order(self):
        """Return an post-order traversal of the tree."""
        def helper(node):
            if node == None:
                return []
            else:
                return helper(node.left) + helper(node.right) + [node.data]
        return helper(self.root)
    
    def search(self, data):
        """
        Determine if an element is in the tree.
        
        Arguments:
        data -- The element being searched for
        """
        def helper(node):
            if node == None:
                return False
            elif data == node.data:
                return True
            elif data < node.data:
                return helper(node.left)
            else:
                return helper(node.right)
        return helper(self.root)
