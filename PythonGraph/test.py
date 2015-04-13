# Ryan Forsyth
# 04/10/2015

import filecmp
import unittest
from graph import Graph

class TestGraph(unittest.TestCase):
    """
    Tests of graph.py
    """

    def test_basic(self):
        """
        Test the Graph constructor, __str__, and is_directed
        """

        g = Graph([[1,2],[3,4]], ["1", "2"])
        self.assertEqual(str(g), "1 2\n3 4\n")
        self.assertTrue(g.is_directed)
        
        g.insert("3", ["2"]) # Insert a third node and connect it to node 2
        self.assertEqual(str(g), "1 2 0\n3 4 0\n0 1 0\n")


    def test_seven_bridges(self):
        """
        Test to_dot, eulerian_path_exists
        """

        g = Graph([[0, 2, 1, 2], [2, 0, 1, 0], [1, 1, 0, 1], [2, 0, 1, 0]], ["node 0", "node 1", "node 2", "node 3"])
        g.to_dot("test.dot")
        self.assertTrue(filecmp.cmp("test.dot", "seven_bridges.dot"))
        self.assertFalse(g.eulerian_path_exists())      # Path
        self.assertFalse(g.eulerian_path_exists(True))  # Cycle


if __name__ == '__main__':
    unittest.main()
