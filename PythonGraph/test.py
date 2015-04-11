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

        g = Graph([[1,2],[3,4]])
        self.assertEquals(str(g), "1 2\n3 4\n")
        self.assertTrue(g.is_directed)


    def test_to_dot(self):
        """
        Test to_dot method
        """

        g = Graph([[0, 2, 1, 2], [2, 0, 1, 0], [1, 1, 0, 1], [2, 0, 1, 0]])
        g.to_dot("test.dot")
        self.assertTrue(filecmp.cmp("test.dot", "seven_bridges.dot"))


if __name__ == '__main__':
    unittest.main()
