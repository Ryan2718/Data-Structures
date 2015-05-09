"""
Tests of the binary_search_tree module.

Ryan Forsyth
05/09/2015
"""

from binary_search_tree import BinarySearchTree as bst
import unittest

class Tester(unittest.TestCase):
    def test_comprehensive(self):
        """Comprehensive Test"""
        t = bst()
        data_list = [1, 2, 34, 5, 1729, 20, 19, 3]
        t.insert_multiple(data_list)
        
        expected = [1, 2, 3, 5, 19, 20, 34, 1729]
        result = t.in_order()
        self.assertEqual(expected, result)
        
        expected = [1, 2, 34, 5, 3, 20, 19, 1729]
        result = t.pre_order()
        self.assertEqual(expected, result)
        
        expected = [3, 19, 20, 5, 1729, 34, 2, 1]
        result = t.post_order()
        self.assertEqual(expected, result)
        
        self.assertTrue(t.search(20))
        self.assertFalse(t.search(42))
    
if __name__ == '__main__':
    unittest.main()
