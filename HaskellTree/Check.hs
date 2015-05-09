module Check where

import Data.List
import Test.QuickCheck
import BinarySearchTree

-- The in-order traversal of a binary search tree should always be a sorted list
prop_sorted :: [Int] -> Bool
prop_sorted list =
    let tree = insertMultiple None list in
    inOrder tree == sort list

-- run "runhaskell Check.hs" from terminal prompt
main :: IO ()
main = do
        quickCheck prop_sorted
