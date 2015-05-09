module Test where

import Test.HUnit
import BinarySearchTree

tree :: Tree Int
tree = insertMultiple None [1, 2, 34, 5, 1729, 20, 19, 3]

tests :: Test
tests = test [
    "testInOrder" ~: "" ~: [1, 2, 3, 5, 19, 20, 34, 1729] ~=? (inOrder tree),
    "testPreOrder" ~: "" ~: [1, 2, 34, 5, 3, 20, 19, 1729] ~=? (preOrder tree),
    "testPostOrder" ~: "" ~: [3, 19, 20, 5, 1729, 34, 2, 1] ~=? (postOrder tree),
    "testSearch1" ~: "" ~: True ~=? (search tree 20),
    "testSearch2" ~: "" ~: False ~=? (search tree 42)]
   
-- run "runhaskell Test.hs" from the terminal prompt 
main :: IO ()
main = do
        runTestTT tests
        return ()
