{-|
Module      : BinarySearchTree

Ryan Forsyth

May 8, 2015
-}

module BinarySearchTree (
    Tree(None),
    inOrder,
    preOrder,
    postOrder,
    insert,
    insertMultiple,
    search,
) where


-- Constructors

data Tree a = None | Node a (Tree a) (Tree a)


-- Instances

instance (Eq a) => Eq (Tree a) where
    None == None = True
    Node n left right == Node n' left' right' = (n == n') &&
                                                (left == left') &&
                                                (right == right')
    _ == _ = False

instance (Show a) => Show (Tree a) where
    show = show . inOrder

instance Functor Tree where
    fmap f None = None
    fmap f (Node n left right) = Node (f n) (fmap f left) (fmap f right)


-- Tree Traversals
    
-- | The in-order traversal of the tree
inOrder :: Tree a -> [a]
inOrder None = []
inOrder (Node n left right) = (inOrder left) ++ [n] ++ (inOrder right)

-- | The pre-order traversal of the tree
preOrder :: Tree a -> [a]
preOrder None = []
preOrder (Node n left right) = [n] ++ (preOrder left) ++ (preOrder right)

-- | The post-order traversal of the tree
postOrder :: Tree a -> [a]
postOrder None = []
postOrder (Node n left right) = (postOrder left) ++ (postOrder right) ++ [n]


-- Insertion

-- | Insert an element into the tree
insert :: (Ord a) => Tree a ->  a -> Tree a
insert None element = Node element None None
insert (Node n left right) element =
    if (element < n)
    then Node n (insert left element) right
    else Node n left (insert right element)

-- | Insert multiple elements into the tree
insertMultiple :: (Ord a) => Tree a -> [a] -> Tree a
insertMultiple tree elements = foldl insert tree elements


-- Searching

-- | Search for an item in the tree
search :: (Ord a) => Tree a -> a -> Bool
search None _ = False
search (Node n left right) element
   | element < n  = search left element
   | element > n  = search right element
   | element == n = True
   | otherwise = error "Impossible"
    
