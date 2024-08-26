-- | Implementation of integer valued binary trees.
--
-- The focus of these exercises is to get comfortable with a more
-- complex custom type, and using recursion.
module RoseTree () where


-- See https://tgdwyer.github.io/haskell2/#algebraic-data-types
-- | A 'RoseTree' is a tree with an arbitrary number of children per node.
data RoseTree a = Node a [RoseTree a] | Nil
  deriving(Show)

-- Example Tree
tree :: RoseTree Int
tree = Node 1 [Node 1 [], Nil, Node 2 [Node 5 []]]

one :: RoseTree Int
one = Node 1 []

-- | Find the number of nodes in a tree.
--
-- See https://tgdwyer.github.io/haskell2/#pattern-matching
--
-- >>> size Nil
-- 0
--
-- >>> size one
-- 1
--
-- >>> size tree
-- 4
size :: RoseTree a -> Int
size = undefined

-- | Find the depth of a tree (number of levels)
--
-- See https://tgdwyer.github.io/haskell2/#pattern-matching
--
-- >>> depth Leaf
-- 0
--
-- >>> depth (Node 1 Leaf Leaf)
-- 1
--
-- >>> depth tree
-- 3
depth :: RoseTree a -> Int
depth = undefined

-- | Map a function over a tree.
--
-- >>> mapTree (+1) Nil
-- Nil
--
-- >>> mapTree (*1) one
-- Node 1 Leaf Leaf
--
-- >>> mapTree (`mod` 2) tree
-- Node 1 [Node 1 [],Nil,Node 0 [Node 1 []]]
mapTree :: (a -> b) -> RoseTree a -> RoseTree b
mapTree = undefined
