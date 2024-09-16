module Examples (nestedMap) where

-- |
-- Some small functions that highlight useful functions in the Prelude.
--
-- Make sure to to use Hoogle https://hoogle.haskell.org/ to find functions!
--
-- None of these functions should use explicit recursion. Try to combine
-- useful small functions together.
--
import           Data.Function (on)
import           Data.List     (groupBy, sortOn)

-- \$setup

-- | Sort each sublist by first value in tuple
--
-- >>> sortList [[(1,"a"),(3, "b"),(2, "d")], [(5, "a"),(6, "b")]]
-- [[(1,"a"),(2,"d"),(3,"b")],[(5,"a"),(6,"b")]]
-- >>> sortList [[(Just 1,1),(Nothing, 3),(Just 5, 5)], [(Just 6, 0),(Nothing, 1)]]
-- [[(Nothing,3),(Just 1,1),(Just 5,5)],[(Nothing,1),(Just 6,0)]]
sortList :: Ord a => [[(a, b)]] -> [[(a, b)]]
sortList = undefined

-- | Group consecutive elements which are equal after applying function
--
-- >>> groupEqual id ['a', 'a', 'b', 'b', 'b', 'b', 'c','c','c','d','d','e','e']
-- ["aa","bbbb","ccc","dd","ee"]
-- >>> groupEqual (`mod` 5) [5,5,5,5,10,10,10,15,15,15]
-- [[5,5,5,5,10,10,10,15,15,15]]
--
-- /Hint/: Lookup `groupBy` which we have imported from Data.List
groupEqual :: Eq b => (a -> b) -> [a] -> [[a]]
groupEqual f = undefined

-- | Map a function over a Functor of Functors
--
-- /Hint/: maybe we need 2 fmaps
--
-- >>> nestedMap (+1) [[1, 2, 3], [4, 5, 6]]
-- [[2,3,4],[5,6,7]]
--
-- >>> nestedMap (+1) [Just 1, Just 2]
-- [Just 2,Just 3]
--
-- >>> nestedMap (+1) [Just 1, Nothing]
-- [Just 2,Nothing]
--
-- >>> nestedMap (+1) (Just (Just 5))
-- Just (Just 6)
--
-- >>> nestedMap (+1) (Just Nothing)
-- Just Nothing
nestedMap :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
nestedMap = undefined
