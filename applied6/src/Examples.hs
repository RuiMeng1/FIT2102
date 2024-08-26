-- |
-- Some small functions that highlight useful functions in the Prelude.
--
-- Make sure to to use Hoogle https://hoogle.haskell.org/ to find functions!
--
-- None of these functions should use explicit recursion. Try to combine
-- useful small functions together.
--
-- You will have to write the types for some of the functions. Polymorphism must
-- be used when possible instead of fixed types.
--
-- Polymorphism is one of the strong suits of Haskell, allowing users to write a single function
-- that will work on multiple types instead of having to define one per possible
-- type.
--
-- By convention, a polymorphic type is called `a`, but any (lower case)
-- letter would work.
module Examples () where
import Control.Arrow (Arrow(first))

-- \$setup

-- | Calculate eulerProblem1
--
-- /Hint/: Useful functions: sum and filter
--
-- >>> eulerProblem1 1000
-- 233168
eulerProblem1 :: Int -> Int
eulerProblem1 n = sum $ filter div3or5 [1..n]
    where
        div3or5 num = (num `mod` 3 == 0) || (num `mod` 5 == 0)

-- | Function to check if every element in a list is even
--
-- >>> allEvens [1,2,3,4,5]
-- False
-- >>> allEvens [2,4]
-- True
allEvens :: [Int] -> Bool
allEvens arr = filter odd arr == []
        
-- | Function to check if any element is odd
--
-- >>> anyOdd [1,2,3,4,5]
-- True
-- >>> anyOdd [0,0,0,4]
-- False
anyOdd :: [Int] -> Bool
anyOdd arr = [] /= filter odd arr

-- | Function to sum every element in two lists
--
-- >>> sumTwoLists [1,2,3,4,5] [1,2,3,4,5]
-- [2,4,6,8,10]
sumTwoLists :: [Integer] -> [Integer] -> [Integer]
sumTwoLists [] [] = []
sumTwoLists (x:xs) (y:ys) = (x + y) : sumTwoLists xs ys

-- | Function to get first item in a list of tuples
--
-- See https://tgdwyer.github.io/haskell2/#type-parameters-and-polymorphism
--
-- >>> firstItems [(2,1), (4,3), (6,5)]
-- [2,4,6]

firstItems :: [(a, b)] -> [a]
firstItems = map fst

-- | Apply function to every element in a nested list
--
-- /Hint/: With a normal map, we want to apply a function to every element
--  in a list. With a nested map, we want to apply a *mapping* functions to
--  every element in a list.
--
-- >>> nestedMap (+1) [[1,2,3], [4,5,6], [7,8,9]]
-- [[2,3,4],[5,6,7],[8,9,10]]

nestedMap :: (a -> b) -> [[a]] -> [[b]]
nestedMap = map.map

-- | Apply function to every element in a nested list
-- | and flatten the result
--
-- /Hint/: Y29uY2F0TWFwIGlzIGEgdmVyeSBjb29sIGZ1bmN0aW9u
--
-- >>> nestedConcatMap (+1) [[1,2,3], [4,5,6], [7,8,9]]
-- [2,3,4,5,6,7,8,9,10]
nestedConcatMap :: (a -> b) -> [[a]] -> [b]
nestedConcatMap f = concatMap (map f)