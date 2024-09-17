{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- | Implement instances for the 'Applicative' typeclass.
module Applicative
    ( Applicative
    , pure
    , (<*>)
    )
where

import           Base
import           Functor

class Functor f => Applicative f where
  pure :: a -> f a
  -- Pronounced "apply"
  (<*>) :: f (a -> b) -> f a -> f b

infixl 4 <*>

-- | Insert into Id.
--
-- >>> Id (+10) <*> Id 8
-- Id 18
instance Applicative Id where
    pure :: a -> Id a
    pure = error "pure id not implemented"

    (<*>) :: Id (a -> b) -> Id a -> Id b
    (<*>) = error "apply id not implemented"

-- | Apply to a Maybe, must return `Nothing` if either the function or the
-- element is a `Nothing`.
--
-- >>> Just (+8) <*> Just 7
-- Just 15
--
-- >>> Nothing <*> Just 7
-- Nothing
--
-- >>> Just (+8) <*> Nothing
-- Nothing
instance Applicative Maybe where
    pure :: a -> Maybe a
    pure = error "pure maybe not implemented"

    (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    (<*>) = error "apply maybe not implemented"

-- | Apply a list of functions over a list of elements, producing the
-- concatenation of the successive results.
--
-- This evaluates [(1+1), (2+1), (3+1), (1*10), (2*10), (3*10)]
-- >>> [(+1), (*10)] <*> [1, 2, 3]
-- [2,3,4,10,20,30]
instance Applicative [] where
    pure :: a -> [a]
    pure = error "pure list not implemented"

    -- /Hint/: You can implement this using
    --      - list comprehension, or
    --      - foldr and map, or
    --      - map and recursion
    (<*>) :: [a -> b] -> [a] -> [b]
    (<*>) = error "apply list not implemented"

-- | The 'Applicative' class derives from 'Functor', rewrite `fmap` using only
-- `pure` and `<*>`.
--
-- Be cautious when using this `fmap` when defining your own `<*>`s - as this
-- calls `<*>`, you may get an infinite loop!
--
-- >>> (+1) `fmap` (Id 2)
-- Id 3
--
-- >>> (+1) `fmap` Nothing
-- Nothing
--
-- >>> (+1) `fmap` [1, 2, 3]
-- [2,3,4]
fmap :: Applicative f => (a -> b) -> f a -> f b
fmap = error "fmap not implemented"

{-
    ******************** Supplementary **************************8
-}

-- | Applicative instance of a function that takes an r.
--
-- /Hint/: `pure x` creates a function that always return x, no matter what it's given.
--
-- >>> f = pure "hello"
-- >>> f 95
-- "hello"
--
-- /Hint/: READ THROUGH THIS CAREFULLY, MULTIPLE TIMES:
-- `(<*>)` takes a function which produces a function for a given r,
--           and a function which produces a value for a given r,
--           and gives a function that applies:
--               the function produced by the first argument,
--               to the value produced by the second argument.
--
-- /Hint/: If you're really stuck, this stack overflow post might help as well
-- https://stackoverflow.com/questions/11810889/functions-as-applicative-functors-haskell-lyah
--
-- This test case calculates (2*) (2+1)
-- >>> f = (*) <*> (+1)
-- >>> f 2
-- 6
instance Applicative ((->) r) where
    pure :: a -> (r -> a)
    -- pure :: a -> r -> a
    pure = error "pure function not implemented"

    (<*>) :: (r -> (a -> b)) -> (r -> a) -> (r -> b)
    -- (<*>) :: (r -> (a -> b)) -> (r -> a) -> r -> b
    (<*>) = error "apply function not implemented"

-- | Takes an of functions in an Applicative context, and a value in the same context
--   and applies the functions to the value
--
-- >>> nestedApply [Just (+1), Just (*2)] (Just 2)
-- [Just 3,Just 4]
--
-- >>> nestedApply [Id (++" hello"),Id (++" world!")] (Id "hello")
-- [Id "hello hello",Id "hello world!"]
nestedApply :: (Applicative f, Applicative g) => g (f (a -> b)) -> f a -> g (f b)
nestedApply = error "nestedApply not implemented"

-- | Apply to a RoseTree, i.e. return a tree composed of trees created by the
-- successive application of functions to initial nodes.
--
-- /Hint/: complete 'nestedMap' and the 'Functor' instance for 'RoseTree' (in 'Functor.hs')
-- first.
-- You will need to replace `>` with `>>>` to get test cases to run.
--
-- /Hint/: study the tests below closely...
-- You will see that for:
--   lhs <*> rhs
-- we create a new rose tree whose root is the
-- application of the function at the root of lhs to the root of rhs,
-- then its children are the mapping of the root function, mapped over the children of rhs,
-- and concatenated with the application of the remaining functions in lhs to rhs.
--
-- /Hint/: use 'nestedMap' and 'nestedApply'
--
-- >> (Node (+1) [Node (*2) []]) <*> Nil
-- Nil
--
-- > Nil <*> (Node 7 [Node 1 [], Node 2 [], Node 3 [Node 4 []]])
-- Nil
--
-- >> (Node (+1) []) <*> (Node 7 [Node 1 [], Node 2 [], Node 3 [Node 4 []]])
-- Node 8 [Node 2 [],Node 3 [],Node 4 [Node 5 []]]
--
-- >> (Node (+1) [Node (*2) []]) <*> (Node 5 [Node 2 [], Node 8 [Node 1 []]])
-- Node 6 [Node 3 [],Node 9 [Node 2 []],Node 10 [Node 4 [],Node 16 [Node 2 []]]]
instance Applicative RoseTree where
    pure :: a -> RoseTree a
    pure = error "pure rosetree not implemented"

    (<*>) :: RoseTree (a -> b) -> RoseTree a -> RoseTree b
    (<*>) = error "apply rosetree not implemented"
