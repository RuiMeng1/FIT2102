-- |
-- Some small functions that highlight useful functions in the Prelude.
--
-- Make sure to to use Hoogle https://hoogle.haskell.org/ to find functions!
--
-- None of these functions should use explicit recursion. Try to combine
-- useful small functions together.
--
-- REFER TO THE README FOR SOME USEFUL INFORMATION.
--
-- You will have to write the types for some of the functions. Polymorphism must
-- be used when possible instead of fixed types.
-- See https://tgdwyer.github.io/haskell2/#type-parameters-and-polymorphism
--
-- These functions are intended to leverage useful comparison typeclasses,
--  see https://tgdwyer.github.io/haskell2/#typeclasses
--  see https://tgdwyer.github.io/haskell3/#functor
--  see https://tgdwyer.github.io/haskell3/#applicative
module ApplicativeFunctions () where

import           Prelude hiding ((<*))

-- | Takes an unary function and applies it to one element wrapped in a context.
--
-- >>> liftA (+1) (Just 7)
liftA :: Applicative f => (a -> b) -> f a -> f b
liftA = fmap

-- | Takes a binary function and applies it to two elements wrapped in a context.
--
-- >>> liftA2 (+) [1, 2, 3] [4, 5]
-- [5,6,6,7,7,8]
--
-- >>> liftA2 (+) (Just 7) (Just 8)
-- Just 15
--
-- >>> liftA2 (+) (Just 7) Nothing
-- Nothing
--
-- >>> liftA2 (+) Nothing (Just 8)
-- Nothing
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = fmap f a  <*> b

-- | Implement a version of the applicative that combines the effects of both sides, but retains only the value of the left-hand side
--
-- The "effect" of a Maybe is to either succeed with a Just or fail with a Nothing.
-- Both sides succeed, return the left value:
-- >>> Just 5 <* Just 2
-- Just 5
--
-- Left side fails so the effect is Nothing
-- >>> Nothing <* Just 2
-- Nothing
--
-- right side fails so the effect is Nothing
-- >>> Just 2 <* Nothing
-- Nothing
--
-- The "effect" of the list applicative is to apply all functions in the list on the left to all values in the list on the right.
-- The <* operator creates a function for each value on the left which just returns that value, regardless of what it is applied to,
-- and applies that function to each value in the list on the right.
-- Therefore, <* gives us the list on the left, but with a multiplicity of the cartesian product of the two lists:
-- >>> [4,5] <* [1,2,3]
-- [4,4,4,5,5,5]
--
-- /Hint/: you'll need to lift a binary function that returns the first argument regardless of its second argument into the applicative context
(<*) :: Applicative f => f a -> f b -> f a
(<*) = liftA2 const 
