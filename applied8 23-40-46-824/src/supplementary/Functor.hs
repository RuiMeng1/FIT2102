{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- | Implement instances for the 'Functor' typeclass.
module Functor
    ( Functor
    , (<$>)
    , nestedMap
    )
where

import           Base

-- | All instances of the 'Functor' typeclass must satisfy two laws. These laws
-- are not checked by the compiler. These laws are given as:
--
-- * The law of identity
--
-- > fmap id  ==  id
--
-- * The law of composition
--
-- > fmap (f . g)  ==  fmap f . fmap g
class Functor f where
  -- Pronounced "f map"
  (<$>) :: (a -> b) -> f a -> f b

infixl 4 <$>

-- | Map a function on the 'Id' functor.
--
-- >>> (+1) <$> Id 2
-- Id 3
instance Functor Id where
    (<$>) :: (a -> b) -> Id a -> Id b
    (<$>) = error "fmap id not implemented"

-- | Map a function on the 'Pair' functor.
--
-- >>> (+1) <$> (Pair 5 7)
-- Pair 6 8
--
-- >>> (*2) <$> (Pair 5 7)
-- Pair 10 14
instance Functor Pair where
    (<$>) :: (a -> b) -> Pair a -> Pair b
    (<$>) = error "fmap pair not implemented"

-- | Map a function on the list functor.
--
-- /Hint/: where have we seen this before?
--
-- >>> (+1) <$> []
-- []
--
-- >>> (+1) <$> [1, 2, 3]
-- [2,3,4]
instance Functor [] where
    (<$>) :: (a -> b) -> [a] -> [b]
    (<$>) = error "fmap list not implemented"

-- | Map a function on the 'Maybe' functor.
--
-- >>> (+1) <$> Nothing
-- Nothing
--
-- >>> (+1) <$> Just 2
-- Just 3
instance Functor Maybe where
    (<$>) :: (a -> b) -> Maybe a -> Maybe b
    (<$>) = error "fmap maybe not implemented"

-- | Instance of Functor for a function that takes type r as input
--
-- /Hint/: What other function that operates on functions has a similar type?
--
-- /Hint/: This test case takes 3, evaluates it with (*2) and then passes the result to (+1)
-- >>> ((+1) <$> (*2)) 3
-- 7
instance Functor ((->)r) where
    (<$>) :: (b -> c) -> (r -> b) -> (r -> c)
    (<$>) = error "fmap function not implemented"

-- | Instance of Functor for a tuple that takes type a as input
--
-- /Hint/: Look at the type signature of <$>
--
-- >>> (+1) <$> (1,2)
-- (1,3)
instance Functor ((,) a) where
    (<$>) :: (b -> c) -> (a, b) -> (a, c)
    (<$>) = error "fmap tuple not implemented"

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
nestedMap f = ((f <$>) <$>)

-- | Map a function on each element of a 'RoseTree', where:
--
-- > data RoseTree a = Nil | Node a [RoseTree a]
--
-- thus: a rosetree 'Node' has a value and a list of children rosetrees.
-- You will need to replace `>` with `>>>` to get test cases to run.
--
-- /Hint/: This follows the same pattern as the examples above.  However, you
-- will need to map the 'RoseTree' functor instance over the list of the
-- children (use nestedMap).
--
-- > (+1) <$> Nil
-- Nil
--
-- > (+1) <$> Node 7 [Node 1 [], Node 2 [], Node 3 [Node 4 []]]
-- Node 8 [Node 2 [],Node 3 [],Node 4 [Node 5 []]]
instance Functor RoseTree where
    (<$>) :: (a -> b) -> RoseTree a -> RoseTree b
    (<$>) = error "fmap rosetree not implemented"
