{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- | Some practice on defining typeclasses
--
-- Complete the typeclass instance definitions below.
--
-- see https://tgdwyer.github.io/haskell2/#creating-custom-instances-of-type-classes
--
-- Remember to use polymorphism.
-- see https://tgdwyer.github.io/haskell2/#type-parameters-and-polymorphism
module Maybe () where

import           Control.Applicative (Alternative, empty, (<|>))
import           Prelude             hiding (Just, Maybe, Nothing)

data Maybe a = Just a | Nothing
  deriving (Show)

-- | Justs are equal if both values are equal
-- | Nothings are equal
--
-- see https://tgdwyer.github.io/haskell2/#typeclasses
--
-- >>> Just 2 == Just 2
-- >>> Nothing == Nothing
-- >>> Nothing == Just 1
-- True
-- True
-- False
instance Eq a => Eq (Maybe a) where
  (==) :: Maybe a -> Maybe a -> Bool
  Just x == Just y = x == y
  Nothing == Nothing = True
  _ == _ = False



-- |
-- Just are ordered by comparing the value if Just
-- Justs are greater than nothing
-- see https://tgdwyer.github.io/haskell2/#typeclasses
--
-- >>> Just 3 > Just 2
-- True
--
-- >>> Just 3 < Just 4
-- True
--
-- >>> Just 3 > Nothing
-- True
--
-- >>> Just 3 >= Just 3
-- True
instance Ord a => Ord (Maybe a) where
  compare :: Maybe a -> Maybe a -> Ordering
  compare Nothing Nothing = EQ
  compare Nothing _ = LT
  compare _ Nothing = GT
  compare (Just x) (Just y) = compare x y
-- |
-- Mapping over a Maybe applies the function if it exists
--
-- see https://tgdwyer.github.io/haskell3/#functor
--
-- >>> fmap (+1) (Just 1)
-- Just 2
-- >>> (+1) <$> Nothing
-- Nothing
instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just $ f x

-- |
-- see https://tgdwyer.github.io/haskell3/#applicative
--
-- >>> pure 1 :: Maybe Int
-- Just 1
--
-- >>> Just (*2) <*> Just 2
-- Just 4
--
-- >>> (+) <$> Just 1 <*> Just 5
-- Just 6

instance Applicative Maybe where
  pure :: a -> Maybe a
  pure = Just

  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  (<*>) (Just a) (Just b) = Just (a b)
  (<*>) Nothing Nothing = Nothing
  (<*>) (Just a) Nothing = Nothing

-- |
-- see https://tgdwyer.github.io/haskell3/#alternative
--
-- >>> empty :: Maybe Int
-- Nothing
--
-- >>> Just 1 <|> Just 2
-- Just 1
--
-- >>> Just 2 <|> Nothing
-- Just 2
--
-- >>> Nothing <|> Just 2
-- Just 2
instance Alternative Maybe where
  empty :: Maybe a
  empty = Nothing

  (<|>) :: Maybe a -> Maybe a -> Maybe a
  (<|>) (Just a) _ = Just a  
  (<|>) Nothing b = b        
  
