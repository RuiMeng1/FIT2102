{-# OPTIONS_HADDOCK hide #-}
-- | Restricted Prelude
module Base(
  module Data.Monoid,
  module Data.Maybe,
  module Data.Bool,
  module Data.Char,
  module Data.Eq,
  module System.IO,
  RoseTree(..), Id(..), Pair(..),
  Num(..), Int, Show(..), Integer, String, Ord(..), RealFrac,
  flip, foldr, even, undefined, const, id, reverse, foldl, succ,
  last, lines, error, div, replicate, zipWith, elem, notElem, map,
  (.), ($), (++)
) where

import           Prelude     (Int, Integer, Num (..), Ord (..), RealFrac,
                              Show (..), String, const, div, elem, error, even,
                              flip, foldl, foldr, id, last, lines, map, notElem,
                              replicate, reverse, succ, undefined, zipWith, ($),
                              (++), (.))

import           Data.Bool
import           Data.Char
import           Data.Eq
import           Data.Maybe
import           Data.Monoid
import           System.IO

-- | A 'RoseTree' is a tree with an arbitrary number of children per node.
data RoseTree a = Node a [RoseTree a] | Nil
  deriving (Show)

-- | The 'Id' type wraps a value in the identity.
newtype Id a = Id a
  deriving (Show)

-- | A 'Pair' contains two values.
data Pair a = Pair a a
  deriving (Show)
