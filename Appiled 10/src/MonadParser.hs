{-# OPTIONS_GHC -Wno-orphans #-}
module MonadParser where

import           Control.Applicative
import           Control.Monad
import           Instances
import           JSON
import           Parser

-- $setup
-- >>> let add = \n -> \m -> Parser (\x -> Just(x, (n+m)))


-- | Monadic Parser
--
-- >>> parse (pure 1 >>= add 2) ""
-- Just ("",3)
--
-- prop> \i j -> parse (pure i >>= add j) "" == Just ("",(i + j))
instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) (Parser p) f = undefined

-- | Parse a C++ style array with a fixed length.
-- Can you do this with applicative? Or do we need to use Monads...
-- You can assume the type will always be integers
-- /Hint/ Use a do block!
-- >>> parse parseListLength "std::array<int, 3> var = {1, 2, 3}"
-- Just ("",[1,2,3])
-- >>> parse parseListLength "std::array<int, 2> list = {4, 5}"
-- Just ("",[4,5])
-- >>> parse parseListLength "std::array<int, 5> anotherList = {7, 8, 9, 10}"
-- Nothing
parseListLength :: Parser [Int]
parseListLength = undefined
