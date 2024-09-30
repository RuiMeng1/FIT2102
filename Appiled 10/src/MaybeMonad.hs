{-# OPTIONS_GHC -Wno-orphans #-}
module MaybeMonad where


import           Maybe   (Maybe (..))
import           Prelude hiding (Just, Maybe, Nothing)
import Control.Applicative (Applicative(liftA2))

-- | The `return` function should wrap a value in a `Just`.
--
-- >>> return 5 :: Maybe Int
-- Just 5
--
-- >>> return "hello" :: Maybe String
-- Just "hello"

-- | The `>>=` function should correctly handle `Nothing`.
--
-- >>> Nothing >>= (\x -> Just (x + 1)) :: Maybe Int
-- Nothing

-- | The `>>=` function should apply the function to `Just` values.
--
-- >>> Just 10 >>= (\x -> Just (x + 1)) :: Maybe Int
-- Just 11
--
-- >>> Just "hello" >>= (\x -> Just (x ++ " world")) :: Maybe String
-- Just "hello world"

-- | The `>>=` function should handle functions that return `Nothing`.
--
-- >>> Just 10 >>= (\x -> Nothing) :: Maybe Int
-- Nothing

-- | The `>>=` function should correctly chain operations.
--
-- >>> Just 5 >>= (\x -> Just (x * 2)) >>= (\y -> Just (y + 1)) :: Maybe Int
-- Just 11

instance Monad Maybe where
  return :: a -> Maybe a
  return = Just 

  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  (Just x) >>= k = k x
  Nothing >>= k = Nothing


-- | The `calculateResult` function adds two `Maybe Int` values.
--   If either value is `Nothing`, the result is `Nothing`.
--   Otherwise, it returns the sum wrapped in a `Just`.
--
-- You must use `case` syntax to handle this function.
-- Examples:
--
-- >>> calculateResult (Just 5) (Just 10)
-- Just 15
--
-- >>> calculateResult Nothing (Just 10)
-- Nothing
--
-- >>> calculateResult (Just 5) Nothing
-- Nothing
--
-- >>> calculateResult Nothing Nothing
-- Nothing
--
-- >>> calculateResult (Just 7) (Just (-3))
-- Just 4
calculateResult :: Maybe Int -> Maybe Int -> Maybe Int
calculateResult = undefined


-- | This function is the same as previous, but uses the `>>=` (bind) operator to handle the unwrapping of `Maybe` values
-- and the potential short-circuiting when encountering a `Nothing`.
--
-- Examples:
--
-- >>> calculateResult' (Just 5) (Just 10)
-- Just 15
--
-- >>> calculateResult' Nothing (Just 10)
-- Nothing
--
-- >>> calculateResult' (Just 5) Nothing
-- Nothing
--
-- >>> calculateResult' Nothing Nothing
-- Nothing
--
-- >>> calculateResult' (Just 7) (Just (-3))
-- Just 4
calculateResult' :: Maybe Int -> Maybe Int -> Maybe Int
calculateResult' = undefined

-- | This function is the same as previous, but use do notation to handle the unwrapping of `Maybe` values
-- and the potential short-circuiting when encountering a `Nothing`.
--
-- Examples:
--
-- >>> calculateResult'' (Just 5) (Just 10)
-- Just 15
--
-- >>> calculateResult'' Nothing (Just 10)
-- Nothing
--
-- >>> calculateResult'' (Just 5) Nothing
-- Nothing
--
-- >>> calculateResult'' Nothing Nothing
-- Nothing
--
-- >>> calculateResult'' (Just 7) (Just (-3))
-- Just 4

calculateResult''  :: Maybe Int -> Maybe Int -> Maybe Int
calculateResult'' = undefined
