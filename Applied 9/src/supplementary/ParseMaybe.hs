module ParseMaybe () where

import           Instances (Parser (..), int, parse)

-- | Parse a Nothing value.
--
-- Parse the string "Nothing", if succeeds return Nothing.
--
-- >>> parse nothing "something"
-- Nothing
-- >>> parse nothing "Nothing"
-- Just ("",Nothing)
nothing :: Parser (Maybe a)
nothing = undefined

-- | Parse a Just value.
--
-- Parse the string "Just ", followed by the given parser.
--
-- /Hint/: The parsed value should be wrapped with a Just
--
-- >>> parse (just int) "Just 1"
-- Just ("",Just 1)
-- >>> parse (just int) "Nothing"
-- Nothing
just :: Parser a -> Parser (Maybe a)
just p = undefined

-- just p = string "Just " *> (pure <$> p)

-- | Parse a 'Maybe'
--
-- This is named parseMaybe due to a nameclash with Prelude.maybe
--
-- /Hint/: What does (<|>) do?
--
-- /Hint 2/: Maybe types can only be just or nothing
--
-- >>> parse (parseMaybe int) "Just 1"
-- Just ("",Just 1)
--
-- >>> parse (parseMaybe int) "Nothing"
-- Just ("",Nothing)
--
-- >>> parse (parseMaybe int) "Something Else"
-- Nothing
parseMaybe :: Parser a -> Parser (Maybe a)
parseMaybe = undefined
