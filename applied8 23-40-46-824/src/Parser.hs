{-# LANGUAGE InstanceSigs #-}

-- |
--  Previously we have seen parsing functions that look like this:
--   String -> Maybe (String, a)
--  but we don't have a good way of composing these functions together, or
--  operating on the parsed values without manually extracting them.
--
--  We will now use the typeclasses Functor and Applicative to make this easier.
--
--  see https://tgdwyer.github.io/haskell3/#a-simple-applicative-functor-for-parsing
module Parser
  ( Parser (..),
    parse,
    is,
    char,
    int,
  )
where

-- You may add more imports as you wish/need
import           Control.Applicative (Alternative (empty, many, (<|>), some),
                                      Applicative (liftA2))

-- $setup
-- import Control.Applicative
-- import Data.Char

-- | This is our Parser which holds a parsing function.
--
--   The function returns
--      - Nothing, if the parsing fails
--      - Just (r, p), where r is the unparsed portion of the input,
--        and p is the parsed input
newtype Parser a = Parser (String -> Maybe (String, a))

-- | Wrapper function for parsing.
--
-- This just extracts the function from a Parser value.
--
-- >>> f s = Just ("", s)
-- >>> parse (Parser f) "abc"
-- Just ("","abc")
parse :: Parser a -> (String -> Maybe (String, a))
parse (Parser p)  = p 

-- | Parse a single character
--
-- >>> parse char "abc"
-- Just ("bc",'a')
--
-- >>> parse char ""
-- Nothing
char :: Parser Char
char = Parser f
  where
    f ""       = empty
    f (x : xs) = pure (xs, x)

-- | Parse numbers as int until non-digit
--
-- >>> parse int "123abc"
-- Just ("abc",123)
--
-- >>> parse int "abc"
-- Nothing
int :: Parser Int
int = Parser f
  where
    f s = case reads s of
      [(x, rest)] -> Just (rest, x)
      _           -> Nothing


-- | Parses a specific character, otherwise return Nothing
--
-- >>> parse (is 'c') "cba"
-- Just ("ba",'c')
-- >>> parse (is 'c') "abc"
-- Nothing
is :: Char -> Parser Char
is c = Parser f
  where
    f "" = Nothing
    f (x:xs)
      | x == c = pure (xs,x)
      | otherwise = Nothing

-- | Parses not a specific character, otherwise return Nothing
--
-- >>> parse (isNot 'c') "cba"
-- Nothing
-- >>> parse (isNot 'c') "abc"
-- Just ("bc",'a')
isNot :: Char -> Parser Char
isNot c = Parser f
  where
    f "" = Nothing
    f (x:xs)
      | x == c = Nothing
      | otherwise = pure (xs, x)


-- | Applies the mapping function to the *result* (parsed value) of the parser.
--
-- see https://tgdwyer.github.io/haskell3/#functor
--
--  value of the parsing function Maybe (String, a)
--
-- /Hint 2/: You will have to manually construct a parsing function and parser.
--
-- /Hint 3/: Think about each step carefully
--
--   The returned parser should do the following:
--
--   - Parser with a function that takes in some input string
--
--   - Executes the input parser on the input string, then
--
--     - If the parse is successful (Just value)
--
--       - Take the parsed value and remainder
--
--         - Return a Just value with the remainder, and
--         - the mapping function applied to the parsed value
--
--     - If the parse is unsuccessful, return Nothing
--
--
-- /Challenge/: Write this using fmaps
--
--   There are 3 layers of functors here:
--    - the function ((->) String)
--    - The Maybe
--    - The tuple (String, a)
--
--   /Hint/: Can we make use of the functor instances for these types?
--    What does fmap do for each of these types?
--
--   /Hint 2/: Using fmap =>  three layers of functors, three fmaps?
--
-- /End Challenge/:
--
-- >>> import Data.Char (toUpper)
-- >>> parse (toUpper <$> char) "abc"
-- Just ("bc",'A')
-- >>> parse (fmap toUpper char) "abc"

-- >>> char "abc"
--- Just ("bc", 'a')

-- data Parser a = Parser (String -> Maybe (String, a))

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ 
    \input -> 
      case  p input  of
           Nothing -> Nothing 
           (Just (str, a)) -> Just (str, f a)

-- |
--
-- Similar to Functor, we want to apply the function to the parsed value.
--
-- The "effect" of the Parser type is in two parts:
--
-- 1. If the Parser fails at any point, that is the parsing function returns
--    a Nothing value, then all future parsing functions should also return
--    a Nothing value.
--
-- 2. The remaining input after parsing should be passed as input to the
--    next parsing function.
--
-- see https://tgdwyer.github.io/haskell3/#applicative
--
-- >>> parse (pure 1) "abc"
-- Just ("abc",1)
--
-- >>> parse (pure (+1) <*> int) "123abc"
-- Just ("abc",124)
--
-- >>> parse (is '(' *> is 'a') "(a"
-- Just ("",'a')
--
-- >>> parse (liftA2 (+) int (is '+' *> int)) "1+2"
-- Just ("",3)
instance Applicative Parser where
  -- Returns a parser that always succeeds with the given value,
  -- ignoring the input.
  pure :: a -> Parser a
  pure a = Parser $ \x -> Just (x, a)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  p1@(Parser a) <*> p2@(Parser b) = Parser $ 
    \input ->
      case a input of
        Nothing -> Nothing
        Just (str, f) -> parse (f <$> p2) str

-- |
--
-- 'Alternative' is used for parsers that support choice (alternative) and
-- failure (empty). In the context of this 'Parser' type:
--
-- 1. 'empty' represents a parser that always fails
--
-- 2. '<|>' combines two parsers. It tries the first parser on the input,
-- and if it fails it tries the second parser.
--
-- see https://tgdwyer.github.io/haskell3/#alternative
--
-- >>> parse (int <|> pure 0) "123abc"
-- Just ("abc",123)
--
-- >>> parse (is 'h' <|> is 'w') "world, hello!"
-- Just ("orld, hello!",'w')
--
-- >>> parse (int <|> empty) "world"
-- Nothing

-- newtype Parser a = Parser (String -> Maybe (String, a)) REMEMBER
instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const Nothing -- This function always returns nothing (first arg)

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) p1@(Parser a) p2@(Parser b) = Parser $
    \input -> 
      case a input of
        Nothing -> b input
        Just (str, f)  -> Just (str, f) -- can also just do success -> success as we know anything output that isn't Nothing means it parsed successfully

-- | Recursively parse a string
--  If the string is non-empty ((x:xs)), you need to parse the first character (x) and then continue parsing the rest of the string (xs).
--  You will need to accumulate the parsed characters into a list.
--  For that, consider how you can prepend (:) the parsed character to the result of the recursive string xs call.
-- /Spoiler/: (:) <$> char x ????
-- >>> parse (string "abc") "abcdef"
-- Just ("def","abc")
-- >>> parse (string "abc") "abxdef"
-- Nothing
-- >>> parse (string "") "anyinput"
-- Just ("anyinput","")
string :: String -> Parser String
string ""     = pure ""
string (x:xs) = (:) <$> is x <*> string xs 

-- | Parse one or more spaces
--
-- This parser consumes as many *spaces* as possible. If there are no spaces, it returns an empty string.
-- Hint: What functions do alternative give us
--
-- >>> parse whitespace "   abc"
-- Just ("abc","   ")
-- >>> parse whitespace "abc"
-- Just ("abc","")
-- >>> parse whitespace "    "
-- Just ("","    ")
-- >>> parse whitespace ""
-- Just ("","")
whitespace :: Parser String
whitespace = some (is ' ') <|> pure ""
-- whitespace is very similar to many v but instead of return empty list it returns empty string when failed
-- many v = some v <|> pure []

-- | Parse a URL-like string until a space is encountered
--
-- This parser consumes characters until it finds a space.
-- It's useful for parsing the URL portion of an HTTP request, assuming spaces are not allowed in the URL.
-- Hint: What does alternative give us
--
-- >>> parse parseURL "http://example.com/homepage GET"
-- Just (" GET","http://example.com/homepage")
-- >>> parse parseURL "my-url POST"
-- Just (" POST","my-url")
-- >>> parse parseURL "invalid_url"
-- Just ("","invalid_url")
parseURL :: Parser (String, String)
parseURL = (,) <$> many (isNot ' ') <*> (whitespace *> pure "")


-- | Parse either "GET" or "POST"
--
-- This parser checks if the input starts with "GET" or "POST".
-- It succeeds if either one is matched, returning the rest of the input along with the matched string.
--
-- >>> parse getOrPost "GET /index.html"
-- Just (" /index.html","GET")
-- >>> parse getOrPost "POST /submit-form"
-- Just (" /submit-form","POST")
-- >>> parse getOrPost "PUT /update"
-- Nothing
-- >>> parse getOrPost "post /lowercase"
-- Nothing
getOrPost :: Parser String
getOrPost = (string "GET" <|> string "POST")


-- | Parse a tuple of integers
-- >>> parse parseIntTuple "(3,5)"
-- Just ("",(3,5))   
-- Just ("",(33,5))
-- >>> parse parseIntTuple "(33,5,4)"
-- Nothing
--
parseIntTuple :: Parser (Int, Int)
parseIntTuple = (,) <$> (is '(' *> int) <*> (is ',' *> int <* is ')')

-- | Parse a simple HTTP request method and URL
--
-- This parser expects an HTTP request method (either "GET" or "POST"), followed by some whitespace, then a URL-like string,
-- and finally the protocol which will be assumed to be HTTP/1.1
-- It returns a tuple where the first element is the HTTP method, and the second is the parsed URL.
--
--
-- Try to get some inspiration from parseIntTuple above.
-- >>> parse parseHTTPRequest "GET /index.html HTTP/1.1"
-- Just ("",("GET","/index.html"))
-- >>> parse parseHTTPRequest "POST /submit-form HTTP/1.1"
-- Just ("",("POST","/submit-form"))
-- >>> parse parseHTTPRequest "GET  /home HTTP/1.1"
-- Just ("",("GET","/home"))
-- >>> parse parseHTTPRequest "PUT /update HTTP/1.1"
-- Nothing
-- >>> parse parseHTTPRequest "POST/homepage HTTP/1.1"
-- Just ("",("POST","/homepage"))
parseHTTPRequest :: Parser (String, String)
parseHTTPRequest = undefined
