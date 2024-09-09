-- | Functions that take a string and convert it to a value. For now
-- we will only consider these functions, but in future weeks we will
-- see how these can be much more useful.
--
-- THERE IS USEFUL INFORMATION IN THE README ABOUT THE PARSING FUNCTION TYPE.
module Parser (parseHTTPRequest) where

-- | Parse a single character
--
-- >>> char "abc"
-- Just ("bc",'a')
--
-- >>> char ""
-- Nothing
char :: String -> Maybe (String, Char)
char (x : xs) = Just (xs, x)
char _        = Nothing

-- | Parse numbers as int until non-digit
--
-- We use the `reads` function to parse the string as an Int, and convert
-- the result to our desired type. This is mainly for convenience.
--
-- Do NOT use reads elsewhere unless explicitly approved or provided.
--
-- >>> int "123abc"
-- Just ("abc",123)
--
-- >>> int "abc"
-- Nothing
int :: String -> Maybe (String, Int)
int s = case (reads s :: [(Int, String)]) of
  [(x, rest)] -> Just (rest, x)
  _           -> Nothing

-- | Parses a specific character, otherwise return Nothing
--
-- /Hint/: What form of pattern matching can we use to values we want?
--
-- /Hint 2/: What can we use the `char` function for?
--
-- /Optional/: Use guards to simplify the validation expression.
--
-- >>> (is 'c') "cba"
-- Just ("ba",'c')
--
-- >>> (is 'c') "abc"
-- Nothing
is :: Char -> String -> Maybe (String, Char)
is c s = 
  case char s of
    Just (rest, x) -> if x == c
                        then Just (rest, x)
                        else Nothing
    Nothing -> Nothing

-- | The 'matches' function checks if two strings match character by character.
-- If the characters match up to the length of the first string, it returns 'True'.
-- Otherwise, it returns 'False'.
-- /Hint/ use zipWith
-- >>> matches "GET" "GET /index.html"
-- True
--
-- >>> matches "HELLO" "HELLO WORLD"
-- True
--
-- >>> matches "HELLO" "WORLD"
-- False
--
-- >>> matches "" "ANYTHING"
-- True
--
-- >>> matches "SOMETHING" ""
-- False
matches :: String -> String -> Bool
matches [] _ = True
matches _ [] = False
matches a b = all id $ zipWith (==) a b



-- | Parse a string exactly from the input.
-- /Hint/ the drop function might be helpful
-- >>> string "GET" "GET /index.html HTTP/1.1"
-- Just (" /index.html HTTP/1.1","GET")
--
-- >>> string "HELLO" "HELLO WORLD"
-- Just (" WORLD","HELLO")
--
-- >>> string "HELLO" "WORLD"
-- Nothing
string :: String -> String -> Maybe (String, String)
string "" s = Just (s, "")
string expected input =
  if matches expected input
  then Just (drop (length expected) input, expected)
  else Nothing

-- | Parse the string GET
-- >>> parseGET "GET /index.html HTTP/1.1"
-- Just (" /index.html HTTP/1.1","GET")
--
-- >>> parseGET "POST /index.html HTTP/1.1"
-- Nothing
parseGET :: String -> Maybe (String, String)
parseGET = string "GET"

-- | Parse whitespace function checks if the input string starts with either a space (' ') or a tab ('\t').
-- If it does, it removes all of the leading whitespace and returns the rest of the string inside a `Just`.
-- Otherwise, it returns `Nothing`.
-- We gave you the solution to this, but, feel free to try and understand it.
--
-- >>> whitespace " hello"
-- Just ("hello",())
--
-- >>> whitespace "\tworld"
-- Just ("world",())
--
-- >>> whitespace "goodbye"
-- Nothing
--
-- >>> whitespace "\t\ttest"
-- Just ("test",())
whitespace :: String -> Maybe (String, ())
whitespace x = case x of
  (' ' : x)  -> whitespace' x
  ('\t' : x) -> whitespace' x
  _          -> Nothing
  where
    whitespace' :: String -> Maybe (String, ())
    whitespace' (' ' : xs) = whitespace' xs
    whitespace' ('\t' : xs) = whitespace' xs
    whitespace' xs = Just (xs, ())

-- | Parses a URL string and extracts the URL and the remaining portion of the string.
--
-- The function takes an input string and splits it into two parts based on the first space character (' '):
-- the URL and the remaining portion (usually the HTTP version). The function returns a `Maybe (String, String)`,
-- where `Nothing` is returned for an empty string, and `Just (remaining, url)` is returned for valid inputs.
--
-- >>> parseURL "/index.html HTTP/1.1"
-- Just (" HTTP/1.1","/index.html")
--
-- >>> parseURL "hello/world HTTP/1.1"
-- Just (" HTTP/1.1","hello/world")
--
-- >>> parseURL ""
-- Nothing
--
-- >>> parseURL " HTTP/1.1"
-- Just (" HTTP/1.1","")
parseURL :: String -> Maybe (String, String)
parseURL [] = Nothing
parseURL s = undefined
  where
    (url, rest) = span (/= ' ') s

-- | Parses HTTP GET requests and returns the remaining part of the string
-- along with the URL.
--
-- The function assumes the input starts with a valid "GET" request followed by
-- some whitespace and a URL.
-- >>> parseHTTPRequest "GET /index.html"
-- Just ("",("GET","/index.html"))
--
-- >>> parseHTTPRequest "GET     /hello/world remaning"
-- Just (" remaning",("GET","/hello/world"))
--
-- >>> parseHTTPRequest "POST /hello/world"
-- Nothing
--
-- >>> parseHTTPRequest "GET"
-- Nothing
parseHTTPRequest :: String -> Maybe (String, (String, String))
parseHTTPRequest input =
  case undefined of
    Just (rest1, _) ->
      case undefined of
        Just (rest2, _) ->
          case undefined of
            Just (rest3, url) -> undefined
            Nothing           -> Nothing
        Nothing -> Nothing
    Nothing -> Nothing
