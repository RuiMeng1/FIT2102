module SocketParser () where

import           Control.Applicative (Alternative (many, some, (<|>)),
                                      Applicative (liftA2))
import           Instances           (Parser (..), int, parse)
import           JSON                (JsonValue (JNull), json, sepBy, stringTok,
                                      tok)
import           Parser              (is, isNot, spaces, string)


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
parseURL :: Parser String
parseURL = many $ isNot ' '

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
getOrPost = string "GET" <|> string "POST"

-- | Parse an HTTP request, with optional arguments as JSON.
-- Consuming any trailing whitespace
--
-- >>> parse parseHTTPRequest "POST /submit-form HTTP/1.1 {\"key1\": true, \"key2\": [1, 2, 3]}   "
-- Just ("",("POST","/submit-form",JObject [("key1",JTrue),("key2",JArray [JInteger 1,JInteger 2,JInteger 3])]))
-- >>> parse parseHTTPRequest "POST /submit-form HTTP/1.1 {\"key1\": null}"
-- Just ("",("POST","/submit-form",JObject [("key1",JNull)]))
-- >>> parse parseHTTPRequest "GET /index.html HTTP/1.1"
-- Just ("",("GET","/index.html",JNull))
parseHTTPRequest :: Parser (String, String, JsonValue)
parseHTTPRequest =  (,,)    <$> (getOrPost <* spaces)
                            <*> (parseURL <* spaces <* stringTok "HTTP/1.1" <* spaces)
                            <*> (json <|> pure JNull) <* spaces
