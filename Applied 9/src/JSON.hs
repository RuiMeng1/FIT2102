-- | Implement a limited JSON parser.
--
-- Use the provided JsonValue type to create a parser for JSON values.
-- A good starting point is to consider how to parse the literal types
-- such as JTrue, JFalse, JNull, JInteger, and JString.
--
-- Make sure to refer to Instances.hs and Parser.hs for utility functions,
-- parsers, and parser combinators to help you.
--
-- **Reminders**
-- You must NOT manually construct the Parser type using the Parser constructor,
-- nor use the `parse` function to chain together parsers. You must also NOT
-- use the `reads` function or equivalent. The cases where this is the
-- intended solution have already been provided to you.
--
-- The README contains a list of useful functions and operators worth looking at.
--
-- see https://tgdwyer.github.io/parsercombinators/#a-parser-that-returns-an-adt
module JSON (json, sepBy, JsonValue (JNull), tok, stringTok) where

import           Control.Applicative (Alternative (many, (<|>)),
                                      Applicative (liftA2))
import           Data.Foldable       (asum)
import           Instances           (Parser (..), int, parse)
import           Parser              (is, isNot, spaces, string)

type KeyVal = (String, JsonValue)

-- | JSON value representation.
data JsonValue
  = JInteger Int
  | JString String
  | JObject [KeyVal]
  | JArray [JsonValue]
  | JTrue
  | JFalse
  | JNull
  deriving (Show, Eq)

-- | Write a function that applies the given parser, then
--  parse a space character if it exists.

-- >>> parse (tok (is 'a')) "a bc"
-- Just ("bc",'a')

-- >>> parse (tok (is 'a')) "abc"
-- Just ("bc",'a')
tok :: Parser a -> Parser a
tok a = a <* spaces

-- | Write a function that parses the given char followed by 0 or more spaces.
--
-- /Hint/: Remember the `is` parser
--
-- >>> parse (isTok 'a') "abc"
-- Just ("bc",'a')
--
-- >>> parse (isTok 'a') "dabc"
-- Nothing
isTok :: Char -> Parser Char
isTok a = is a <* spaces

-- | Write a function that parses a comma followed by 0 or more spaces.
--
-- /Hint/: Remember the `is` parser
--
-- >>> parse commaTok ", bc"
-- Just ("bc",',')
--
-- >>> parse commaTok "dabc"
-- Nothing
commaTok :: Parser Char
commaTok = is ',' <* spaces

-- | Write a function that parses the given string, followed by 0 or more
-- spaces.
--
-- /Hint/: Remember the `string` parser
--
-- >>> parse (stringTok "abc") "abc  "
-- Just ("","abc")
--
-- >>> parse (stringTok "abc") "bc  "
-- Nothing
stringTok :: String -> Parser String
stringTok a = string a <* spaces

-- | Parse a JSON integer.
--
-- >>> parse jsonInteger "234"
-- Just ("",JInteger 234)
--
-- >>> parse jsonInteger "-234"
-- Just ("",JInteger (-234))
--
-- >>> parse jsonInteger "-123.45"
-- Nothing
--
-- >>> parse jsonInteger "-"
-- Nothing
--
-- >>> parse jsonInteger "abc"
-- Nothing
jsonInteger :: Parser JsonValue
jsonInteger = JInteger <$> int 

-- | Parse a JSON true literal.
-- /Hint/ Useful function PCQ=
-- 5 <$ [1, 2, 3]  -- Replaces every element in the list with 5
-- Result: [5, 5, 5]

-- >>> parse jsonTrue "true"
-- Just ("",JTrue)
--
-- >>> parse jsonTrue "TRUE"
-- Nothing
jsonTrue :: Parser JsonValue
jsonTrue = JTrue <$ string "true"

-- | Parse a JSON false literal.
--
-- >>> parse jsonFalse "false"
-- Just ("",JFalse)
--
-- >>> parse jsonFalse "FALSE"
-- Nothing

jsonFalse :: Parser JsonValue
jsonFalse = JFalse <$ string "false"

-- | Parse a JSON false boolean.
--
-- >>> parse jsonBool "false"
-- Just ("",JFalse)
--
-- >>> parse jsonBool "true"
-- Just ("",JTrue)
--
-- >>> parse jsonBool "TRUE"
-- Nothing
jsonBool :: Parser JsonValue
jsonBool = jsonTrue <|> jsonFalse

-- | Parse a JSON null literal.
--
-- >>> parse jsonNull "null"
-- Just ("",JNull)
--
-- >>> parse jsonNull "NULL"
-- Nothing
jsonNull :: Parser JsonValue
jsonNull = JNull <$ string "null"

-- | Parse a sequence of at least one values with a separator.
--
-- >>> parse ((isTok 'a') `sepBy1` commaTok) ""
-- Nothing
--
-- >>> parse ((isTok 'a') `sepBy1` commaTok) "a"
-- Just ("","a")
--
-- >>> parse ((isTok 'a') `sepBy1` commaTok) "a,a"
-- Just ("","aa")
--
-- >>> parse ((tok int) `sepBy1` commaTok) "1,2,3"
-- Just ("",[1,2,3])

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 a b = a <:> many (b *> a)
  where
    -- (Optional) cons the results of two parsers
    (<:>) :: Parser a -> Parser [a] -> Parser [a]
    (<:>) = liftA2 (:)

-- | Parse a sequence of values with a separator.
--
-- >>> parse ((isTok 'a') `sepBy` commaTok) ""
-- Just ("","")
-- >>> parse ((isTok 'a') `sepBy` commaTok) "a"
-- Just ("","a")
-- >>> parse ((isTok 'a') `sepBy` commaT›
-- Just ("","aa")
-- >>> parse ((tok int) `sepBy` commaTok) "1,2,3"
-- Just ("",[1,2,3])

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy a b = sepBy1 a b <|> pure []

-- | A quoteString is any series of any non-" characters surrounded by " "
--
-- >>> parse quoteString "\" abc\""
-- Just (""," abc")
--
-- >>> parse quoteString "\"abc\"def"
-- Just ("def","abc")
--
-- >>> parse quoteString "abc"
-- Nothing
--
-- >>> parse quoteString "\"\\abc\"def"
-- Just ("def","\\abc")
quoteString :: Parser String
quoteString = is '\"' *> many (isNot '\"') <* is '\"' <* spaces

-- | Parse a JSON string. Handle double-quotes.
--
-- >>> parse jsonString "\" abc\""
-- Just ("",JString " abc")
--
-- >>> parse jsonString "\"abc\"def"
-- Just ("def",JString "abc")
--
-- >>> parse jsonString "abc"
-- Nothing
--
-- >>> parse jsonString "\"\\abc\"def"
-- Just ("def",JString "\\abc")
jsonString :: Parser JsonValue
jsonString = JString <$> quoteString 

-- Parses jsonValue
jsonValue :: Parser JsonValue -- tries parsing a jsonValue
jsonValue = tok (asum [jsonNull, jsonBool, jsonString, jsonInteger, jsonArray, jsonObject]) -- recursive when it parses another array or object


-- | Parse a JSON array.
--
-- /Hint/: Remember the type [JsonValue] means a list of JsonValues, and the
--  parser for JsonValue is called jsonValue.
--
-- /Hint 2/: This parser is co-recursive!
--  To test this parser, you will need to implement jsonValue as well.
--
-- see https://tgdwyer.github.io/parsercombinators/#creating-a-parse-tree
--
-- >>> parse jsonArray "[]"
-- Just ("",JArray [])
--
-- >>> parse jsonArray "[true]"
-- Just ("",JArray [JTrue])
--
-- >>> parse jsonArray "[true, 5, []]"
-- Just ("",JArray [JTrue,JInteger 5,JArray []])

-- | Parse a JSON array.
jsonArray :: Parser JsonValue
jsonArray = JArray <$> (isTok '[' *> (jsonValue `sepBy` commaTok) <* isTok ']') -- the centre seperates the objects inside by commas and tries to parse a JSON value for each

-- | parses ':'
colonTok:: Parser Char
colonTok = is ':' <* spaces

-- | Parse a JSON object.
--
-- /Hint/: Remember the type KeyVal = [(String, JsonValue)].
--
-- /Hint 2/: This parser is also co-recursive!
--  To test this parser, you will need to implement jsonValue as well.
--
-- >>> parse jsonObject "{}"
-- Just ("",JObject [])
--
-- >>> parse jsonObject "{ \"key1\" : true }"
-- Just ("",JObject [("key1",JTrue)])
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : false }"
-- Just ("",JObject [("key1",JTrue),("key2",JFalse)])
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : false } xyz"
-- Just ("xyz",JObject [("key1",JTrue),("key2",JFalse)])

-- | Parse a JSON object.


jsonObject :: Parser JsonValue
jsonObject = JObject <$>  (isTok '{' *> (keyVal `sepBy` commaTok) <* isTok '}')
  where
    -- input for keyVal is:
    -- "\"key1\"" and "true"
    keyVal = (,) <$> (quoteString <* colonTok) <*> jsonValue
    -- NOTE: i had to remodify quoteString to remove any spaces at the end as it was causing parsing error





-- | Parse a JSON value
-- Either a Boolean, Integer, String, Null
--
-- >>> parse jsonVal "\" abc\""
-- Just ("",JString " abc")
--
-- >>> parse jsonVal "false"
-- Just ("",JFalse)
--
-- >>> parse jsonVal "234"
-- Just ("",JInteger 234)
--
-- >>> parse jsonVal "null"
-- Just ("",JNull)
jsonVal :: Parser JsonValue
jsonVal = jsonBool <|> jsonInteger <|> jsonString <|> jsonNull

-- | Parse a JSON container, either an array or an object.
--
-- >>> parse jsonContainer "[true, 5, []]"
-- Just ("",JArray [JTrue,JInteger 5,JArray []])
--
-- >>> parse jsonContainer "{ \"key1\" : true , \"key2\" : false } xyz"
-- Just ("xyz",JObject [("key1",JTrue),("key2",JFalse)])
jsonContainer :: Parser JsonValue
jsonContainer = jsonArray <|> jsonObject

-- | Parse a JSON string.
--
-- /Hint/: Use the parsers we have just made for each component of the JSON
--  type.
--
--
-- >>> parse json "true"
-- Just ("",JTrue)
--
-- >>> parse json "{ \"key1\" : true , \"key2\" : [7, false] }"
-- Just ("",JObject [("key1",JTrue),("key2",JArray [JInteger 7,JFalse])])
--
-- >>> parse json "{ \"key1\" : true , \"key2\" : [7, false] , \"key3\" : { \"key4\" : null } }"
-- Just ("",JObject [("key1",JTrue),("key2",JArray [JInteger 7,JFalse]),("key3",JObject [("key4",JNull)])])
json :: Parser JsonValue
json = jsonVal <|> jsonContainer
