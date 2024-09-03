module Pretty (formatOutput) where
import           Data.List (intercalate)

type WebSocketMessage = (String, String)

-- | Function to pretty print a single WebSocket message
-- The indent consists of two spaces per indent level.
-- >>> prettyPrintMessage 0 ("GET", "http://example.com")
-- "type: GET\nlink: http://example.com\n----"
--
-- >>> prettyPrintMessage 1 ("POST", "http://example.com")
-- "  type: POST\n  link: http://example.com\n  ----"
--
-- >>> prettyPrintMessage 2 ("GET", "http://example2.com")
-- "    type: GET\n    link: http://example2.com\n    ----"
--
prettyPrintMessage :: Int -> WebSocketMessage -> String
prettyPrintMessage indentLevel (msgType, url) =
  replicate (2 * indentLevel) ' ' ++ "type: " ++ msgType ++ "\n" ++
  replicate (2 * indentLevel) ' ' ++ "link: " ++ url ++ "\n" ++
  replicate (2 * indentLevel) ' ' ++ "----"

-- | calcIndent function calculates the indentation level based on WebSocket message types.
-- If the previous message, equal the current type, the indent should increase by one.
-- Arguments:
-- (prevType, acc) : A tuple representing the previous message type and a list of indentation levels accumulated so far.
-- (msgType, msg) : A tuple representing the current WebSocket message.
--
-- >>> calcIndent ("GET", [0]) ("GET", "http://example.com")
-- ("GET",[0,1])
--
-- >>> calcIndent ("GET", [0]) ("POST", "http://example.com")
-- ("POST",[0,0])
--
-- >>> calcIndent ("POST", [0,1,2]) ("POST", "http://example.com")
-- ("POST",[0,1,2,3])
--
-- >>> calcIndent ("POST", [0,1,2]) ("GET", "http://example.com")
-- ("GET",[0,1,2,0])

calcIndent :: (String, [Int]) -> WebSocketMessage -> (String, [Int])
calcIndent (prevType, acc) (msgType, _) = undefined


-- | Function to pretty print an array of WebSocket messages with indentation based on message type
-- /Hint/ Use foldl to `reduce` across an accumulator of type  (String, [Int])
-- >>> calculateIndents [("GET", "http://example.com"), ("GET", "http://example2.com"), ("POST", "http://example3.com"), ("POST", "http://example4.com"), ("GET", "http://example5.com")]
-- [0,1,0,1,0]
--
-- >>> calculateIndents [("POST", "http://example.com"), ("POST", "http://example2.com"), ("POST", "http://example3.com"), ("GET", "http://example4.com")]
-- [0,1,2,0]
--
-- >>> calculateIndents [("GET", "http://example.com"), ("POST", "http://example2.com"), ("POST", "http://example3.com"), ("GET", "http://example4.com"), ("GET", "http://example5.com")]
-- [0,0,1,0,1]
--
-- >>> calculateIndents [("GET", "http://example.com")]
-- [0]
calculateIndents :: [WebSocketMessage] -> [Int]
calculateIndents = undefined

-- | formatOutput generates a string that pretty-prints a list of WebSocket messages
-- with appropriate indentation based on message types.
-- >>> formatOutput exampleMessages
-- "type: GET\nlink: /index.html\n----\n  type: GET\n  link: /hello/world\n  ----\n    type: GET\n    link: /\n    ----\ntype: POST\nlink: /submit\n----\ntype: GET\nlink: /contact\n----\n  type: GET\n  link: /about\n  ----\ntype: POST\nlink: /login\n----\n  type: POST\n  link: /logout\n  ----"
formatOutput :: [WebSocketMessage] -> String
formatOutput = undefined

-- Example usage
exampleMessages :: [WebSocketMessage]
exampleMessages =
  [ ("GET", "/index.html")
  , ("GET", "/hello/world")
  , ("GET", "/")
  , ("POST", "/submit")
  , ("GET", "/contact")
  , ("GET", "/about")
  , ("POST", "/login")
  , ("POST", "/logout")
  ]
