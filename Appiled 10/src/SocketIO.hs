module SocketIO (runServer, routeRequest) where

import           Data.List      (isInfixOf)
import           Data.Maybe     (fromMaybe)
import           Instances      (parse)
import           JSON           (JsonValue)
import           Network.Socket
import           SocketParser   (parseHTTPRequest)
import           System.IO
import           Text.Read      (readMaybe)

type RouteFunction = (String, String, JsonValue) -> IO (Maybe String)

-- | Implement the function `logConnectionAccepted`.
-- This function should print a simple message, such as "Connection accepted", to the console.
-- Use `putStrLn` to output the message when a connection is accepted by the server.
-- >>> logConnectionAccepted
-- Connection accepted
logConnectionAccepted :: IO ()
logConnectionAccepted = undefined

-- | Converts a Socket to a Handle for easier IO operations.
-- Implement the function that converts a `Socket` to a `Handle`.
-- HINT: You will need `socketToHandle`.
convertSocketToHandle :: Socket -> IO Handle
convertSocketToHandle conn = undefined

-- | Reads a client request from a Handle.
-- You can use `hGetLine` to read a single line from the provided `Handle`.
readClientRequest :: Handle -> IO String
readClientRequest = undefined

-- | Sends the response back to the client.
-- The `sendResponse` function sends a response to the client via the provided `Handle`.
-- It should use `hPutStr` to write the response string to the handle and `hFlush` to ensure that all the data is sent immediately.
-- After sending the response, it logs "Response sent" to the console for confirmation.
sendResponse :: Handle -> String -> IO ()
sendResponse handle response = undefined

-- | Cleans up by closing the Handle and Socket.
-- The `cleanupConnection` function ensures that resources are properly released after handling the client request.
-- It first should close the `Handle` using `hClose` and then closes the `Socket` using `close`.
-- Properly closing the connection is important to avoid resource leaks and ensure the server runs efficiently.
cleanupConnection :: Handle -> Socket -> IO ()
cleanupConnection handle conn = undefined


-- | Follow the steps below to implement the handleConnection function.
-- /Hint/: this exercise is intended to give you a feel for what "real-world" Haskell
-- code looks like, but you don't have to particularly understand the communication protocol.
-- As far as you need to be concerned, it is just a bunch of calls to the functions above and below (it should be
-- obvious which ones from the comments at each step) as well as simple logic
-- to get the results of the functions (most of which are returned in the IO monad) and
-- chain them together.  Make sure you understand `monad` and `do` block syntax before you attempt.
-- Look carefully at the types of functions to know how to call them and access their results correctly.
handleConnection :: RouteFunction -> Socket -> IO ()
handleConnection f conn = do
    -- Step 1: Log the connection being accepted.
    undefined
    -- Step 2: Convert the socket to a handle.
    undefined
    -- Step 3: Read the headers from the client request.
    headers <- pure [""]
    -- Step 4: Extract the content length from the headers (defaulting to 0 if not present).
    undefined
    -- Step 5: If content length > 0, read the request body; otherwise, set the body to an empty string.
    body <- pure ""
    -- Step 6 (Given): Construct the complete request by concatenating the headers and body.
    let request = head headers ++ (' ':body)
    -- Step 7: Generate a response based on the route function and the complete request.
    undefined
    -- Step 8: Send the generated response back to the client via the handle.
    undefined
    -- Step 9: Clean up by closing the handle and the connection.
    undefined

-- | Server loop to accept and handle incoming connections
-- /Hint/: As above, more simple IO Monad function calls in do-block syntax.
serverLoop :: RouteFunction -> Socket -> IO ()
serverLoop f sock = do
    -- Step 1: Accept a connection from a client.
    -- The `accept` function listens for incoming connections on the socket `sock`.
    undefined
    -- Step 2: Extract the client's socket from the connection tuple.
    undefined
    -- Step 3: Handle the connection with the client.
    undefined
    -- Step 4: Continue the server loop to accept more connections.
    undefined



-- /********************
-- GIVEN FUNCTIONS
-- Please have a look through these, they may be useful.
-- /********************

-- | The `routeRequest` function is a basic router that takes a request type,
-- site path, and arguments, and returns a potential HTTP response.
routeRequest :: RouteFunction
routeRequest  (req, site, args)  =
    case (req, site) of
        ("GET", "/api") -> pure $ Just $ responseWithBody apiReturnValue
        ("POST", "/submit-form") -> pure $ Just $ responseWithBody submitReturnValue
        _                -> pure Nothing
        where
            apiReturnValue = "{\"message\": \"Hello from GET\"}"
            submitReturnValue = "{\"message\": \"Hello from POST\"}"

            responseWithBody body =
                "HTTP/1.1 200 OK\r\n" ++
                "Content-Length: " ++ show (length body) ++ "\r\n" ++
                "Content-Type: application/json\r\n\r\n" ++
                body


-- | The `readHeaders` function reads HTTP headers from a `Handle` until an empty
-- line or carriage return ("\r") is encountered, indicating the end of the headers.
readHeaders :: Handle -> IO [String]
readHeaders handle = collectHeaders []
  where
    collectHeaders :: [String] -> IO [String]
    collectHeaders acc = do
        line <- hGetLine handle
        if null line || line == "\r"
            then return acc
            else collectHeaders (acc ++ [line])

-- | Function to parse the content-length from a single line header
-- >>> getContentLength ["Host: example.com","content-length: 123","Connection: close"]
-- Just 123
--
-- >>> getContentLength ["Host: example.com","Connection: close"]
-- Nothing
--
-- >>> getContentLength ["content-length: 456","Host: example.com","Connection: close"]
-- Just 456
--
-- >>> getContentLength ["Host: example.com","Connection: close","content-length: abc"]
-- Nothing
--
getContentLength :: [String] -> Maybe Int
getContentLength headers = do
    contentLengthLine <- findContentLengthLine headers  -- Using Maybe binding
    let contentLengthStr = dropWhile (== ' ') $ drop 15 contentLengthLine
    readMaybe contentLengthStr

-- | Helper function to find the line containing "content-length"
-- >>> findContentLengthLine ["Host: example.com","content-length: 789","Connection: close"]
-- Just "content-length: 789"
--
-- >>> findContentLengthLine ["Host: example.com","Connection: close"]
-- Nothing
findContentLengthLine :: [String] -> Maybe String
findContentLengthLine headers =
    safeHead $ filter ("content-length:" `isInfixOf`) headers

-- | safe version of the Head function
-- >>> safeHead [1, 2, 3]
-- Just 1
--
-- >>> safeHead ([] :: [Int])
-- Nothing
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x


-- Function to read the body based on the content-length header
readBody :: Handle -> Int -> IO String
readBody handle contentLength = do
    contents <- hGetContents handle
    return (take contentLength contents)

-- | Helper function to remove unnecessary characters like newlines and carriage returns
-- >>> cleanRequest "   /hello/world  "
-- "/hello/world"
cleanRequest :: String -> String
cleanRequest = unwords . words

-- | The 'generateResponse' function takes a 'RouteFunction' and a raw request
--   string as input, processes the request, and generates a corresponding response.
--   The process includes cleaning the request, parsing it, and then using the
--   provided 'RouteFunction' to determine the appropriate response.
--
--   If the request cannot be parsed, the function returns a predefined bad request
--   response. If the 'RouteFunction' successfully generates a response, it returns
--   that response; otherwise, it defaults to the bad request response.
generateResponse :: RouteFunction -> String -> IO String
generateResponse f request = do
    let cleanedRequest = cleanRequest request

    maybeResponse <- case parseRequest cleanedRequest of
        Nothing     -> return Nothing
        Just parsed -> f parsed

    let response = fromMaybe badRequest maybeResponse

    return response


-- Default bad request response
badRequest :: String
badRequest = "HTTP/1.1 400 Bad Request\r\n\r\n"

-- The `parseRequest` function is responsible for parsing an incoming request string.
parseRequest :: String -> Maybe (String, String, JsonValue)
parseRequest = (snd <$>) . parse parseHTTPRequest -- Simplified, replace with actual parsing logic

-- | Main server loop to initialize and run the server.
-- This function sets up the socket, binds it to an address, and starts listening for incoming connections.
-- It will loop indefinitely, handling one connection at a time.
runServer :: RouteFunction -> IO ()
runServer f = withSocketsDo $ do
    addrInfo <- getAddrInfo (Just defaultHints { addrFlags = [AI_PASSIVE] }) Nothing (Just "3000")
    let serverAddr = head addrInfo
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    bind sock (addrAddress serverAddr)
    listen sock 1
    putStrLn "Server listening on port 3000"
    serverLoop f sock
