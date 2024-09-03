module Main (main) where

import           Parser (parseHTTPRequest)
import           Pretty (formatOutput)

main :: IO ()
main = do
    input <- getLine
    case parseHTTPRequest input of
        Just (_, (reqType, url)) -> putStrLn (formatOutput [(reqType, url)])
        Nothing                  -> putStrLn "Failed to parse request"
    main  -- Loops back to get more input
