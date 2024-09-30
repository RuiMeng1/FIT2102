module Main (main) where

import           SocketIO (routeRequest, runServer)

main :: IO ()
main = runServer routeRequest
