{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import System.ZMQ4.Monadic

import Control.Concurrent (threadDelay)

main :: IO ()
main = do
    runZMQ $ do
        s <- socket Req
        bind s "ipc:///tmp/node_ipc"
        forever $ do
          send s [] "Hello from Haskell server"
          liftIO $ threadDelay (1 * 1000 * 1000)
          receive s
