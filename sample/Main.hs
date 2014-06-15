{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Route (route)

import qualified Data.ByteString.Lazy as L

main :: IO ()
main = run 4242 $
    route [ ("/foo", fooHandler)
          , ("/foo/bar", barHandler)
          , ("/foo/:bar/:baz", bazHandler)
          ]
  where
    fooHandler _ _  k = k $ responseLBS status200 [] "foo!"
    barHandler _ _  k = k $ responseLBS status200 [] "bar!"
    bazHandler p rq k = do
        print $ "pathInfo: " ++ show (pathInfo rq)
        print $ "captured: " ++ show p
        k $ responseLBS status200 []
          $ maybe L.empty L.fromStrict (lookup "baz" p)
