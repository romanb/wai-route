{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.Encoding (encodeUtf8)
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
    fooHandler _ _  = return $ responseLBS status200 [] "foo!"
    barHandler _ _  = return $ responseLBS status200 [] "bar!"
    bazHandler p rq = do
        print $ "pathInfo: " ++ show (pathInfo rq)
        print $ "captured: " ++ show p
        return $ responseLBS status200 []
               $ maybe L.empty (L.fromStrict . encodeUtf8) (lookup "baz" p)
