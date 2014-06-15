-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Route
    ( Handler
    , route
    ) where

import Data.ByteString (ByteString)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Route.Tree
import Prelude hiding (lookup)

import qualified Data.ByteString.Lazy as L

-- | A 'Handler' is a generalized 'Application' that receives the captured
-- path parameters as its first argument.
type Handler m = [(ByteString, ByteString)]        -- ^ The captured path parameters.
               -> Request                          -- ^ The matched 'Request'.
               -> (Response -> m ResponseReceived) -- ^ The continuation.
               -> m ResponseReceived

-- | Routes requests to 'Handler's according to a routing table.
route :: Monad m
      => [(ByteString, Handler m)]
      -> Request
      -> (Response -> m ResponseReceived)
      -> m ResponseReceived
route rs rq k = case lookup (fromList rs) path of
    Just (f, c) -> f c rq k
    Nothing     -> k notFound
  where
    path     = segments (rawPathInfo rq)
    notFound = responseLBS status404 [] L.empty
